//// -*- indent-tabs-mode: nil -*-

//// Copyright(C) 2019, Dmitry Ignatiev<lovesan.ru at gmail.com>


// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

// ReSharper disable MemberCanBePrivate.Global

namespace BikeInterop
{
    /// <summary>
    /// Represents a Lisp object
    /// </summary>
    public class LispObject : IDisposable
    {
        private static readonly MethodInfo ExpressionLambdaDefinition =
            typeof(Expression)
                .GetMethods(BindingFlags.Public | BindingFlags.Static)
                .Where(mi => mi.Name == nameof(Expression.Lambda) && mi.IsGenericMethodDefinition)
                .Single(mi =>
                {
                    var args = mi.GetParameters();
                    return args.Length == 3 &&
                           args[0].ParameterType == typeof(Expression) &&
                           args[1].ParameterType == typeof(bool) &&
                           args[2].ParameterType == typeof(ParameterExpression[]);
                });

        private static FreeHandleCallback _freeLispHandle;
        private static ApplyCallback _apply;

        private static readonly IntPtr EclNil = new IntPtr(1);

        private bool _disposed;

        private LispObject(IntPtr handle) => Handle = handle;

        public IntPtr Handle { get; private set; }

        public static LispObject Create(IntPtr handle)
        {
            return handle == IntPtr.Zero ? null : new LispObject(handle);
        }

        ~LispObject()
        {
            Dispose();
        }

        public void Dispose()
        {
            if (_disposed)
                return;
            _freeLispHandle(Handle);
            Handle = IntPtr.Zero;
            _disposed = true;
        }

        internal static void InstallCallbacks(
            IntPtr freeLispHandleCallback,
            IntPtr applyCallback,
            IntPtr eclImportCurrentThreadCallback,
            IntPtr eclReleaseCurrentThreadCallback)
        {
            var freeLispHandle = Marshal.GetDelegateForFunctionPointer<FreeHandleCallback>(freeLispHandleCallback);
            var apply = Marshal.GetDelegateForFunctionPointer<ApplyCallback>(applyCallback);
            if (eclImportCurrentThreadCallback != IntPtr.Zero &&
                eclReleaseCurrentThreadCallback != IntPtr.Zero)
            {
                var eclImportCurrentThread = Marshal.GetDelegateForFunctionPointer<EclImportCurrentThreadCallback>(eclImportCurrentThreadCallback);
                var eclReleaseCurrentThread = Marshal.GetDelegateForFunctionPointer<EclReleaseCurrentThreadCallback>(eclReleaseCurrentThreadCallback);
                _freeLispHandle = (handle) =>
                {
                    var imported = eclImportCurrentThread(EclNil, EclNil);
                    try
                    {
                        freeLispHandle(handle);
                    }
                    finally
                    {
                        if (imported)
                            eclReleaseCurrentThread();
                    }
                };
                _apply = (IntPtr function, IntPtr args, IntPtr codes, int nArgs, out IntPtr exception, out bool dotnetException) =>
                {
                    var imported = eclImportCurrentThread(EclNil, EclNil);
                    try
                    {
                        return apply(function, args, codes, nArgs, out exception, out dotnetException);
                    }
                    finally
                    {
                        if (imported)
                            eclReleaseCurrentThread();
                    }
                };
            }
            else
            {
                _freeLispHandle = freeLispHandle;
                _apply = apply;
            }
        }

        /// <summary>
        /// Compiles delegate trampoline for a lisp object
        /// </summary>
        /// <param name="delegateType">Delegate type</param>
        /// <returns>Compiled delegate</returns>
        public Delegate AsDelegate(Type delegateType)
        {
            var invokeMethod = delegateType.GetMethod(nameof(Action.Invoke));
            if(invokeMethod == null)
                throw new ArgumentException("Not a delegate type", nameof(delegateType));
            var returnType = invokeMethod.ReturnType;
            var parameters = invokeMethod.GetParameters();
            return CompileTrampoline(delegateType, returnType, parameters);
        }

        internal Delegate CompileTrampoline(
            Type delegateType,
            Type returnType,
            IReadOnlyCollection<ParameterInfo> argsInfos)
        {
            var argExpressions = argsInfos.Select(a => Expression.Parameter(a.ParameterType, a.Name)).ToList();
            var arrayVariable = Expression.Variable(typeof(object[]));
            var arrayCreation = Expression.Assign(
                arrayVariable,
                Expression.NewArrayBounds(
                    typeof(object),
                    Expression.Constant(argsInfos.Count)));
            var blockExpressions = new List<Expression> {arrayCreation};
            for (var i = 0; i < argsInfos.Count; ++i)
            {
                blockExpressions.Add(
                    Expression.Assign(
                        Expression.ArrayAccess(
                            arrayVariable,
                            Expression.Constant(i)),
                        Expression.Convert(argExpressions[i], typeof(object))));
            }
            var method = typeof(LispObject).GetMethod(nameof(CallTrampoline), BindingFlags.Instance | BindingFlags.NonPublic);
            var callExpr = Expression.Call(
                Expression.Constant(this),
                // ReSharper disable once AssignNullToNotNullAttribute
                method,
                arrayVariable);
            if (returnType == typeof(void))
            {
                blockExpressions.Add(callExpr);
            }
            else
            {
                blockExpressions.Add(callExpr.Cast(returnType));
            }
            var block = Expression.Block(
                new[] {arrayVariable},
                blockExpressions);
            var lambdaConstructor = ExpressionLambdaDefinition.MakeGenericMethod(delegateType);
            var lambdaExpr = lambdaConstructor.Invoke(
                null,
                new object[] {block, true, argExpressions.ToArray()});
            var lambdaType = typeof(Expression<>).MakeGenericType(delegateType);
            var compiled = lambdaType.InvokeMember(
                nameof(LambdaExpression.Compile),
                BindingFlags.InvokeMethod | BindingFlags.Instance | BindingFlags.Public,
                null,
                lambdaExpr,
                null);
            return (Delegate)compiled;
        }

        // TODO: Handle ref/out/params
        private unsafe object CallTrampoline(object[] args)
        {
            var argCount = args.Length;
            var realArgs = stackalloc IntPtr[argCount];
            var typeCodes = stackalloc int[argCount];
            for (var i = 0; i < argCount; ++i)
            {
                realArgs[i] = Externals.BoxObject(args[i]);
                typeCodes[i] = args[i].GetFullTypeCode();
            }

            var rv = _apply(Handle, (IntPtr) realArgs, (IntPtr)typeCodes, argCount, out var exInfo, out var isDotnetException);
            var ex = exInfo == IntPtr.Zero
                ? null
                : isDotnetException
                    ? new TargetInvocationException((Exception) Externals.UnboxObject(exInfo))
                    : (Exception)new LispException(Create(exInfo));
            object obj = null;
            if (rv != IntPtr.Zero)
            {
                var handle = GCHandle.FromIntPtr(rv);
                obj = handle.Target;
                // ReSharper disable once SwitchStatementMissingSomeCases
                switch (Convert.GetTypeCode(obj))
                {
                    case TypeCode.Empty:
                    case TypeCode.Boolean:
                    case TypeCode.Char:
                    case TypeCode.String:
                    case TypeCode.Double:
                    case TypeCode.Single:
                    case TypeCode.Byte:
                    case TypeCode.SByte:
                    case TypeCode.Int16:
                    case TypeCode.UInt16:
                    case TypeCode.Int32:
                    case TypeCode.UInt32:
                    case TypeCode.Int64:
                    case TypeCode.UInt64:
                        handle.Free();
                        break;
                    /*
                    case TypeCode.DateTime:
                    case TypeCode.DBNull:
                    case TypeCode.Decimal:
                    case TypeCode.Object:
                    default:
                        break;
                    */
                }
            }

            if (ex != null)
                throw ex;
            return obj;
        }
    }
}
