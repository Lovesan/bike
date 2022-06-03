// ReSharper disable RedundantUsingDirective
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
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
// ReSharper disable once RedundantUsingDirective
using System.Threading.Tasks;
using System.Transactions;

// ReSharper disable MemberCanBePrivate.Global

// ReSharper disable UnusedMember.Global

#if ENABLE_TASK_HACK
#warning Enabling task hack
#endif

namespace BikeInterop
{
    public static class Externals
    {
#if ENABLE_TASK_HACK
        public const bool HasTaskHack = true;
#else
        public const bool HasTaskHack = false;
#endif

        private static readonly object[] EmptyArray = new object[0];

        private static readonly Type[] EmptyTypeArray = new Type[0];

        private static readonly Logger Log = Logger.Get(typeof(Externals));


        /// <summary>
        /// Linux(Unix?) specific
        /// </summary>
        public static void InitializeCoreFxSignals()
        {
            var processType = Assembly.Load("System.Diagnostics.Process")
                .GetType("System.Diagnostics.Process", false, true);
            var processEnsureSigChildHandlerAction =
                processType
                    ?.GetMethod("EnsureSigChildHandler",
                        BindingFlags.Static | BindingFlags.NonPublic)
                    ?.CreateDelegate(typeof(Action)) as Action;

            var processEnsureInitializedAction =
                processType
                    ?.GetMethod("EnsureInitialized",
                        BindingFlags.Static | BindingFlags.NonPublic)
                    ?.CreateDelegate(typeof(Action)) as Action;

            var consolePal = Assembly.Load("System.Console")
                .GetType("System.ConsolePal", false, true);
            var ensureConsoleInitializedAction =
                consolePal?.GetMethod("EnsureConsoleInitialized", BindingFlags.Static | BindingFlags.NonPublic)
                    ?.CreateDelegate(typeof(Action)) as Action;

            processEnsureSigChildHandlerAction?.Invoke();
            processEnsureInitializedAction?.Invoke();
            ensureConsoleInitializedAction?.Invoke();
        }

        public static void GetLoadedAssemblies(
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            Exception e = null;
            object assemblies = null;
#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                assemblies = AppDomain.CurrentDomain.GetAssemblies();
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif
            result = BoxObject(assemblies);
            typeCode = assemblies.GetFullTypeCode();
            exception = BoxObject(e);
        }

        public static void InstallCallbacks(
            IntPtr freeLispHandleCallback,
            IntPtr applyCallback,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            Exception e = null;
#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                LispObject.InstallCallbacks(
                    freeLispHandleCallback,
                    applyCallback);
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            if (e != null)
                exception = BoxObject(e);
        }

        public static void GetTypeByName(
            [MarshalAs(UnmanagedType.LPWStr)] string name,
            bool throwOnError,
            IntPtr assembly,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            typeCode = (int) TypeCode.Empty;
            result = IntPtr.Zero;
            Exception e = null;
            object invocationResult = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var realAssembly = UnboxObject(assembly) as Assembly;
                invocationResult = realAssembly != null
                    ? realAssembly.GetType(name, throwOnError, true)
                    : Type.GetType(name, throwOnError, true);
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            result = BoxObject(invocationResult);
            typeCode = invocationResult.GetFullTypeCode();
            exception = BoxObject(e);
        }

        public static void Invoke(
            IntPtr target,
            bool isStatic,
            [MarshalAs(UnmanagedType.LPWStr)] string methodName,
            IntPtr typeArgs,
            int nTypeArgs,
            IntPtr args,
            int nArgs,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            result = IntPtr.Zero;
            object e = null;
            object invocationResult = null;
            typeCode = (int)TypeCode.Empty;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var instance = isStatic ? null : UnboxObject(target);
                var type = isStatic ? (Type)UnboxObject(target) : instance.GetType();
                var realArgs = nArgs == 0 ? EmptyArray : UnboxArgs(args, nArgs);
                if (nTypeArgs > 0)
                {
                    var realTypeArgs = UnboxTypeArgs(typeArgs, nTypeArgs);
                    var method = type.MakeGenericMethod(methodName, isStatic, realTypeArgs, realArgs);
                    invocationResult = method.Invoke(instance, realArgs);
                }
                else
                {
                    var flags = BindingFlags.Public | BindingFlags.InvokeMethod | BindingFlags.IgnoreCase;
                    flags |= isStatic ? BindingFlags.Static : BindingFlags.Instance;
                    invocationResult = type.InvokeMember(
                        methodName,
                        flags,
                        null,
                        instance,
                        realArgs,
                        CultureInfo.CurrentCulture);
                }
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif
            result = BoxObject(invocationResult);
            typeCode = invocationResult.GetFullTypeCode();
            if (e is LispException lispException)
                e = lispException.Value;
            exception = BoxObject(e);
        }

        public static void GetProperty(
            IntPtr target,
            bool isStatic,
            [MarshalAs(UnmanagedType.LPWStr)] string propertyName,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            result = IntPtr.Zero;
            object e = null;
            object invocationResult = null;
            typeCode = (int)TypeCode.Empty;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var instance = isStatic ? null : UnboxObject(target);
                var type = isStatic ? (Type)UnboxObject(target) : instance.GetType();
                if (type.IsCOMObject)
                {
                    invocationResult = type.InvokeMember(
                        propertyName,
                        BindingFlags.GetProperty | BindingFlags.IgnoreCase,
                        null,
                        instance,
                        EmptyArray,
                        CultureInfo.CurrentCulture
                    );
                }
                else
                {
                    var flags = BindingFlags.Public | BindingFlags.GetProperty | BindingFlags.IgnoreCase;
                    flags |= isStatic ? BindingFlags.Static : BindingFlags.Instance;
                    var property = type.GetProperty(propertyName, flags);
                    if (property == null || property.GetIndexParameters().Length > 0)
                        throw new MissingMemberException(type.FullName, propertyName);
                    invocationResult = property.GetValue(instance);
                }
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif
            result = BoxObject(invocationResult);
            typeCode = invocationResult.GetFullTypeCode();
            if (e is LispException lispException)
                e = lispException.Value;
            exception = BoxObject(e);
        }

        public static void SetProperty(
            IntPtr target,
            bool isStatic,
            [MarshalAs(UnmanagedType.LPWStr)] string propertyName,
            IntPtr value,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            object e = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var instance = isStatic ? null : UnboxObject(target);
                var type = isStatic ? (Type)UnboxObject(target) : instance.GetType();
                var realValue = UnboxObject(value);
                if (type.IsCOMObject)
                {
                    type.InvokeMember(
                        propertyName,
                        BindingFlags.SetProperty | BindingFlags.IgnoreCase,
                        null,
                        instance,
                        new [] {realValue},
                        CultureInfo.CurrentCulture);
                }
                else
                {
                    var flags = BindingFlags.Public | BindingFlags.SetProperty | BindingFlags.IgnoreCase;
                    flags |= isStatic ? BindingFlags.Static : BindingFlags.Instance;
                    var property = type.GetProperty(propertyName, flags);
                    if (property == null || property.GetIndexParameters().Length > 0)
                        throw new MissingMemberException(type.FullName, propertyName);
                    property.SetValue(instance, realValue);
                }
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif
            if (e is LispException lispException)
                e = lispException.Value;
            exception = BoxObject(e);
        }

        public static void GetIndex(
            IntPtr target,
            IntPtr args,
            int nArgs,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            result = IntPtr.Zero;
            object e = null;
            object invocationResult = null;
            typeCode = (int)TypeCode.Empty;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var instance = UnboxObject(target);
                var type = instance.GetType();
                var realArgs = UnboxArgs(args, nArgs);
                if (type.IsCOMObject)
                {
                    invocationResult = type.InvokeMember(
                        string.Empty,
                        BindingFlags.GetProperty,
                        null,
                        instance,
                        realArgs,
                        CultureInfo.CurrentCulture);
                }
                else
                {
                    const BindingFlags flags = BindingFlags.Public |
                                               BindingFlags.GetProperty |
                                               BindingFlags.IgnoreCase |
                                               BindingFlags.Instance;
                    var indexer = type.GetProperties(flags).FirstOrDefault(x => x.GetIndexParameters().Length > 0);
                    if (indexer == null)
                        throw new MissingMemberException($"Indexer not found on {type.FullName}");
                    invocationResult = indexer.GetValue(instance, realArgs);
                }
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif
            result = BoxObject(invocationResult);
            typeCode = invocationResult.GetFullTypeCode();
            if (e is LispException lispException)
                e = lispException.Value;
            exception = BoxObject(e);
        }

        public static void SetIndex(
            IntPtr target,
            IntPtr value,
            IntPtr args,
            int nArgs,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            object e = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var instance = UnboxObject(target);
                var type = instance.GetType();
                if (type.IsCOMObject)
                {
                    var realArgs = new object[nArgs + 1];
                    realArgs[0] = UnboxObject(value);
                    UnboxArgsInto(realArgs, 1, args, nArgs);
                    type.InvokeMember(
                        string.Empty,
                        BindingFlags.SetProperty,
                        null,
                        instance,
                        realArgs,
                        CultureInfo.CurrentCulture);
                }
                else
                {
                    var realArgs = UnboxArgs(args, nArgs);
                    var realValue = UnboxObject(value);
                    const BindingFlags flags = BindingFlags.Public |
                                               BindingFlags.GetProperty |
                                               BindingFlags.IgnoreCase |
                                               BindingFlags.Instance;
                    var indexer = type.GetProperties(flags).FirstOrDefault(x => x.GetIndexParameters().Length > 0);
                    if (indexer == null)
                        throw new MissingMemberException($"Indexer not found on {type.FullName}");
                    indexer.SetValue(instance, realValue, realArgs);
                }
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif
            if (e is LispException lispException)
                e = lispException.Value;
            exception = BoxObject(e);
        }

        public static void GetField(
            IntPtr target,
            bool isStatic,
            [MarshalAs(UnmanagedType.LPWStr)] string fieldName,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            result = IntPtr.Zero;
            Exception e = null;
            object invocationResult = null;
            typeCode = (int)TypeCode.Empty;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var instance = isStatic ? null : UnboxObject(target);
                var type = isStatic ? (Type)UnboxObject(target) : instance.GetType();
                var flags = BindingFlags.GetField | BindingFlags.Public | BindingFlags.IgnoreCase;
                flags |= isStatic ? BindingFlags.Static : BindingFlags.Instance;
                invocationResult = type.InvokeMember(
                    fieldName,
                    flags,
                    null,
                    instance,
                    EmptyArray,
                    CultureInfo.CurrentCulture);
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif
            result = BoxObject(invocationResult);
            typeCode = invocationResult.GetFullTypeCode();
            exception = BoxObject(e);
        }


        public static void SetField(
            IntPtr target,
            bool isStatic,
            [MarshalAs(UnmanagedType.LPWStr)] string fieldName,
            IntPtr value,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            Exception e = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var instance = isStatic ? null : UnboxObject(target);
                var type = isStatic ? (Type) UnboxObject(target) : instance.GetType();
                var valueObject = UnboxObject(value);
                var flags = BindingFlags.SetField | BindingFlags.Public | BindingFlags.IgnoreCase;
                flags |= isStatic ? BindingFlags.Static : BindingFlags.Instance;
                type.InvokeMember(
                    fieldName,
                    flags,
                    null,
                    instance,
                    new [] {valueObject},
                    CultureInfo.CurrentCulture);
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif
            exception = BoxObject(e);
        }

        public static void InvokeConstructor(
            IntPtr type,
            IntPtr args,
            int nArgs,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            result = IntPtr.Zero;
            object e = null;
            object invocationResult = null;
            typeCode = (int)TypeCode.Empty;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var realType = (Type) UnboxObject(type);
                var realArgs = nArgs == 0 ? EmptyArray : UnboxArgs(args, nArgs);
                invocationResult = Activator.CreateInstance(realType, realArgs);
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            result = BoxObject(invocationResult);
            typeCode = invocationResult.GetFullTypeCode();
            if (e is LispException lispException)
                e = lispException.Value;
            exception = BoxObject(e);
        }

        public static void GetDelegateForLispFunction(
            IntPtr function,
            IntPtr delegateType,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            result = IntPtr.Zero;
            typeCode = (int) TypeCode.Empty;
            exception = IntPtr.Zero;
            Exception e = null;
            object invocationResult = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var type = (Type)UnboxObject(delegateType);
                invocationResult = LispObject.Create(function).AsDelegate(type);
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            result = BoxObject(invocationResult);
            typeCode = invocationResult.GetFullTypeCode();
            exception = BoxObject(e);
        }

        public static void GetDelegateTrampoline(
            IntPtr methodInfo,
            IntPtr args,
            int nArgs,
            out IntPtr functionPointer,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            result = IntPtr.Zero;
            typeCode = (int)TypeCode.Empty;
            exception = IntPtr.Zero;
            Exception e = null;
            object invocationResult = null;
            var pointer = IntPtr.Zero;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {

                var methodBase = (MethodBase)UnboxObject(methodInfo);
                var argTypes = nArgs == 0 ? EmptyTypeArray : UnboxTypeArgs(args, nArgs);
                if (methodBase is MethodInfo realMethodInfo)
                {
                    invocationResult = TrampolineCompiler.CompileMethodInfo(
                        realMethodInfo,
                        out var ptr,
                        argTypes);
                    pointer = ptr;
                }
                else
                {
                    var constructorInfo = (ConstructorInfo) methodBase;
                    invocationResult = TrampolineCompiler.CompileConstructor(constructorInfo, out var ptr);
                    pointer = ptr;
                }
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            functionPointer = pointer;
            result = BoxObject(invocationResult);
            typeCode = invocationResult.GetFullTypeCode();
            exception = BoxObject(e);
        }

        public static void GetAccessorTrampolines(
            IntPtr memberInfo,
            int accessorMemberType,
            out IntPtr reader,
            out IntPtr readerPointer,
            out IntPtr writer,
            out IntPtr writerPointer,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            reader = IntPtr.Zero;
            readerPointer = IntPtr.Zero;
            writer = IntPtr.Zero;
            writerPointer = IntPtr.Zero;
            Exception e = null;
            Delegate readerObject = null;
            var readerPtr = IntPtr.Zero;
            Delegate writerObject = null;
            var writerPtr = IntPtr.Zero;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var type = (AccessorMemberTypes) accessorMemberType;
                Delegate rdr;
                IntPtr innerReaderPtr;
                Delegate wtr;
                IntPtr innerWriterPtr;

                switch (type)
                {
                    case AccessorMemberTypes.Field:
                        var fieldInfo = (FieldInfo) UnboxObject(memberInfo);
                        TrampolineCompiler.CompileField(fieldInfo, out rdr, out innerReaderPtr, out wtr, out innerWriterPtr);
                        break;
                    case AccessorMemberTypes.Property:
                        var propertyInfo = (PropertyInfo) UnboxObject(memberInfo);
                        TrampolineCompiler.CompileProperty(propertyInfo, out rdr, out innerReaderPtr, out wtr, out innerWriterPtr);
                        break;
                    case AccessorMemberTypes.Indexer:
                        var indexerInfo = (PropertyInfo) UnboxObject(memberInfo);
                        TrampolineCompiler.CompileIndexer(indexerInfo, out rdr, out innerReaderPtr, out wtr, out innerWriterPtr);
                        break;
                    // ReSharper disable once RedundantCaseLabel
                    case AccessorMemberTypes.Undefined:
                    default:
                        throw new ArgumentException("Invalid accessor member type", nameof(accessorMemberType));
                }

                readerObject = rdr;
                readerPtr = innerReaderPtr;
                writerObject = wtr;
                writerPtr = innerWriterPtr;
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif
            reader = BoxObject(readerObject);
            readerPointer = readerPtr;
            writer = BoxObject(writerObject);
            writerPointer = writerPtr;
            exception = BoxObject(e);
        }

        public static void FreeHandle(IntPtr pointer)
        {
            if (IntPtr.Zero == pointer)
                return;
            var handle = GCHandle.FromIntPtr(pointer);
            handle.Free();
        }

        public static IntPtr BoxBoolean(bool value)
        {
            return BoxObject(value);
        }

        public static IntPtr BoxChar(int value)
        {
            return BoxObject((char) value);
        }

        public static IntPtr BoxUInt8(byte value)
        {
            return BoxObject(value);
        }

        public static IntPtr BoxInt8(sbyte value)
        {
            return BoxObject(value);
        }

        public static IntPtr BoxInt16(short value)
        {
            return BoxObject(value);
        }

        public static IntPtr BoxUInt16(ushort value)
        {
            return BoxObject(value);
        }

        public static IntPtr BoxInt32(int value)
        {
            return BoxObject(value);
        }

        public static IntPtr BoxUInt32(int value)
        {
            return BoxObject(value);
        }

        public static IntPtr BoxInt64(long value)
        {
            return BoxObject(value);
        }

        public static IntPtr BoxUInt64(ulong value)
        {
            return BoxObject(value);
        }

        public static IntPtr BoxIntPtr(IntPtr value)
        {
            return BoxObject(value);
        }

        public static IntPtr BoxSingle(float value)
        {
            return BoxObject(value);
        }

        public static IntPtr BoxDouble(double value)
        {
            return BoxObject(value);
        }

        public static IntPtr BoxString([MarshalAs(UnmanagedType.LPWStr)] string value)
        {
            return BoxObject(value);
        }

        public static IntPtr BoxLispObject(IntPtr handle)
        {
            return BoxObject(LispObject.Create(handle));
        }

        public static bool UnboxBoolean(IntPtr value)
        {
            return (bool) UnboxObject(value);
        }

        public static int UnboxChar(IntPtr value)
        {
            return (char) UnboxObject(value);
        }

        public static sbyte UnboxInt8(IntPtr value)
        {
            return (sbyte) UnboxObject(value);
        }

        public static byte UnboxUInt8(IntPtr value)
        {
            return (byte) UnboxObject(value);
        }

        public static short UnboxInt16(IntPtr value)
        {
            return (short) UnboxObject(value);
        }

        public static ushort UnboxUInt16(IntPtr value)
        {
            return (ushort) UnboxObject(value);
        }

        public static int UnboxInt32(IntPtr value)
        {
            return (int) UnboxObject(value);
        }

        public static uint UnboxUInt32(IntPtr value)
        {
            return (uint) UnboxObject(value);
        }

        public static long UnboxInt64(IntPtr value)
        {
            return (long) UnboxObject(value);
        }

        public static ulong UnboxUInt64(IntPtr value)
        {
            return (ulong) UnboxObject(value);
        }

        public static IntPtr UnboxIntPtr(IntPtr value)
        {
            return (IntPtr) UnboxObject(value);
        }

        public static float UnboxSingle(IntPtr value)
        {
            return (float) UnboxObject(value);
        }

        public static double UnboxDouble(IntPtr value)
        {
            return (double) UnboxObject(value);
        }

        public static IntPtr UnboxLispObject(IntPtr value)
        {
            return ((LispObject) UnboxObject(value)).Handle;
        }

        public static int GetStringLength(IntPtr value)
        {
            return ((string) UnboxObject(value)).Length;
        }

        public static unsafe void UnboxString(IntPtr value, ushort* buf, int size)
        {
            var handle = GCHandle.FromIntPtr(value);
            var str = (string) handle.Target;
            if(str.Length == 0)
                return;
            fixed (char* ptr = str)
            {
                Buffer.MemoryCopy(ptr, buf, size, str.Length * sizeof(char));
            }
        }

        public static bool IsLispObject(IntPtr value)
        {
            if (value == IntPtr.Zero)
                return false;
            return UnboxObject(value) is LispObject;
        }

        public static void PinObject(
            IntPtr obj,
            out IntPtr pointer,
            out IntPtr handle,
            out IntPtr exception)
        {
            pointer = IntPtr.Zero;
            handle = IntPtr.Zero;
            exception = IntPtr.Zero;
            Exception e = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var realObject = UnboxObject(obj);
                var gcHandle = GCHandle.Alloc(realObject, GCHandleType.Pinned);
                pointer = gcHandle.AddrOfPinnedObject();
                handle = GCHandle.ToIntPtr(gcHandle);
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            exception = BoxObject(e);
        }

        public static void MakeArrayOf(
            IntPtr type,
            IntPtr args,
            int nArgs,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            result = IntPtr.Zero;
            typeCode = (int)TypeCode.Empty;
            exception = IntPtr.Zero;
            Exception e = null;
            object invocationResult = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var realType = (Type)UnboxObject(type);
                var indices = UnboxIndexArgs(args, nArgs);
                invocationResult = Array.CreateInstance(realType, indices);
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            typeCode = invocationResult.GetFullTypeCode();
            result = BoxObject(invocationResult);
            exception = BoxObject(e);
        }

        public static void MakeVectorOf(
            IntPtr type,
            int length,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            result = IntPtr.Zero;
            typeCode = (int)TypeCode.Empty;
            exception = IntPtr.Zero;
            Exception e = null;
            object invocationResult = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var realType = (Type) UnboxObject(type);
                invocationResult = Array.CreateInstance(realType, length);
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            typeCode = invocationResult.GetFullTypeCode();
            result = BoxObject(invocationResult);
            exception = BoxObject(e);
        }

        public static void ArrayLength(
            IntPtr array,
            out long result,
            out int typeCode,
            out IntPtr exception)
        {
            result = 0;
            typeCode = (int)TypeCode.Empty;
            exception = IntPtr.Zero;
            Exception e = null;
            long value = 0;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var realArray = (Array)UnboxObject(array);
                value = realArray.LongLength;
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            typeCode = (int)TypeCode.Int64;
            result = value;
            exception = BoxObject(e);
        }

        public static void ArrayGet(
           IntPtr array,
           IntPtr args,
           int nArgs,
           out IntPtr result,
           out int typeCode,
           out IntPtr exception)
        {
            result = IntPtr.Zero;
            typeCode = (int)TypeCode.Empty;
            exception = IntPtr.Zero;
            Exception e = null;
            object invocationResult = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var realArray = (Array)UnboxObject(array);
                var indices = UnboxIndexArgs(args, nArgs);
                invocationResult = realArray.GetValue(indices);
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            result = BoxObject(invocationResult);
            typeCode = invocationResult.GetFullTypeCode();
            exception = BoxObject(e);
        }

        public static void ArraySet(
            IntPtr array,
            IntPtr args,
            int nArgs,
            IntPtr value,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            Exception e = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var realArray = (Array)UnboxObject(array);
                var realValue = UnboxObject(value);
                var indices = UnboxIndexArgs(args, nArgs);
                realArray.SetValue(realValue, indices);
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            exception = BoxObject(e);
        }

        public static void VectorGet(
            IntPtr array,
            long index,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            result = IntPtr.Zero;
            typeCode = (int)TypeCode.Empty;
            exception = IntPtr.Zero;
            Exception e = null;
            object invocationResult = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var realArray = (Array) UnboxObject(array);
                invocationResult = realArray.GetValue(index);
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            result = BoxObject(invocationResult);
            typeCode = invocationResult.GetFullTypeCode();
            exception = BoxObject(e);
        }

        public static void VectorSet(
            IntPtr array,
            long index,
            IntPtr value,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            Exception e = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var realArray = (Array)UnboxObject(array);
                var realValue = UnboxObject(value);
                realArray.SetValue(realValue, index);
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            exception = BoxObject(e);
        }

        public static int GetFullTypeCode(IntPtr value)
        {
            return UnboxObject(value)?.GetFullTypeCode() ?? 0;
        }

        public static IntPtr GetTypeOf(IntPtr value)
        {
            return BoxObject(UnboxObject(value)?.GetType() ?? typeof(object));
        }

        public static IntPtr GetTypeFullName(IntPtr type)
        {
            return BoxObject(((Type) UnboxObject(type)).FullName);
        }

        public static IntPtr EnumToObject(
            IntPtr type,
            long value,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            Exception e = null;
            object invocationResult = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var realType = (Type)UnboxObject(type);
                invocationResult = Enum.ToObject(realType, value);
            }
            catch (TargetInvocationException ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
#if DEBUG
                Log.Exception(ex);
#endif
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            exception = BoxObject(e);
            return BoxObject(invocationResult);
        }

        internal static IntPtr BoxObject(object value)
        {
            if (value == null)
                return IntPtr.Zero;
            var handle = GCHandle.Alloc(value);
            return GCHandle.ToIntPtr(handle);
        }

        internal static object UnboxObject(IntPtr pointer)
        {
            if (IntPtr.Zero == pointer)
                return null;
            var handle = GCHandle.FromIntPtr(pointer);
            return handle.Target;
        }

        private static unsafe object[] UnboxArgs(IntPtr args, int nArgs)
        {
            var realArgs = new object[nArgs];
            var argsPointer = (IntPtr*) args;
            if (argsPointer == null)
                return realArgs;
            for (var i = 0; i < nArgs; ++i)
            {
                realArgs[i] = UnboxObject(argsPointer[i]);
            }

            return realArgs;
        }

        private static unsafe void UnboxArgsInto(object[] result, int startIndex, IntPtr args, int nArgs)
        {
            var argsPointer = (IntPtr*) args;
            if (argsPointer == null)
                return;
            for (var i = 0; i < nArgs; ++i)
            {
                result[i + startIndex] = UnboxObject(argsPointer[i]);
            }
        }

        private static unsafe Type[] UnboxTypeArgs(IntPtr args, int nArgs)
        {
            var types = new Type[nArgs];
            var argsPointer = (IntPtr*) args;
            if (argsPointer == null)
                throw new ArgumentNullException(nameof(args));
            for (var i = 0; i < nArgs; ++i)
            {
                types[i] = (Type) UnboxObject(argsPointer[i]);
            }

            return types;
        }

        private static unsafe long[] UnboxIndexArgs(IntPtr args, int nArgs)
        {
            var indices = new long[nArgs];
            var argsPointer = (long*) args;
            if(argsPointer == null)
                throw new ArgumentNullException(nameof(args));
            for (var i = 0; i < nArgs; ++i)
            {
                indices[i] = argsPointer[i];
            }

            return indices;
        }

        #region Generated delegate types for possibility of hostfxr usage
        // ReSharper disable all
        public delegate void InitializeCoreFxSignals_Delegate();
        public delegate void GetLoadedAssemblies_Delegate(out IntPtr result, out Int32 typeCode, out IntPtr exception);
        public delegate void InstallCallbacks_Delegate(IntPtr freeLispHandleCallback, IntPtr applyCallback, out IntPtr exception);
        public delegate void GetTypeByName_Delegate(String name, Boolean throwOnError, IntPtr assembly, out IntPtr result, out Int32 typeCode, out IntPtr exception);
        public delegate void Invoke_Delegate(IntPtr target, Boolean isStatic, String methodName, IntPtr typeArgs, Int32 nTypeArgs, IntPtr args, Int32 nArgs, out IntPtr result, out Int32 typeCode, out IntPtr exception);
        public delegate void GetProperty_Delegate(IntPtr target, Boolean isStatic, String propertyName, out IntPtr result, out Int32 typeCode, out IntPtr exception);
        public delegate void SetProperty_Delegate(IntPtr target, Boolean isStatic, String propertyName, IntPtr value, out IntPtr exception);
        public delegate void GetIndex_Delegate(IntPtr target, IntPtr args, Int32 nArgs, out IntPtr result, out Int32 typeCode, out IntPtr exception);
        public delegate void SetIndex_Delegate(IntPtr target, IntPtr value, IntPtr args, Int32 nArgs, out IntPtr exception);
        public delegate void GetField_Delegate(IntPtr target, Boolean isStatic, String fieldName, out IntPtr result, out Int32 typeCode, out IntPtr exception);
        public delegate void SetField_Delegate(IntPtr target, Boolean isStatic, String fieldName, IntPtr value, out IntPtr exception);
        public delegate void InvokeConstructor_Delegate(IntPtr type, IntPtr args, Int32 nArgs, out IntPtr result, out Int32 typeCode, out IntPtr exception);
        public delegate void GetDelegateForLispFunction_Delegate(IntPtr function, IntPtr delegateType, out IntPtr result, out Int32 typeCode, out IntPtr exception);
        public delegate void GetDelegateTrampoline_Delegate(IntPtr methodInfo, IntPtr args, Int32 nArgs, out IntPtr functionPointer, out IntPtr result, out Int32 typeCode, out IntPtr exception);
        public delegate void GetAccessorTrampolines_Delegate(IntPtr memberInfo, Int32 accessorMemberType, out IntPtr reader, out IntPtr readerPointer, out IntPtr writer, out IntPtr writerPointer, out IntPtr exception);
        public delegate void FreeHandle_Delegate(IntPtr pointer);
        public delegate IntPtr BoxBoolean_Delegate(Boolean value);
        public delegate IntPtr BoxChar_Delegate(Int32 value);
        public delegate IntPtr BoxUInt8_Delegate(Byte value);
        public delegate IntPtr BoxInt8_Delegate(SByte value);
        public delegate IntPtr BoxInt16_Delegate(Int16 value);
        public delegate IntPtr BoxUInt16_Delegate(UInt16 value);
        public delegate IntPtr BoxInt32_Delegate(Int32 value);
        public delegate IntPtr BoxUInt32_Delegate(Int32 value);
        public delegate IntPtr BoxInt64_Delegate(Int64 value);
        public delegate IntPtr BoxUInt64_Delegate(UInt64 value);
        public delegate IntPtr BoxIntPtr_Delegate(IntPtr value);
        public delegate IntPtr BoxSingle_Delegate(Single value);
        public delegate IntPtr BoxDouble_Delegate(Double value);
        public delegate IntPtr BoxString_Delegate(String value);
        public delegate IntPtr BoxLispObject_Delegate(IntPtr handle);
        public delegate Boolean UnboxBoolean_Delegate(IntPtr value);
        public delegate Int32 UnboxChar_Delegate(IntPtr value);
        public delegate SByte UnboxInt8_Delegate(IntPtr value);
        public delegate Byte UnboxUInt8_Delegate(IntPtr value);
        public delegate Int16 UnboxInt16_Delegate(IntPtr value);
        public delegate UInt16 UnboxUInt16_Delegate(IntPtr value);
        public delegate Int32 UnboxInt32_Delegate(IntPtr value);
        public delegate UInt32 UnboxUInt32_Delegate(IntPtr value);
        public delegate Int64 UnboxInt64_Delegate(IntPtr value);
        public delegate UInt64 UnboxUInt64_Delegate(IntPtr value);
        public delegate IntPtr UnboxIntPtr_Delegate(IntPtr value);
        public delegate Single UnboxSingle_Delegate(IntPtr value);
        public delegate Double UnboxDouble_Delegate(IntPtr value);
        public delegate IntPtr UnboxLispObject_Delegate(IntPtr value);
        public delegate Int32 GetStringLength_Delegate(IntPtr value);
        public unsafe delegate void UnboxString_Delegate(IntPtr value, UInt16* buf, Int32 size);
        public delegate Boolean IsLispObject_Delegate(IntPtr value);
        public delegate void PinObject_Delegate(IntPtr obj, out IntPtr pointer, out IntPtr handle, out IntPtr exception);
        public delegate void MakeArrayOf_Delegate(IntPtr type, IntPtr args, Int32 nArgs, out IntPtr result, out Int32 typeCode, out IntPtr exception);
        public delegate void MakeVectorOf_Delegate(IntPtr type, Int32 length, out IntPtr result, out Int32 typeCode, out IntPtr exception);
        public delegate void ArrayLength_Delegate(IntPtr array, out Int64 result, out Int32 typeCode, out IntPtr exception);
        public delegate void ArrayGet_Delegate(IntPtr array, IntPtr args, Int32 nArgs, out IntPtr result, out Int32 typeCode, out IntPtr exception);
        public delegate void ArraySet_Delegate(IntPtr array, IntPtr args, Int32 nArgs, IntPtr value, out IntPtr exception);
        public delegate void VectorGet_Delegate(IntPtr array, Int64 index, out IntPtr result, out Int32 typeCode, out IntPtr exception);
        public delegate void VectorSet_Delegate(IntPtr array, Int64 index, IntPtr value, out IntPtr exception);
        public delegate Int32 GetFullTypeCode_Delegate(IntPtr value);
        public delegate IntPtr GetTypeOf_Delegate(IntPtr value);
        public delegate IntPtr GetTypeFullName_Delegate(IntPtr type);
        public delegate IntPtr EnumToObject_Delegate(IntPtr type, Int64 value, out IntPtr exception);
        #endregion
    }
}
