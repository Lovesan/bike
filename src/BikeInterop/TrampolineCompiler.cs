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
using System.Runtime.InteropServices;

namespace BikeInterop
{
    /// <summary>
    /// Compiles trampolines for direct calls from lisp
    /// </summary>
    public static class TrampolineCompiler
    {
        /// <summary>
        /// Compiles indexer access trampolines
        /// </summary>
        /// <param name="info">Property info</param>
        /// <param name="reader">Reader delegate</param>
        /// <param name="readerPtr">Unmanaged pointer to reader delegate</param>
        /// <param name="writer">Writer delegate</param>
        /// <param name="writerPtr">Unmanaged pointer to writer delegate</param>
        /// 
        public static void CompileIndexer(
            PropertyInfo info,
            out Delegate reader,
            out IntPtr readerPtr,
            out Delegate writer,
            out IntPtr writerPtr)
        {
            reader = writer = null;
            readerPtr = writerPtr = IntPtr.Zero;
            if (info == null)
                throw new ArgumentException("Empty parameter", nameof(info));
            if (IsByRefLike(info.PropertyType))
                throw new RefStructParameterException(info, info.PropertyType);
            if (!IsIndexerProperty(info))
                throw new ArgumentException("Can only compile indexers this way", nameof(info));

            if (info.CanRead)
            {
                reader = CompileMethodInfo(info.GetMethod, out readerPtr);
            }

            if (info.CanWrite)
            {
                writer = CompileMethodInfo(info.SetMethod, out writerPtr);
            }
        }

        /// <summary>
        /// Compiles property access trampolines
        /// </summary>
        /// <param name="info">Property info</param>
        /// <param name="reader">Reader delegate</param>
        /// <param name="readerPtr">Unmanaged pointer to reader delegate</param>
        /// <param name="writer">Writer delegate</param>
        /// <param name="writerPtr">Unmanaged pointer to writer delegate</param>
        public static void CompileProperty(
            PropertyInfo info,
            out Delegate reader,
            out IntPtr readerPtr,
            out Delegate writer,
            out IntPtr writerPtr)
        {
            reader = writer = null;
            readerPtr = writerPtr = IntPtr.Zero;
            if (info == null)
                throw new ArgumentException("Empty parameter", nameof(info));
            if (IsByRefLike(info.PropertyType))
                throw new RefStructParameterException(info, info.PropertyType);
            if (IsIndexerProperty(info))
                throw new ArgumentException("Cannot compile indexers this way", nameof(info));

            if (info.CanRead)
            {
                reader = CompilePropertyAccessor(info, !info.GetMethod.IsStatic, out readerPtr, true);
            }

            if (info.CanWrite)
            {
                writer = CompilePropertyAccessor(info, !info.SetMethod.IsStatic, out writerPtr, false);
            }
        }

        private static Delegate CompilePropertyAccessor(PropertyInfo info, bool instance, out IntPtr pointer, bool getter)
        {
            pointer = IntPtr.Zero;
            var type = info.PropertyType;
            var directlyConvertible = type.IsDirectlyConvertible();

            // Possible lambda params
            var self = instance ? GetParam<IntPtr>(false, "self") : null;
            var val = getter ? null : GetParam(directlyConvertible && !type.IsPointer ? type : typeof(IntPtr), false, "val");
            var typeCode = getter && !directlyConvertible ? GetParam<int>(true, "tc") : null;
            var ex = GetParam<IntPtr>(true, "ex");
            var lambdaParams = new List<ParameterExpression>
            {
                self,
                val,
                typeCode,
                ex
            }.RemoveNulls();


            // Possible outer block variables
            var exception = GetVariable<object>(false, "exception");
            var invocationResult = getter ? GetVariable(type.IsPointer ? typeof(IntPtr) : type, false, "invocationResult") : null;
            var outerVariables = new List<ParameterExpression>
            {
                exception,
                invocationResult
            }.RemoveNulls();

            // Outer block actions
            var outerExpressions = outerVariables
                .Select(var => var.AssignDefault(var.Type))
                .ToList();
            foreach (var param in lambdaParams)
            {
                if (param.IsByRef)
                {
                    outerExpressions.AddAssignDefault(param, param.Type);
                }
            }

            var selfUnbox = instance ? self.Unbox(info.DeclaringType) : null;
            var accessExpression = getter
                ? Expression.Assign(
                    invocationResult,
                    type.IsPointer ? Expression.Property(selfUnbox, info).ConvertFromPointer() : Expression.Property(selfUnbox, info))
                : Expression.Assign(
                    Expression.Property(selfUnbox, info),
                    directlyConvertible ? (type.IsPointer ? val.ConvertToPointer(type) : val) : val.Unbox(type));

            var innerBlock = Expression.Block(typeof(void), accessExpression);

            var exVar = Expression.Variable(typeof(Exception), "ex");
            var catchBody = Expression.Block(
                typeof(void),
                Expression.Assign(exception, exVar.ConvertExpression<object>()));
            var catchBlock = Expression.Catch(exVar, catchBody);
            var tryCatch = Expression.TryCatch(innerBlock, catchBlock);


            // And try catch block
#if ENABLE_TASK_HACK
            outerExpressions.Add(tryCatch.WrapToTask());
#else
            outerExpressions.Add(tryCatch);
#endif

            outerExpressions.AddAssignBox(
                ex,
                exception.TypeIs<LispException>().Then(
                    exception
                        .ConvertExpression<LispException>()
                        .GetProperty<LispException, object>(nameof(LispException.Value)),
                    exception));

            if (getter)
            {
                if (directlyConvertible)
                {
                    outerExpressions.Add(invocationResult);
                }
                else
                {
                    outerExpressions.AddAssign(typeCode, invocationResult.GetExpressionTypeCode());
                    outerExpressions.AddBox(invocationResult);
                }
            }

            var outerBlock = Expression.Block(
                 getter ? (directlyConvertible && !type.IsPointer ? type : typeof(IntPtr)) : typeof(void),
                outerVariables,
                outerExpressions);
            var lambda = Expression.Lambda(outerBlock, true, lambdaParams);
            var compiled = lambda.Compile(false);
            pointer = Marshal.GetFunctionPointerForDelegate(compiled);
            return compiled;
        }

        /// <summary>
        /// Compiles field access trampolines
        /// </summary>
        /// <param name="info">Field info</param>
        /// <param name="reader">Reader delegate</param>
        /// <param name="readerPtr">Unmanaged pointer to reader delegate</param>
        /// <param name="writer">Writer delegate</param>
        /// <param name="writerPtr">Unmanaged pointer to writer delegate</param>
        public static void CompileField(
            FieldInfo info,
            out Delegate reader,
            out IntPtr readerPtr,
            out Delegate writer,
            out IntPtr writerPtr)
        {
            reader = writer = null;
            readerPtr = writerPtr = IntPtr.Zero;
            if (info == null)
                throw new ArgumentException("Empty parameter", nameof(info));
            if (IsByRefLike(info.FieldType))
                throw new RefStructParameterException(info, info.FieldType);
            reader = CompileFieldAccessor(info, out readerPtr, true);
            if (!info.IsInitOnly && !info.IsLiteral)
            {
                writer = CompileFieldAccessor(info, out writerPtr, false);
            }
        }

        private static Delegate CompileFieldAccessor(FieldInfo info, out IntPtr pointer, bool getter)
        {
            pointer = IntPtr.Zero;
            var type = info.FieldType;
            var instance = !info.IsStatic;
            var directlyConvertible = type.IsDirectlyConvertible();

            // Possible lambda params
            var self = instance ? GetParam<IntPtr>(false, "self") : null;
            var val = getter ? null : GetParam(directlyConvertible && !type.IsPointer ? type : typeof(IntPtr), false, "val");
            var typeCode = getter && !directlyConvertible ? GetParam<int>(true, "tc") : null;
            var ex = GetParam<IntPtr>(true, "ex");
            var lambdaParams = new List<ParameterExpression>
            {
                self,
                val,
                typeCode,
                ex
            }.RemoveNulls();
            

            // Possible outer block variables
            var exception = GetVariable<object>(false, "exception");
            var invocationResult = getter ? GetVariable(type.IsPointer ? typeof(IntPtr) : type, false, "invocationResult") : null;
            var outerVariables = new List<ParameterExpression>
            {
                exception,
                invocationResult
            }.RemoveNulls();

            // Outer block actions
            var outerExpressions = outerVariables
                .Select(var => var.AssignDefault(var.Type))
                .ToList();
            foreach (var param in lambdaParams)
            {
                if (param.IsByRef)
                {
                    outerExpressions.AddAssignDefault(param, param.Type);
                }
            }

            var selfUnbox = instance ? self.Unbox(info.DeclaringType) : null; 
            var accessExpression = getter
                ? Expression.Assign(
                    invocationResult,
                    type.IsPointer ? Expression.Field(selfUnbox, info).ConvertFromPointer() : Expression.Field(selfUnbox, info))
                : Expression.Assign(
                    Expression.Field(selfUnbox, info),
                    directlyConvertible ? (type.IsPointer ? val.ConvertToPointer(type) : val) : val.Unbox(type));

            var innerBlock = Expression.Block(typeof(void), accessExpression);

            var exVar = Expression.Variable(typeof(Exception), "ex");
            var catchBody = Expression.Block(
                typeof(void),
                Expression.Assign(exception, exVar.ConvertExpression<object>()));
            var catchBlock = Expression.Catch(exVar, catchBody);
            var tryCatch = Expression.TryCatch(innerBlock, catchBlock);


            // And try catch block
#if ENABLE_TASK_HACK
            outerExpressions.Add(tryCatch.WrapToTask());
#else
            outerExpressions.Add(tryCatch);
#endif
            
            outerExpressions.AddAssignBox(
                ex,
                exception.TypeIs<LispException>().Then(
                    exception
                        .ConvertExpression<LispException>()
                        .GetProperty<LispException, object>(nameof(LispException.Value)),
                    exception));

            if (getter)
            {
                if (directlyConvertible)
                {
                    outerExpressions.Add(invocationResult);
                }
                else
                {
                    outerExpressions.AddAssign(typeCode, invocationResult.GetExpressionTypeCode());
                    outerExpressions.AddBox(invocationResult);
                }
            }

            var outerBlock = Expression.Block(
                 getter ? (directlyConvertible && !type.IsPointer ? type : typeof(IntPtr)) : typeof(void),
                outerVariables,
                outerExpressions);
            var lambda = Expression.Lambda(outerBlock, true, lambdaParams);
            var compiled = lambda.Compile(false);
            pointer = Marshal.GetFunctionPointerForDelegate(compiled);
            return compiled;
        }

        /// <summary>
        /// Compiles constructor trampoline
        /// </summary>
        /// <param name="info">Method info</param>
        /// <param name="pointer">Unmanaged pointer to delegate</param>
        /// <returns>Compiled trampoline</returns>
        public static Delegate CompileConstructor(ConstructorInfo info, out IntPtr pointer)
        {
            if (info == null)
                throw new ArgumentException("Empty parameter", nameof(info));
            pointer = IntPtr.Zero;
            var parameters = info.GetParameters();
            var refStructParam = parameters.FirstOrDefault(x => IsByRefLike(x.ParameterType));
            if (refStructParam != null)
                throw new RefStructParameterException(refStructParam, info, refStructParam.ParameterType);
            var returnType = info.DeclaringType ?? typeof(object);
            if (IsByRefLike(returnType))
                throw new RefStructParameterException(info, returnType);
            var trampolineParams = new List<ParameterExpression>();
            var outInitializerList = new List<Expression>();
            var refParamList = new List<ParameterExpression>();
            var refTypeCodes = new List<ParameterExpression>();
            var refVariables = new List<ParameterExpression>();
            var refVariableInitializerList = new List<Expression>();
            var internalRefVariables = new List<ParameterExpression>();
            var assignmentsToOuterRef = new List<Expression>();
            var assignmentsToInnerRef = new List<Expression>();
            var methodParameters = new List<Expression>();

            var resultParam = GetParam<IntPtr>(true, "result");
            var typeCodeParam = GetParam<int>(true, "typeCode");
            var exParameter = GetParam<IntPtr>(true, "exception");

            outInitializerList.AddAssignDefault<IntPtr>(exParameter);

            var invocationResultVar = GetVariable(returnType.IsPointer ? typeof(IntPtr) : typeof(object), false, "invocationResult");
            var objectExceptionVar = GetVariable<object>(false, "objectException");

            refVariableInitializerList.AddAssign(objectExceptionVar, GetDefaultExpression<object>());
            outInitializerList.AddAssignDefault<IntPtr>(resultParam);
            outInitializerList.AddAssignDefault<int>(typeCodeParam);
            refVariableInitializerList.AddAssign(
                invocationResultVar,
                GetDefaultExpression(returnType.IsPointer ? typeof(IntPtr) : typeof(object)));

            // process params
            // TODO: handle ByRef pointer parameter types, they are rare but do exist
            foreach (var param in parameters)
            {
                var realType = param.ParameterType;
                var varType = realType.IsByRef ? realType.GetElementType() : realType;
                var directlyConvertible = varType.IsDirectlyConvertible();
                // ReSharper disable once PossibleNullReferenceException
                var trampolineType = directlyConvertible
                    ? (realType.IsPointer ? typeof(IntPtr) : realType)
                    : realType.IsByRef ? typeof(IntPtr).MakeByRefType() : typeof(IntPtr);
                var isByRef = trampolineType.IsByRef;
                var isOut = isByRef && param.IsOut;
                var trampolineParam = GetParam(trampolineType, false, param.Name);
                trampolineParams.Add(trampolineParam);
                if (isByRef)
                {
                    if (!directlyConvertible)
                    {
                        var refTypeCode = GetParam<int>(true);
                        trampolineParams.Add(refTypeCode);
                        refTypeCodes.Add(refTypeCode);
                    }
                    refParamList.Add(trampolineParam);
                    if (isOut)
                    {
                        outInitializerList.AddAssign(
                            trampolineParam,
                             directlyConvertible
                                 ? GetDefaultExpression(varType)
                                 : GetDefaultExpression<IntPtr>());
                    }

                    var refVariable = GetVariable(varType, false);
                    refVariables.Add(refVariable);
                    refVariableInitializerList.AddAssign(
                        refVariable,
                        GetDefaultExpression(varType).ConvertExpression(varType));

                    var internalRefVariable = GetVariable(varType, false);
                    internalRefVariables.Add(internalRefVariable);
                    assignmentsToOuterRef.AddAssign(refVariable, internalRefVariable);

                    if (isOut)
                    {
                        assignmentsToInnerRef.AddAssign(
                            internalRefVariable,
                            GetDefaultExpression(varType).ConvertExpression(varType));
                    }
                    else
                    {
                        if (directlyConvertible)
                        {
                            assignmentsToInnerRef.AddAssign(internalRefVariable, trampolineParam);
                        }
                        else
                        {
                            assignmentsToInnerRef.AddAssignUnbox(
                                internalRefVariable,
                                trampolineParam,
                                varType);
                        }
                    }

                    methodParameters.Add(internalRefVariable);
                }
                else
                {
                    if (directlyConvertible)
                    {
                        // ReSharper disable once PossibleNullReferenceException
                        methodParameters.Add(varType.IsPointer
                            ? trampolineParam.ConvertToPointer(varType)
                            : trampolineParam);
                    }
                    else
                    {
                        methodParameters.AddUnbox(trampolineParam, varType);
                    }
                }
            }
            trampolineParams.Add(resultParam);
            trampolineParams.Add(typeCodeParam);
            trampolineParams.Add(exParameter);

            var outerBlockVariables = new List<ParameterExpression> { objectExceptionVar };
            outerBlockVariables.Add(invocationResultVar);
            outerBlockVariables.AddRange(refVariables);

            var outerBlockExpressions = new List<Expression>();
            outerBlockExpressions.AddRange(outInitializerList);
            outerBlockExpressions.AddRange(refVariableInitializerList);

            // Here goes the call

            var innerBlockExpressions = new List<Expression>();
            innerBlockExpressions.AddRange(assignmentsToInnerRef);
            var callExpression = Expression.New(info, methodParameters);
            if (callExpression.Type.IsPointer)
            {
                innerBlockExpressions.AddAssign(
                    invocationResultVar, callExpression.ConvertFromPointer());
            }
            else
            {
                innerBlockExpressions.AddAssign(
                    invocationResultVar,
                    callExpression.ConvertExpression<object>());
            }
            innerBlockExpressions.AddRange(assignmentsToOuterRef);
            var innerBlock = Expression.Block(typeof(void), internalRefVariables, innerBlockExpressions);

            var exVar = Expression.Variable(typeof(Exception), "ex");
            var catchBody = Expression.Block(
                typeof(void),
                Expression.Assign(objectExceptionVar, exVar.ConvertExpression<object>()));
            var catchBlock = Expression.Catch(exVar, catchBody);
            var tryCatch = Expression.TryCatch(innerBlock, catchBlock);

            // And try catch block
#if ENABLE_TASK_HACK
            outerBlockExpressions.Add(tryCatch.WrapToTask());
#else
            outerBlockExpressions.Add(tryCatch);
#endif

            outerBlockExpressions.AddAssignBox(resultParam, invocationResultVar);
            outerBlockExpressions.AddAssign(typeCodeParam, invocationResultVar.GetExpressionTypeCode());
            outerBlockExpressions.AddAssignBox(
                exParameter,
                objectExceptionVar.TypeIs<LispException>().Then(
                    objectExceptionVar
                        .ConvertExpression<LispException>()
                        .GetProperty<LispException, object>(nameof(LispException.Value)),
                    objectExceptionVar));

            for (int i = 0, j = 0; i < refParamList.Count; ++i)
            {
                var param = refParamList[i];
                var var = refVariables[i];
                var isPrimitive = var.Type.IsDirectlyConvertible();
                if (isPrimitive)
                {
                    outerBlockExpressions.AddAssign(param, var);
                }
                else
                {
                    var tcParam = refTypeCodes[j];
                    outerBlockExpressions.AddAssignBox(param, var.ConvertExpression<object>());
                    outerBlockExpressions.AddAssign(tcParam, var.ConvertExpression<object>().GetExpressionTypeCode());
                    ++j;
                }
            }

            var outerBlock = Expression.Block(typeof(void), outerBlockVariables, outerBlockExpressions);
            var lambdaExpression = Expression.Lambda(outerBlock, true, trampolineParams);
            var compiled = lambdaExpression.Compile(false);
            pointer = Marshal.GetFunctionPointerForDelegate(compiled);
            return compiled;
        }

        /// <summary>
        /// Compiles method trampoline
        /// </summary>
        /// <param name="info">Method info</param>
        /// <param name="pointer">Unmanaged pointer to delegate</param>
        /// <param name="typeArgs">Type arguments for generic types</param>
        /// <returns>Compiled trampoline</returns>
        public static Delegate CompileMethodInfo(MethodInfo info, out IntPtr pointer, params Type[] typeArgs)
        {
            if (info == null)
                throw new ArgumentException("Empty parameter", nameof(info));
            pointer = IntPtr.Zero;
            if (info.IsGenericMethodDefinition)
                info = info.MakeGenericMethod(typeArgs);
            var parameters = info.GetParameters();
            var refStructParam = parameters.FirstOrDefault(x => IsByRefLike(x.ParameterType));
            if(refStructParam != null)
                throw new RefStructParameterException(refStructParam, info, refStructParam.ParameterType);
            var returnType = info.ReturnType;
            if(IsByRefLike(returnType))
                throw new RefStructParameterException(info.ReturnParameter, info, returnType);
            var isVoid = info.ReturnType == typeof(void);
            var isStatic = info.IsStatic;
            var trampolineParams = new List<ParameterExpression>();
            var outInitializerList = new List<Expression>();
            var refParamList = new List<ParameterExpression>();
            var refTypeCodes = new List<ParameterExpression>();
            var refVariables = new List<ParameterExpression>();
            var refVariableInitializerList = new List<Expression>();
            var internalRefVariables = new List<ParameterExpression>();
            var assignmentsToOuterRef = new List<Expression>();
            var assignmentsToInnerRef = new List<Expression>();
            var methodParameters = new List<Expression>();

            var resultParam = isVoid ? null : GetParam<IntPtr>(true, "result");
            var typeCodeParam = isVoid ? null : GetParam<int>(true, "typeCode");
            var exParameter = GetParam<IntPtr>(true, "exception");

            outInitializerList.AddAssignDefault<IntPtr>(exParameter);

            var invocationResultVar = isVoid
                ? null
                : GetVariable(returnType.IsPointer ? typeof(IntPtr) : typeof(object), false, "invocationResult");
            var objectExceptionVar = GetVariable<object>(false, "objectException");

            Expression selfExpression = null;
            if (!isStatic)
            {
                var selfParam = GetParam<IntPtr>(false, "self");
                trampolineParams.Add(selfParam);
                // ReSharper disable once AssignNullToNotNullAttribute
                selfExpression = Expression.Convert(selfParam.Unbox(), info.DeclaringType);
            }

            refVariableInitializerList.AddAssign(objectExceptionVar, GetDefaultExpression<object>());
            if (!isVoid)
            {
                outInitializerList.AddAssignDefault<IntPtr>(resultParam);
                outInitializerList.AddAssignDefault<int>(typeCodeParam);
                refVariableInitializerList.AddAssign(
                    invocationResultVar,
                    GetDefaultExpression(returnType.IsPointer ? typeof(IntPtr) : typeof(object)));
            }

            // process params
            // TODO: handle ByRef pointer parameter types, they are rare but do exist
            foreach (var param in parameters)
            {
                var realType = param.ParameterType;
                var varType = realType.IsByRef ? realType.GetElementType() : realType;
                var directlyConvertible = varType.IsDirectlyConvertible();
                // ReSharper disable once PossibleNullReferenceException
                var trampolineType = directlyConvertible
                    ? (realType.IsPointer ? typeof(IntPtr) :  realType)
                    : realType.IsByRef ? typeof(IntPtr).MakeByRefType() : typeof(IntPtr);
                var isByRef = trampolineType.IsByRef;
                var isOut = isByRef && param.IsOut;
                var trampolineParam = GetParam(trampolineType, false, param.Name);
                trampolineParams.Add(trampolineParam);
                if (isByRef)
                {
                    if (!directlyConvertible)
                    {
                        var refTypeCode = GetParam<int>(true);
                        trampolineParams.Add(refTypeCode);
                        refTypeCodes.Add(refTypeCode);
                    }
                    refParamList.Add(trampolineParam);
                    if (isOut)
                    {
                        outInitializerList.AddAssign(
                            trampolineParam,
                             directlyConvertible
                                 ? GetDefaultExpression(varType)
                                 : GetDefaultExpression<IntPtr>());
                    }

                    var refVariable = GetVariable(varType, false);
                    refVariables.Add(refVariable);
                    refVariableInitializerList.AddAssign(
                        refVariable,
                        GetDefaultExpression(varType).ConvertExpression(varType));

                    var internalRefVariable = GetVariable(varType, false);
                    internalRefVariables.Add(internalRefVariable);
                    assignmentsToOuterRef.AddAssign(refVariable, internalRefVariable);

                    if (isOut)
                    {
                        assignmentsToInnerRef.AddAssign(
                            internalRefVariable,
                            GetDefaultExpression(varType).ConvertExpression(varType));
                    }
                    else
                    {
                        if (directlyConvertible)
                        {
                            assignmentsToInnerRef.AddAssign(internalRefVariable, trampolineParam);
                        }
                        else
                        {
                            assignmentsToInnerRef.AddAssignUnbox(
                                internalRefVariable,
                                trampolineParam,
                                varType);
                        }
                    }

                    methodParameters.Add(internalRefVariable);
                }
                else
                {
                    if (directlyConvertible)
                    {
                        // ReSharper disable once PossibleNullReferenceException
                        methodParameters.Add(varType.IsPointer
                            ? trampolineParam.ConvertToPointer(varType)
                            : trampolineParam);
                    }
                    else
                    {
                        methodParameters.AddUnbox(trampolineParam, varType);
                    }
                }
            }
            if (!isVoid)
            {
                trampolineParams.Add(resultParam);
                trampolineParams.Add(typeCodeParam);
            }
            trampolineParams.Add(exParameter);

            var outerBlockVariables = new List<ParameterExpression> {objectExceptionVar};
            if(!isVoid)
                outerBlockVariables.Add(invocationResultVar);
            outerBlockVariables.AddRange(refVariables);

            var outerBlockExpressions = new List<Expression>();
            outerBlockExpressions.AddRange(outInitializerList);
            outerBlockExpressions.AddRange(refVariableInitializerList);

            // Here goes the call

            var innerBlockExpressions = new List<Expression>();
            innerBlockExpressions.AddRange(assignmentsToInnerRef);
            var callExpression = Expression.Call(selfExpression, info, methodParameters);
            if (!isVoid)
            {
                if (callExpression.Type.IsPointer)
                {
                    innerBlockExpressions.AddAssign(
                        invocationResultVar, callExpression.ConvertFromPointer());
                }
                else
                {
                    innerBlockExpressions.AddAssign(
                        invocationResultVar,
                        callExpression.ConvertExpression<object>());
                }
            }
            else
            {
                innerBlockExpressions.Add(callExpression);
            }
            innerBlockExpressions.AddRange(assignmentsToOuterRef);
            var innerBlock = Expression.Block(typeof(void), internalRefVariables, innerBlockExpressions);

            var exVar = Expression.Variable(typeof(Exception), "ex");
            var catchBody = Expression.Block(
                typeof(void),
                Expression.Assign(objectExceptionVar, exVar.ConvertExpression<object>()));
            var catchBlock = Expression.Catch(exVar, catchBody);
            var tryCatch = Expression.TryCatch(innerBlock, catchBlock);

            // And try catch block
#if ENABLE_TASK_HACK
            outerBlockExpressions.Add(tryCatch.WrapToTask());
#else
            outerBlockExpressions.Add(tryCatch);
#endif


            if (!isVoid)
            {
                outerBlockExpressions.AddAssignBox(resultParam, invocationResultVar);
                outerBlockExpressions.AddAssign(typeCodeParam, invocationResultVar.GetExpressionTypeCode());
            }
            outerBlockExpressions.AddAssignBox(
                exParameter,
                objectExceptionVar.TypeIs<LispException>().Then(
                    objectExceptionVar
                        .ConvertExpression<LispException>()
                        .GetProperty<LispException, object>(nameof(LispException.Value)),
                    objectExceptionVar));

            for (int i = 0, j = 0; i < refParamList.Count; ++i)
            {
                var param = refParamList[i];
                var var = refVariables[i];
                var isPrimitive = var.Type.IsDirectlyConvertible();
                if (isPrimitive)
                {
                    outerBlockExpressions.AddAssign(param, var);
                }
                else
                {
                    var tcParam = refTypeCodes[j];
                    outerBlockExpressions.AddAssignBox(param, var.ConvertExpression<object>());
                    outerBlockExpressions.AddAssign(tcParam, var.ConvertExpression<object>().GetExpressionTypeCode());
                    ++j;
                }
            }

            var outerBlock = Expression.Block(typeof(void), outerBlockVariables, outerBlockExpressions);
            var lambdaExpression = Expression.Lambda(outerBlock, true, trampolineParams);
            var compiled = lambdaExpression.Compile(false);
            pointer = Marshal.GetFunctionPointerForDelegate(compiled);
            return compiled;
        }

        private static ParameterExpression GetVariable<T>(bool byRef, string name = null)
        {
            return GetVariable(typeof(T), byRef, name);
        }

        private static ParameterExpression GetVariable(Type type, bool byRef, string name = null)
        {
            if (byRef)
                type = type.MakeByRefType();
            return string.IsNullOrWhiteSpace(name) ? Expression.Variable(type) : Expression.Variable(type, name);
        }


        private static ParameterExpression GetParam<T>(bool byRef, string name = null)
        {
            return GetParam(typeof(T), byRef, name);
        }

        private static ParameterExpression GetParam(Type type, bool byRef, string name = null)
        {
            if (byRef)
                type = type.MakeByRefType();
            return string.IsNullOrWhiteSpace(name) ? Expression.Parameter(type) : Expression.Parameter(type, name);
        }

        private static ConstantExpression GetDefaultExpression<T>()
        {
            return GetDefaultExpression(typeof(T));
        }

        private static ConstantExpression GetDefaultExpression(Type type)
        {
            return Expression.Constant(type.IsValueType ? Activator.CreateInstance(type) : null);
        }

        private static bool IsIndexerProperty(PropertyInfo property)
        {
            return property.GetIndexParameters().Length > 0;
        }

        private static readonly Func<Type, bool> IsByRefLike;
        
        static TrampolineCompiler()
        {
            var prop = typeof(Type).GetProperty(IsByRefLikeTypePropertyName, typeof(bool));
            if (prop == null)
            {
                // No worries, we are running a .Net Core of low enough version, so
                //   wont' encounter "ref struct"s
                IsByRefLike = _ => false;
            }
            else
            {
                var lambdaArg = Expression.Parameter(typeof(Type), "type");
                var call = Expression.Property(lambdaArg, prop);
                var lambda = Expression.Lambda<Func<Type, bool>>(call, true, lambdaArg);
                IsByRefLike = lambda.Compile(false);
            }
        }

        private const string IsByRefLikeTypePropertyName = "IsByRefLike";
    }
}
