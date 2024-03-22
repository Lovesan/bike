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
using System.Threading;
using System.Threading.Tasks;

namespace BikeInterop
{
    public static class ExpressionExtensions
    {
        public static TryExpression WrapInTryCatch(
            this ParameterExpression outVariable,
            params Expression[] expressions)
        {
            return outVariable.WrapInTryCatch(Enumerable.Empty<ParameterExpression>(), expressions);
        }

        public static TryExpression WrapInTryCatch(
            this ParameterExpression outVariable,
            IEnumerable<ParameterExpression> blockVariables,
            IEnumerable<Expression> expressions)
        {
            var innerBlock = Expression.Block(typeof(void), blockVariables, expressions);
            var tiExVar = Expression.Variable(typeof(TargetInvocationException), "ex");
            var innerExceptionProp = typeof(TargetInvocationException).GetProperty(nameof(TargetInvocationException.InnerException));
            var exVar = Expression.Variable(typeof(Exception), "ex");
            var catchBodyTargetInvocation = Expression.Block(
                typeof(void),
                Expression.Assign(
                    outVariable,
                    // ReSharper disable once AssignNullToNotNullAttribute
                    Expression.Property(tiExVar, innerExceptionProp).ConvertExpression<object>()));
            var catchBodyGeneric = Expression.Block(
                typeof(void),
                Expression.Assign(outVariable, exVar.ConvertExpression<object>()));
            var targetInvocationCatchBlock = Expression.Catch(tiExVar, catchBodyTargetInvocation);
            var genericCatchBlock = Expression.Catch(exVar, catchBodyGeneric);
            return Expression.TryCatch(innerBlock, targetInvocationCatchBlock, genericCatchBlock);
        }

        public static Expression WrapToTask(this Expression expression)
        {
            var lambda = Expression.Lambda(expression, true);
            var factoryProperty = typeof(Task).GetProperty(
                nameof(Task.Factory),
                BindingFlags.Public | BindingFlags.Static | BindingFlags.GetProperty);
            // ReSharper disable once AssignNullToNotNullAttribute
            var factoryExpression = Expression.Property(null, factoryProperty);
            var startNewMethod = typeof(TaskFactory).GetMethod(
                nameof(TaskFactory.StartNew),
                new[] {typeof(Action)});
            // ReSharper disable once AssignNullToNotNullAttribute
            var startNewExpression = Expression.Call(factoryExpression, startNewMethod, lambda);
            var taskArrayExpression = Expression.NewArrayInit(
                typeof(Task),
                startNewExpression);
            var waitAnyMethod = typeof(Task).GetMethod(
                nameof(Task.WaitAny),
                new[] {typeof(Task[])});
            // ReSharper disable once AssignNullToNotNullAttribute
            var waitAnyMethodExpression = Expression.Call(null, waitAnyMethod, taskArrayExpression);
            return waitAnyMethodExpression;
        }

        public static Expression GetExpressionTypeCode(this Expression expression)
        {
            var method = typeof(TypeCodeExtensions)
                .GetMethod(nameof(TypeCodeExtensions.GetFullTypeCode),
                    BindingFlags.Public | BindingFlags.Static | BindingFlags.InvokeMethod);
            // ReSharper disable once AssignNullToNotNullAttribute
            return Expression.Call(null, method, expression.ConvertExpression<object>());
        }

        public static Expression Then(this Expression expression, Expression ifTrue, Expression ifFalse)
        {
            return Expression.Condition(expression, ifTrue, ifFalse);
        }

        public static Expression GetProperty<T, TProperty>(this Expression expression, string propertyName)
        {
            return expression.GetProperty(typeof(T), propertyName, typeof(TProperty));
        }

        public static Expression GetProperty(this Expression expression, Type type, string propertyName, Type propertyType = null)
        {
            var property = type.GetProperty(propertyName);
            if(property == null)
                throw new ArgumentException(nameof(propertyName));
            var resultExpression = Expression.Property(expression, property);
            if (propertyType != null)
                return resultExpression.ConvertExpression(propertyType);
            return resultExpression;
        }

        public static Expression TypeIs<T>(this Expression expression)
        {
            return expression.TypeIs(typeof(T));
        }

        public static Expression TypeIs(this Expression expression, Type type)
        {
            return Expression.TypeIs(expression, type);
        }

        public static Expression Cast(this Expression expression, Type type)
        {
            var def = typeof(TypeCaster).GetMethod(nameof(TypeCaster.Cast));
            // ReSharper disable once PossibleNullReferenceException
            var method = def.MakeGenericMethod(type);
            return Expression.Call(null, method, expression);
        }

        public static Expression ConvertExpression<T>(this Expression expression)
        {
            return expression.ConvertExpression(typeof(T));
        }

        public static Expression ConvertFromPointer(this Expression expression)
        {
            var type = expression.Type;
            var method = typeof(PointerConverters)
                .GetMethods(BindingFlags.Public | BindingFlags.Static)
                .FirstOrDefault(x => x.GetParameters()[0].ParameterType == type);
            if (method == null)
                throw new ArgumentException($"Value of {type} cannot be converted from typed pointer", nameof(type));
            return Expression.Call(null, method, expression);
        }

        public static Expression ConvertToPointer(this Expression expression, Type type)
        {
            var method = typeof(PointerConverters)
                .GetMethods(BindingFlags.Public | BindingFlags.Static)
                .FirstOrDefault(x => x.ReturnType == type);
            if(method == null)
                throw new ArgumentException($"Value of {type} cannot be converted to typed pointer", nameof(type));
            return Expression.Call(null, method, expression);
        }

        public static Expression ConvertExpression(this Expression expression, Type type)
        {
            return type.IsValueType
                ? Expression.Convert(expression, type)
                : Expression.TypeAs(expression, type);
        }

        public static IList<Expression> AddAssign(
            this IList<Expression> list,
            Expression left,
            Expression right)
        {
            list.Add(Expression.Assign(left, right));
            return list;
        }

        public static IList<Expression> AddAssignUnbox(
            this IList<Expression> list,
            Expression left,
            Expression right,
            Type convertTo = null)
        {
            list.AddAssign(left, right.Unbox(convertTo));
            return list;
        }

        public static IList<Expression> AddUnbox(
            this IList<Expression> list,
            Expression expression,
            Type convertTo = null)
        {
            list.Add(expression.Unbox(convertTo));
            return list;
        }

        public static Expression Unbox(this Expression expression, Type convertTo = null)
        {
            var methodInfo = typeof(Externals).GetMethod(
                nameof(Externals.UnboxObject),
                BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.InvokeMethod | BindingFlags.Static);
            // ReSharper disable once AssignNullToNotNullAttribute
            Expression result = Expression.Call(null, methodInfo, expression);
            if (convertTo != null)
                result = result.ConvertExpression(convertTo);
            return result;
        }

        public static IList<Expression> AddAssignBox(
            this IList<Expression> list,
            Expression left,
            Expression right)
        {
            list.Add(Expression.Assign(left, right.Box()));
            return list;
        }

        public static IList<Expression> AddBox(this IList<Expression> list, Expression expression)
        {
            list.Add(expression.Box());
            return list;
        }

        public static Expression Box(this Expression expression)
        {
            var methodInfo = typeof(Externals).GetMethod(
                nameof(Externals.BoxObject),
                BindingFlags.NonPublic | BindingFlags.Public | BindingFlags.InvokeMethod | BindingFlags.Static);
            // ReSharper disable once AssignNullToNotNullAttribute
            var call = Expression.Call(null, methodInfo, expression.ConvertExpression<object>());
            return call;
        }

        public static IList<Expression> AddAssignDefault<T>(
            this IList<Expression> list, Expression expression)
        {
            return list.AddAssignDefault(expression, typeof(T));
        }

        public static IList<Expression> AddAssignDefault(
            this IList<Expression> list, Expression expression, Type type)
        {
            list.Add(expression.AssignDefault(type));
            return list;
        }

        public static Expression AssignDefault<T>(this Expression expression)
        {
            return expression.AssignDefault(typeof(T));
        }

        public static Expression AssignDefault(this Expression expression, Type type)
        {
            return Expression.Assign(expression, GetDefaultExpression(type).ConvertExpression(type));
        }

        public static IList<ParameterExpression> AddParameter<T>(this IList<ParameterExpression> list, string name = null)
        {
            return list.AddParameter(typeof(T), name);
        }

        public static IList<ParameterExpression> AddParameter(this IList<ParameterExpression> list, Type type, string name = null)
        {
            list.Add(string.IsNullOrWhiteSpace(name) ? Expression.Variable(type) : Expression.Variable(type, name));
            return list;
        }

        public static IList<ParameterExpression> AddVariable(this IList<ParameterExpression> list, Type type, string name = null)
        {
            list.Add(string.IsNullOrWhiteSpace(name) ? Expression.Variable(type) : Expression.Variable(type, name));
            return list;
        }

        public static List<TE> RemoveNulls<TE>(this IEnumerable<TE> list)
            where TE : Expression
        {
            return list.Where(x => x != null).ToList();
        }

        private static ConstantExpression GetDefaultExpression<T>()
        {
            return GetDefaultExpression(typeof(T));
        }

        private static ConstantExpression GetDefaultExpression(Type type)
        {
            return Expression.Constant(type.IsValueType ? Activator.CreateInstance(type) : null);
        }
    }
}
