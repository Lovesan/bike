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
using System.Reflection;

namespace BikeInterop
{
    /// <summary>
    /// Helper type extensions
    /// </summary>
    public static class TypeExtensions
    {
        /// <summary>
        /// Tests whether instances of a type could be directly passed to and from Lisp
        /// </summary>
        /// <param name="type">A type</param>
        /// <returns><value>true</value> when a type is directly convertible, otherwise false</returns>
        public static bool IsDirectlyConvertible(this Type type)
        {
            return DirectlyConvertibleTypes.Contains(type);
        }

        /// <summary>
        /// Creates generic method instance, which belongs to the specified type and has
        ///   the specified name
        /// </summary>
        /// <param name="type">A type</param>
        /// <param name="name">Method name</param>
        /// <param name="isStatic">Whether look for a static method</param>
        /// <param name="typeArgs">Generic type parameters</param>
        /// <param name="args">Method arguments</param>
        /// <returns>Generic method</returns>
        public static MethodInfo MakeGenericMethod(this Type type, string name, bool isStatic, Type[] typeArgs, object[] args)
        {
            var flags = BindingFlags.Public | BindingFlags.InvokeMethod | BindingFlags.IgnoreCase;
            flags |= isStatic ? BindingFlags.Static : BindingFlags.Instance;
            var definition = type.GetMethods(flags)
                .FirstOrDefault(methodInfo => methodInfo.IsGenericMethodDefinition &&
                                              NameEquals(methodInfo, name) &&
                                              GenericArgumentsSuit(methodInfo, typeArgs) &&
                                              ParametersSuit(methodInfo, args));
            if(definition == null)
                throw new MissingMethodException(type.FullName, name);
            return definition.MakeGenericMethod(typeArgs);
        }

        private static bool NameEquals(MemberInfo mi, string name)
        {
            return string.Equals(mi.Name, name, StringComparison.OrdinalIgnoreCase);
        }

        private static bool GenericArgumentsSuit(MethodBase mi, IReadOnlyCollection<Type> typeArgs)
        {
            var args = mi.GetGenericArguments();
            return args.Length == typeArgs.Count;
        }

        private static bool ParametersSuit(MethodBase mi, IReadOnlyCollection<object> args)
        {
            var parameters = mi.GetParameters();
            var hasParamsArray = parameters.Any(IsParamsArray);
            var requiredCount = parameters.Count(pi => !pi.IsOptional && !IsParamsArray(pi));
            return parameters.Length == args.Count ||
                   hasParamsArray && args.Count >= requiredCount;
        }

        private static bool IsParamsArray(ParameterInfo pi)
        {
            return pi.GetCustomAttribute<ParamArrayAttribute>() != null;
        }

        private static readonly HashSet<Type> DirectlyConvertibleTypes = new HashSet<Type>
        {
            typeof(char),
            typeof(byte),
            typeof(sbyte),
            typeof(short),
            typeof(ushort),
            typeof(int),
            typeof(uint),
            typeof(long),
            typeof(ulong),
            typeof(float),
            typeof(double),
            typeof(bool),
            typeof(IntPtr)
        };
    }
}
