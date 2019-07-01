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
using System.Reflection;

namespace BikeInterop
{
    /// <summary>
    /// Represents a situation where <see cref="TrampolineCompiler"/> has
    ///   encountered a ref struct, which it cannot process
    /// </summary>
    public class RefStructParameterException : BikeException
    {
        public RefStructParameterException(MemberInfo memberInfo, Type refStructType)
            : this(null, memberInfo, refStructType)
        { }

        public RefStructParameterException(ParameterInfo parameterInfo, MemberInfo memberInfo, Type refStructType)
            : base("Unable to compile ref struct parameters and return values")
            => (ParameterInfo, MemberInfo, RefStructType) = (parameterInfo, memberInfo, refStructType);

        /// <summary>
        /// An optional parameter which has <see cref="RefStructType"/>
        /// </summary>
        public ParameterInfo ParameterInfo { get; }

        /// <summary>
        /// A member which could not be compiled into trampoline
        /// </summary>
        public MemberInfo MemberInfo { get; }

        /// <summary>
        /// A type of a struct which cannot be used
        /// </summary>
        public Type RefStructType { get; }
    }
}
