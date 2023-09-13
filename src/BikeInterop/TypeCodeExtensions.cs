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

namespace BikeInterop
{
    public static class TypeCodeExtensions
    {
        /// <summary>
        /// Retrieves an extended type code for an <see cref="obj"/>
        /// </summary>
        /// <param name="obj">Object to get extended type code information about</param>
        /// <returns>Extended type code</returns>
        public static ExtendedTypeCode GetExtendedTypeCode(this object obj)
        {
            if (obj is Enum)
                return ExtendedTypeCode.Enum;
            if (obj is LispObject)
                return ExtendedTypeCode.LispObject;
            if (obj is Type)
                return ExtendedTypeCode.Type;
            if (obj is Delegate)
                return ExtendedTypeCode.Delegate;
            if (obj is Exception)
                return ExtendedTypeCode.Exception;
            if (obj is IntPtr)
                return ExtendedTypeCode.IntPtr;
            return ExtendedTypeCode.Normal;
        }

        /// <summary>
        /// Retrieves merged <see cref="TypeCode"/> and <see cref="ExtendedTypeCode"/>
        ///   for an object
        /// </summary>
        /// <param name="obj">Object</param>
        /// <returns>Low 8 bits are the <see cref="TypeCode"/> and next 8 are <see cref="ExtendedTypeCode"/></returns>
        public static int GetFullTypeCode(this object obj)
        {
            var typeCode = Convert.GetTypeCode(obj);
            var rv = (int) typeCode;
            if (typeCode == TypeCode.Object || rv >= (int)TypeCode.SByte && rv <= (int)TypeCode.UInt64)
            {
                rv |= (int)obj.GetExtendedTypeCode();
            }
            return rv;
        }
    }
}
