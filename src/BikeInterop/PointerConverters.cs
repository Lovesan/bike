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
    /// <summary>
    /// Helper class for pointer conversions
    /// </summary>
    internal static class PointerConverters
    {
        public static unsafe char* ToChar(IntPtr ptr) => (char*)ptr;

        public static unsafe sbyte* ToSByte(IntPtr ptr) => (sbyte*) ptr;

        public static unsafe byte* ToByte(IntPtr ptr) => (byte*)ptr;

        public static unsafe short* ToInt16(IntPtr ptr) => (short*)ptr;

        public static unsafe ushort* ToUInt16(IntPtr ptr) => (ushort*)ptr;

        public static unsafe int* ToInt32(IntPtr ptr) => (int*)ptr;

        public static unsafe uint* ToUInt32(IntPtr ptr) => (uint*)ptr;

        public static unsafe long* ToInt64(IntPtr ptr) => (long*)ptr;

        public static unsafe ulong* ToUInt64(IntPtr ptr) => (ulong*)ptr;

        public static unsafe float* ToSingle(IntPtr ptr) => (float*) ptr;

        public static unsafe double* ToDouble(IntPtr ptr) => (double*)ptr;

        public static unsafe void* ToVoid(IntPtr ptr) => (void*)ptr;

        public static unsafe IntPtr* ToIntPtr(IntPtr ptr) => (IntPtr*)ptr;

        public static unsafe UIntPtr* ToUIntPtr(IntPtr ptr) => (UIntPtr*)ptr;

        public static unsafe IntPtr FromChar(char* ptr) => (IntPtr)ptr;

        public static unsafe IntPtr FromSByte(sbyte* ptr) => (IntPtr)ptr;

        public static unsafe IntPtr FromByte(byte* ptr) => (IntPtr)ptr;

        public static unsafe IntPtr FromInt16(short* ptr) => (IntPtr)ptr;

        public static unsafe IntPtr FromUInt16(ushort* ptr) => (IntPtr)ptr;

        public static unsafe IntPtr FromInt32(int* ptr) => (IntPtr)ptr;

        public static unsafe IntPtr FromUInt32(uint* ptr) => (IntPtr)ptr;

        public static unsafe IntPtr FromInt64(long* ptr) => (IntPtr)ptr;

        public static unsafe IntPtr FromUInt64(ulong* ptr) => (IntPtr)ptr;

        public static unsafe IntPtr FromSingle(float* ptr) => (IntPtr) ptr;

        public static unsafe IntPtr FromDouble(double* ptr) => (IntPtr) ptr;

        public static unsafe IntPtr FromVoid(void* ptr) => (IntPtr)ptr;

        public static unsafe IntPtr FromIntPtr(IntPtr* ptr) => (IntPtr) ptr;

        public static unsafe IntPtr FromUIntPtr(UIntPtr* ptr) => (IntPtr)ptr;
    }
}
