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
    /// Lisp callback which applies a Lisp function to the arguments
    /// </summary>
    /// <param name="function">Lisp function handle</param>
    /// <param name="args">Argument array pointer</param>
    /// <param name="typeCodes">Argument type code pointer</param>
    /// <param name="nArgs">Argument count</param>
    /// <param name="releaseRvHandle">Whether to release the handle of the return value object</param>
    /// <param name="exception">An exception object, should one occur</param>
    /// <param name="isDotnetException">Designates whether an exception object comes from .NET</param>
    /// <returns></returns>
    public delegate IntPtr ApplyCallback(IntPtr function, IntPtr args, IntPtr typeCodes, int nArgs, out bool releaseRvHandle, out IntPtr exception, out bool isDotnetException);
}
