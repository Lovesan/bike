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
using System.IO;
using System.Text;

namespace BikeInterop
{
    /// <summary>
    /// Some dummy logger. Need to use something else in the future.
    /// </summary>
    public class Logger
    {
        private readonly string _source;
        private readonly TextWriter _stderr;

        private Logger(string source)
        {
            _source = source;
            if (Console.IsErrorRedirected)
            {
                _stderr = new StreamWriter(
                    Console.OpenStandardError(),
                    new UTF8Encoding(false,false),
                    4096,
                    true);
            }
            else
            {
                _stderr = Console.Error;
            }
        }

        public void Exception(Exception e)
        {
            _stderr.WriteLine("[{0}] [ERROR] [{1}] {2}\n{3}", _source, e.GetType().FullName, e.Message, e.StackTrace);
        }

        public static Logger Get<T>()
        {
            return Get(typeof(T));
        }

        public static Logger Get(Type type)
        {
            return Get(type.FullName);
        }

        public static Logger Get(string source)
        {
            if (string.IsNullOrWhiteSpace(source))
                source = typeof(Logger).FullName;
            return new Logger(source);
        }
    }
}
