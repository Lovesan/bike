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
using System.Globalization;
using System.Reflection;
using System.Runtime.InteropServices;
// ReSharper disable once RedundantUsingDirective
using System.Threading.Tasks;
// ReSharper disable MemberCanBePrivate.Global

// ReSharper disable UnusedMember.Global

#if ENABLE_TASK_HACK
#warning Enabling task hack
#endif

namespace BikeInterop
{
    public static class Externals
    {
        private static readonly object[] EmptyArray = new object[0];

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
                Console.Error.WriteLine(ex);
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
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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

        public static void GetGenericTypeByName(
            [MarshalAs(UnmanagedType.LPWStr)] string name,
            IntPtr args,
            int nArgs,
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
                var definitionName = string.Format(CultureInfo.InvariantCulture, "{0}`{1}", name, nArgs);
                var realAssembly = UnboxObject(assembly) as Assembly;
                var typeArgs = UnboxTypeArgs(args, nArgs);
                var definition = realAssembly != null
                    ? realAssembly.GetType(definitionName, throwOnError, true)
                    : Type.GetType(definitionName, throwOnError, true);
                // ReSharper disable once PossibleNullReferenceException
                invocationResult = definition.MakeGenericType(typeArgs);
            }
            catch (TargetInvocationException ex)
            {
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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

        public static void MakeGenericType(
            IntPtr type,
            IntPtr args,
            int nArgs,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            typeCode = (int)TypeCode.Empty;
            result = IntPtr.Zero;
            Exception e = null;
            object invocationResult = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var definition = (Type) UnboxObject(type);
                var typeArgs = UnboxTypeArgs(args, nArgs);
                invocationResult = definition.MakeGenericType(typeArgs);
            }
            catch (TargetInvocationException ex)
            {
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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

        public static void MakeArrayType(
            IntPtr type,
            int rank,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            exception = IntPtr.Zero;
            typeCode = (int)TypeCode.Empty;
            result = IntPtr.Zero;
            Exception e = null;
            object invocationResult = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var underlyingType = (Type) UnboxObject(type);
                invocationResult = underlyingType.MakeArrayType(rank);
            }
            catch (TargetInvocationException ex)
            {
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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

        public static bool IsDelegateType(IntPtr type)
        {
            var obj = UnboxObject(type);
            return obj is Type realType &&
                   realType.IsSubclassOf(typeof(Delegate));
        }

        public static void InvokeMember(
            IntPtr target,
            [MarshalAs(UnmanagedType.LPWStr)] string methodName,
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
            typeCode = (int) TypeCode.Empty;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var instance = UnboxObject(target);
                var type = instance.GetType();
                var realArgs = UnboxArgs(args, nArgs);
                invocationResult = type.InvokeMember(
                    methodName,
                    BindingFlags.Public |
                    BindingFlags.Instance |
                    BindingFlags.InvokeMethod |
                    BindingFlags.IgnoreCase,
                    null,
                    UnboxObject(target),
                    realArgs,
                    CultureInfo.CurrentCulture);
            }
            catch (TargetInvocationException ex)
            {
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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

        public static void GetField(
            IntPtr target,
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
                var instance = UnboxObject(target);
                var type = instance.GetType();
                invocationResult = type.InvokeMember(
                    fieldName,
                    BindingFlags.GetField |
                    BindingFlags.Public |
                    BindingFlags.Instance | 
                    BindingFlags.IgnoreCase,
                    null,
                    instance,
                    EmptyArray,
                    CultureInfo.CurrentCulture);
            }
            catch (TargetInvocationException ex)
            {
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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
                var instance = UnboxObject(target);
                var valueObject = UnboxObject(value);
                var type = instance.GetType();
                type.InvokeMember(
                    fieldName,
                    BindingFlags.SetField |
                    BindingFlags.Public |
                    BindingFlags.Instance |
                    BindingFlags.IgnoreCase,
                    null,
                    instance,
                    new [] {valueObject},
                    CultureInfo.CurrentCulture);
            }
            catch (TargetInvocationException ex)
            {
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif
            exception = BoxObject(e);
        }

        public static void InvokeStatic(
            IntPtr type,
            [MarshalAs(UnmanagedType.LPWStr)] string methodName,
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
            typeCode = (int) TypeCode.Empty;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var realType = (Type) UnboxObject(type);
                var realArgs = UnboxArgs(args, nArgs);
                invocationResult = realType.InvokeMember(
                    methodName,
                    BindingFlags.Public | 
                    BindingFlags.Static | 
                    BindingFlags.InvokeMethod |
                    BindingFlags.IgnoreCase,
                    null,
                    null,
                    realArgs,
                    CultureInfo.CurrentCulture);
            }
            catch (TargetInvocationException ex)
            {
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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


        public static void GetStaticField(
            IntPtr type,
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
                var realType = (Type) UnboxObject(type);
                invocationResult = realType.InvokeMember(
                    fieldName,
                    BindingFlags.GetField |
                    BindingFlags.IgnoreCase |
                    BindingFlags.Public |
                    BindingFlags.Static,
                    null,
                    null,
                    EmptyArray,
                    CultureInfo.CurrentCulture);
            }
            catch (TargetInvocationException ex)
            {
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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


        public static void SetStaticField(
            IntPtr type,
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
                var realType = (Type)UnboxObject(type);
                var valueObject = UnboxObject(value);
                realType.InvokeMember(
                    fieldName,
                    BindingFlags.SetField |
                    BindingFlags.IgnoreCase |
                    BindingFlags.Public |
                    BindingFlags.Static,
                    null,
                    null,
                    new[] { valueObject },
                    CultureInfo.CurrentCulture);
            }
            catch (TargetInvocationException ex)
            {
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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
                var realArgs = UnboxArgs(args, nArgs);
                invocationResult = Activator.CreateInstance(realType, realArgs);
            }
            catch (TargetInvocationException ex)
            {
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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

        public static void InvokeDelegate(
            IntPtr boxedDelegate,
            IntPtr args,
            int nArgs,
            out IntPtr result,
            out int typeCode,
            out IntPtr exception)
        {
            result = IntPtr.Zero;
            typeCode = (int)TypeCode.Empty;
            exception = IntPtr.Zero;
            object e = null;
            object invocationResult = null;

#if ENABLE_TASK_HACK
            var task = Task.Factory.StartNew(() =>
            {
#endif
            try
            {
                var realDelegate = (Delegate) UnboxObject(boxedDelegate);
                var realArgs = UnboxArgs(args, nArgs);
                invocationResult = realDelegate.DynamicInvoke(realArgs);
            }
            catch (TargetInvocationException ex)
            {
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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

        public static bool IsType(IntPtr value)
        {
            if (value == IntPtr.Zero)
                return false;
            return UnboxObject(value) is Type;
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
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
                e = ex;
            }
#if ENABLE_TASK_HACK
            });
            Task.WaitAny(task);
#endif

            exception = BoxObject(e);
        }

        public static void ConvertTo(
            IntPtr value,
            IntPtr type,
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
                var realValue = UnboxObject(value);
                var realType = (Type) UnboxObject(type);
                invocationResult = Convert.ChangeType(realValue, realType);
            }
            catch (TargetInvocationException ex)
            {
                Console.Error.WriteLine(ex);
                e = ex.InnerException;
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine(ex);
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
    }
}
