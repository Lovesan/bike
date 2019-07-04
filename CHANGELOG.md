# 0.3.1 - Bugfixing release
  * Fixed property setter trampoline compiler
  * Added SBCL to CI tests

# 0.3.0 - Workaround for SBCL/Linux crashes
  * stassats implemented a workaround for SBCL/CoreCLR signal interference on Linux
  * Fixed bug with handle-table resize
  * Verified that library works inside SBCL/nanoserver container
    * ```docker pull love5an/dotnet-core-sdk-common-lisp:2.2-sbcl-1.5.4-nanoserver-1903```
  * Implemented several internal APIs for future enhancements of type resolution

# 0.2.0 - Type member cache
  * Major refactoring
  * Split internals system into several files
  * Added generic method invocation support
  * Reflection-based API is now separated from the main one,
    which uses a global cache of .Net type member descriptions,
    and contains pre-compiled direct trampolines for
    .Net fields, methods and properties
  * Fixed several memory leaks
  * Finally implemented proper LPASTR cffi type on Windows,
    for strings used by coreclr initialization API
  * Added dummy logger for .Net interop library


# 0.1.1 - Bugfixing release
  * Added CI tests for Clozure CL on Linux/x86_64
  * Few minor CCL fixes
  * Minor misprint fix

# 0.1.0 - Initial release
