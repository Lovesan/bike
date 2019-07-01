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
