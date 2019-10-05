# 0.7.3 Revert use of APP_PATHS and NI_PATHS
  * Fixed misprint left from previous version

# 0.7.2 Removed use of uiop:native-namestring in favor of native-path function
  * ```uiop:native-namestring``` function seems to be somewhat broken on CCL on Windows.
    It does not provide paths with backslashes which are expected by ```coreclr_initialize```

# 0.7.1 Fix compatibility with previous versions
  * ```GET-LOADED-ASSEMBLIES``` now utilizes interop library method and does not accept any arguments
  * Removed ```CURRENT-APP-DOMAIN``` function from the list of exported functions

# 0.7.0 .Net Core 3.0 compatibility
  * Added few fixes for .Net Core 3.0 compatibility
  * ```GET-LOADED-ASSEMBLIES``` now accepts optional ```AppDomain``` argument

# 0.6.3 Revert deps for bike.asd
  * Put all the dependencies into ```bike.asd``` file
  * Make use of latest CCL for CI tests

# 0.6.2 Bugfixing release
  * Fixed callbacks.lisp example file: use namespaces before building an assembly

# 0.6.1 Bugfixing release
  * Fixed return value cast for callbacks
  * Added few more examples
  * Use ```flet``` inside of ```exception-case``` instead of lambdas

# 0.6.0 Exception handlers, interface methods and IDisposable
  * Added the ability to invoke interface methods of an object (i.e. implemented by private interface implementation)
  * Added useful macros for working with ```IDisposable``` - ```WITH-DISPOSABLE``` and friends
  * Added ```DO-ENUMERABLE``` macro, which works like C# ```foreach```
  * Added ```EXCEPTION-BIND``` and ```EXCEPTION-CASE``` macros which act like their ```HANDLER-*``` counterparts but for .Net exceptions

# 0.5.2 Make use of TargetInvocationException for callbacks
  * Wrap .Net exceptions coming from callbacks into TargetInvocationException for not losing .Net stack traces
  * Added ```import-assembly-from``` function for importing assemblies from files
  * Allowed use and unuse a list of namespaces at a time

# 0.5.1 APP_PATHS
  * Made use of APP_PATHS and APP_NI_PATHS during CoreCLR initialization

# 0.5.0 - Target invocation cache. Internal DEFKNOWN API.
  * Implemented target invocation cache akin to .Net DLR one. All non-reflection APIs use this
    * Compiled trampolines are cached(based on member declaring type and argument types) and reused later on
  * Added internal ```defknown``` API based on trampoline compilation
    * A similiar API for establishing direct calls would be exposed to the user a bit later
  * Optimize count of host methods in favor of ```defknown``` API
  * Implemented generic type caster (use ```box``` function with optional parameter)
  * Renamed ```get-type``` function to ```bike-type-of```
    * ```get-type``` now acts exactly as ```Type.GetType```, i.e. retrieves a type by name
      * You should use ```resolve-type``` function for that, however, because of it is being more robust
  * .Net exceptions caught by Lisp code inside callbacks are now unwrapped into .Net exceptions on return to .Net code
  * Moved .Net initialization to host.lisp file. Removed an amount of duplicate code
  * Fixed few bugs in .Net type name parser

# 0.4.2 - Revert REF accessor to reflection
  * Use reflection until method resolution would be implemented

# 0.4.1 - Bugfixing release
  * Fixed multidimensional array type resolution

# 0.4.0 - AspNet Core MVC example. New type resolution process. Signal handling on linux improvements.
  * Improved coexistence of CoreCLR and Lisp runtimes on Linux.
    * Handle CoreFX signals by lisp, except SIGCHLD, which is perfectly handled by .Net
  * Added an example of embedding lisp into AspNet MVC pipeline
  * Completely reworked type resolution process.
    * Removed ```import-type``` function. Please use only the ```resolve-type``` function.
  * Support for ```ref``` and ```*```(pointer) types
  * Added clean.lisp script
  * Improved CoreCLR version comparison during library search
  * Added ```load-assembly-from``` function, to load assemblies from files
  * Added enum type handling, see ```enum``` function
  * Improved trampoline compilers, they now support pointer and ref types
  * Changed keyword parameter ```:type``` of ```list-to-bike-vector``` function to ```:element-type```
  * Other minor bugfixing and improvements

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
