    I've got a bike, you can ride it if you like
    It's got a basket, a bell that rings
    And things to make it look good
    I'd give it to you if I could, but I borrowed it
*-- Pink Floyd*


# Bike reinvents two wheels

* First of all, it is [RDNZL](https://common-lisp.net/~loliveira/ediware/rdnzl/_darcs/current/doc/) reborn

* This time, on .Net Core, without a line of C++ code, and fully cross-platform

Now you have the batteries included! Which are of the size of Battersea Power Station.

# Library status

|          | Windows | Linux | macOS  |
|:--------:|:-------:|:-----:|:------:|
| **SBCL** | [![OK](https://placehold.co/80x30/239922/FFF?text=OK)](#) | [![OK](https://placehold.co/80x30/239922/FFF?text=OK)](#) | [![?](https://placehold.co/80x30/999/FFF?text=%3F)](#) |
| **CCL**  | [![OK*](https://placehold.co/80x30/239922/FFF?text=OK%2A)](#foreign-thread-callbacks) | [![OK](https://placehold.co/80x30/239922/FFF?text=OK)](#)  | [![?](https://placehold.co/80x30/999/FFF?text=%3F)](#) |
| **ECL**  | [![ECL Bug](https://placehold.co/80x30/A00/DD2?text=ECL%20Bug)](#windows) | [![OK](https://placehold.co/80x30/239922/FFF?text=OK)](#)  | [![?](https://placehold.co/80x30/999/FFF?text=%3F)](#) |
| Other    | [![?](https://placehold.co/80x30/999/FFF?text=%3F)](#) | [![?](https://placehold.co/80x30/999/FFF?text=%3F)](#) | [![?](https://placehold.co/80x30/999/FFF?text=%3F)](#) |

\* With some exceptions

The above only applies to X86-64.

See below for [known issues](#known-issues).

#### CircleCI status (.NET 9, SBCL and CCL on ubuntu-noble)

[![CircleCI](https://circleci.com/gh/Lovesan/bike/tree/master.svg?style=svg)](https://circleci.com/gh/Lovesan/bike/tree/master)

## TL;DR

This library implements cross-platform Common Lisp interface to .Net Core platform, using lisp compatibility layers.

````lisp
(use-package :bike)

(named-readtables:in-readtable bike-syntax)

(import-assembly 'System.Runtime.InteropServices.RuntimeInformation)

(use-namespace 'System)
(use-namespace 'System.Runtime.InteropServices)

(defun hello ()
  (let* ((os [:RuntimeInformation %OSDescription])
         (delegate (new '(Action :string)
                        (lambda (who)
                          (format t "Hello ~a!~%You are running .Net Core~% inside ~a ~a~% on ~a"
                                  who
                                  (lisp-implementation-type)
                                  (lisp-implementation-version)
                                  os))))
         (user [:Environment %UserName]))
    [delegate Invoke user]))

(hello)

;; ==>

;; Hello lovesan!
;; You are running .Net Core
;;  inside SBCL 1.5.3
;;  on Linux 4.15.0-1041-aws #43-Ubuntu SMP Thu Jun 6 13:39:11 UTC 2019
````

See [examples](examples/) and [documentation](doc/README.md) for more info.

## Installation

The most basic way to install the library at this moment would be to use [quicklisp](https://www.quicklisp.org/):

````lisp
(ql:quickload :bike)
````

To get the latest version you can also clone the repo into ````~/quicklisp/local-projects/bike```` directory.

The library, once loaded, searches for .Net Core runtime and for ````BikeInterop.dll```` library in several places, like in the executable directory.

In case of the library is unable to locate .Net Core in one of the predefined places, it then asks ````dotnet```` command to list available runtimes and picks up latest one.

The interop .Net library(````BikeInterop.dll````), should be unavailable, is being built, again, by utilizing the ````dotnet```` command.

To build the library, you would, of course, need .Net Core SDK installed. But running the code in production should only require .Net Core runtime installed and the built interop binary nearby.

The library handles image restore, by utilizing ````uiop````. It searches for CoreCLR and interop layer again, then reloads cached types, and properly handles all the state.

Given this, you can deploy dumped images to other machines.

````lisp
(defun hello () (bike:invoke 'System.Console 'WriteLine "Hello, World!"))
(setf uiop:*image-entry-point* #'hello)
(uiop:dump-image "hello.exe" :executable t)

;; ./hello.exe
;; ==> Hello, World!
````

## Known Issues

SBCL is the main development and testing platform. CCL is also used for testing.

### Floating point exceptions from .NET runtime

.NET is unable to handle FPU exceptions, so the library disables all FPU exceptions on startup.

This requires implementation-specific code. See [src/ffi.lisp](src/ffi.lisp)

The downside is, you get a NaN instead of a DIVISION-BY-ZERO/FLOATING-POINT-INVALID-OPERATION/etc when you encounter such situations
doing float calculations.

IDE(SLIME, SLY, etc) may create threads before the library has been loaded, and these threads may have the FPU exceptions enabled. So in these threads, the problem with weird exceptions still applies.

To fix this, add the following to your RC file:

SBCL (~/.sbclrc):
````lisp
(sb-vm::set-floating-point-modes :traps nil)
````
CCL(~/ccl-init.lisp on Windows or ~/.ccl-init.lisp on Linux):
````lisp
(ccl:set-fpu-mode :invalid nil
                  :inexact nil
                  :overflow nil
                  :underflow nil
                  :division-by-zero nil)
````
ECL(~/.eclrc)
````lisp
(progn (ext:trap-fpe 'floating-point-invalid-operation nil)
       (ext:trap-fpe 'division-by-zero nil)
       (ext:trap-fpe 'floating-point-overflow nil)
       (ext:trap-fpe 'floating-point-underflow nil)
       (ext:trap-fpe 'floating-point-inexact nil))
````

### Task.Result and other things which block .Net code

Avoid using this if you pass Lisp callbacks to .Net code - this may cause deadlocks. You've been warned.

### Foreign thread callbacks

.Net is known for extensive use of thread pools and such.

A CL implementation without proper support of foreign thread callbacks probably crashes should you pass a callback to some .NET code which would execute it on a different .Net thread.

- SBCL has full support for foreign thread callbacks.
- Unlike CCL, for example.
- ECL requires foreign threads to be manually registered in the Lisp runtime. The library does this automatically.

### Windows

The library works well on SBCL/Windows, runtimes and garbage collectors seem to coexist peacefully.
SBCL callbacks can even be utilized by .Net [System.Threading.Tasks](https://docs.microsoft.com/en-us/dotnet/api/system.threading.tasks?view=netcore-2.2)

CCL/Windows does not handle foreign thread callbacks properly but otherwise, the library works fine on this implementation.

ECL/Win64 currently has this bug, which prevents the library from building: https://gitlab.com/embeddable-common-lisp/ecl/-/issues/679

#### Older Windows issues

There were some issues with SBCL and exceptions in the past, but those were resolved.

The reason for those issues was described [in my (D.I.) SBCL patch that had been applied in 2019.](https://sourceforge.net/p/sbcl/mailman/sbcl-devel/thread/CAK3-8Ji8XrjZd8ttKa0XOFPTewbg%2Bf2t5U3ZCwWGdcv6S6W_mQ%40mail.gmail.com/#msg36687909)

Basically, on x86-64 Windows, SBCL uses VEH, and its handler had been catching all the exceptions before .Net Core handlers even had a chance to look at their ones. This, next, sometimes led to a situation where SBCL disallowed .Net runtime to enter into the correct state, which led to the corruption of both runtimes and process crashes.

Seems like it has been resolved. [This particular patch from Luís Oliveira greatly enhanced SBCL exception handling.](https://github.com/sbcl/sbcl/commit/50085a82c7bd6dfb91599f236e3d002f49ebec72)

### Linux

The library should work fine on Linux.

#### Older linux issues

Early SBCL versions(namely [pre 1.5.4.13-b656602a3](https://sourceforge.net/p/sbcl/sbcl/ci/b656602a309fc9647dd01255154c1068305f12f7/tree/)) were frequently crashing into LDB with a cryptic message of ```blockables unblocked``` if .Net Core runtime was present in a lisp process.

The reason for this is that SBCL and .Net Core stomp on each other signals, in a kind of a wrong way.

[Seems that it is not only a case of .Net Core.](https://irclog.tymoon.eu/freenode/lisp?around=1500933122)

Thankfully, Stas Boukarev(stassats) implemented a workaround in the latest SBCL.

We still have to understand what may that workaround do to .Net Core runtime, but at least, it solves a problem of unavoidable and frequent crashes.

On overall, this need to be debugged out further. Maybe we would ask for help someone from .Net Core team.

**UPD** **2019-07-13:**  CoreFX(the .Net Core stdlib) also establishes signals for handling System.Diagnostics.Process classes and Console Ctrl handlers. We let it handle processes, because it can handle that perfectly(that means, including processes started by lisp, e.g. using ```uiop:run-program```), but revert the SIGINT handler for lisp one.


### MacOS X

Testers are welcome.

### ARM

Here be dragons

## TODO

* Documentation and tests are always good

* Add more examples which utilize third-party assemblies, NuGet, or something like that

* ```dotnet``` command interface

* NuGet interface

* Fancy async/await syntax

* MacOS X testing

* Implement some cache for parsed type definitions

* Expose DEFKNOW-alike API to the user

* Investigate CoreCLR interop on Linux.

* Optimize invocation cache (maybe write some hash functions instead of using ```sxhash``` etc)

* Write compiler macros for API and type resolution

* etc.
