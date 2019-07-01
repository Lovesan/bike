    I've got a bike, you can ride it if you like
    It's got a basket, a bell that rings
    And things to make it look good
    I'd give it to you if I could, but I borrowed it
*-- Pink Floyd*


# Bike reinvents two wheels

* First of all, it is [RDNZL](https://common-lisp.net/~loliveira/ediware/rdnzl/_darcs/current/doc/) reborn

* This time, on .Net Core, without a line of C++ code, and fully cross-platform

Now you have the batteries included! Which are of the size of Battersea Power Station.

[![CircleCI](https://circleci.com/gh/Lovesan/bike/tree/master.svg?style=svg)](https://circleci.com/gh/Lovesan/bike/tree/master)

## TL;DR

This library implements cross-platform Common Lisp interface to .Net Core platform, using lisp compatibility layers.

````lisp
(use-package :bike)

(import-assembly 'System.Runtime.InteropServices.RuntimeInformation)

(use-namespace 'System.Runtime.InteropServices)

(defun hello ()
  (let* ((os (property 'RuntimeInformation 'OSDescription))
         (delegate (new '(System.Action :string)
                        (lambda (who)
                          (format t "Hello ~a!~%You are running .Net Core~% inside ~a ~a~% on ~a"
                                  who
                                  (lisp-implementation-type)
                                  (lisp-implementation-version)
                                  os))))
         (user (property 'System.Environment 'UserName)))
    (invoke delegate 'invoke user)))

(hello)

;; ==>

;; Hello lovesan!
;; You are running .Net Core
;;  inside SBCL 1.5.3
;;  on Linux 4.15.0-1041-aws #43-Ubuntu SMP Thu Jun 6 13:39:11 UTC 2019
````

## Installation

The most basic way to install the library at this moment would be to drop the contents of the repository into ````~/quicklisp/local-projects/bike```` directory.

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

### SBCL

SBCL is the main development and testing platform.

#### Windows

The library seems to work well on SBCL/Windows, the runtimes and garbage collectors seem to coexist peacefully. SBCL callbacks can even be utilized by .Net [System.Threading.Tasks](https://docs.microsoft.com/en-us/dotnet/api/system.threading.tasks?view=netcore-2.2)

However, there is one important issue here. That is the NullReferenceException.

Although rare in production code, this would probably lead to SBCL runtime corruption and crash, should one occur on a lisp thread.

The reason for this is described [in my (D.I.) SBCL patch that had been applied recently.](https://sourceforge.net/p/sbcl/mailman/sbcl-devel/thread/CAK3-8Ji8XrjZd8ttKa0XOFPTewbg%2Bf2t5U3ZCwWGdcv6S6W_mQ%40mail.gmail.com/#msg36687909)

Basically, on x86-64 Windows, SBCL uses VEH, and its handler catches all the exceptions before .Net Core handlers even have a chance to look at their ones. This, next, leads to a situation where SBCL disallows .Net runtime to enter into correct state, which leads to the corruption of both runtimes and process crash.

The above patch fixed the situation for usual exceptions (Exception, IndexOutOfBoundsExcepion, etc), but the EXCEPTION_ACCESS_VIOLATION(which .Net translates into NullReferenceException should the address be small enough) handler remains here. It is not clear what we should do with that situation. Maybe add another ````if```` statement, which would exclude low address space, but the best thing we can do is actually rewrite SBCL handlers on x64 Windows to use [RtlInstallFunctionTableCallback](https://docs.microsoft.com/en-us/windows/desktop/api/winnt/nf-winnt-rtlinstallfunctiontablecallback) instead of VEH.

#### Linux

Sometimes SBCL crashes into LDB with a cryptic message of ```blockables unblocked``` should .Net Core runtime be present on a lisp thread. The reason for this is, possibly, that SBCL somehow interferes with unix signals installed by .Net(or vice versa), in a kind of a wrong way.

This need to be debugged out. [Seems that it is not only a case of .Net Core.](https://irclog.tymoon.eu/freenode/lisp?around=1500933122)

It may be something wrong with how SBCL handles pseudo-atomic thing.

#### MacOS X

Testers are welcome.

## TODO

* Documentation and tests are always good

* Add examples which utilize third-party assemblies, NuGet, or something like that

* MacOS X testing

* Fancy reader syntax like the one RDNZL had

* Implement method lookup based on existing type member cache

* Refactor trampoline compiler code

* Implement direct function definitions akin to CFFI's ```defcfun```

* Fix SBCL crashes on Linux. Fix SBCL crashes on Windows with NullReferenceException.

* etc.
