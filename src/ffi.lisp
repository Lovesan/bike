;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-

;;; Copyright (C) 2019, Dmitry Ignatiev <lovesan.ru at gmail.com>

;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:bike)

;; .NET expects clean FPU state. So sacrifice FP exceptions
(defun disable-fpu-exceptions ()
  "Disables FPU exceptions for the current thread and its child threads."
  #+sbcl (sb-vm::set-floating-point-modes :traps nil)
  #+ccl (ccl:set-fpu-mode :invalid nil
                          :inexact nil
                          :overflow nil
                          :underflow nil
                          :division-by-zero nil)
  #+ecl (progn (ext:trap-fpe 'floating-point-invalid-operation nil)
               (ext:trap-fpe 'division-by-zero nil)
               (ext:trap-fpe 'floating-point-overflow nil)
               (ext:trap-fpe 'floating-point-underflow nil)
               (ext:trap-fpe 'floating-point-inexact nil))
  #-(or sbcl ccl ecl)
  (warn
   "BIKE: Lisp must disable floating point exceptions
  before loading the libarary.
Please contribute to porting to your implementation.
Add impl. specific code to bike/src/ffi.lisp")
  #+(and coreclr-windows (not (or sbcl ccl ecl)))
  (cffi:foreign-funcall "_fpreset")
  (values))

(register-image-restore-hook 'disable-fpu-exceptions)

#+coreclr-restore-signals
(progn
  (defcstruct sigaction
    (handler :pointer)
    (sa-mask (:array :uint64 16))
    (sa-flags :int)
    (sa-restorer :pointer))

  (declaim (inline sigaction))
  (defcfun ("sigaction" sigaction)
      :int
    (sig :int)
    (action :pointer)
    (old :pointer))
  (define-compiler-macro sigaction (sig action old)
    `(foreign-funcall "sigaction"
                      :int ,sig
                      :pointer ,action
                      :pointer ,old
                      :void))

  (defconstant +nsig+ 65)
  (defconstant +sig-block+ 0)
  (defconstant +sig-dfl+ 0)
  (defconstant +sig-ign+ 1)
  (defconstant +sigint+ 2)
  (defconstant +sigquit+ 3)
  (defconstant +sigill+ 4)
  (defconstant +sigtrap+ 5)
  (defconstant +sigabrt+ 6)
  (defconstant +sigbus+ 7)
  (defconstant +sigfpe+ 8)
  (defconstant +sigkill+ 9)
  (defconstant +sigusr1+ 10)
  (defconstant +sigsegv+ 11)
  (defconstant +sigusr2+ 12)
  (defconstant +sigalrm+ 14)
  (defconstant +sigterm+ 15)
  (defconstant +sigchld+ 17)
  (defconstant +sigurg+ 23)
  (defconstant +sigwinch+ 28)
  (defconstant +sigrtmin+ 34)

  (define-global-var -lisp-sigactions- (null-pointer))
  (define-global-var -dotnet-sigactions- (null-pointer))
  (define-global-var -new-sigactions- (null-pointer))
  (define-global-var -default-sigaction- (null-pointer))
  (define-global-var -ignore-sigaction- (null-pointer))

  (declaim (inline sigaction-address))
  (defun sigaction-address (start n)
    (declare (type (integer 0 64) n)
             (type foreign-pointer start))
    (inc-pointer start (* n (foreign-type-size '(:struct sigaction)))))

  (defun save-lisp-sigactions ()
    (setf -lisp-sigactions- (foreign-alloc '(:struct sigaction) :count +nsig+))
    (setf -dotnet-sigactions- (foreign-alloc '(:struct sigaction) :count +nsig+))
    (setf -default-sigaction- (foreign-alloc '(:struct sigaction)))
    (setf -ignore-sigaction- (foreign-alloc '(:struct sigaction)))
    (setf (foreign-slot-value -default-sigaction- '(:struct sigaction) 'handler)
          (make-pointer +sig-dfl+))
    (setf (foreign-slot-value -ignore-sigaction- '(:struct sigaction) 'handler)
          (make-pointer +sig-ign+))
    (dotimes (i +nsig+)
      (sigaction i
                 (null-pointer)
                 (sigaction-address -lisp-sigactions- i)))
    (values))

  (defun disable-all-posix-signal-handling ()
    (when -has-system-native-
      (let ((fp (foreign-symbol-pointer "SystemNative_DisablePosixSignalHandling"
                                        :library 'system-native)))
        (when fp
          (loop :for i :from 1 :below +nsig+
                :do (foreign-funcall-pointer fp () :int i))))))

  ;; not used for the moment
  (defcallback chained-sigaction
      :void ((sig :int) (data :pointer) (ctx :pointer))
    (let ((lisp-handler (foreign-slot-value
                         (sigaction-address -lisp-sigactions- sig)
                         '(:struct sigaction)
                         'handler))
          (dotnet-handler (foreign-slot-value
                           (sigaction-address -dotnet-sigactions- sig)
                           '(:struct sigaction)
                           'handler)))
      (format t "~X ~X~%" lisp-handler dotnet-handler)
      (if (pointer-eq lisp-handler dotnet-handler)
        (foreign-funcall-pointer lisp-handler () :int sig
                                                 :pointer data
                                                 :pointer ctx)
        (progn (foreign-funcall-pointer lisp-handler () :int sig
                                                        :pointer data
                                                        :pointer ctx)
               (foreign-funcall-pointer dotnet-handler () :int sig
                                                          :pointer data
                                                          :pointer ctx)))))

  (defun restore-lisp-sigactions ()
    "Restores lisp signal actions"
    (dotimes (i +nsig+)
      (let ((addr (sigaction-address -lisp-sigactions- i)))
        (sigaction i addr (null-pointer)))))

  (register-image-restore-hook 'save-lisp-sigactions
                               (null-pointer-p -lisp-sigactions-)))

;;; vim: ft=lisp et
