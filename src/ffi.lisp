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

(declaim (inline coreclr-initialize))
(defcfun (coreclr-initialize
          "coreclr_initialize"
          :library coreclr
          :convention :stdcall)
    :uint
  (exe-path lpastr)
  (app-domain-name lpastr)
  (property-count :int)
  (property-keys :pointer)
  (property-values :pointer)
  (host-handle :pointer)
  (domain-id :pointer))

(define-compiler-macro coreclr-initialize (exe-path
                                           app-domain-name
                                           property-count
                                           property-keys
                                           property-values
                                           host-handle
                                           domain-id)
  `(foreign-funcall ("coreclr_initialize" :convention :stdcall
                                          :library coreclr)
                    lpastr ,exe-path
                    lpastr ,app-domain-name
                    :int ,property-count
                    :pointer ,property-keys
                    :pointer ,property-values
                    :pointer ,host-handle
                    :pointer ,domain-id
                    :uint))

(defcfun (coreclr-shutdown-2
          "coreclr_shutdown_2"
          :library coreclr
          :convention :stdcall)
    :uint
  (host-handle :pointer)
  (domain-id :uint)
  (exit-code :pointer))

(defcfun (coreclr-create-delegate
          "coreclr_create_delegate"
          :library coreclr
          :convention :stdcall)
    :uint
  (host-handle :pointer)
  (domain-id :uint)
  (entry-assembly lpastr)
  (entry-type lpastr)
  (entry-method lpastr)
  (delegate :pointer))

(defcfun (coreclr-execute-assembly
          "coreclr_execute_assembly"
          :library coreclr
          :convention :stdcall)
    :uint
  (host-handle :pointer)
  (domain-id :uint)
  (argc :int)
  (argv :pointer)
  (assembly-path lpastr)
  (exit-code :pointer))

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
  (defconstant +sigsegv+ 11)
  (defconstant +sigusr2+ 12)
  (defconstant +sigalrm+ 14)
  (defconstant +sigterm+ 15)
  (defconstant +sigchld+ 17)
  (defconstant +sigurg+ 23)
  (defconstant +sigwinch+ 28)

  (#+sbcl sb-ext:defglobal #-sbcl defvar +lisp-sigactions+ (null-pointer))
  (#+sbcl sb-ext:defglobal #-sbcl defvar +dotnet-sigactions+ (null-pointer))
  (#+sbcl sb-ext:defglobal #-sbcl defvar +new-sigactions+ (null-pointer))
  (#+sbcl sb-ext:defglobal #-sbcl defvar +default-sigaction+ (null-pointer))
  (#+sbcl sb-ext:defglobal #-sbcl defvar +ignore-sigaction+ (null-pointer))

  (declaim (inline sigaction-address))
  (defun sigaction-address (start n)
    (declare (type (integer 0 64) n)
             (type foreign-pointer start))
    (inc-pointer start (* n (foreign-type-size '(:struct sigaction)))))

  (defun save-lisp-sigactions ()
    (setf +lisp-sigactions+ (foreign-alloc '(:struct sigaction) :count +nsig+))
    (setf +dotnet-sigactions+ (foreign-alloc '(:struct sigaction) :count +nsig+))
    (setf +default-sigaction+ (foreign-alloc '(:struct sigaction)))
    (setf +ignore-sigaction+ (foreign-alloc '(:struct sigaction)))
    (setf (foreign-slot-value +default-sigaction+ '(:struct sigaction) 'handler)
          (make-pointer +sig-dfl+))
    (setf (foreign-slot-value +ignore-sigaction+ '(:struct sigaction) 'handler)
          (make-pointer +sig-ign+))
    (dotimes (i +nsig+)
      (sigaction i
                 (null-pointer)
                 (sigaction-address +lisp-sigactions+ i)))
    (values))

  (defun disable-all-posix-signal-handling ()
    (when *has-libsystem-native*
      (let ((fp (foreign-symbol-pointer "SystemNative_DisablePosixSignalHandling"
                                        :library 'libsystem-native)))
        (when fp
          (loop :for i :from 1 :below +nsig+
                :do (foreign-funcall-pointer fp () :int i))))))

  ;; not used for the moment
  (defcallback chained-sigaction
      :void ((sig :int) (data :pointer) (ctx :pointer))
    (let ((lisp-handler (foreign-slot-value
                         (sigaction-address +lisp-sigactions+ sig)
                         '(:struct sigaction)
                         'handler))
          (dotnet-handler (foreign-slot-value
                           (sigaction-address +dotnet-sigactions+ sig)
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
      (let ((addr (sigaction-address +lisp-sigactions+ i)))
        (sigaction i addr (null-pointer)))))

  (uiop:register-image-restore-hook #'save-lisp-sigactions
                                    (null-pointer-p +lisp-sigactions+)))

;;; vim: ft=lisp et
