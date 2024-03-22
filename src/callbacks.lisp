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

(defcallback (free-lisp-handle-callback :convention :stdcall)
    :void ((handle :pointer))
  (%free-lisp-handle (pointer-address handle)))

(defcallback (apply-callback :convention :stdcall)
    :pointer ((fun-handle :pointer)
              (args-ptr :pointer)
              (type-codes-ptr :pointer)
              (n-args :int)
              (release-rv-ptr :pointer)
              (out-ex :pointer)
              (out-ex-from-dotnet :pointer))
  (handler-case
      (let ((function (%handle-table-get (pointer-address fun-handle)))
            (args '()))
        (setf (mem-ref out-ex-from-dotnet :bool) nil
              (mem-ref out-ex :pointer) (null-pointer)
              (mem-ref release-rv-ptr :bool) nil)
        (dotimes (i n-args)
          (let ((boxed (mem-aref args-ptr :pointer i))
                (type-code (mem-aref type-codes-ptr :int i)))
            (multiple-value-bind (arg cleanup)
                (%unbox boxed type-code)
              (when cleanup (%free-handle boxed))
              (push arg args))))
        (multiple-value-bind (boxed cleanup)
            (%box (apply function (nreverse args)))
          (setf (mem-ref release-rv-ptr :bool) cleanup)
          boxed))
    (dotnet-error (e)
      (setf (mem-ref out-ex-from-dotnet :bool) t
            (mem-ref out-ex :pointer)
            (%dotnet-object-handle (dotnet-error-object e)))
      (null-pointer))
    (error (e)
      (setf (mem-ref out-ex-from-dotnet :bool) nil
            (mem-ref out-ex :pointer)
            (make-pointer (%alloc-lisp-handle e)))
      (null-pointer))))

(define-global-var -callbacks-initialized- nil)

(defun initialize-callbacks ()
  (with-foreign-object (ex :pointer)
    (hostcall install-callbacks
              :pointer (callback free-lisp-handle-callback)
              :pointer (callback apply-callback)
              :pointer
              #+ecl (foreign-symbol-pointer "ecl_import_current_thread")
              #-(or ecl) (null-pointer)
              :pointer
              #+ecl (foreign-symbol-pointer "ecl_release_current_thread")
              #-(or ecl) (null-pointer)
              :pointer ex)
    (%transform-exception (mem-ref ex :pointer))
    (setf -callbacks-initialized- t)))

(register-image-restore-hook 'initialize-callbacks (not -callbacks-initialized-))

;;; vim: ft=lisp et
