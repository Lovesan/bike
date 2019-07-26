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

(in-package #:bike-examples)

(ensure-examples-assembly)

(use-namespace 'System)
(use-namespace 'BikeExamples)

(defun example-exception-case ()
  "Catches a .Net exception"
  (exception-case
      (progn (write-line "Trying to acquire non-existing type")
             (invoke 'Type 'GetType "$NonExistingType$" t))
    (Exception (e) (format t "~&Caught: ~a~%"
                           (property (bike-type-of e) 'FullName))))
  (values))

(defun example-exception-bind ()
  "Establishes a restart around .Net exception and invokes it"
  ;; Note that this example won't work if you will establish a restart
  ;;  in a callback passed to .Net code, since exceptions are marshalled
  ;;  on Lisp/.Net boundaries, and hence the restart would be disabled
  ;;  before your handler would be invoked.
  (exception-bind ((ExampleException (lambda (e)
                                       (format t "~&Caught: ~a~%Invoking restart CONTINUE~%"
                                               (property (invoke e 'GetType) 'FullName))
                                       (invoke-restart 'continue))))
    (write-line "Establishing a restart")
    (restart-case
        (invoke 'ExampleClass 'ThrowsFunction)
      (continue () (write-line "Restart CONTINUE invoked")))
    (write-line "Successful exit")
    (values)))

;;; vim: ft=lisp et
