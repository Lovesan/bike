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

(defmethod print-object ((dotnet-error dotnet-error) stream)
  (let* ((object (dotnet-error-object dotnet-error))
         (type (%to-string (%bike-type-of object)))
         (message (remove #\Return (exception-message object)))
         (trace (remove #\Return (exception-stack-trace object))))
    (format stream ".Net exception ~a~%~a~%~a" type message trace))
  dotnet-error)

(defmethod print-object ((object dotnet-object) stream)
  (let ((str (remove #\Return (%to-string object))))
    (if (or *print-readably* *print-escape*)
      (print-unreadable-object (object stream)
        (let ((type (%to-string (%bike-type-of object)))
              (gc-handle (pointer-address (%dotnet-object-handle object))))
          (if (string= str type)
            (format stream "~a {~8,'0X}" type gc-handle)
            (format stream "~a ~a {~8,'0X}" type str gc-handle))))
      (write-string str stream)))
  object)

;;; vim: ft=lisp et
