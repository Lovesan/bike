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

(define-condition bike-condition (condition)
  ()
  (:documentation "Represents generic condition"))

(define-condition bike-error (bike-condition error)
  ()
  (:report "Represents an erroneous condition"))

(define-condition bike-warning (bike-condition warning)
  ()
  (:report "Represents a subnormal but not erroneous condition"))

(define-condition invalid-type-designator (bike-error)
  ((%datum :initarg :datum :reader invalid-type-designator-datum))
  (:report (lambda (c s)
             (format s "Invalid type designator: ~s"
                     (invalid-type-designator-datum c)))))

(define-condition type-resolution-error (bike-error)
  ((%datum :initarg :datum :reader type-resolution-error-datum))
  (:report (lambda (c s)
             (format s "Unable to resolve type: ~s"
                     (type-resolution-error-datum c)))))

(define-condition dotnet-error (bike-error)
  ((%object :reader dotnet-error-object
            :initarg :object))
  (:documentation "Represents a .Net Exception")
  (:report report-dotnet-error))

(defun report-dotnet-error (dotnet-error stream)
  (let* ((object (dotnet-error-object dotnet-error))
         (type (%to-string (%get-type object)))
         (message (remove #\Return (%invoke-member object "get_Message")))
         (trace (remove #\Return (%invoke-member object "get_StackTrace"))))
    (format stream ".Net exception ~a~%~a~%~a" type message trace))
  dotnet-error)

(defmacro check-exception (form)
  (with-gensyms (result exception handle)
    `(multiple-value-bind (,result ,exception)
         ,form
       (when ,exception
         (let ((,handle (%dotnet-exception-handle ,exception)))
           (if (%is-lisp-object ,handle)
             (error (%handle-table-get
                     (pointer-address
                      (%%unbox-lisp-object ,handle))))
             (error 'dotnet-error :object ,exception))))
       ,result)))

;;; vim: ft=lisp et
