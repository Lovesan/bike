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

(defun %resolve-generic-types (spec)
  (declare (type (cons string-designator (cons dotnet-type-designator list)) spec))
  (destructuring-bind (name &rest types) spec
    (let ((name (simple-character-string-upcase name))
          (types (mapcar #'resolve-type types)))
      (values name types))))

(defun reflection-invoke (target method &rest args)
  (declare (type (or dotnet-object* dotnet-type-designator) target)
           (type dotnet-method-designator method)
           (dynamic-extent args))
  "Using reflection, invokes a method designated by METHOD on a TARGET which can
 either be a type specifier, in which case a static method is
 invoked, or an instance, which would lead to instance method
 invocation."
  (multiple-value-bind (method-name type-args)
      (if (consp method)
        (%resolve-generic-types method)
        (values (simple-character-string-upcase method) '()))
    (if (typep target 'dotnet-object*)
      (apply #'%invoke target nil type-args method-name args)
      (apply #'%invoke (resolve-type target) t type-args method-name args))))

(defun reflection-property (target name)
  (declare (type (or dotnet-object* dotnet-type-designator) target)
           (type string-designator name))
  "Using reflection, retrieves a value of property named NAME from a TARGET, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access."
  (let ((name (simple-character-string-upcase name)))
    (if (typep target 'dotnet-object*)
      (%get-property target nil name)
      (%get-property (resolve-type target) t name))))

(defun (setf reflection-property) (new-value target name)
  (declare (type (or dotnet-object* dotnet-type-designator) target)
           (type string-designator name))
  "Using reflection, changes a value of property named NAME of a TARGET, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access."
  (let ((name (simple-character-string-upcase name)))
    (if (typep target 'dotnet-object*)
      (%set-property target nil name new-value)
      (%set-property (resolve-type target) t name new-value))))

(defun reflection-ref (target index &rest indices)
  (declare (type dotnet-object* target)
           (dynamic-extent indices))
  "Using reflection, retrieves a value of an indexer from a TARGET, which
 must be an instance."
  (apply #'%get-index target index indices))

(defun (setf reflection-ref) (new-value target index &rest indices)
  (declare (type dotnet-object* target)
           (dynamic-extent indices))
  "Using reflection, changes a value of an indexer from a TARGET, which
 must be an instance."
  (apply #'%set-index target new-value index indices))

(defun reflection-field (target name)
  (declare (type (or dotnet-object* dotnet-type-designator) target)
           (type string-designator name))
  "Using reflection, retrieves a value of field named NAME from TARGET, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access."
  (let ((name (simple-character-string-upcase name)))
    (if (typep target 'dotnet-object*)
      (%get-field target nil name)
      (%get-field (resolve-type target) t name))))

(defun (setf reflection-field) (new-value target name)
  (declare (type (or dotnet-object* dotnet-type-designator) target)
           (type string-designator name))
  "Using reflection, changes a value of field named NAME of a TARGET, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access."
  (let ((name (simple-character-string-upcase name)))
    (if (typep target 'dotnet-object*)
      (%set-field target nil name new-value)
      (%set-field (resolve-type target) t name new-value))))

(defun reflection-new (type &rest args)
  (declare (type dotnet-type-designator type)
           (dynamic-extent args))
  "Using reflection, creates an instance of the specified TYPE.
In case of the TYPE being a delegate type, first,
 and only, argument, must be a lisp function-like
 object."
  (let ((type (resolve-type type)))
    (if (delegate-type-p type)
      (let ((lisp-function (first args)))
        (declare (type (or symbol function) lisp-function))
        (%get-delegate-for-lisp-function lisp-function type))
      (apply #'%invoke-constructor type args))))

(defun list-to-bike-vector (list &key (start 0) end (element-type "System.Object"))
  (declare (type list list)
           (type non-negative-fixnum start)
           (type (or null non-negative-fixnum) end))
  "Converts a LIST to a one-dimensional .Net array. of the specified TYPE.
 START and END designate LIST bounds and the resulting
 vector size, correspondingly"
  (let* ((end (or end (length list)))
         (count (max 0 (- end start)))
         (vector (%invoke-constructor (resolve-type `(:array ,element-type)) count)))
    (do ((i 0 (1+ i))
         (sublist (loop :for l :on list
                        :for n :below start
                        :finally (return l))
                  (cdr sublist)))
        ((= i count) vector)
      (setf (dnvref vector i) (car sublist)))))

;;; vim: ft=lisp et
