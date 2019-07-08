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
    (let ((name (%mknetsym name))
          (types (mapcar #'resolve-type types)))
      (values name types))))

(defun reflection-invoke (target method &rest args)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type dotnet-method-designator method)
           (dynamic-extent args))
  "Using reflection, invokes a method designated by METHOD on a TARGET which can
 either be a type specifier, in which case a static method is
 invoked, or an instance, which would lead to instance method
 invocation."
  (multiple-value-bind (method-name type-args)
      (if (consp method)
        (%resolve-generic-types method)
        (values (%mknetsym method) '()))
    (if (dotnet-object-p target)
      (apply #'%invoke target nil type-args method-name args)
      (apply #'%invoke (resolve-type target) t type-args method-name args))))

(defun reflection-property (target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Using reflection, retrieves a value of property named NAME from a TARGET, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access."
  (if (dotnet-object-p target)
    (%get-property target nil (%mknetsym name))
    (%get-property (resolve-type target) t (%mknetsym name))))

(defun (setf reflection-property) (new-value target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Using reflection, changes a value of property named NAME of a TARGET, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access."
  (if (dotnet-object-p target)
    (%set-property target nil (%mknetsym name) new-value)
    (%set-property (resolve-type target) t (%mknetsym name) new-value)))

(defun reflection-ref (target index &rest indices)
  (declare (type dotnet-object target)
           (dynamic-extent indices))
  "Using reflection, retrieves a value of an indexer from a TARGET, which
 must be an instance."
  (apply #'%get-index target index indices))

(defun (setf reflection-ref) (new-value target index &rest indices)
  (declare (type dotnet-object target)
           (dynamic-extent indices))
  "Using reflection, changes a value of an indexer from a TARGET, which
 must be an instance."
  (apply #'%set-index target new-value index indices))

(defun reflection-field (target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Using reflection, retrieves a value of field named NAME from TARGET, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access."
  (if (dotnet-object-p target)
    (%get-field target nil (%mknetsym name))
    (%get-field (resolve-type target) t (%mknetsym name))))

(defun (setf reflection-field) (new-value target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Using reflection, changes a value of field named NAME of a TARGET, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access."
  (if (dotnet-object-p target)
    (%set-field target nil (%mknetsym name) new-value)
    (%set-field (resolve-type target) t (%mknetsym name) new-value)))

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

(defmethod print-object ((dotnet-error dotnet-error) stream)
  (let* ((object (dotnet-error-object dotnet-error))
         (type (reflection-invoke (reflection-invoke object "GetType") "ToString"))
         (message (remove #\Return (reflection-property object "Message")))
         (trace (remove #\Return (reflection-property object "StackTrace"))))
    (format stream ".Net exception ~a~%~a~%~a" type message trace))
  dotnet-error)

(defmethod print-object ((object dotnet-object) stream)
  (print-unreadable-object (object stream)
    (let ((type (reflection-invoke (reflection-invoke object "GetType") "ToString"))
          (str (reflection-invoke object "ToString"))
          (gc-handle (pointer-address (%dotnet-object-handle object))))
      (if (string= str type)
        (format stream "~a {~8,'0X}" type gc-handle)
        (format stream "~a ~a {~8,'0X}" type str gc-handle))))
  object)

(defmethod print-object ((object type-entry) stream)
  (print-unreadable-object (object stream :type t)
    (with-type-entry (type) object
      (let ((gc-handle (pointer-address (%dotnet-type-handle type)))
            (name (reflection-invoke type "ToString")))
        (format stream "~a {~8,'0X}" name gc-handle)))
    object))

;;; vim: ft=lisp et
