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

(defun invoke (target method &rest args)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type dotnet-method-designator method)
           (dynamic-extent args))
  "Invokes a method designated by METHOD on a TARGET which can
 either be a type specifier, in which case a static method is
 invoked, or an instance, which would lead to instance method
 invocation."
  (let* ((instancep (dotnet-object-p target))
         (type (if instancep
                 (%bike-type-of target)
                 (resolve-type target)))
         (genericp (consp method))
         (name (%mknetsym (if genericp (car method) method)))
         (type-args (when genericp
                      (unless (cdr method)
                        (error 'generic-argument-count-mismatch
                               :token name
                               :value ""
                               :position 0
                               :datum method))
                      (mapcar #'resolve-type (cdr method)))))
    (apply #'%invoke-method type (%mknetsym name) (and instancep target) type-args args)))

(defun property (target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Retrieves a value of property named NAME from a TARGET, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access."
  (let* ((instancep (dotnet-object-p target))
         (type (if instancep
                 (%bike-type-of target)
                 (resolve-type target))))
    (%access-property type (%mknetsym name) (and instancep target) t nil)))

(defun (setf property) (new-value target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Changes a value of property named NAME of a TARGET, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access."
  (let* ((instancep (dotnet-object-p target))
         (type (if instancep
                 (%bike-type-of target)
                 (resolve-type target))))
    (%access-property type (%mknetsym name) (and instancep target) nil new-value)))

(defun ref (target index &rest indices)
  (declare (type dotnet-object target)
           (dynamic-extent indices))
  "Retrieves a value of an indexer from a TARGET, which
 must be an instance."
  (apply #'%access-indexer target t nil (cons index indices)))

(defun (setf ref) (new-value target index &rest indices)
  (declare (type dotnet-object target)
           (dynamic-extent indices))
  "Changes a value of an indexer from a TARGET, which
 must be an instance."
  (apply #'%access-indexer target nil new-value (cons index indices)))

(defun field (target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Retrieves a value of field named NAME from TARGET, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access."
  (let* ((instancep (dotnet-object-p target))
         (type (if instancep
                 (%bike-type-of target)
                 (resolve-type target))))
    (%access-field type (%mknetsym name) (and instancep target) t nil)))

(defun (setf field) (new-value target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Changes a value of field named NAME of a TARGET, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access."
  (let* ((instancep (dotnet-object-p target))
         (type (if instancep
                 (%bike-type-of target)
                 (resolve-type target))))
    (%access-field type (%mknetsym name) (and instancep target) nil new-value))
  new-value)

(defun new (type &rest args)
  (declare (type dotnet-type-designator type)
           (dynamic-extent args))
  "Creates an instance of the specified TYPE.
In case of the TYPE being a delegate type, first,
 and only, argument, must be a lisp function-like
 object."
  (let ((type (resolve-type type)))
    (if (delegate-type-p type)
      (let ((lisp-function (first args)))
        (declare (type (or symbol function) lisp-function))
        (%get-delegate-for-lisp-function lisp-function type))
      (apply #'%new type args))))

(defun unbox (object)
  "Attempts to unbox an OBJECT into lisp object"
  (if (dotnet-object-p object)
    (let ((code (%get-full-type-code (%dotnet-object-handle object))))
      (values (%unbox (%dotnet-object-handle object) code t)))
    object))

(defun box (object &optional (type nil typep))
  (declare (type (or null dotnet-type-designator) type))
  "Makes a boxed representation of an OBJECT"
  (let ((boxed (if (dotnet-object-p object)
                 object
                 (let* ((ptr (%box object))
                        (code (%get-full-type-code ptr)))
                   (%get-boxed-object ptr (ash code -8) nil)))))
    (if typep
      (invoke 'BikeInterop.TypeCaster (list 'Cast type) object)
      boxed)))

;;; vim: ft=lisp et
