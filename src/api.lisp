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

(defmethod print-object ((object dotnet-object) stream)
  (print-unreadable-object (object stream)
    (let ((type (%to-string (%get-type object)))
          (str (%to-string object))
          (gc-handle (pointer-address (%dotnet-object-handle object))))
      (if (string= str type)
        (format stream "~a {~8,'0X}" type gc-handle)
        (format stream "~a ~a {~8,'0X}" type str gc-handle))))
  object)

(defun invoke (target method-name &rest args)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator method-name)
           (dynamic-extent args))
  "Invokes a method named METHOD-NAME on a TARGET which can
 either be a type specifier, in which case a static method is
 invoked, or an instance, which would lead to instance method
 invocation."
  (if (not (dotnet-object-p target))
    (check-exception (apply #'%invoke-static
                            (resolve-type target)
                            (%mknetsym method-name)
                            args))
    (check-exception (apply #'%invoke-member
                            target
                            (%mknetsym method-name)
                            args))))

(defun property (target name &rest indexers)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name)
           (dynamic-extent indexers))
  "Retrieves a value of property named NAME from a TARGET, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access."
  (let ((propmethod (format nil "get_~a" (%mknetsym name))))
    (if (not (dotnet-object-p target))
      (check-exception (apply #'%invoke-static
                              (resolve-type target)
                              propmethod
                              indexers))
      (check-exception (apply #'%invoke-member
                              target
                              propmethod
                              indexers)))))

(defun (setf property) (new-value target name &rest indexers)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name)
           (dynamic-extent indexers))
  "Changes a value of property named NAME of a TARGET, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access."
  (let ((propmethod (format nil "set_~a" (%mknetsym name))))
    (if (not (dotnet-object-p target))
      (check-exception (apply #'%invoke-static
                              (resolve-type target)
                              propmethod
                              new-value
                              indexers))
      (check-exception (apply #'%invoke-member
                              target
                              propmethod
                              new-value
                              indexers)))
    new-value))

(defun field (target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Retrieves a value of field named NAME from TARGET, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access."
  (if (not (dotnet-object-p target))
    (check-exception
     (%get-static-field (resolve-type target) (%mknetsym name)))
    (check-exception
     (%get-field target (%mknetsym name)))))

(defun (setf field) (new-value target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Changes a value of field named NAME of a TARGET, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access."
  (if (not (dotnet-object-p target))
    (check-exception
     (%set-static-field (resolve-type target) (%mknetsym name) new-value))
    (check-exception
     (%set-field target (%mknetsym name) new-value))))

(defun new (type &rest args)
  (declare (type dotnet-type-designator type)
           (dynamic-extent args))
  "Creates an instance of the specified TYPE.
In case of the TYPE being a delegate type, first,
 and only, argument, must be a lisp function-like
 object."
  (let ((type (resolve-type type)))
    (check-exception
     (if (%is-delegate-type type)
       (let ((lisp-function (first args)))
         (declare (type (or symbol function) lisp-function))
         (%get-delegate-for-lisp-function lisp-function type))
       (apply #'%invoke-constructor type args)))))

(defun unbox (object)
  (declare (type dotnet-object object))
  "Attempts to unbox an OBJECT into lisp object"
  (check-exception (%convert-to object (resolve-type "System.Object") t)))

(defun box (object &optional (type nil typep))
  (declare (type (or null dotnet-type-designator) type))
  "Makes a boxed representation of an OBJECT"
  (check-exception (%convert-to object (resolve-type (if typep type "System.Object")))))

(defmacro do-bike-vector ((elt-var vector &optional result) &body body)
  "Evaluates BODY forms in a loop where ELT-VAR is subsequently bound to
 each element of a VECTOR, which should evaluate to .Net array of rank 1.
 Returns RESULT form."
  (with-gensyms (i v)
    `(let ((,v ,vector))
       (declare (type dotnet-object ,v))
       (dotimes (,i (check-exception (%array-length ,v)) ,result)
         (let ((,elt-var (check-exception (%net-vref ,v ,i))))
           ,@body)))))

(defun dnvref (vector index)
  (declare (type dotnet-object vector)
           (type non-negative-fixnum index))
  "Accesses a .Net VECTOR (an array of rank 1) at INDEX"
  (check-exception (%net-vref vector index)))

(defun (setf dnvref) (new-value vector index)
  (declare (type dotnet-object vector)
           (type non-negative-fixnum index))
  "Accesses a .Net VECTOR (an array of rank 1) at INDEX"
  (check-exception (funcall #'(setf %net-vref) new-value vector index)))

;;; vim: ft=lisp et
