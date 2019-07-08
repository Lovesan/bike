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

(defun %make-param-entry (info)
  (declare (type dotnet-object info))
  (let* ((name (%mknetsym (reflection-property info "Name")))
         (type (reflection-property info "ParameterType"))
         (outp (reflection-property info "IsOut"))
         (refp (reflection-property type "IsByRef"))
         (optionalp (reflection-property info "IsOptional"))
         (pointerp (reflection-property type "IsPointer"))
         (dir (if refp (if outp :out :ref) :in))
         (paramsp (and (reflection-invoke "System.Attribute"
                                          "GetCustomAttribute"
                                          info
                                          (resolve-type "System.ParamArrayAttribute"))
                       t))
         (pos (reflection-property info "Position"))
         (primitive (cdr (assoc (reflection-property type "FullName")
                                +primitive-types+
                                :test #'string-equal))))
    (%param-entry info name type (or primitive
                                     (and pointerp :pointer))
                  pointerp optionalp dir pos paramsp)))

(defun %get-method-info-args (info)
  (declare (type dotnet-object info))
  (let* ((parameters (reflection-invoke info "GetParameters"))
         (entries '()))
    (do-bike-vector (param parameters)
      (push (%make-param-entry param) entries))
    (sort entries #'< :key #'%param-entry-position)))

(defun %make-method-entry (info prev)
  (declare (type dotnet-object info))
  (block constructor
    (let* ((staticp (reflection-property info "IsStatic"))
           (type-args (bike-vector-to-list
                       (reflection-invoke info "GetGenericArguments")))
           (type-arg-count (length type-args))
           (args (%get-method-info-args info))
           (arg-count (length args))
           (arg-types (mapcar #'%param-entry-type args))
           (arg-type-count (length arg-types))
           (return-type (reflection-property info "ReturnType"))
           (voidp (string= "System.Void"
                           (reflection-property return-type "FullName")))
           (callable #'identity)
           (delegate nil)
           (delegate-pointer (null-pointer)))
      (when (zerop type-arg-count) ;; do not compile generic method definitions here
        (multiple-value-bind (d fptr) (%get-delegate-trampoline info '())
          (setf delegate d
                delegate-pointer fptr
                callable (compile-method-trampoline fptr staticp voidp args))))
      (%method-entry info return-type type-arg-count type-args
                     arg-count args arg-types arg-type-count
                     delegate delegate-pointer callable prev))))

(defun %add-method-entry (info hash)
  (when (reflection-property info "IsGenericMethod")
    (setf info (reflection-invoke info "GetGenericMethodDefinition")))
  (let* ((name (%mknetsym (reflection-property info "Name")))
         (prev (gethash name hash))
         (entry (%make-method-entry info prev)))
    (setf (gethash name hash) entry)
    entry))

;;; vim: ft=lisp et
