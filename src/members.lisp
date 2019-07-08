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

(defun %ref-param-handler (e)
  (declare (type dotnet-error e))
  (let ((ex (dotnet-error-object e)))
    (when (string-equal "BikeInterop.RefStructParameterException"
                        (reflection-property (reflection-invoke ex "GetType")
                                             "FullName"))
      (invoke-restart 'skip))))

(defun %init-members (entry)
  (with-type-entry ((fields-hash fields)
                    (props-hash properties)
                    (methods-hash methods)
                    (entry-indexer indexer)
                    (enum-values-hash enum-values)
                    enum-p
                    members-initialized-p
                    type)
                   entry
    (handler-bind ((dotnet-error #'%ref-param-handler))
      (let ((fields (reflection-invoke type "GetFields"))
            (properties (reflection-invoke type "GetProperties"))
            (methods (reflection-invoke type "GetMethods"))
            indexer)
        (when (> (%array-length methods) 0)
          (setf methods-hash (make-hash-table :test #'equal))
          (do-bike-vector (info methods)
            (restart-case
                (%add-method-entry info methods-hash)
              (skip () ()))))
        (when (> (%array-length fields) 0)
          (setf fields-hash (make-hash-table :test #'equal))
          (do-bike-vector (info fields)
            (restart-case
                (let* ((entry (%make-field-entry info))
                       (name (%field-entry-name entry)))
                  (setf (gethash name fields-hash) entry))
              (skip () ()))))
        (when (> (%array-length properties) 0)
          (setf props-hash (make-hash-table :test #'equal))
          (do-bike-vector (info properties)
            (if (%is-indexer info)
              (setf indexer info)
              (restart-case
                  (let* ((entry (%make-property-entry info))
                         (name (%property-entry-name entry)))
                    (setf (gethash name props-hash) entry))
                (skip () ())))))
        (when indexer
          (restart-case
              (setf (%type-entry-indexer entry)
                    (%make-indexer-entry indexer))
            (skip () ())))
        (setf members-initialized-p t))
      (when enum-p
        (setf enum-values-hash (make-hash-table :test #'equal))
        (let ((integral-type (reflection-invoke type 'GetEnumUnderlyingType))
              (enum-values (reflection-invoke type 'GetEnumValues)))
          (do-bike-vector (val enum-values)
            (setf (gethash (%mknetsym (reflection-invoke type 'GetEnumName val))
                           enum-values-hash)
                  (%convert-to val integral-type t))))))
    (values)))

(defun %clear-members-cache (type-entry)
  (declare (type type-entry type-entry))
  (with-type-entry (methods properties fields indexer
                    members-initialized-p)
                   type-entry
    (setf methods nil
          properties nil
          fields nil
          indexer nil
          members-initialized-p nil))
  (values))

(defun clear-members-cache (type &optional assembly)
  (declare (type dotnet-type-designator type))
  "Clears member cache for a specified TYPE"
  (with-type-table-lock (:read)
    (let ((entry (ignore-errors (%resolve-type-entry type assembly))))
      (when entry
        (with-type-table-lock (:write)
          (%clear-members-cache entry))))))

(defun %ensure-members-initialized (entry)
  (declare (type type-entry entry))
  (with-type-entry (members-initialized-p type) entry
    (unless members-initialized-p
      (with-type-table-lock (:write)
        (if (or (generic-type-definition-p type)
                (pointer-type-p type)
                (ref-type-p type))
          (setf members-initialized-p t)
          (%init-members entry))))
    (values)))

(defun ensure-members-initialized (type)
  (declare (type dotnet-type-designator type))
  "Ensures that compiled trampolines are ready on a TYPE"
  (with-type-table-lock (:read)
    (%ensure-members-initialized
     (%resolve-type-entry type))))

(defmacro with-initialized-type-entry ((entry-var object-or-type &rest slots) &body body)
  (with-gensyms (target)
    `(with-type-table-lock (:read)
       (let* ((,target ,object-or-type)
              (,entry-var (if (dotnet-object-p ,target)
                            (%ensure-type-entry (get-type ,target))
                            (%resolve-type-entry ,target))))
         (declare (type type-entry ,entry-var))
         (%ensure-members-initialized ,entry-var)
         (with-type-entry ,slots ,entry-var ,@body)))))

(defun resolve-field (target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Resolves a field named NAME for a TARGET which can either be
 a .Net object or a type designator. Static field is resolved in
 latter case."
  (with-initialized-type-entry (entry target type fields)
    (let* ((name (%mknetsym name))
           (field (and fields (gethash name fields)))
           (instancep (dotnet-object-p target)))
      (when (or (null field)
                (and instancep (%field-entry-staticp field)))
        (error 'field-resolution-error :type type
                                       :static-p (not instancep)
                                       :member name))
      field)))

(defun resolve-property (target name)
  "Resolves a property named NAME for a TARGET which can either be
 a .Net object or a type designator. Static field is resolved in
 latter case."
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  (with-initialized-type-entry (entry target type properties)
    (let* ((name (%mknetsym name))
           (property (and properties (gethash name properties)))
           (instancep (dotnet-object-p target)))
      (when (or (null property)
                (and instancep (%property-entry-staticp property)))
        (error 'property-resolution-error :type type
                                          :static-p (not instancep)
                                          :member name))
      property)))

(defun resolve-indexer (target)
  "Resolves indexer on a TARGET"
  (declare (type dotnet-object target))
  (with-initialized-type-entry (entry target type indexer)
    (unless indexer
      (error 'indexer-resolution-error
             :type type
             :static-p nil))
    indexer))

;;; vim: ft=lisp et
