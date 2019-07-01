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

(defun %init-members (type entry)
  (handler-bind ((dotnet-error #'%ref-param-handler))
    (let ((fields (reflection-invoke type "GetFields"))
          (fields-hash (%type-entry-fields entry))
          (properties (reflection-invoke type "GetProperties"))
          (props-hash (%type-entry-properties entry))
          (methods (reflection-invoke type "GetMethods"))
          (methods-hash (%type-entry-methods entry))
          indexer)
      (do-bike-vector (info methods)
        (restart-case
            (%add-method-entry info methods-hash)
          (skip () ())))
      (do-bike-vector (info fields)
        (restart-case
            (let* ((entry (%make-field-entry info))
                   (name (%field-entry-name entry)))
              (setf (gethash name fields-hash) entry))
          (skip () ())))
      (do-bike-vector (info properties)
        (if (%is-indexer info)
          (setf indexer info)
          (restart-case
              (let* ((entry (%make-property-entry info))
                     (name (%property-entry-name entry)))
                (setf (gethash name props-hash) entry))
            (skip () ()))))
      (when indexer
        (restart-case
            (setf (%type-entry-indexer entry)
                  (%make-indexer-entry indexer))
          (skip () ())))
      (setf (%type-entry-members-initialized-p entry) t))))

(defun %clear-members-cache (type-entry)
  (declare (type type-entry type-entry))
  (clrhash (%type-entry-methods type-entry))
  (clrhash (%type-entry-properties type-entry))
  (clrhash (%type-entry-fields type-entry))
  (setf (%type-entry-indexer type-entry) nil
        (%type-entry-members-initialized-p type-entry) nil)
  (values))

(defun clear-members-cache (type)
  (declare (type dotnet-type-designator type))
  "Clears member cache for a specified TYPE"
  (with-type-table (data ns lock)
    (with-read-lock (lock)
      (multiple-value-bind (type entry)
          (%resolve-type type nil)
        (declare (ignore type))
        (when entry
          (with-write-lock (lock)
            (%clear-members-cache entry)))))))

(defun %ensure-members-initialized (type entry)
  (declare (type type-entry entry)
           (type dotnet-object type))
  (unless (%type-entry-members-initialized-p entry)
    (with-type-table-lock (:write)
      (%init-members type entry)))
  (values))

(defun ensure-members-initialized (type)
  (declare (type dotnet-type-designator type))
  "Ensures that compiled trampolines are ready on a TYPE"
  (with-type-table-lock (:read)
    (multiple-value-bind (type entry) (%resolve-type type t)
      (%ensure-members-initialized type entry))))

(defun resolve-field (target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Resolves a field named NAME for a TARGET which can either be
 a .Net object or a type designator. Static field is resolved in
 latter case."
  (with-type-table-lock (:read)
    (multiple-value-bind (type entry) (%ensure-type target)
      (%ensure-members-initialized type entry)
      (let* ((name (%mknetsym name))
             (field (gethash name (%type-entry-fields entry)))
             (instancep (dotnet-object-p target)))
        (when (or (null field)
                  (and instancep (%field-entry-staticp field)))
          (error 'field-resolution-error :type type
                                         :static-p (not instancep)
                                         :member name))
        field))))

(defun resolve-property (target name)
  "Resolves a property named NAME for a TARGET which can either be
 a .Net object or a type designator. Static field is resolved in
 latter case."
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  (with-type-table-lock (:read)
    (multiple-value-bind (type entry) (%ensure-type target)
      (%ensure-members-initialized type entry)
      (let* ((name (%mknetsym name))
             (property (gethash name (%type-entry-properties entry)))
             (instancep (dotnet-object-p target)))
        (when (or (null property)
                  (and instancep (%property-entry-staticp property)))
          (error 'property-resolution-error :type type
                                            :static-p (not instancep)
                                            :member name))
        property))))

(defun resolve-indexer (target)
  "Resolves indexer on a TARGET"
  (declare (type dotnet-object target))
  (with-type-table-lock (:read)
    (multiple-value-bind (type entry) (%ensure-type target)
      (%ensure-members-initialized type entry)
      (let ((indexer (%type-entry-indexer entry)))
        (unless indexer
          (error 'indexer-resolution-error :type type
                                           :static-p nil))
        indexer))))

;;; vim: ft=lisp et
