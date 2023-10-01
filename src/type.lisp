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

(deftype dotnet-type-designator ()
  '(or dotnet-type string-designator (cons string-designator list)))

(deftype dotnet-method-designator ()
  '(or string-designator (cons string-designator (cons dotnet-type-designator list))))

(defstruct (type-table (:constructor %type-table ())
                       (:predicate type-table-p)
                       (:conc-name %type-table-))
  "Represents a storage for loaded types and used namespaces"
  (data (make-hash-table :test 'equal) :type hash-table
                                       :read-only t)
  (namespaces '() :type list)
  (aliases (make-hash-table :test 'equal) :type hash-table
                                          :read-only t)
  (lock (make-rwlock :name "Type table lock")
   :type rwlock
   :read-only t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +max-array-rank+ 32))

(defstruct (type-entry (:constructor
                           %make-type-entry
                           (type &key qualified-name
                                      (type-args '())
                                      (rank 0)
                                      element-entry
                                 &aux
                                 (hash-code (%get-hash-code type))
                                 (delegate-p (delegate-type-p type))
                                 (generic-p (not (endp type-args)))
                                 (array-p (> rank 0))
                                 (pointer-p (pointer-type-p type))
                                 (ref-p (ref-type-p type))
                                 (enum-p (enum-type-p type))
                                 (enum-values (and enum-p (get-enum-hash-table type)))
                                 (type-arg-count (length type-args))
                                 (has-members-p (not (or pointer-p ref-p)))))
                       (:predicate type-entry-p)
                       (:conc-name %type-entry-)
                       (:copier nil))
  "Represents an entry in the type table"
  (type (required-slot) :type dotnet-type :read-only t)
  (hash-code 0 :type (signed-byte 32) :read-only t)
  (qualified-name nil :type (or null string) :read-only t)
  (generic-p nil :type boolean :read-only t)
  (pointer-p nil :type boolean :read-only t)
  (delegate-p nil :type boolean :read-only t)
  (array-p nil :type boolean :read-only t)
  (enum-p nil :type boolean :read-only t)
  (has-members-p nil :type boolean :read-only t)
  (ref-p nil :type boolean :read-only t)
  (element-entry nil :type (or null type-entry) :read-only t)
  (rank 0 :type (integer 0 32) :read-only t)
  (type-args '() :type list :read-only t)
  (type-arg-count 0 :type non-negative-fixnum :read-only t)
  (instances '() :type (or (cons type-entry list) null))
  (ref-entry nil :type (or null type-entry))
  (pointer-entry nil :type (or null type-entry))
  (enum-values nil :type (or null hash-table) :read-only t)
  (array-entries nil :type (or null (simple-array (or null type-entry) (#.+max-array-rank+))))
  (mz-vector-entry nil :type (or null type-entry)))

(define-global-var -type-table- nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %collect-accessors (conc-name slots)
    (declare (type list slots)
             (type string-designator conc-name))
    (loop :for form :in slots
          :for slot = (if (consp form) (second form) form)
          :for name = (if (consp form) (first form) slot)
          :for accessor = (symbolicate conc-name slot)
          :collect `(,name ,accessor))))

(defmacro with-type-entry ((&rest slots) entry-form &body body)
  (with-gensyms (type-entry)
    (let ((accessors (%collect-accessors '%type-entry- slots)))
      `(let ((,type-entry ,entry-form))
         (declare (type type-entry ,type-entry))
         (with-accessors ,accessors ,type-entry ,@body)))))

(defmacro with-type-table ((&rest slots) &body body)
  (with-gensyms (table)
    (let ((accessors (%collect-accessors '%type-table- slots)))
      `(let ((,table -type-table-))
         (declare (type type-table ,table))
         (with-accessors ,accessors ,table ,@body)))))

(defmacro with-type-table-lock ((&optional (type :read)) &body body)
  (declare (type (member :read :write) type))
  (with-gensyms (table lock)
    `(let ((,table -type-table-))
       (declare (type type-table ,table))
       (with-accessors ((,lock %type-table-lock))
           ,table
         (,(ecase type (:read 'with-read-lock) (:write 'with-write-lock))
          (,lock)
          ,@body)))))

(declaim (inline %make-namespace-prefix))
(defun %make-namespace-prefix (namespace)
  (declare (type string-designator namespace))
  (string-upcase (concatenate 'string (string namespace) ".")))

(defun %use-namespace (namespace)
  (declare (type string-designator namespace))
  (let ((prefix (%make-namespace-prefix namespace)))
    (with-type-table (namespaces)
      (pushnew prefix namespaces :test #'equal)
      (values))))

(defun use-namespace (namespaces)
  (declare (type (or list string-designator) namespaces))
  "Adds a namespace (or a list of namespaces) to the list of used namespaces."
  (with-type-table-lock (:write)
    (if (listp namespaces)
      (dolist (ns namespaces)
        (%use-namespace ns))
      (%use-namespace namespaces)))
  (values))

(defun %unuse-namespace (namespace)
  (declare (type string-designator namespace))
  (let ((prefix (%make-namespace-prefix namespace)))
    (with-type-table (namespaces)
      (removef namespaces prefix :test #'equal))
    (values)))

(defun unuse-namespace (namespaces)
  (declare (type (or list string-designator) namespaces))
  "Removes a namespace (or a list of namespaces) from the list of used namespaces."
  (with-type-table-lock (:write)
    (if (listp namespaces)
      (dolist (ns namespaces)
        (%unuse-namespace ns))
      (%unuse-namespace namespaces)))
  (values))

(defun unuse-all-namespaces ()
  "Clears the list of used namespaces."
  (with-type-table (namespaces lock)
    (with-write-lock (lock)
      (setf namespaces '()))))

(defun namespace-used-p (namespace)
  (let ((prefix (%make-namespace-prefix namespace)))
    (with-type-table (namespaces lock)
      (with-read-lock (lock)
        (member prefix namespaces :test #'string=)))))

(defun get-used-namespaces ()
  (with-type-table (namespaces lock)
    (with-read-lock (lock)
      (loop :for p :of-type string :in namespaces
            :for plen = (length p)
            :collect (subseq p 0 (1- plen))))))

(defun use-type-alias (alias type)
  (declare (type string-designator alias)
           (type dotnet-type-designator type))
  "Adds an ALIAS for TYPE in a type cache. TYPE designator must
 not contain actual type objects if current lisp image
 is to be restored."
  (with-type-table (lock aliases)
    (with-write-lock (lock)
      (setf (gethash (simple-character-string-upcase alias) aliases) type)))
  (values))

(defun unuse-type-alias (alias)
  (declare (type string-designator alias))
  "Removes an alias from the current type cache."
  (with-type-table (lock aliases)
    (with-write-lock (lock)
      (remhash (simple-character-string-upcase alias) aliases)))
  (values))

(defun unuse-all-type-aliases ()
  "Clears current type aliases cache."
  (with-type-table (lock aliases)
    (with-write-lock (lock)
      (clrhash aliases)))
  (values))

(defmethod print-object ((object type-entry) stream)
  (print-unreadable-object (object stream :type t)
    (with-type-entry (type) object
      (let ((gc-handle (pointer-address (%dotnet-type-handle type)))
            (name (%to-string type)))
        (format stream "~a {~8,'0X}" name gc-handle)))
    object))

;;; vim: ft=lisp et
