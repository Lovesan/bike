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

;; Implement our own version of DLR

(deftype member-kind () '(member :method :property :field :indexer :constructor))

(defstruct (invocation-entry (:constructor make-ientry)
                             (:copier nil)
                             (:predicate invocation-entry-p)
                             (:conc-name ientry-))
  (type (required-slot) :type dotnet-type :read-only t)
  (name (required-slot) :type dotnet-name :read-only t)
  (kind (required-slot) :type member-kind :read-only t)
  (type-arg-count 0 :type non-negative-fixnum :read-only t)
  (type-args '() :type list :read-only t)
  (arg-type-count 0 :type non-negative-fixnum :read-only t)
  (arg-types '() :type list :read-only t)
  (reader nil :type (or null function) :read-only t)
  (reader-delegate nil :type (or null dotnet-object) :read-only t)
  (writer nil :type (or null function) :read-only t)
  (writer-delegate nil :type (or null dotnet-object) :read-only t)
  (next nil :type (or null invocation-entry)))

(defmacro with-ientry ((&rest slots) entry-form &body body)
  (with-gensyms (entry)
    (let ((accessors (%collect-accessors 'ientry- slots)))
      `(let ((,entry ,entry-form))
         (declare (type invocation-entry ,entry))
         (with-accessors ,accessors ,entry ,@body)))))

(defconstant +min-invocation-cache-buckets+ 64)

(defconstant +max-invocation-cache-count+ (min most-positive-fixnum
                                               10000))

(defconstant +invocation-cache-rehash-threshold+ 1.5)

(defstruct (invocation-cache (:constructor make-icache)
                             (:copier nil)
                             (:predicate invocation-cache-p)
                             (:conc-name icache-))
  (buckets (make-array +min-invocation-cache-buckets+
                       :initial-element nil)
   :type simple-vector)
  (count 0 :type non-negative-fixnum)
  (lock (make-rwlock) :type rwlock :read-only t))

(#+sbcl sb-ext:defglobal #-sbcl defvar +invocation-cache+ nil)

(declaim (type (or null invocation-cache) +invocation-cache+))

(defmacro with-icache ((&rest slots)  &body body)
  (with-gensyms (cache)
    (let ((accessors (%collect-accessors 'icache- slots)))
      `(let ((,cache +invocation-cache+))
         (declare (type invocation-cache ,cache))
         (with-accessors ,accessors ,cache ,@body)))))

(defun ientry-hash-code* (type name kind &key (type-arg-count 0)
                                              (type-args '())
                                              (arg-type-count 0)
                                              (arg-types '()))
  (declare (type dotnet-type type)
           (type dotnet-name name)
           (type member-kind kind)
           (type non-negative-fixnum type-arg-count arg-type-count)
           (type list type-args arg-types))
  (let ((code (logxor (%get-hash-code type)
                      (sxhash name)
                      (sxhash kind)
                      (sxhash type-arg-count)
                      (sxhash arg-type-count))))
    (declare (type (signed-byte #.+pointer-bits+) code))
    (dolist (type-arg type-args)
      (setf code (logxor code (%get-hash-code type-arg))))
    (dolist (arg-type arg-types code)
      (setf code (logxor code (%get-hash-code arg-type))))))

(defun ientry-hash-code (entry)
  (declare (type invocation-entry entry))
  (with-ientry (type name kind type-arg-count type-args arg-type-count arg-types) entry
    (ientry-hash-code* type name kind :type-arg-count type-arg-count
                                      :type-args type-args
                                      :arg-type-count arg-type-count
                                      :arg-types arg-types)))

(defun ientry-equals* (entry type* name* kind* &key ((:type-arg-count type-arg-count*) 0)
                                                    ((:type-args type-args*) '())
                                                    ((:arg-type-count arg-type-count*) 0)
                                                    ((:arg-types arg-types*) '()))
  (declare (type invocation-entry entry)
           (type dotnet-type type*)
           (type dotnet-name name*)
           (type member-kind kind*)
           (type non-negative-fixnum arg-type-count* type-arg-count*)
           (type list arg-types* type-args*))
  (with-ientry (type name kind type-arg-count type-args arg-type-count arg-types) entry
    (flet ((types-equal (left right)
             (declare (type list left right))
             (loop :for a :of-type dotnet-type :in left
                   :and b :of-type dotnet-type :in right
                   :when (not (bike-equals a b))
                     :do (return nil)
                   :finally (return t))))
      (and (bike-equals type type*)
           (string= name name*)
           (eq kind kind*)
           (= type-arg-count type-arg-count*)
           (= arg-type-count arg-type-count*)
           (types-equal type-args type-args*)
           (types-equal arg-types arg-types*)))))

(defun ientry-equals (left right)
  (declare (type invocation-entry left right))
  (with-ientry (type name kind type-arg-count type-args arg-type-count arg-types) right
    (ientry-equals* left type name kind :type-arg-count type-arg-count
                                        :type-args type-args
                                        :arg-type-count arg-type-count
                                        :arg-types arg-types)))

(defun %get-ientry (type name kind &key (type-arg-count 0)
                                        (type-args '())
                                        (arg-type-count 0)
                                        (arg-types '()))
  (declare (type dotnet-type type)
           (type dotnet-name name)
           (type member-kind kind)
           (type non-negative-fixnum type-arg-count arg-type-count)
           (type list type-args arg-types))
  (with-icache (buckets)
    (let* ((vector buckets)
           (hash-code (ientry-hash-code* type name kind
                                         :type-arg-count type-arg-count
                                         :type-args type-args
                                         :arg-type-count arg-type-count
                                         :arg-types arg-types))
           (nbucket (mod hash-code (length vector))))
      (loop :for entry = (svref vector nbucket)
              :then (ientry-next entry)
            :while entry :do
              (when (ientry-equals* entry type name kind
                                    :type-arg-count type-arg-count
                                    :type-args type-args
                                    :arg-type-count arg-type-count
                                    :arg-types arg-types)
                (return entry))))))

(defun get-ientry (type name kind &key (type-arg-count 0)
                                       (type-args '())
                                       (arg-type-count 0)
                                       (arg-types '()))
  (declare (type dotnet-type type)
           (type dotnet-name name)
           (type member-kind kind)
           (type non-negative-fixnum type-arg-count arg-type-count)
           (type list type-args arg-types))
  "Retrieves invocation entry from invocation cache"
  (with-icache (lock)
    (with-read-lock (lock)
      (%get-ientry type name kind
                   :type-arg-count type-arg-count
                   :type-args type-args
                   :arg-type-count arg-type-count
                   :arg-types arg-types))))

(defun %clear-invocation-cache ()
  (with-icache (buckets count lock)
    (let ((vector buckets))
      (setf count 0)
      (dotimes (i (length vector))
        (setf (svref vector i) nil))))
  (values))

(defun clear-invocation-cache ()
  "Clears an invocation cache"
  (with-icache (lock)
    (with-write-lock (lock)
      (%clear-invocation-cache))))

(defun %resize-icache ()
  (with-icache (buckets count)
    (if (= count +max-invocation-cache-count+)
      ;; simply clear the cache
      ;; TODO: do something more smart
      (%clear-invocation-cache)
      (let* ((vector buckets)
             (size (length vector))
             (new-size (min most-positive-fixnum
                            (1+ (* size 2)))))
        (when (> new-size size)
          (let ((new-vector (make-array new-size
                                        :initial-element nil)))
            (dotimes (i size)
              (let ((bucket-entries (loop :for entry = (svref vector i)
                                            :then (ientry-next entry)
                                          :while entry :collect entry)))
                (dolist (entry bucket-entries)
                  (let* ((hash-code (ientry-hash-code entry))
                         (nbucket (mod hash-code new-size))
                         (prev (svref new-vector nbucket)))
                    (setf (svref new-vector nbucket) entry
                          (ientry-next entry) prev)))))
            (setf buckets new-vector))))))
  (values))


(defun add-ientry (type name kind &key (type-arg-count 0)
                                       (type-args '())
                                       (arg-type-count 0)
                                       (arg-types '())
                                       (reader nil)
                                       (reader-delegate nil)
                                       (writer nil)
                                       (writer-delegate nil))
  (declare (type dotnet-type type)
           (type dotnet-name name)
           (type member-kind kind)
           (type non-negative-fixnum type-arg-count arg-type-count)
           (type list type-args arg-types)
           (type (or null function) reader writer)
           (type (or null dotnet-object) reader-delegate writer-delegate))
  "Gets or adds an invocation entry to invocation cache"
  (with-icache (buckets count lock)
    (with-write-lock (lock)
      (or (%get-ientry type name kind
                       :type-arg-count type-arg-count
                       :type-args type-args
                       :arg-type-count arg-type-count
                       :arg-types arg-types)
          (let ((vector buckets))
            (when (> (/ count (length vector))
                     +invocation-cache-rehash-threshold+)
              (%resize-icache)
              (setf vector buckets))
            (let* ((hash-code (ientry-hash-code* type name kind
                                                 :type-arg-count type-arg-count
                                                 :type-args type-args
                                                 :arg-type-count arg-type-count
                                                 :arg-types arg-types))
                   (nbucket (mod hash-code (length vector)))
                   (prev (svref vector nbucket))
                   (entry (make-ientry :type type
                                       :name name
                                       :kind kind
                                       :type-arg-count type-arg-count
                                       :type-args type-args
                                       :arg-type-count arg-type-count
                                       :arg-types arg-types
                                       :reader reader
                                       :reader-delegate reader-delegate
                                       :writer writer
                                       :writer-delegate writer-delegate
                                       :next prev)))
              (setf (svref vector nbucket) entry)
              (incf count)
              entry))))))

(defun initialize-invocation-cache ()
  (setf +invocation-cache+ (make-icache))
  (values))


(uiop:register-image-restore-hook #'initialize-invocation-cache
                                  (null +invocation-cache+))

;;; vim: ft=lisp et
