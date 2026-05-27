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

(deftype member-kind () '(member :method :property :field :event :indexer :constructor))

(defstruct (invocation-entry (:constructor make-ientry)
                             (:copier nil)
                             (:predicate invocation-entry-p)
                             (:conc-name ientry-))
  (type (required-slot) :type dotnet-type :read-only t)
  (name (required-slot) :type simple-character-string :read-only t)
  (kind (required-slot) :type member-kind :read-only t)
  (hash-code 0 :type non-negative-fixnum :read-only t)
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

(defconstant +invocation-cache-soft-cap-ratio+ 0.8)

(defconstant +invocation-cache-rehash-threshold+ 0.75)

(defconstant +invocation-cache-hash-bits+ (integer-length most-positive-fixnum))

(defconstant +invocation-cache-hash-mask+ most-positive-fixnum)

(defconstant +invocation-cache-hash-rotation+ 5)

(defconstant +invocation-cache-hash-left-mask+
  (ash +invocation-cache-hash-mask+ (- +invocation-cache-hash-rotation+)))

(defconstant +invocation-cache-hash-salt+
  (logand #x9E3779B97F4A7C15 +invocation-cache-hash-mask+))

(defstruct (invocation-cache (:constructor make-icache)
                             (:copier nil)
                             (:predicate invocation-cache-p)
                             (:conc-name icache-))
  (buckets (make-array +min-invocation-cache-buckets+
                       :initial-element nil)
   :type simple-vector)
  (count 0 :type non-negative-fixnum)
  (random-state (make-random-state t)
   :type random-state
   :read-only t)
  (lock (make-rwlock :name "Invocation cache lock")
   :type rwlock
   :read-only t))

(define-global-var -invocation-cache- nil)
(declaim (type (or null invocation-cache) -invocation-cache-))

(defmacro with-icache ((&rest slots)  &body body)
  (with-gensyms (cache)
    (let ((accessors (%collect-accessors 'icache- slots)))
      `(let ((,cache -invocation-cache-))
         (declare (type invocation-cache ,cache))
         (with-accessors ,accessors ,cache ,@body)))))

(declaim (inline normalize-invocation-hash-code))
(defun normalize-invocation-hash-code (code)
  (declare (type integer code))
  (the non-negative-fixnum
       (logand code +invocation-cache-hash-mask+)))

(declaim (inline rotate-invocation-hash-code))
(defun rotate-invocation-hash-code (code)
  (declare (type non-negative-fixnum code))
  (the non-negative-fixnum
       (logior (ash (logand code +invocation-cache-hash-left-mask+)
                    +invocation-cache-hash-rotation+)
               (ash code
                    (- +invocation-cache-hash-rotation+
                       +invocation-cache-hash-bits+)))))

(declaim (inline invocation-hash-combine))
(defun invocation-hash-combine (code value)
  (declare (type non-negative-fixnum code)
           (type integer value))
  (let ((value (normalize-invocation-hash-code value)))
    (declare (type non-negative-fixnum value))
    (the non-negative-fixnum
         (logand (logxor (rotate-invocation-hash-code code)
                         value
                         (ash value -7)
                         +invocation-cache-hash-salt+)
                 +invocation-cache-hash-mask+))))

(defun ientry-hash-code* (type name kind &key (type-arg-count 0)
                                              (type-args '())
                                              (arg-type-count 0)
                                              (arg-types '()))
  (declare (type dotnet-type type)
           (type simple-character-string name)
           (type member-kind kind)
           (type non-negative-fixnum type-arg-count arg-type-count)
           (type list type-args arg-types))
  (let ((code (normalize-invocation-hash-code (%get-hash-code type))))
    (declare (type non-negative-fixnum code))
    (setf code (invocation-hash-combine code (sxhash name))
          code (invocation-hash-combine code (sxhash kind))
          code (invocation-hash-combine code (sxhash type-arg-count))
          code (invocation-hash-combine code (sxhash arg-type-count)))
    (dolist (type-arg type-args)
      (setf code (invocation-hash-combine code (%get-hash-code type-arg))))
    (dolist (arg-type arg-types code)
      (setf code (invocation-hash-combine code (%get-hash-code arg-type))))))

(defun ientry-equals* (entry type* name* kind* &key ((:type-arg-count type-arg-count*) 0)
                                                    ((:type-args type-args*) '())
                                                    ((:arg-type-count arg-type-count*) 0)
                                                    ((:arg-types arg-types*) '()))
  (declare (type invocation-entry entry)
           (type dotnet-type type*)
           (type simple-character-string name*)
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
           (type simple-character-string name)
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
              (when (and (= hash-code (ientry-hash-code entry))
                         (ientry-equals* entry type name kind
                                         :type-arg-count type-arg-count
                                         :type-args type-args
                                         :arg-type-count arg-type-count
                                         :arg-types arg-types))
                (return entry))))))

(defun get-ientry (type name kind &key (type-arg-count 0)
                                       (type-args '())
                                       (arg-type-count 0)
                                       (arg-types '()))
  (declare (type dotnet-type type)
           (type simple-character-string name)
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

(declaim (inline invocation-cache-soft-cap))
(defun invocation-cache-soft-cap ()
  (the non-negative-fixnum
       (floor (* +max-invocation-cache-count+
                 +invocation-cache-soft-cap-ratio+))))

(defun %icache-bucket-chain-length (entry)
  (declare (type (or null invocation-entry) entry))
  (loop :for e = entry :then (ientry-next e)
        :while e
        :count t))

(defun %evict-icache-entry-from-bucket (vector bucket random-state)
  (declare (type simple-vector vector)
           (type non-negative-fixnum bucket)
           (type random-state random-state))
  (let* ((entry (svref vector bucket))
         (chain-length (%icache-bucket-chain-length entry))
         (victim-index (random chain-length random-state))
         prev)
    (declare (type positive-fixnum chain-length)
             (type non-negative-fixnum victim-index)
             (type (or null invocation-entry) entry prev))
    (dotimes (i victim-index)
      (setf prev entry
            entry (ientry-next entry)))
    (if prev
      (setf (ientry-next prev) (ientry-next entry))
      (setf (svref vector bucket) (ientry-next entry)))
    (setf (ientry-next entry) nil)
    (values)))

(defun %evict-random-icache-entry (vector random-state)
  (declare (type simple-vector vector)
           (type random-state random-state))
  (let* ((size (length vector))
         (start (random size random-state)))
    (declare (type positive-fixnum size)
             (type non-negative-fixnum start))
    (dotimes (offset size)
      (let ((bucket (mod (+ start offset) size)))
        (declare (type non-negative-fixnum bucket))
        (when (svref vector bucket)
          (%evict-icache-entry-from-bucket vector bucket random-state)
          (return t))))))

(defun %evict-icache-entries ()
  (with-icache (buckets count random-state)
    (let* ((vector buckets)
           (soft-cap (invocation-cache-soft-cap))
           (evict-count (max 0 (- count soft-cap))))
      (declare (type non-negative-fixnum soft-cap evict-count))
      (dotimes (i evict-count)
        (declare (ignorable i))
        (unless (%evict-random-icache-entry vector random-state)
          (return))
        (decf count)))))

(defun %resize-icache ()
  (with-icache (buckets)
    (let* ((vector buckets)
           (size (length vector))
           (new-size (min most-positive-fixnum
                          (1+ (* size 2)))))
      (when (> new-size size)
        (let ((new-vector (make-array new-size
                                      :initial-element nil)))
          (dotimes (i size)
            (do ((entry (svref vector i) next)
                 next)
                ((null entry))
              (setf next (ientry-next entry))
              (let* ((nbucket (mod (ientry-hash-code entry) new-size))
                     (prev (svref new-vector nbucket)))
                (setf (svref new-vector nbucket) entry
                      (ientry-next entry) prev))))
          (setf buckets new-vector)))))
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
           (type simple-character-string name)
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
            (when (>= count +max-invocation-cache-count+)
              (%evict-icache-entries)
              (setf vector buckets))
            (when (> (/ (float count) (length vector))
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
                                       :name (copy-seq name)
                                       :kind kind
                                       :hash-code hash-code
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
  (setf -invocation-cache- (make-icache))
  (values))

(register-image-restore-hook 'initialize-invocation-cache
                             (null -invocation-cache-))

;;; vim: ft=lisp et
