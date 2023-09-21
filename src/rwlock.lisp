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

(in-package #:bike-internals)

(defconstant +min-rwlock-readers-size+ 16)

(defstruct (rwlock (:constructor %make-rwlock (name))
                   (:predicate rwlockp)
                   (:conc-name %rwlock-))
  "Read-Write lock with read preferrence"
  (name nil :type (or null string))
  (cv-lock (bt2:make-lock) :type bt2:lock :read-only t)
  (reader-cv (bt2:make-condition-variable)
   :type bt2:condition-variable :read-only t)
  (writer-cv (bt2:make-condition-variable)
   :type bt2:condition-variable :read-only t)
  (reader-count 0 :type fixnum)
  (writer-count 0 :type fixnum)
  (waiting-reader-count 0 :type fixnum)
  (waiting-writer-count 0 :type fixnum)
  ;; Collecting readers into a bag probably decreases efficiency a bit
  ;;  but prevents certain kinds of deadlocks
  (readers (make-array +min-rwlock-readers-size+ :initial-element nil)
   :type simple-array)
  (readers-fill-pointer 0 :type (integer 0 #.(1- array-total-size-limit)))
  (writer nil :type (or null bt2:thread)))

(defun make-rwlock (&key name)
  (declare (type (or null string) name))
  "Creates an instance of Read-Write lock with read preferrence"
  (%make-rwlock name))

(defun rwlock-name (rwlock)
  (declare (type rwlock rwlock))
  "Retrieves name of a read-write lock"
  (%rwlock-name rwlock))

(declaim (inline rwlock-reader-position))
(defun rwlock-reader-position (rwlock thread)
  (declare (type rwlock rwlock))
  (loop :with readers :of-type simple-vector = (%rwlock-readers rwlock)
        :with end :of-type non-negative-fixnum = (%rwlock-readers-fill-pointer rwlock)
        :with i :of-type fixnum = (1- end) :do
          (when (< i 0) (return))
          (when (eq thread (svref readers i))
            (return i))
          (decf i)))

(defun rwlock-remove-reader (rwlock thread)
  (declare (type rwlock rwlock))
  (let ((pos (rwlock-reader-position rwlock thread)))
    (when pos
      (setf (svref (%rwlock-readers rwlock) pos) nil)
      pos)))

(declaim (inline rwlock-ensure-readers-storage))
(defun rwlock-ensure-readers-storage (rwlock)
  (declare (type rwlock rwlock))
  (let* ((readers (%rwlock-readers rwlock))
         (size (length readers))
         (end (%rwlock-readers-fill-pointer rwlock)))
    (unless (< end size)
      (let ((new-readers (make-array (1+ (* size 2)) :initial-element nil)))
        (replace new-readers readers :start1 0 :end1 size
                                     :start2 0 :end2 size)
        (setf (%rwlock-readers rwlock) new-readers)))
    (values)))

(defun rwlock-add-reader (rwlock thread)
  (declare (type rwlock rwlock))
  (with-accessors ((end %rwlock-readers-fill-pointer)
                   (readers %rwlock-readers))
      rwlock
    (let ((free-pos (rwlock-reader-position rwlock nil)))
      (unless free-pos
        (rwlock-ensure-readers-storage rwlock)
        (setf free-pos end)
        (incf end))
      (setf (svref readers free-pos) thread)
      free-pos)))

(defun rwlock-begin-read (rwlock)
  (declare (type rwlock rwlock))
  (with-accessors ((lock %rwlock-cv-lock)
                   (reader-cv %rwlock-reader-cv)
                   (reader-count %rwlock-reader-count)
                   (writer-count %rwlock-writer-count)
                   (waiting-count %rwlock-waiting-reader-count)
                   (readers %rwlock-readers)
                   (writer %rwlock-writer))
      rwlock
    (let ((self (bt2:current-thread)))
      (bt2:with-lock-held (lock)
        (loop
          (cond ((or (plusp reader-count)
                     (eq writer self))
                 (incf reader-count)
                 (rwlock-add-reader rwlock self)
                 (return))
                ((zerop writer-count)
                 (unless (null writer)
                   (error "Dangling writer in rwlock ~s" rwlock))
                 (incf reader-count)
                 (rwlock-add-reader rwlock self)
                 (return))
                (t (incf waiting-count)
                   (bt2:condition-wait reader-cv lock)
                   (decf waiting-count)))))))
  (values))

(defun rwlock-end-read (rwlock)
  (declare (type rwlock rwlock))
  (with-accessors ((lock %rwlock-cv-lock)
                   (writer-cv %rwlock-writer-cv)
                   (reader-count %rwlock-reader-count)
                   (writer-count %rwlock-writer-count)
                   (waiting-writer-count %rwlock-waiting-writer-count)
                   (writer %rwlock-writer))
      rwlock
    (let ((self (bt2:current-thread)) wake-writer)
      (bt2:with-lock-held (lock)
        (unless (plusp reader-count)
          (error "Non-positive reader count in rwlock ~s" rwlock))
        (unless (rwlock-remove-reader rwlock self)
          (error "Corrupted reader list in rwlock ~s" rwlock))
        (when (zerop (decf reader-count))
          (when (plusp waiting-writer-count)
            (setf wake-writer t))))
      (when wake-writer (bt2:condition-notify writer-cv))))
  (values))

(defun rwlock-begin-write (rwlock)
  (declare (type rwlock rwlock))
  (with-accessors ((lock %rwlock-cv-lock)
                   (writer-cv %rwlock-writer-cv)
                   (reader-count %rwlock-reader-count)
                   (writer-count %rwlock-writer-count)
                   (waiting-count %rwlock-waiting-writer-count)
                   (writer %rwlock-writer))
      rwlock
    (let ((self (bt2:current-thread)))
      (bt2:with-lock-held (lock)
        (loop (when (eql writer self)
                (incf writer-count)
                (return))
              (when (zerop writer-count)
                (unless (null writer)
                  (error "Dangling writer in rwlock ~s" rwlock))
                (when (or (zerop reader-count)
                          ;; upgrade to write lock
                          (rwlock-reader-position rwlock self))
                  (incf writer-count)
                  (setf writer self)
                  (return)))
              (incf waiting-count)
              (bt2:condition-wait writer-cv lock)
              (decf waiting-count)))))
  (values))

(defun rwlock-end-write (rwlock)
  (declare (type rwlock rwlock))
  (with-accessors ((lock %rwlock-cv-lock)
                   (reader-cv %rwlock-reader-cv)
                   (writer-cv %rwlock-writer-cv)
                   (reader-count %rwlock-reader-count)
                   (writer-count %rwlock-writer-count)
                   (waiting-writer-count %rwlock-waiting-writer-count)
                   (waiting-reader-count %rwlock-waiting-reader-count)
                   (writer %rwlock-writer))
      rwlock
    (let ((self (bt2:current-thread))
          wake-writer wake-readers)
      (bt2:with-lock-held (lock)
        (unless (eq writer self)
          (error "~s does not own write lock ~s" self rwlock))
        (unless (plusp writer-count)
          (error "Non-positive writer count in rwlock ~s" rwlock))
        (decf writer-count)
        (when (zerop writer-count)
          (setf writer nil)
          ;; prefer waking up readers
          (cond ((plusp waiting-reader-count)
                 (setf wake-readers t))
                ((plusp waiting-writer-count)
                 (setf wake-writer t)))))
      (when wake-readers
        (bt2:condition-broadcast reader-cv))
      (when wake-writer
        (bt2:condition-notify writer-cv))))
  (values))

(defmacro with-read-lock ((rwlock) &body body)
  (with-gensyms (lock)
    `(let ((,lock ,rwlock))
       (declare (type rwlock ,lock))
       (rwlock-begin-read ,rwlock)
       (unwind-protect (locally ,@body)
         (rwlock-end-read ,lock)))))

(defmacro with-write-lock ((rwlock) &body body)
  (with-gensyms (lock)
    `(let ((,lock ,rwlock))
       (declare (type rwlock ,lock))
       (rwlock-begin-write ,lock)
       (unwind-protect (locally ,@body)
         (rwlock-end-write ,lock)))))

;;; vim: ft=lisp et
