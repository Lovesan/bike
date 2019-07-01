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

(defstruct (rwlock (:constructor %make-rwlock ())
                   (:predicate rwlockp)
                   (:conc-name %rwlock-))
  "Read-Write lock with read preferrence"
  (read-lock (bt:make-lock) :type bt:lock :read-only t)
  (write-lock (bt:make-recursive-lock) :type bt:recursive-lock :read-only t)
  (read-count 0 :type non-negative-fixnum))

(defun make-rwlock ()
  "Creates an instance of Read-Write lock with read preferrence"
  (%make-rwlock))

(defun rwlock-begin-read (rwlock)
  (declare (type rwlock rwlock))
  (bt:with-lock-held ((%rwlock-read-lock rwlock))
    (when (= 1 (incf (%rwlock-read-count rwlock)))
      (bt:acquire-lock (%rwlock-write-lock rwlock))))
  (values))

(defun rwlock-end-read (rwlock)
  (declare (type rwlock rwlock))
  (bt:with-lock-held ((%rwlock-read-lock rwlock))
    (when (zerop (decf (%rwlock-read-count rwlock)))
      (bt:release-lock (%rwlock-write-lock rwlock))))
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
       (bt:acquire-recursive-lock (%rwlock-write-lock ,lock))
       (unwind-protect (locally ,@body)
         (bt:release-recursive-lock (%rwlock-write-lock ,lock))))))

;;; vim: ft=lisp et
