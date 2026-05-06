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

(defconstant +default-handle-table-size+ 4096)
(defconstant +handle-table-active+ -1)

(defstruct (handle-table (:constructor %handle-table ())
                         (:conc-name %handle-table-)
                         (:predicate handle-table-p))
  (data (make-array +default-handle-table-size+
                    :initial-element nil)
   :type (simple-array t (*)))
  (weak-flags (make-array +default-handle-table-size+
                          :element-type 'bit
                          :initial-element 0)
   :type simple-bit-vector)
  (free-next (make-array +default-handle-table-size+
                         :element-type 'fixnum
                         :initial-element 0)
   :type (simple-array fixnum (*)))
  (length 1 :type fixnum)
  (free-head 0 :type fixnum)
  (lock (make-rwlock :name "Handle table lock")
   :type rwlock
   :read-only t))

(define-global-var -handle-table- nil)
(declaim (type (or null handle-table) -handle-table-))

(defun init-handle-table ()
  (setf -handle-table- (%handle-table)))

(register-image-restore-hook 'init-handle-table (null -handle-table-))

(defmacro with-handle-table ((data-var &optional (length-var (gensym))
                                                 (free-head-var (gensym))
                                                 (weak-flags-var (gensym))
                                                 (free-next-var (gensym))
                                                 (lock-type :write))
                             &body body)
  (with-gensyms (table lock)
    `(let ((,table -handle-table-))
       (declare (type handle-table ,table))
       (with-accessors ((,data-var %handle-table-data)
                        (,length-var %handle-table-length)
                        (,free-head-var %handle-table-free-head)
                        (,weak-flags-var %handle-table-weak-flags)
                        (,free-next-var %handle-table-free-next)
                        (,lock %handle-table-lock))
           ,table
         (,@(if lock-type
              `(,(if (eq lock-type :write) 'with-write-lock 'with-read-lock)
                (,lock))
              '(progn))
          (locally ,@body))))))

(defun %resize-handle-table ()
  (with-handle-table (data length free-head weak-flags free-next nil)
    (let* ((size (length data))
           (new-size (min (1- array-total-size-limit) (1+ (* size 2)))))
      (when (< size new-size)
        (let ((new-data (make-array new-size :initial-element nil))
              (new-weak-flags (make-array new-size
                                          :element-type 'bit
                                          :initial-element 0))
              (new-free-next (make-array new-size
                                         :element-type 'fixnum
                                         :initial-element 0)))
          (replace new-data data)
          (replace new-weak-flags weak-flags)
          (replace new-free-next free-next)
          (setf data new-data
                weak-flags new-weak-flags
                free-next new-free-next)
          t)))))

(defun %alloc-lisp-handle (object &optional weakp)
  (with-handle-table (data length free-head weak-flags free-next :write)
    (let ((value (if weakp
                   (tg:make-weak-pointer object)
                   object))
          (weak-flag (if weakp 1 0)))
      (cond ((plusp free-head)
             (let* ((index free-head)
                    (next (the fixnum (aref free-next index))))
               (setf free-head next
                     (svref data index) value
                     (sbit weak-flags index) weak-flag
                     (aref free-next index) +handle-table-active+)
               index))
            ((or (< length (length data))
                 (%resize-handle-table))
             (let ((index length))
               (setf (svref data index) value
                     (sbit weak-flags index) weak-flag
                     (aref free-next index) +handle-table-active+)
               (incf length)
               index))
            (t 0)))))

(defun %free-lisp-handle (handle)
  (declare (type fixnum handle))
  (with-handle-table (data length free-head weak-flags free-next :write)
    (when (and (> handle 0)
               (< handle length)
               (= +handle-table-active+ (aref free-next handle)))
      (setf (svref data handle) nil
            (sbit weak-flags handle) 0
            (aref free-next handle) free-head
            free-head handle)))
  (values))

(defun %handle-table-get (handle)
  (declare (type fixnum handle))
  (with-handle-table (data length free-head weak-flags free-next :read)
    (when (and (> handle 0)
               (< handle length)
               (= +handle-table-active+ (aref free-next handle)))
      (let ((value (svref data handle)))
        (if (zerop (sbit weak-flags handle))
          value
          (tg:weak-pointer-value value))))))

(defun %clear-handle-table ()
  (with-handle-table (data length free-head weak-flags free-next :write)
    (dotimes (i length)
      (setf (svref data i) nil
            (sbit weak-flags i) 0
            (aref free-next i) 0))
    (setf length 1
          free-head 0)
    (values)))

;;; vim: ft=lisp et
