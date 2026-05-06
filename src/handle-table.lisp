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

(defstruct (handle-table (:constructor %handle-table ())
                         (:conc-name %handle-table-)
                         (:predicate handle-table-p))
  (data (make-array +default-handle-table-size+
                    :initial-element nil)
   :type (simple-array t (*)))
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
                                                 (lock-type :write))
                             &body body)
  (with-gensyms (table lock)
    `(let ((,table -handle-table-))
       (declare (type handle-table ,table))
       (with-accessors ((,data-var %handle-table-data)
                        (,length-var %handle-table-length)
                        (,free-head-var %handle-table-free-head)
                        (,lock %handle-table-lock))
           ,table
         (,@(if lock-type
              `(,(if (eq lock-type :write) 'with-write-lock 'with-read-lock)
                (,lock))
              '(progn))
          (locally ,@body))))))

(defun %resize-handle-table ()
  (with-handle-table (data length free-head nil)
    (let* ((size (length data))
           (new-size (min (1- array-total-size-limit) (1+ (* size 2)))))
      (when (< size new-size)
        (let ((new-data (make-array new-size :initial-element nil)))
          (replace new-data data)
          (setf data new-data)
          t)))))

(defun %alloc-lisp-handle (object &optional weakp)
  (with-handle-table (data length free-head :write)
    (let ((value (cons (if weakp
                         (tg:make-weak-pointer object)
                         object)
                       weakp)))
      (cond ((plusp free-head)
             (let* ((index free-head)
                    (next (the fixnum (svref data index))))
               (setf free-head next
                     (svref data index) value)
               index))
            ((or (< length (length data))
                 (%resize-handle-table))
             (let ((index length))
               (setf (svref data index) value)
               (incf length)
               index))
            (t 0)))))

(defun %free-lisp-handle (handle)
  (declare (type fixnum handle))
  (with-handle-table (data length free-head :write)
    (when (and (> handle 0)
               (< handle length)
               (consp (svref data handle)))
      (setf (svref data handle) free-head
            free-head handle)))
  (values))

(defun %handle-table-get (handle)
  (declare (type fixnum handle))
  (with-handle-table (data length free-head :read)
    (when (and (> handle 0)
               (< handle length))
      (let ((value (svref data handle)))
        (when (consp value)
          (destructuring-bind (ptr . weakp)
              value
            (if weakp
              (tg:weak-pointer-value ptr)
              ptr)))))))

(defun %clear-handle-table ()
  (with-handle-table (data length free-head :write)
    (dotimes (i length)
      (setf (svref data i) nil))
    (setf length 1
          free-head 0)
    (values)))

;;; vim: ft=lisp et
