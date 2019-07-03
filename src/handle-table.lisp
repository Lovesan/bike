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
  (lock (make-rwlock) :type rwlock :read-only t))

#+sbcl
(sb-ext:defglobal *handle-table* nil)

#-sbcl
(defvar *handle-table* nil)

(declaim (type (or null handle-table) *handle-table*))

(defun %init-handle-table ()
  (setf *handle-table* (%handle-table)))

(uiop:register-image-restore-hook #'%init-handle-table (null *handle-table*))

(defmacro with-handle-table ((data-var &optional (length-var (gensym)) (lock-type :write))
                             &body body)
  (with-gensyms (table lock)
    `(let ((,table *handle-table*))
       (declare (type handle-table ,table))
       (with-accessors ((,data-var %handle-table-data)
                        (,length-var %handle-table-length)
                        (,lock %handle-table-lock))
           ,table
         (,@(if lock-type
              `(,(if (eq lock-type :write) 'with-read-lock 'with-write-lock)
                (,lock))
              '(progn))
          (locally ,@body))))))

(defun %resize-handle-table ()
  (with-handle-table (data length nil)
    (let* ((size (length data))
           (new-size (min most-positive-fixnum (1+ (* size 2)))))
      (when (< size new-size)
        (let ((new-data (make-array new-size :initial-element nil)))
          (replace new-data data :end1 new-size :end2 size)
          (setf data new-data)
          t)))))

(defun %alloc-lisp-handle (object)
  (with-handle-table (data length :write)
    (let* ((slots data)
           (index (loop :for i :of-type fixnum :from 1 :below length
                        :when (null (svref slots i)) :do (return i)
                          :finally (return 0))))
      (cond ((> index 0)
             (setf (svref slots index) object)
             index)
            ((or (< length (length slots))
                 (%resize-handle-table))
             (let ((slots data)
                   (index length))
               (setf (svref slots index) object)
               (incf length)
               index))
            (t 0)))))

(defun %free-lisp-handle (handle)
  (declare (type fixnum handle))
  (with-handle-table (data length :write)
    (when (and (> handle 0)
               (< handle length))
      (setf (svref data handle) nil)))
  (values))

(defun %handle-table-get (handle)
  (declare (type fixnum handle))
  (with-handle-table (data length :read)
    (when (and (> handle 0)
               (< handle length))
      (svref data handle))))

;;; vim: ft=lisp et
