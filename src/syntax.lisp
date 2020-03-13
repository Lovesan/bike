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

(defun maybe-read-char (stream &rest characters)
  (let ((peek (peek-char t stream)))
    (when (member peek characters)
      (read-char stream))))

(defun read-open-bracket (stream c)
  (declare (ignore c))
  (let* ((staticp (maybe-read-char stream #\:))
         (target (read stream t nil t))
         (prefix (maybe-read-char stream #\$ #\%))
         (member (read stream t nil t))
         (args (read-delimited-list #\] stream t)))
    `(,(case prefix
         (#\% 'property)
         (#\$ 'field)
         (t 'invoke))
      ,(if staticp `(quote ,target) target)
      (quote ,member)
      ,@args)))

(defun read-close-bracket (stream c)
  (declare (ignore stream c))
  (error 'bike-reader-error
         :message "Unexpected close bracket: ]"))

(defun read-sharp-open-bracket (stream c n)
  (declare (ignore c n))
  (let ((target (read stream t nil t))
        (idx1 (read stream t nil t))
        (rest (read-delimited-list #\] stream t)))
    `(ref ,target ,idx1 ,@rest)))

(defun read-sharp-e (stream c n)
  (declare (ignore c n))
  (let ((form (read stream t nil)))
    (unless (typep form '(cons string-designator cons))
      (error 'bike-reader-error
             :message (format nil "~a is not a list of form (EnumType EnumValue1 EnumValue2 ...)"
                              form)))
    `(enum ',(first form) ,@(mapcar (lambda (x) `(quote ,x)) (rest form)))))

(defreadtable bike-syntax
  (:merge :standard)
  (:macro-char #\[ 'read-open-bracket nil)
  (:macro-char #\] 'read-close-bracket nil)
  (:dispatch-macro-char #\# #\[ 'read-sharp-open-bracket)
  (:dispatch-macro-char #\# #\e 'read-sharp-e))

;;; vim: ft=lisp et
