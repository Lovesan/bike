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
  (let ((peek (peek-char t stream t nil t)))
    (when (member peek characters)
      (read-char stream t nil t))))

(defun read-open-bracket (stream c)
  (declare (ignore c))
  (let* ((staticp (maybe-read-char stream #\:))
         (target (read stream t nil t))
         (endp (maybe-read-char stream #\])))
    (if (and staticp endp)
      `(resolve-type ',target)
      (let* ((prefix (maybe-read-char stream #\$ #\% #\+ #\-))
             (member (read stream t nil t))
             (args (read-delimited-list #\] stream t)))
        (cond ((and (member prefix '(#\$ #\%)) args)
               (bike-reader-error
                stream
                (format nil
                        (strcat
                         "~&Field and property forms must not have arguments.~%"
                         "Form was: [~:[~;:~]~s ~:[~;~:*~a~]~a ~{~a~^ ~}]")
                        staticp
                        target
                        prefix
                        member
                        args)))
              ((and (member prefix '(#\+ #\-)) (/= 1 (length args)))
               (bike-reader-error
                stream
                (format nil
                        (strcat
                         "~&Event accessor form must have exactly one argument.~%"
                         "Form was: [~:[~;:~]~s ~:[~;~:*~a~]~a ~{~a~^ ~}]")
                        staticp
                        target
                        prefix
                        member
                        args))))
        `(,(case prefix
             (#\% 'property)
             (#\$ 'field)
             (#\+ 'event-add)
             (#\- 'event-remove)
             (t 'invoke))
          ,(if staticp `(quote ,target) target)
          (quote ,member)
          ,@args)))))

(defun read-close-bracket (stream c)
  (declare (ignore c))
  (bike-reader-error stream "Unexpected close bracket: ]"))

(defun read-sharp-open-bracket (stream c n)
  (declare (ignore c n))
  (let ((target (read stream t nil t))
        (idx1 (read stream t nil t))
        (rest (read-delimited-list #\] stream t)))
    `(ref ,target ,idx1 ,@rest)))

(defun read-sharp-e (stream c n)
  (declare (ignore c n))
  (let ((c (peek-char t stream t nil t)))
    (unless (eql c #\()
      (bike-reader-error
       stream
       (format nil "Expected '(' after '#e'. Got: ~:[end of file~;~:*'~a'~]" c)))
    (read-char stream t nil t)
    (let ((form (read-delimited-list #\) stream t)))
      `(enum ',(first form) ,@(mapcar (lambda (x) `(quote ,x)) (rest form))))))

(defreadtable bike-syntax
  (:merge :standard)
  (:macro-char #\[ 'read-open-bracket nil)
  (:macro-char #\] 'read-close-bracket nil)
  (:dispatch-macro-char #\# #\[ 'read-sharp-open-bracket)
  (:dispatch-macro-char #\# #\e 'read-sharp-e))

(defun bike-syntax-enabled-p ()
  (eq (find-readtable 'bike-syntax) *readtable*))

;;; vim: ft=lisp et
