
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

(deftype simple-character-string ()
  "A simple one-dimensional array which element type is CHARACTER"
  '(simple-array character (*)))

(declaim (inline %base-string-to-string))
(defun %base-string-to-string (string &optional upcase)
  (declare (type simple-base-string string))
  (let* ((length (length string))
         (result (make-string length)))
    (if upcase
      (dotimes (i length)
        (setf (schar result i) (char-upcase (schar string i))))
      (dotimes (i length)
        (setf (schar result i) (schar string i))))
    result))

(declaim (inline %maybe-string-upcase))
(defun %maybe-string-upcase (string)
  (declare (type simple-character-string string))
  (let ((len (length string)))
    (if (dotimes (i len t)
          (when (lower-case-p (schar string i))
            (return nil)))
      string
      (let ((result (copy-seq string)))
        (nstring-upcase result)
        result))))

(defun simple-character-string (designator)
  (declare (type string-designator designator))
  "Converts a STRING-DESIGNATOR to a SIMPLE-CHARACTER-STRING."
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (flet ((from-base-string (s) (%base-string-to-string s)))
      (etypecase designator
        (simple-character-string designator)
        (symbol (let ((name (symbol-name designator)))
                  (if (typep name 'simple-character-string)
                    name
                    (from-base-string name))))
        (simple-base-string (from-base-string designator))
        (character
         (make-array 1 :element-type 'character
                       :initial-element designator))))))

(declaim (inline make-simple-character-string))
(defun make-simple-character-string (length)
  (declare (type (integer 0 #.(1- array-total-size-limit)) length))
  "Allocates a SIMPLE-CHARACTER-STRING that can hold LENGTH characters."
  (make-array length :element-type 'character))

(defun simple-character-string-upcase (designator)
  (declare (type string-designator designator))
  "Converts a STRING-DESIGNATOR to an upper-case SIMPLE-CHARACTER-STRING."
  (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
    (flet ((maybe-upcase (s) (%maybe-string-upcase s))
           (from-base-string (s) (%base-string-to-string s t)))
      (etypecase designator
        (simple-character-string (maybe-upcase designator))
        (symbol (let ((name (symbol-name designator)))
                  (if (typep name 'simple-character-string)
                    (maybe-upcase name)
                    (from-base-string name))))
        (simple-base-string (from-base-string designator))
        (base-char
         (make-array 1 :element-type 'character
                       :initial-element (char-upcase designator)))
        (character
         (make-array 1 :element-type 'character
                       :initial-element (char-upcase designator)))))))

;;; vim: ft=lisp et
