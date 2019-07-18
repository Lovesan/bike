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

(declaim (inline %resolve-enum-value))
(defun %resolve-enum-value (type enum-values designator)
  (cond ((dotnet-object-p designator)
         (let* ((handle (%dotnet-object-handle designator))
                (code (%get-full-type-code handle)))
           (multiple-value-bind
                 (unboxed cleanup) (%unbox handle code t)
             (declare (ignore cleanup)
                      (type (signed-byte 64) unboxed))
             unboxed)))
        ((typep designator '(signed-byte 64)) designator)
        ((typep designator 'string-designator)
         (let* ((name (%mknetsym designator))
                (value (gethash name enum-values)))
           (unless value
             (error 'field-resolution-error
                    :type type
                    :static-p t
                    :field designator))
           value))
        (t (error 'field-resolution-error
                  :type type
                  :static-p t
                  :field designator))))

(defun enum (enum-type value &rest values)
  (declare (type dotnet-type-designator enum-type)
           (type (or (signed-byte 64) dotnet-object string-designator) value)
           (dynamic-extent values))
  "Converts LOGIOR'ed VALUE and VALUES to enum object of specified TYPE"
  (with-type-entry (type enum-values enum-p)
                   (with-type-table-lock (:read)
                     (%resolve-type-entry enum-type))
    (unless enum-p (error 'enum-resolution-error :datum type))
    (flet ((resolve (val) (%resolve-enum-value type enum-values val)))
      (let ((value (resolve value)))
        (declare (type (signed-byte 64) value))
        (dolist (val values)
          (setf value (logior value (resolve val))))
        (%enum-to-object type value)))))

;;; vim: ft=lisp et
