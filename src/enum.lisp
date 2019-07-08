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

(defun enum (enum-type value &rest values)
  (declare (type dotnet-type-designator enum-type)
           (type (or (signed-byte 64) dotnet-object string-designator) value)
           (dynamic-extent values))
  "Converts LOGIOR'ed VALUE and VALUES to enum object of specified TYPE"
  (with-initialized-type-entry (entry enum-type type enum-values enum-p)
    (unless enum-p (error 'enum-resolution-error :datum type))
    (flet ((resolve-value (designator)
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
                               :field name))
                      value))
                   (t (error 'field-resolution-error
                             :type type
                             :static-p t
                             :field designator)))))
      (let ((value (resolve-value value)))
        (declare (type (signed-byte 64) value))
        (dolist (val values)
          (setf value (logior value (resolve-value val))))
        (with-foreign-object (ex :pointer)
          (let* ((rv (%dotnet-object
                      (hostcall enum-to-object
                                :pointer (%dotnet-type-handle type)
                                :int64 value
                                :pointer ex
                                :pointer)))
                 (ex (mem-ref ex :pointer)))
            (%transform-exception ex)
            rv))))))

;;; vim: ft=lisp et
