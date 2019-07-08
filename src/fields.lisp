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

(defun %make-field-entry (info)
  (declare (type dotnet-object info))
  (let* ((name (%mknetsym (reflection-property info "Name")))
         (type (reflection-property info "FieldType"))
         (staticp (reflection-property info "IsStatic"))
         (pointerp (reflection-property type "IsPointer"))
         (primitive-type (cdr (assoc (reflection-property type "FullName")
                                     +primitive-types+
                                     :test #'string-equal))))
    (multiple-value-bind
          (reader-delegate reader-ptr writer-delegate writer-ptr)
        (%get-accessor-trampolines info :field)
      (let ((reader (when reader-delegate
                      (compile-reader-trampoline reader-ptr staticp primitive-type)))
            (writer (when writer-delegate
                      (compile-writer-trampoline writer-ptr staticp primitive-type))))
        (%field-entry info
                      name
                      staticp
                      (or primitive-type
                          (and pointerp :pointer))
                      pointerp
                      type
                      reader
                      reader-delegate
                      reader-ptr
                      writer
                      writer-delegate
                      writer-ptr)))))

;;; vim: ft=lisp et
