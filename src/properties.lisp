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

(defun %is-indexer (property-info)
  (let ((index-params (reflection-invoke property-info "GetIndexParameters")))
    (> (%array-length index-params) 0)))

(defun %make-property-entry (info)
  (declare (type dotnet-object info))
  (let* ((name (%mknetsym (reflection-property info "Name")))
         (type (reflection-property info "PropertyType"))
         (staticp (reflection-property (dnvref (reflection-invoke info "GetAccessors") 0)
                                       "IsStatic"))
         (pointerp (reflection-property type "IsPointer"))
         (primitive-type (cdr (assoc (reflection-property type "FullName")
                                     +primitive-types+
                                     :test #'string-equal))))
    (multiple-value-bind
          (reader-delegate reader-ptr writer-delegate writer-ptr)
        (%get-accessor-trampolines info :property)
      (let ((reader (when reader-delegate
                      (compile-reader-trampoline reader-ptr staticp primitive-type)))
            (writer (when writer-delegate
                      (compile-writer-trampoline writer-ptr staticp primitive-type))))
        (%property-entry info
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

(defun %make-indexer-entry (info)
  (declare (type dotnet-object info)) ;; PropertyInfo object
  (let* ((name (%mknetsym (reflection-property info "Name")))
         (type (reflection-property info "PropertyType"))
         (primitive-type (cdr (assoc (reflection-property type "FullName")
                                     +primitive-types+
                                     :test #'string-equal)))
         (pointerp (reflection-property type "IsPointer"))
         (args (sort (mapcar #'%make-param-entry
                             (bike-vector-to-list
                              (reflection-invoke info "GetIndexParameters")))
                     #'< :key #'%param-entry-position))
         (arg-count (length args)))
    (multiple-value-bind
          (reader-delegate reader-ptr writer-delegate writer-ptr)
        (%get-accessor-trampolines info :indexer)
      (let* ((reader-args (when reader-delegate
                            (%get-method-info-args
                             (reflection-property info "GetMethod"))))
             (reader (when reader-delegate
                       (compile-method-trampoline
                        reader-ptr nil nil reader-args)))
             (writer-args (when writer-delegate
                            (%get-method-info-args
                             (reflection-property info "SetMethod"))))
             (writer (when writer-delegate
                       (compile-method-trampoline
                        writer-ptr nil t writer-args))))
        (%indexer-entry info
                        name
                        (or primitive-type
                            (and pointerp :pointer))
                        pointerp
                        type
                        arg-count
                        args
                        reader
                        reader-delegate
                        reader-ptr
                        writer
                        writer-delegate
                        writer-ptr)))))

;;; vim: ft=lisp et
