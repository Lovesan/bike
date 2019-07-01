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

(defstruct (coreclr-host
            (:constructor %coreclr-host)
            (:conc-name %coreclr-host-)
            (:predicate coreclr-host-p))
  (handle (null-pointer) :type foreign-pointer)
  (domain-id 0 :type (unsigned-byte 32))
  (domain-name "" :type string)
  (free-handle (null-pointer) :type foreign-pointer)
  (get-type-by-name (null-pointer) :type foreign-pointer)
  (get-generic-type-by-name (null-pointer) :type foreign-pointer)
  (make-generic-type (null-pointer) :type foreign-pointer)
  (make-array-type (null-pointer) :type foreign-pointer)
  (is-delegate-type (null-pointer) :type foreign-pointer)
  (invoke (null-pointer) :type foreign-pointer)
  (invoke-constructor (null-pointer) :type foreign-pointer)
  (invoke-delegate (null-pointer) :type foreign-pointer)
  (get-field (null-pointer) :type foreign-pointer)
  (set-field (null-pointer) :type foreign-pointer)
  (get-property (null-pointer) :type foreign-pointer)
  (set-property (null-pointer) :type foreign-pointer)
  (get-index (null-pointer) :type foreign-pointer)
  (set-index (null-pointer) :type foreign-pointer)
  (box-boolean (null-pointer) :type foreign-pointer)
  (box-char (null-pointer) :type foreign-pointer)
  (box-uint8 (null-pointer) :type foreign-pointer)
  (box-int8 (null-pointer) :type foreign-pointer)
  (box-int16 (null-pointer) :type foreign-pointer)
  (box-uint16 (null-pointer) :type foreign-pointer)
  (box-int32 (null-pointer) :type foreign-pointer)
  (box-uint32 (null-pointer) :type foreign-pointer)
  (box-int64 (null-pointer) :type foreign-pointer)
  (box-uint64 (null-pointer) :type foreign-pointer)
  (box-intptr (null-pointer) :type foreign-pointer)
  (box-single (null-pointer) :type foreign-pointer)
  (box-double (null-pointer) :type foreign-pointer)
  (box-string (null-pointer) :type foreign-pointer)
  (box-lisp-object (null-pointer) :type foreign-pointer)
  (unbox-boolean (null-pointer) :type foreign-pointer)
  (unbox-char (null-pointer) :type foreign-pointer)
  (unbox-uint8 (null-pointer) :type foreign-pointer)
  (unbox-int8 (null-pointer) :type foreign-pointer)
  (unbox-int16 (null-pointer) :type foreign-pointer)
  (unbox-uint16 (null-pointer) :type foreign-pointer)
  (unbox-int32 (null-pointer) :type foreign-pointer)
  (unbox-uint32 (null-pointer) :type foreign-pointer)
  (unbox-int64 (null-pointer) :type foreign-pointer)
  (unbox-uint64 (null-pointer) :type foreign-pointer)
  (unbox-intptr (null-pointer) :type foreign-pointer)
  (unbox-single (null-pointer) :type foreign-pointer)
  (unbox-double (null-pointer) :type foreign-pointer)
  (unbox-string (null-pointer) :type foreign-pointer)
  (unbox-lisp-object (null-pointer) :type foreign-pointer)
  (get-string-length (null-pointer) :type foreign-pointer)
  (get-delegate-for-lisp-function (null-pointer) :type foreign-pointer)
  (install-callbacks (null-pointer) :type foreign-pointer)
  (is-lisp-object (null-pointer) :type foreign-pointer)
  (is-type (null-pointer) :type foreign-pointer)
  (vector-get (null-pointer) :type foreign-pointer)
  (vector-set (null-pointer) :type foreign-pointer)
  (array-length (null-pointer) :type foreign-pointer)
  (convert-to (null-pointer) :type foreign-pointer)
  (get-delegate-trampoline (null-pointer) :type foreign-pointer)
  (get-accessor-trampolines (null-pointer) :type foreign-pointer)
  (get-full-type-code (null-pointer) :type foreign-pointer)
  (get-type-of (null-pointer) :type foreign-pointer)
  (get-type-full-name (null-pointer) :type foreign-pointer)
  (get-type-assembly-qualified-name (null-pointer) :type foreign-pointer))

#+sbcl
(sb-ext:defglobal *coreclr-host* nil)
#-sbcl
(defvar *coreclr-host* nil)

(declaim (type (or coreclr-host null) *coreclr-host*))

(macrolet
    ((frob (name type &optional (converter 'progn))
       (let ((fname (intern (format nil "~a~a" '%box- name)))
             (conc-name (intern (format nil "~a~a"
                                        '%coreclr-host-box-
                                        name)))
             (val (gensym (string :value))))
         `(progn (declaim (inline ,fname))
                 (defun ,fname (,val)
                   (foreign-funcall-pointer
                    (,conc-name *coreclr-host*)
                    (:convention :stdcall)
                    ,type (,converter ,val)
                    :pointer))))))
  (frob boolean :bool)
  (frob char :uint16 char-code)
  (frob int8 :int8)
  (frob uint8 :uint8)
  (frob int16 :int16)
  (frob uint16 :uint16)
  (frob int32 :int32)
  (frob uint32 :uint32)
  (frob int64 :int64)
  (frob uint64 :uint64)
  (frob single :float)
  (frob double :double)
  (frob intptr :pointer)
  (frob string lpwstr))

(macrolet ((frob (name type &optional (converter 'progn))
             (let ((fname (intern (format nil "~a~a" '%unbox- name)))
                   (conc-name (intern (format nil "~a~a"
                                              '%coreclr-host-unbox-
                                              name)))
                   (val (gensym (string :value))))
               `(progn (declaim (inline ,fname))
                       (defun ,fname (,val)
                         (,converter
                          (foreign-funcall-pointer
                           (,conc-name *coreclr-host*)
                           (:convention :stdcall)
                           :pointer ,val
                           ,type)))))))
  (frob boolean :bool)
  (frob char :uint16 code-char)
  (frob int8 :int8)
  (frob uint8 :uint8)
  (frob int16 :int16)
  (frob uint16 :uint16)
  (frob int32 :int32)
  (frob uint32 :uint32)
  (frob int64 :int64)
  (frob uint64 :uint64)
  (frob single :float)
  (frob double :double)
  (frob intptr :pointer))

(defmacro hostcall (name &rest args-and-types)
  (let ((conc-name (intern (format nil "~a~a" '%coreclr-host- name) :bike)))
    `(foreign-funcall-pointer
      (,conc-name *coreclr-host*)
      (:convention :stdcall)
      ,@args-and-types)))

(declaim (inline %get-string-length))
(defun %get-string-length (value)
  (hostcall get-string-length :pointer value :int))

(declaim (inline %unbox-string))
(defun %unbox-string (value)
  (let* ((length (%get-string-length value))
         (size (* (foreign-type-size :short)
                  length)))
    (if (zerop size)
      ""
      (with-foreign-pointer (ptr size)
        (hostcall unbox-string
                  :pointer value
                  :pointer ptr
                  :int size
                  :void)
        (values (foreign-string-to-lisp ptr :count size
                                            :max-chars length
                                            :encoding :utf-16/le))))))

(declaim (inline %free-handle))
(defun %free-handle (handle)
  (declare (type foreign-pointer handle))
  (hostcall free-handle :pointer handle :void)
  (values))

(defun %install-callbacks (free-handle-callback apply-callback)
  (declare (type foreign-pointer free-handle-callback apply-callback))
  (with-foreign-object (ex :pointer)
    (hostcall install-callbacks
              :pointer free-handle-callback
              :pointer apply-callback
              :pointer ex)
    (mem-ref ex :pointer)))

(declaim (inline %is-lisp-object))
(defun %is-lisp-object (ptr)
  (hostcall is-lisp-object :pointer ptr :bool))

(declaim (inline %is-type))
(defun %is-type (ptr)
  (hostcall is-type :pointer ptr :bool))

(declaim (inline %%box-lisp-object))
(defun %%box-lisp-object (handle)
  (hostcall box-lisp-object
            :pointer (make-pointer handle)
            :pointer))

(declaim (inline %%unbox-lisp-object))
(defun %%unbox-lisp-object (ptr)
  (hostcall unbox-lisp-object
            :pointer ptr
            :pointer))

(declaim (inline %get-full-type-code))
(defun %get-full-type-code (ptr)
  (declare (type foreign-pointer ptr))
  (hostcall get-full-type-code
            :pointer ptr
            :int))

;;; vim: ft=lisp et
