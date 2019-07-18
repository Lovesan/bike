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

(defconstant +type-code-empty+ 0)
(defconstant +type-code-object+ 1)
(defconstant +type-code-db-null+ 2)
(defconstant +type-code-boolean+ 3)
(defconstant +type-code-char+ 4)
(defconstant +type-code-sbyte+ 5)
(defconstant +type-code-byte+ 6)
(defconstant +type-code-int16+ 7)
(defconstant +type-code-uint16+ 8)
(defconstant +type-code-int32+ 9)
(defconstant +type-code-uint32+ 10)
(defconstant +type-code-int64+ 11)
(defconstant +type-code-uint64+ 12)
(defconstant +type-code-single+ 13)
(defconstant +type-code-double+ 14)
(defconstant +type-code-decimal+ 15)
(defconstant +type-code-datetime+ 16)
(defconstant +type-code-string+ 18)

(defconstant +ext-type-code-lisp-object+ 1)
(defconstant +ext-type-code-type+ 2)
(defconstant +ext-type-code-delegate+ 3)
(defconstant +ext-type-code-exception+ 4)
(defconstant +ext-type-code-enum+ 5)

(declaim (inline %free-handle))
(defun %free-handle (handle)
  (declare (type foreign-pointer handle))
  (hostcall free-handle :pointer handle :void)
  (values))

(macrolet ((frob (what &optional (include t))
             (let* ((what (symbolicate '#:dotnet- what))
                    (pct '#:%)
                    (%%ctr (symbolicate pct pct what))
                    (%ctr (symbolicate pct what))
                    (predicate (symbolicate what '- 'p))
                    (conc-name (symbolicate pct what '-))
                    (handle-accessor (symbolicate conc-name 'handle)))
               `(progn (declaim (inline ,%ctr ,%%ctr ,predicate ,handle-accessor))
                       (defstruct (,what ,@(when include `((:include dotnet-object)))
                                         (:constructor ,%%ctr (handle))
                                         (:conc-name ,conc-name)
                                         (:predicate ,predicate))
                         ,@(unless include
                             `((handle (null-pointer) :type foreign-pointer
                                                      :read-only t))))
                       (defun ,%ctr (pointer)
                         (declare (type foreign-pointer pointer))
                         (if (null-pointer-p pointer)
                           nil
                           (let ((object (,%%ctr pointer)))
                             (tg:finalize object (lambda () (%free-handle pointer)))
                             object)))))))
  (frob object nil)
  (frob type)
  (frob delegate)
  (frob exception))


(macrolet
    ((frob (name type)
       (let* ((slot-name (symbolicate 'box- name))
              (fname (symbolicate '% slot-name)))
         `(progn (declaim (inline ,fname))
                 (defun ,fname (value)
                   (hostcall ,slot-name
                             ,type value
                             :pointer))))))
  (frob boolean :bool)
  (frob char dnchar)
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

(macrolet ((frob (name type &optional (converter '(progn)))
             (let* ((slot-name (symbolicate 'unbox- name))
                    (fname (symbolicate '% slot-name)))
               `(progn (declaim (inline ,fname))
                       (defun ,fname (pointer)
                         (declare (type foreign-pointer pointer))
                         (,@converter
                          (hostcall ,slot-name
                                    :pointer (the foreign-pointer pointer)
                                    ,type)))))))
  (frob boolean :bool (the boolean))
  (frob char dnchar (the character))
  (frob int8 :int8 (the (signed-byte 8)))
  (frob uint8 :uint8 (the (unsigned-byte 8)))
  (frob int16 :int16 (the (signed-byte 16)))
  (frob uint16 :uint16 (the (unsigned-byte 16)))
  (frob int32 :int32 (the (signed-byte 32)))
  (frob uint32 :uint32 (the (unsigned-byte 32)))
  (frob int64 :int64 (the (signed-byte 64)))
  (frob uint64 :uint64 (the (unsigned-byte 64)))
  (frob single :float (the single-float))
  (frob double :double (the double-float))
  (frob intptr :pointer (the foreign-pointer)))

(declaim (inline %get-string-length))
(defun %get-string-length (value)
  (declare (type foreign-pointer value))
  (hostcall get-string-length :pointer value :int))

(declaim (inline %unbox-string))
(defun %unbox-string (value)
  (declare (type foreign-pointer value))
  (let* ((length (the fixnum (%get-string-length value)))
         (size (* (foreign-type-size :short)
                  length)))
    (if (zerop size)
      ""
      (with-foreign-objects ((ptr :uint8 size))
        (hostcall unbox-string
                  :pointer value
                  :pointer ptr
                  :int size
                  :void)
        (values (foreign-string-to-lisp ptr :count size
                                            :max-chars length
                                            :encoding :utf-16/le))))))

(declaim (inline %is-lisp-object))
(defun %is-lisp-object (ptr)
  (hostcall is-lisp-object :pointer ptr :bool))

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

(declaim (inline %get-boxed-object)
         (ftype (function (foreign-pointer (unsigned-byte 8) &optional t)
                          (values t boolean))
                %get-boxed-object))
(defun %get-boxed-object (ptr ext-code &optional (unbox-lisp t))
  (declare (type foreign-pointer ptr)
           (type (unsigned-byte 8) ext-code))
  (cond ((and unbox-lisp (= ext-code +ext-type-code-lisp-object+))
         (values (%handle-table-get
                  (pointer-address
                   (%%unbox-lisp-object ptr)))
                 t))
        ((= ext-code +ext-type-code-type+)
         (values (%dotnet-type ptr) nil))
        ((= ext-code +ext-type-code-delegate+)
         (values (%dotnet-delegate ptr) nil))
        ((= ext-code +ext-type-code-exception+)
         (values (%dotnet-exception ptr) nil))
        (t (values (%dotnet-object ptr) nil))))

(declaim (inline %unbox-integer))
(defun %unbox-integer (ptr type-code)
  (declare (type foreign-pointer ptr)
           (type (unsigned-byte 8) type-code))
  (cond ((= type-code +type-code-sbyte+)
         (values (%unbox-int8 ptr) t))
        ((= type-code +type-code-byte+)
         (values (%unbox-uint8 ptr) t))
        ((= type-code +type-code-int16+)
         (values (%unbox-int16 ptr) t))
        ((= type-code +type-code-uint16+)
         (values (%unbox-uint16 ptr) t))
        ((= type-code +type-code-int32+)
         (values (%unbox-int32 ptr) t))
        ((= type-code +type-code-uint32+)
         (values (%unbox-uint32 ptr) t))
        ((= type-code +type-code-int64+)
         (values (%unbox-int64 ptr) t))
        ((= type-code +type-code-uint64+)
         (values (%unbox-uint64 ptr) t))))

(defun %unbox (ptr type-code &optional unbox-enum)
  (declare (type foreign-pointer ptr)
           (type (unsigned-byte 16) type-code))
  (let ((type-code (logand #xFF type-code))
        (ext-code (ash (logand #xFF00 type-code) -8)))
    (cond ((or (zerop type-code)
               (null-pointer-p ptr))
           (values nil nil))
          ((= ext-code +ext-type-code-enum+)
           (if unbox-enum
             (%unbox-integer ptr type-code)
             (values (%dotnet-object ptr) nil)))
          ((= type-code +type-code-object+)
           (%get-boxed-object ptr ext-code))
          ((= type-code +type-code-string+)
           (values (%unbox-string ptr) t))
          ((= type-code +type-code-boolean+)
           (values (%unbox-boolean ptr) t))
          ((= type-code +type-code-char+)
           (values (%unbox-char ptr) t))
          ((and (>= type-code +type-code-sbyte+)
                (<= type-code +type-code-uint64+))
           (%unbox-integer ptr type-code))
          ((= type-code +type-code-single+)
           (values (%unbox-single ptr) t))
          ((= type-code +type-code-double+)
           (values (%unbox-double ptr) t))
          (t (values (%dotnet-object ptr) nil)))))

(defun %box (object)
  (cond ((null object)
         (values (null-pointer) nil))
        ((dotnet-object-p object)
         (values (%dotnet-object-handle object) nil))
        ((or (eq object t) (eq object :true))
         (values (%box-boolean object) t))
        ((eq object :false)
         (values (%box-boolean nil) t))
        ((characterp object)
         (values (%box-char object) t))
        ((stringp object)
         (values (%box-string object) t))
        ((typep object '(signed-byte 8))
         (values (%box-int8 object) t))
        ((typep object '(signed-byte 16))
         (values (%box-int16 object) t))
        ((typep object '(signed-byte 32))
         (values (%box-int32 object) t))
        ((typep object '(signed-byte 64))
         (values (%box-int64 object) t))
        ((typep object 'single-float)
         (values (%box-single object) t))
        ((typep object 'double-float)
         (values (%box-double object) t))
        ((typep object 'foreign-pointer)
         (values (%box-intptr object) t))
        (t (values
            (%%box-lisp-object
             (%alloc-lisp-handle object))
            t))))

;;; vim: ft=lisp et
