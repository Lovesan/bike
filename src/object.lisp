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

(declaim (inline %get-boxed-object))
(defun %get-boxed-object (ptr ext-code)
  (declare (type foreign-pointer ptr)
           (type (unsigned-byte 8) ext-code))
  (cond ((= ext-code +ext-type-code-lisp-object+)
         (%handle-table-get
          (pointer-address
           (%%unbox-lisp-object ptr))))
        ((= ext-code +ext-type-code-type+)
         (%dotnet-type ptr))
        ((= ext-code +ext-type-code-delegate+)
         (%dotnet-delegate ptr))
        ((= ext-code +ext-type-code-exception+)
         (%dotnet-exception ptr))
        (t (%dotnet-object ptr))))

(defun %unbox (ptr type-code)
  (declare (type foreign-pointer ptr)
           (type (unsigned-byte 16) type-code))
  (let ((type-code (logand #xFF type-code))
        (ext-code (ash (logand #xFF00 type-code) -8)))
    (cond ((or (zerop type-code)
               (null-pointer-p ptr))
           (values nil nil))
          ((= type-code +type-code-object+)
           (values (%get-boxed-object ptr ext-code)
                   nil))
          ((= type-code +type-code-string+)
           (values (%unbox-string ptr) t))
          ((= type-code +type-code-boolean+)
           (values (%unbox-boolean ptr) t))
          ((= type-code +type-code-char+)
           (values (%unbox-char ptr) t))
          ((= type-code +type-code-sbyte+)
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
           (values (%unbox-uint64 ptr) t))
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
        ((eq object t)
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
            nil))))

(declaim (inline %transform-args))
(defun %transform-args (args argp)
  (declare (type list args)
           (type foreign-pointer argp))
  (loop :with cleanup-list :of-type list = '()
        :for i :of-type fixnum :from 0
        :for arg :in args :do
          (multiple-value-bind (boxed cleanup) (%box arg)
            (when cleanup (push boxed cleanup-list))
            (setf (mem-aref argp :pointer i) boxed))
        :finally (return cleanup-list)))

(declaim (inline %transform-rv))
(defun %transform-rv (rv code ex &optional (cleanup-list '()))
  (declare (type foreign-pointer rv code ex)
           (type list cleanup-list))
  (let ((rv (mem-ref rv :pointer))
        (code (mem-ref code :int))
        (ex (mem-ref ex :pointer)))
    (multiple-value-bind (result cleanup) (%unbox rv code)
      (when cleanup (%free-handle rv))
      (dolist (boxed cleanup-list) (%free-handle boxed))
      (values result (%dotnet-exception ex)))))

(defun %get-type-by-name (name errorp assembly)
  (declare (type string name)
           (type (or null dotnet-object) assembly))
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall get-type-by-name
              lpwstr name
              :bool errorp
              :pointer (or (and assembly
                                (%dotnet-object-handle assembly))
                           (null-pointer))
              :pointer rv
              :pointer code
              :pointer ex
              :void)
    (%transform-rv rv code ex)))

(defun %get-generic-type-by-name (name errorp assembly type-args)
  (declare (type string name)
           (type (or null dotnet-object) assembly)
           (type list type-args))
  (let ((argc (length type-args)))
    (with-foreign-objects ((rv :pointer)
                           (code :int)
                           (ex :pointer)
                           (argp :pointer argc))
      (%transform-args type-args argp)
      (hostcall get-generic-type-by-name
                lpwstr name
                :pointer argp
                :int argc
                :bool errorp
                :pointer (or (and assembly
                                  (%dotnet-object-handle assembly))
                             (null-pointer))
                :pointer rv
                :pointer code
                :pointer ex
                :void)
      (%transform-rv rv code ex))))

(defun %make-generic-type (type-definition type-args)
  (declare (type dotnet-object type-definition)
           (type list type-args))
  (let ((argc (length type-args)))
    (with-foreign-objects ((rv :pointer)
                           (code :int)
                           (ex :pointer)
                           (argp :pointer argc))
      (%transform-args type-args argp)
      (hostcall make-generic-type
                :pointer (%dotnet-object-handle type-definition)
                :pointer argp
                :int argc
                :pointer rv
                :pointer code
                :pointer ex
                :void)
      (%transform-rv rv code ex))))

(defun %make-array-type (type rank)
  (declare (type dotnet-object type)
           (type (integer 1 32) rank))
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall make-array-type
              :pointer (%dotnet-type-handle type)
              :int rank
              :pointer rv
              :pointer code
              :pointer ex
              :void)
    (%transform-rv rv code ex)))

(defun %invoke-delegate (delegate &rest args)
  (declare (type dotnet-object delegate)
           (dynamic-extent args))
  (let ((argc (length args)))
    (with-foreign-objects ((rv :pointer)
                           (code :int)
                           (ex :pointer)
                           (argp :pointer argc))
      (let ((cleanup-list (%transform-args args argp)))
        (hostcall invoke-delegate
                  :pointer (%dotnet-object-handle delegate)
                  :pointer argp
                  :int argc
                  :pointer rv
                  :pointer code
                  :pointer ex
                  :void)
        (%transform-rv rv code ex cleanup-list)))))

(defun %invoke-constructor (type &rest args)
  (declare (type dotnet-type type)
           (dynamic-extent args))
  (let ((argc (length args)))
    (with-foreign-objects ((rv :pointer)
                           (code :int)
                           (ex :pointer)
                           (argp :pointer argc))
      (let ((cleanup-list (%transform-args args argp)))
        (hostcall invoke-constructor
                  :pointer (%dotnet-object-handle type)
                  :pointer argp
                  :int argc
                  :pointer rv
                  :pointer code
                  :pointer ex
                  :void)
        (%transform-rv rv code ex cleanup-list)))))

(defun %invoke-static (type method-name &rest args)
  (declare (type dotnet-type type)
           (type string method-name)
           (dynamic-extent args))
  (let ((argc (length args)))
    (with-foreign-objects ((rv :pointer)
                           (code :int)
                           (ex :pointer)
                           (argp :pointer argc))
      (let ((cleanup-list (%transform-args args argp)))
        (hostcall invoke-static
                  :pointer (%dotnet-object-handle type)
                  lpwstr method-name
                  :pointer argp
                  :int argc
                  :pointer rv
                  :pointer code
                  :pointer ex
                  :void)
        (%transform-rv rv code ex cleanup-list)))))

(defun %invoke-member (object method-name &rest args)
  (declare (type dotnet-object object)
           (type string method-name)
           (dynamic-extent args))
  (let ((argc (length args)))
    (with-foreign-objects ((rv :pointer)
                           (code :int)
                           (ex :pointer)
                           (argp :pointer argc))
      (let ((cleanup-list (%transform-args args argp)))
        (hostcall invoke-member
                  :pointer (%dotnet-object-handle object)
                  lpwstr method-name
                  :pointer argp
                  :int argc
                  :pointer rv
                  :pointer code
                  :pointer ex
                  :void)
        (%transform-rv rv code ex cleanup-list)))))

(defun %get-field (object field-name)
  (declare (type dotnet-object object)
           (type string field-name))
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall get-field
              :pointer (%dotnet-object-handle object)
              lpwstr field-name
              :pointer rv
              :pointer code
              :pointer ex
              :void)
    (%transform-rv rv code ex)))

(defun %get-static-field (type field-name)
  (declare (type dotnet-type type)
           (type string field-name))
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall get-static-field
              :pointer (%dotnet-object-handle type)
              lpwstr field-name
              :pointer rv
              :pointer code
              :pointer ex
              :void)
    (%transform-rv rv code ex)))

(defun %set-field (object field-name value)
  (declare (type dotnet-object object)
           (type string field-name))
  (multiple-value-bind (boxed cleanup) (%box value)
    (with-foreign-objects ((ex :pointer))
      (hostcall set-field
                :pointer (%dotnet-object-handle object)
                lpwstr field-name
                :pointer boxed
                :pointer ex
                :void)
      (when cleanup (%free-handle boxed))
      (values nil (%dotnet-object (mem-ref ex :pointer))))))

(defun %set-static-field (type field-name value)
  (declare (type dotnet-type type)
           (type string field-name))
  (multiple-value-bind (boxed cleanup) (%box value)
    (with-foreign-objects ((ex :pointer))
      (hostcall set-static-field
                :pointer (%dotnet-object-handle type)
                lpwstr field-name
                :pointer boxed
                :pointer ex
                :void)
      (when cleanup (%free-handle boxed))
      (values nil (%dotnet-object (mem-ref ex :pointer))))))

(defun %get-type (object)
  (declare (type dotnet-object object))
  (%invoke-member object "GetType"))

(defun %to-string (object)
  (declare (type dotnet-object object))
  (%invoke-member object "ToString"))

(defun %get-delegate-for-lisp-function (function delegate-type)
  (declare (type (or symbol function) function)
           (type dotnet-type delegate-type))
  (with-foreign-objects ((rv :pointer)
                         (code :pointer)
                         (ex :pointer))
    (hostcall get-delegate-for-lisp-function
              :pointer (make-pointer (%alloc-lisp-handle function))
              :pointer (%dotnet-object-handle delegate-type)
              :pointer rv
              :pointer code
              :pointer ex)
    (%transform-rv rv code ex)))

(defun %is-delegate-type (type)
  (declare (type dotnet-type type))
  (hostcall is-delegate-type
            :pointer (%dotnet-type-handle type)
            :bool))

(defun %array-length (dotnet-vector)
  (declare (type dotnet-object dotnet-vector))
  (with-foreign-objects ((rv :int64)
                         (code :int)
                         (ex :pointer))
    (hostcall array-length
              :pointer (%dotnet-object-handle dotnet-vector)              
              :pointer rv
              :pointer code
              :pointer ex
              :void)
    (let ((rv (mem-ref rv :int64))
          (ex (mem-ref ex :pointer)))
      (values rv (%dotnet-exception ex)))))

(defun %convert-to (object type &optional unbox)
  (declare (type dotnet-type type))
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (multiple-value-bind (boxed cleanup) (%box object)      
      (hostcall convert-to
                :pointer boxed
                :pointer (%dotnet-type-handle type)
                :pointer rv
                :pointer code
                :pointer ex)
      (when cleanup (%free-handle boxed))
      (if unbox
        (%transform-rv rv code ex)
        (let ((rv (mem-ref rv :pointer))
              (code (mem-ref code :int))
              (ex (mem-ref ex :pointer)))
          (values (%get-boxed-object rv code)
                  (%dotnet-exception ex)))))))

(defun %net-vref (dotnet-vector index)
  (declare (type dotnet-object dotnet-vector)
           (type fixnum index))
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall vector-get
              :pointer (%dotnet-object-handle dotnet-vector)
              :int64 index
              :pointer rv
              :pointer code
              :pointer ex
              :void)
    (%transform-rv rv code ex)))

(defun (setf %net-vref) (new-value dotnet-vector index)
  (declare (type dotnet-object dotnet-vector)
           (type fixnum index))
  (multiple-value-bind (boxed cleanup) (%box new-value)
    (with-foreign-objects ((ex :pointer))
      (hostcall vector-set
                :pointer (%dotnet-object-handle dotnet-vector)
                :int64 index
                :pointer boxed
                :pointer ex
                :void)
      (when cleanup (%free-handle boxed))
      (let ((ex (%dotnet-exception (mem-ref ex :pointer))))
        (values (and (not ex) new-value)
                ex)))))

;;; vim: ft=lisp et
