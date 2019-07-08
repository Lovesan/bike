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

(defconstant +accessor-member-type-field+ 1)
(defconstant +accessor-member-type-property+ 2)
(defconstant +accessor-member-type-indexer+ 3)

(deftype accessor-member-type () '(member :field :property :indexer))

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

(declaim (inline %transform-exception))
(defun %transform-exception (ex)
  (declare (type foreign-pointer ex))
  (unless (null-pointer-p ex)
    (if (%is-lisp-object ex)
      (let ((condition (%handle-table-get
                         (pointer-address
                           (%%unbox-lisp-object ex)))))
        (%free-handle ex)
        (error condition))
      (error 'dotnet-error :object (%dotnet-exception ex))))
  (values))

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
      (%transform-exception ex)
      (values result))))

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
  (declare (type dotnet-type type-definition)
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

(defun %make-pointer-type (type)
  (declare (type dotnet-object type))
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall make-pointer-type
              :pointer (%dotnet-type-handle type)
              :pointer rv
              :pointer code
              :pointer ex
              :void)
    (%transform-rv rv code ex)))

(defun %make-ref-type (type)
  (declare (type dotnet-object type))
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall make-by-ref-type
              :pointer (%dotnet-type-handle type)
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

(defun %invoke (object staticp type-args method-name &rest args)
  (declare (type dotnet-object object)
           (type list type-args)
           (type string method-name)
           (dynamic-extent args))
  (let ((argc (length args))
        (targc (length type-args)))
    (with-foreign-objects ((rv :pointer)
                           (code :int)
                           (ex :pointer)
                           (argp :pointer argc)
                           (targp :pointer targc))
      (loop :for i :of-type non-negative-fixnum :from 0
            :for type-arg :of-type dotnet-type :in type-args :do
              (setf (mem-aref targp :pointer i)
                    (%dotnet-type-handle type-arg)))
      (let ((cleanup-list (%transform-args args argp)))
        (hostcall invoke
                  :pointer (%dotnet-object-handle object)
                  :bool staticp
                  lpwstr method-name
                  :pointer targp
                  :int targc
                  :pointer argp
                  :int argc
                  :pointer rv
                  :pointer code
                  :pointer ex
                  :void)
        (%transform-rv rv code ex cleanup-list)))))

(defun %get-field (object staticp field-name)
  (declare (type dotnet-object object)
           (type string field-name))
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall get-field
              :pointer (%dotnet-object-handle object)
              :bool staticp
              lpwstr field-name
              :pointer rv
              :pointer code
              :pointer ex
              :void)
    (%transform-rv rv code ex)))

(defun %set-field (object staticp field-name value)
  (declare (type dotnet-object object)
           (type string field-name))
  (multiple-value-bind (boxed cleanup) (%box value)
    (with-foreign-objects ((ex :pointer))
      (hostcall set-field
                :pointer (%dotnet-object-handle object)
                :bool staticp
                lpwstr field-name
                :pointer boxed
                :pointer ex
                :void)
      (when cleanup (%free-handle boxed))
      (%transform-exception (mem-ref ex :pointer)))))

(defun %get-property (object staticp property-name)
  (declare (type dotnet-object object)
           (type string property-name))
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall get-property
              :pointer (%dotnet-object-handle object)
              :bool staticp
              lpwstr property-name
              :pointer rv
              :pointer code
              :pointer ex
              :void)
    (%transform-rv rv code ex)))

(defun %set-property (object staticp property-name value)
  (declare (type dotnet-object object)
           (type string property-name))
  (multiple-value-bind (boxed cleanup) (%box value)
    (with-foreign-objects ((ex :pointer))
      (hostcall set-property
                :pointer (%dotnet-object-handle object)
                :bool staticp
                lpwstr property-name
                :pointer boxed
                :pointer ex
                :void)
      (when cleanup (%free-handle boxed))
      (%transform-exception (mem-ref ex :pointer)))))

(defun %get-index (object index &rest indices)
  (declare (type dotnet-object object)
           (dynamic-extent indices))
  (let* ((args (cons index indices))
         (argc (length args)))
    (declare (dynamic-extent args))
    (with-foreign-objects ((rv :pointer)
                           (code :int)
                           (ex :pointer)
                           (argp :pointer argc))
      (let ((cleanup-list (%transform-args args argp)))
        (hostcall get-index
                  :pointer (%dotnet-object-handle object)
                  :pointer argp
                  :int argc
                  :pointer rv
                  :pointer code
                  :pointer ex
                  :void)
        (%transform-rv rv code ex cleanup-list)))))

(defun %set-index (object value index &rest indices)
  (declare (type dotnet-object object)
           (dynamic-extent indices))
  (let* ((args (cons index indices))
         (argc (length args)))
    (declare (dynamic-extent args))
    (multiple-value-bind (boxed cleanup) (%box value)
      (with-foreign-objects ((rv :pointer)
                             (code :int)
                             (ex :pointer)
                             (argp :pointer argc))
        (setf (mem-ref code :int) +type-code-empty+
              (mem-ref rv :pointer) (null-pointer))
        (let ((cleanup-list (%transform-args args argp)))
          (hostcall set-index
                    :pointer (%dotnet-object-handle object)
                    :pointer boxed
                    :pointer argp
                    :int argc
                    :pointer ex
                    :void)
          (when cleanup (%free-handle boxed))
          (%transform-rv rv code ex cleanup-list))))))

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
      (%transform-exception ex)
      (values rv))))

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
          (%transform-exception ex)
          (%get-boxed-object rv code nil))))))

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
      (let ((ex (mem-ref ex :pointer)))
        (%transform-exception ex)
        new-value))))

(defun bike-vector-to-list (vector &key (start 0) end)
  (declare (type dotnet-object vector)
           (type non-negative-fixnum start)
           (type (or null non-negative-fixnum) end))
  "Collects elements of a VECTOR into list, starting from STAR
 index and below END index"
  (loop :with end :of-type non-negative-fixnum
          = (or end (%array-length vector))
        :for i :from start :below end
        :collect (%net-vref vector i)))

(defmacro do-bike-vector ((elt-var vector &optional result) &body body)
  "Evaluates BODY forms in a loop where ELT-VAR is subsequently bound to
 each element of a VECTOR, which should evaluate to .Net array of rank 1.
 Returns RESULT form."
  (with-gensyms (i v)
    `(let ((,v ,vector))
       (declare (type dotnet-object ,v))
       (dotimes (,i (%array-length ,v) ,result)
         (let ((,elt-var (%net-vref ,v ,i)))
           ,@body)))))

(defun dnvref (vector index)
  (declare (type dotnet-object vector)
           (type non-negative-fixnum index))
  "Accesses a .Net VECTOR (an array of rank 1) at INDEX"
  (%net-vref vector index))

(defun (setf dnvref) (new-value vector index)
  (declare (type dotnet-object vector)
           (type non-negative-fixnum index))
  "Accesses a .Net VECTOR (an array of rank 1) at INDEX"
  (funcall #'(setf %net-vref) new-value vector index))

(defun %get-delegate-trampoline (method-info type-args)
  (declare (type dotnet-object method-info)
           (type list type-args))
  (let ((count (length type-args)))
    (with-foreign-objects ((argp :pointer count)
                           (pp :pointer)
                           (rv :pointer)
                           (code :int)
                           (ex :pointer))
      (loop :for i :of-type fixnum :from 0
            :for arg :in type-args
            :do (setf (mem-aref argp :pointer)
                      (%dotnet-type-handle arg)))
      (hostcall get-delegate-trampoline
                :pointer (%dotnet-object-handle method-info)
                :pointer argp
                :int count
                :pointer pp
                :pointer rv
                :pointer code
                :pointer ex)
      (let ((fptr (mem-ref pp :pointer))
            (rv (%dotnet-delegate (mem-ref rv :pointer)))
            (code (mem-ref code :int))
            (ex (mem-ref ex :pointer)))
        (declare (ignore code))
        (%transform-exception ex)
        (values rv fptr)))))

(defun %get-accessor-trampolines (info member-type)
  (declare (type dotnet-object info)
           (type accessor-member-type member-type))
  (let ((member-type (ecase member-type
                       (:field +accessor-member-type-field+)
                       (:property +accessor-member-type-property+)
                       (:indexer +accessor-member-type-indexer+))))
    (with-foreign-objects ((reader :pointer)
                           (reader-ptr :pointer)
                           (writer :pointer)
                           (writer-ptr :pointer)
                           (ex :pointer))
      (hostcall get-accessor-trampolines
                :pointer (%dotnet-object-handle info)
                :int member-type
                :pointer reader
                :pointer reader-ptr
                :pointer writer
                :pointer writer-ptr
                :pointer ex
                :void)
      (let ((reader (%dotnet-delegate (mem-ref reader :pointer)))
            (reader-ptr (mem-ref reader-ptr :pointer))
            (writer (%dotnet-delegate (mem-ref writer :pointer)))
            (writer-ptr (mem-ref writer-ptr :pointer))
            (ex (mem-ref ex :pointer)))
        (%transform-exception ex)
        (values reader reader-ptr writer writer-ptr)))))

(defun get-type (object)
  "Retrieves .Net type of an OBJECT"
  (multiple-value-bind (boxed cleanup)
      (if (dotnet-object-p object)
        (values (%dotnet-object-handle object) nil)
        (%box object))
    (let ((type (%dotnet-type (hostcall get-type-of
                                        :pointer boxed
                                        :pointer))))
      (when cleanup (%free-handle boxed))
      (the dotnet-type type))))

(defun %get-type-full-name (type)
  (declare (type dotnet-type type))
  (let* ((rv (hostcall get-type-full-name
                       :pointer (%dotnet-type-handle type)
                       :pointer))
         (name (%unbox-string rv)))
    (%free-handle rv)
    name))

(defun %get-type-assembly-qualified-name (type)
  (declare (type dotnet-type type))
  (let* ((rv (hostcall get-type-assembly-qualified-name
                       :pointer (%dotnet-type-handle type)
                       :pointer))
         (name (%unbox-string rv)))
    (%free-handle rv)
    name))

(defun generic-type-p (type)
  (declare (type dotnet-type type))
  "Tests whether a TYPE is a generic type"
  (hostcall is-generic-type
            :pointer (%dotnet-type-handle type)
            :bool))

(defun generic-type-definition-p (type)
  (declare (type dotnet-type type))
  "Tests whether a TYPE is a generic type definition"
  (hostcall is-generic-type-definition
            :pointer (%dotnet-type-handle type)
            :bool))

(defun get-generic-type-definition (type)
  (declare (type dotnet-type type))
  "Returns a generic type definition for a generic TYPE"
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall get-generic-type-definition
              :pointer (%dotnet-type-handle type)
              :pointer rv
              :pointer code
              :pointer ex)
    (%transform-rv rv code ex)))

(defun get-element-type (type)
  (declare (type dotnet-type type))
  "Returns an element type for a TYPE"
  (with-foreign-object (ex :pointer)
    (let ((rv (%dotnet-type
               (hostcall get-element-type
                         :pointer (%dotnet-type-handle type)
                         :pointer ex
                         :pointer))))
      (%transform-exception (mem-ref ex :pointer))
      rv)))

(defun get-generic-type-arguments (type)
  (declare (type dotnet-type type))
  "Returns a list of generic type arguments for TYPE"
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall get-generic-type-arguments
              :pointer (%dotnet-type-handle type)
              :pointer rv
              :pointer code
              :pointer ex)
    (bike-vector-to-list (%transform-rv rv code ex))))

(defun compiler-generated-member-p (info)
  (declare (type dotnet-object info))
  "Returns T in case of the MemberInfo designated by INFO
 is compiler-generated"
  (hostcall is-compiler-generated-member
            :pointer (%dotnet-object-handle info)
            :bool))

(defun get-array-type-rank (type)
  (declare (type dotnet-type type))
  "Gets a number of dimensions of an array type"
  (hostcall get-array-type-rank
            :pointer (%dotnet-type-handle type)
            :int))

(defun transient-type-p (type)
  (declare (type dotnet-type type))
  "Returns non-NIL in case of the TYPE is coming from a dynamic assembly"
  (hostcall is-transient-type
            :pointer (%dotnet-type-handle type)
            :bool))

(defun pointer-type-p (type)
  (declare (type dotnet-type type))
  "Returns non-NIL in case of the TYPE is a pointer type"
  (hostcall is-pointer-type
            :pointer (%dotnet-type-handle type)
            :bool))

(defun array-type-p (type)
  (declare (type dotnet-type type))
  "Returns non-NIL in case of the TYPE is an array type"
  (hostcall is-array-type
            :pointer (%dotnet-type-handle type)
            :bool))

(defun ref-type-p (type)
  (declare (type dotnet-type type))
  "Returns non-NIL in case of the TYPE is an ref type"
  (hostcall is-by-ref-type
            :pointer (%dotnet-type-handle type)
            :bool))

(defun enum-type-p (type)
  (declare (type dotnet-type type))
  "Returns non-NIL in case of the TYPE is an enum type"
  (hostcall is-enum-type
            :pointer (%dotnet-type-handle type)
            :bool))

(declaim (inline %object-equals))
(defun %object-equals (left right)
  (declare (type dotnet-object left right))
  (hostcall object-equals
            :pointer (%dotnet-object-handle left)
            :pointer (%dotnet-object-handle right)
            :bool))


(defun load-assembly (assembly-string)
  (declare (type string-designator assembly-string))
  "Loads an assembly designated by ASSEMBLY-STRING"
  (with-foreign-object (ex :pointer)
    (let* ((rv (%dotnet-object
                (hostcall load-assembly
                          lpwstr (string assembly-string)
                          :pointer ex
                          :pointer)))
           (ex (mem-ref ex :pointer)))
      (%transform-exception ex)
      rv)))

;;; vim: ft=lisp et
