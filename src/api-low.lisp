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

(defun %cast (object type)
  (declare (type dotnet-object* object)
           (type dotnet-type type))
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall cast
              :pointer (dotnet-object-handle object)
              :pointer (%dotnet-object-handle type)
              :pointer rv
              :pointer code
              :pointer ex)
    (let* ((rv (mem-ref rv :pointer))
           (code (mem-ref code :int))
           (ex (mem-ref ex :pointer))
           (result (%get-boxed-object rv (ash code -8) nil)))
      (%transform-exception ex)
      (values result))))

(defun %get-loaded-assemblies ()
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall get-loaded-assemblies
              :pointer rv
              :pointer code
              :pointer ex
              :void)
    (%transform-rv rv code ex)))

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
  (declare (type dotnet-object* object)
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
                  :pointer (dotnet-object-handle object)
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
  (declare (type dotnet-object* object)
           (type string field-name))
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall get-field
              :pointer (dotnet-object-handle object)
              :bool staticp
              lpwstr field-name
              :pointer rv
              :pointer code
              :pointer ex
              :void)
    (%transform-rv rv code ex)))

(defun %set-field (object staticp field-name value)
  (declare (type dotnet-object* object)
           (type string field-name))
  (multiple-value-bind (boxed cleanup) (%box value)
    (with-foreign-objects ((ex :pointer))
      (hostcall set-field
                :pointer (dotnet-object-handle object)
                :bool staticp
                lpwstr field-name
                :pointer boxed
                :pointer ex
                :void)
      (when cleanup (%free-handle boxed))
      (%transform-exception (mem-ref ex :pointer)))))

(defun %get-property (object staticp property-name)
  (declare (type dotnet-object* object)
           (type string property-name))
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall get-property
              :pointer (dotnet-object-handle object)
              :bool staticp
              lpwstr property-name
              :pointer rv
              :pointer code
              :pointer ex
              :void)
    (%transform-rv rv code ex)))

(defun %set-property (object staticp property-name value)
  (declare (type dotnet-object* object)
           (type string property-name))
  (multiple-value-bind (boxed cleanup) (%box value)
    (with-foreign-objects ((ex :pointer))
      (hostcall set-property
                :pointer (dotnet-object-handle object)
                :bool staticp
                lpwstr property-name
                :pointer boxed
                :pointer ex
                :void)
      (when cleanup (%free-handle boxed))
      (%transform-exception (mem-ref ex :pointer)))))

(defun %get-index (object index &rest indices)
  (declare (type dotnet-object* object)
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
                  :pointer (dotnet-object-handle object)
                  :pointer argp
                  :int argc
                  :pointer rv
                  :pointer code
                  :pointer ex
                  :void)
        (%transform-rv rv code ex cleanup-list)))))

(defun %set-index (object value index &rest indices)
  (declare (type dotnet-object* object)
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
                    :pointer (dotnet-object-handle object)
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

(defun %make-vector-of (type length)
  (declare (type dotnet-type type)
           (type (integer 0 #.(1- (expt 2 31))) length))
  "Creates a .Net vector of TYPE of the specified LENGTH"
  (with-foreign-objects ((rv :pointer)
                         (code :int)
                         (ex :pointer))
    (hostcall make-vector-of
              :pointer (%dotnet-type-handle type)
              :int32 length
              :pointer rv
              :pointer code
              :pointer ex)
    (%transform-rv rv code ex)))

(defun %list-to-bike-vector (list type &key (start 0) end)
  (declare (type list list)
           (type dotnet-type type)
           (type (integer 0 #.(1- (expt 2 31))) start)
           (type (or null (integer 0 #.(1- (expt 2 31)))) end))
  (let* ((end (or end (length list)))
         (count (- end start))
         (vector (%make-vector-of type count)))
    (do ((i 0 (1+ i))
         (sublist (loop :for l :on list
                        :for n :below start
                        :finally (return l))
                  (cdr sublist)))
        ((= i count) vector)
      (setf (%net-vref vector i) (car sublist)))))

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

(defun dnaref (array &rest indices)
  (declare (type dotnet-object array)
           (dynamic-extent indices))
  "Accesses a .Net ARRAY at INDICES"
  (let ((nargs (length indices)))
    (with-foreign-objects ((args :int64 nargs)
                           (rv :pointer)
                           (code :pointer)
                           (ex :pointer))
      (loop :for idx :of-type (signed-byte 64) :in indices
            :for i :from 0
            :do (setf (mem-aref args :int64 i) idx))
      (hostcall array-get
                :pointer (%dotnet-object-handle array)
                :pointer args
                :int nargs
                :pointer rv
                :pointer code
                :pointer ex
                :void)
      (%transform-rv rv code ex))))

(defun (setf dnaref) (new-value array &rest indices)
  (declare (type dotnet-object array)
           (dynamic-extent indices))
  "Accesses a .Net ARRAY at INDICES"
  (let ((nargs (length indices)))
    (multiple-value-bind (boxed cleanup) (%box new-value)
      (with-foreign-objects ((args :int64 nargs)
                             (ex :pointer))
        (loop :for idx :of-type (signed-byte 64) :in indices
              :for i :from 0
              :do (setf (mem-aref args :int64 i) idx))
        (hostcall array-set
                  :pointer (%dotnet-object-handle array)
                  :pointer args
                  :int nargs
                  :pointer boxed
                  :pointer ex
                  :void)
        (when cleanup (%free-handle boxed))
        (let ((ex (mem-ref ex :pointer)))
          (%transform-exception ex)
          new-value)))))

(declaim (inline %pin-object))
(defun %pin-object (obj)
  (declare (type dotnet-object obj))
  (with-foreign-objects ((ptr :pointer)
                         (handle :pointer)
                         (ex :pointer))
    (hostcall pin-object
              :pointer (%dotnet-object-handle obj)
              :pointer ptr
              :pointer handle
              :pointer ex)
    (let ((ex (mem-ref ex :pointer)))
      (%transform-exception ex)
      (values (mem-ref ptr :pointer)
              (mem-ref handle :pointer)))))

(defmacro with-fixed ((pointer-var object) &body body)
  (declare (type symbol pointer-var))
  "Executes BODY forms in a dynamic environment where
 POINTER-VAR is bound to the pinned data of an OBJECT."
  (with-gensyms (handle)
    `(multiple-value-bind (,pointer-var ,handle)
         (%pin-object ,object)
       (declare (type foreign-pointer ,pointer-var))
       (unwind-protect (locally ,@body)
         (%free-handle ,handle)))))

(defmacro with-fixeds ((&rest bindings) &body body)
  "Executes BODY forms in a dynamic environment where
 BINDINGS are bound to the pinned data of corresponding objects."
  (labels ((rec (bindings new-bindings)
             (if (endp bindings)
               `(let ,new-bindings
                  (declare (type foreign-pointer ,@(mapcar #'car new-bindings)))
                  ,@body)
               (destructuring-bind (pointer-var obj)
                   (first bindings)
                 (with-gensyms (pointer)
                   `(with-fixed (,pointer ,obj)
                      ,(rec (rest bindings)
                            (cons (list pointer-var pointer) new-bindings))))))))
    (rec bindings '())))

(defmacro with-fixeds* ((&rest bindings) &body body)
  "Executes BODY forms in a dynamic environment where
 BINDINGS are bound to the pinned data of corresponding objects."
  (if (endp bindings)
    `(locally ,@body)
    `(with-fixed ,(first bindings)
       (with-fixeds* ,(rest bindings) ,@body))))

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

(declaim (inline bike-type-of))
(defun bike-type-of (object)
  "Retrieves .Net type of an OBJECT"
  (multiple-value-bind (boxed cleanup)
      (%box object)
    (let ((type (%dotnet-type (hostcall get-type-of
                                        :pointer boxed
                                        :pointer))))
      (when cleanup (%free-handle boxed))
      type)))

(defun %get-type-full-name (type)
  (declare (type dotnet-type type))
  (let* ((rv (hostcall get-type-full-name
                       :pointer (%dotnet-type-handle type)
                       :pointer)))
    (unless (pointer-eq rv (null-pointer))
      (prog1 (%unbox-string rv)
        (%free-handle rv)))))

(declaim (inline %%enum-to-object))
(defun %%enum-to-object (type value)
  (declare (type dotnet-type type)
           (type (signed-byte 64) value))
  (with-foreign-objects ((ex :pointer))
    (let* ((rv (%dotnet-object
                (hostcall enum-to-object
                          :pointer (%dotnet-type-handle type)
                          :int64 value
                          :pointer ex
                          :pointer))))
      (%transform-exception (mem-ref ex :pointer))
      rv)))

;;; vim: ft=lisp et
