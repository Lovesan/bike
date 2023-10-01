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

(defconstant +member-type-constructor+ 1)
(defconstant +member-type-event+ 2)
(defconstant +member-type-field+ 4)
(defconstant +member-type-method+ 8)
(defconstant +member-type-property+ 16)
(defconstant +member-type-typeinfo+ 32)
(defconstant +member-type-custom+ 64)
(defconstant +member-type-nested-type+ 128)

(defconstant +binding-flags-default+ 0)
(defconstant +binding-flags-ignore-case+ 1)
(defconstant +binding-flags-declared-only+ 2)
(defconstant +binding-flags-instance+ 4)
(defconstant +binding-flags-static+ 8)
(defconstant +binding-flags-public+ 16)
(defconstant +binding-flags-non-public+ 32)
(defconstant +binding-flags-flatten-hierarchy+ 64)
(defconstant +binding-flags-invoke-method+ 256)
(defconstant +binding-flags-create-instance+ 512)
(defconstant +binding-flags-get-field+ 1024)
(defconstant +binding-flags-set-field+ 2048)
(defconstant +binding-flags-get-property+ 4096)
(defconstant +binding-flags-set-property+ 8192)
(defconstant +binding-flags-put-disp-property+ 16384)
(defconstant +binding-flags-put-ref-dist-property+ 32768)
(defconstant +binding-flags-exact-binding+ 65536)
(defconstant +binding-flags-suppress-change-type+ 131072)
(defconstant +binding-flags-optional-param-binding+ 262144)
(defconstant +binding-flags-ignore-return+ 16777216)
(defconstant +binding-flags-do-not-wrap-exceptions+ 33554432)
(defconstant +default-binding-flags+ (logior +binding-flags-public+
                                             +binding-flags-static+
                                             +binding-flags-instance+
                                             +binding-flags-ignore-case+))


(define-constant +primitive-types+ '(("System.Char" . dnchar)
                                     ("System.Boolean" . :bool)
                                     ("System.SByte" . :int8)
                                     ("System.Byte" . :uint8)
                                     ("System.Int16" . :int16)
                                     ("System.UInt16" . :uint16)
                                     ("System.Int32" . :int32)
                                     ("System.UInt32" . :uint32)
                                     ("System.Int64" . :int64)
                                     ("System.UInt64" . :uint64)
                                     ("System.Single" . :float)
                                     ("System.Double" . :double)
                                     ("System.IntPtr" . :pointer)
                                     ("System.UIntPtr" . :pointer))
  :test #'equal)

(define-constant +primitive-lisp-types+ '(("System.Char" . character)
                                          ("System.String" . (or null string))
                                          ("System.Boolean" . t)
                                          ("System.SByte" . (signed-byte 8))
                                          ("System.Byte" . (unsigned-byte 8))
                                          ("System.Int16" . (signed byte 16))
                                          ("System.UInt16" . (unsigned-byte 16))
                                          ("System.Int32" . (signed-byte 32))
                                          ("System.UInt32" . (unsigned-byte 32))
                                          ("System.Int64" . (signed-byte 64))
                                          ("System.UInt64" . (unsigned-byte 64))
                                          ("System.Single" . single-float)
                                          ("System.Double" . double-float)
                                          ("System.IntPtr" . foreign-pointer)
                                          ("System.UIntPtr" . foreign-pointer))
  :test #'equal)

(deftype known-kind () '(member :method :field :property))

(defstruct (known-entry (:constructor %make-known-entry)
                        (:conc-name %known-)
                        (:predicate known-entry-p))
  "Represents a known member descriptor"
  (type-name (required-slot) :type string :read-only t)
  (member-name (required-slot) :type string :read-only t)
  (type-args '() :type list :read-only t)
  (kind (required-slot) :type known-kind :read-only t)
  (arg-types '() :type list :read-only t)
  (delegates '() :type list :read-only t)
  (doc nil :type (or null string) :read-only t)
  (decls nil :type list :read-only t))

(define-global-var -knowns- nil)

(declaim (type (or null hash-table) knowns))

(defun %get-primitive-type (full-name)
  (declare (type string full-name))
  (cdr (assoc full-name +primitive-types+ :test #'equal)))

(defun %get-lisp-primitive-type (full-name)
  (declare (type string full-name))
  (cdr (assoc full-name +primitive-lisp-types+ :test #'equal)))

(declaim (inline %binding-flags))
(defun %binding-flags (flag &rest flags)
  (%%enum-to-object (%get-type-by-name "System.Reflection.BindingFlags" t nil)
                    (apply #'logior flag flags)))

(defun %params-array-p (info)
  (declare (type dotnet-object info))
  (let ((attribute-type
          (%get-type-by-name "System.Attribute" t nil))
        (param-array-attribute-type
          (%get-type-by-name "System.ParamArrayAttribute" t nil)))
    (and (%invoke attribute-type t '() "GetCustomAttribute"
                  info param-array-attribute-type)
         t)))

(defun %make-param-array-iterator (parameters)
  (declare (type dotnet-object parameters))
  (let* ((count (%array-length parameters))
         (i 0))
    (lambda ()
      (when (< i count)
        (let* ((param (%net-vref parameters i))
               (type (%get-property param nil "ParameterType"))
               (type-name (%get-type-full-name type))
               (outp (%get-property param nil "IsOut"))
               (refp (%get-property type nil "IsByRef"))
               (name (%get-property param nil "Name")))
          (multiple-value-prog1
              (values (or name (string (gensym #.(string '#:arg))))
                      type
                      (%get-primitive-type type-name)
                      (%get-lisp-primitive-type type-name)
                      (if refp (if outp :out :ref) :in)
                      (%params-array-p param))
            (incf i)))))))

(defun %register-known (name type-name member-name type-args kind delegates &optional arg-types)
  (declare (type symbol name)
           (type string type-name member-name)
           (type known-kind kind)
           (type (or list dotnet-object) delegates)
           (type list arg-types type-args))
  (setf delegates (ensure-list delegates))
  (setf (gethash name -knowns-)
        (%make-known-entry :type-name type-name
                           :member-name member-name
                           :type-args type-args
                           :kind kind
                           :arg-types arg-types
                           :delegates delegates))
  (values))

(defun %ensure-known-field (name type type-name member-name doc decls)
  (declare (type symbol name)
           (type string type-name member-name)
           (type list decls)
           (type (or null string) doc))
  (let ((info (%invoke type nil '() "GetField"
                       member-name
                       (%binding-flags +binding-flags-public+
                                       +binding-flags-static+
                                       +binding-flags-instance+
                                       +binding-flags-ignore-case+))))
    (unless info
      (error 'field-resolution-error :type type :field member-name))
    (let* ((staticp (%get-property info nil "IsStatic"))
           (field-type (%get-property info nil "FieldType"))
           (field-type-name (%get-type-full-name field-type))
           (primitive-type (%get-primitive-type field-type-name))
           (lisp-type (%get-lisp-primitive-type field-type-name)))
      (multiple-value-bind (reader reader-ptr writer writer-ptr)
          (%get-accessor-trampolines info :field)
        (when reader
          (compile-reader-trampoline
           reader-ptr staticp primitive-type lisp-type name doc decls))
        (when writer
          (compile-writer-trampoline
           writer-ptr staticp primitive-type lisp-type `(setf ,name) doc decls))
        (%register-known name type-name member-name '() :field (cons reader writer))))))

(defun %ensure-known-property (name type type-name member-name doc decls)
  (declare (type symbol name)
           (type string type-name member-name)
           (type list decls)
           (type (or null string) doc))
  (let ((info (%invoke type nil '() "GetProperty"
                       member-name
                       (%binding-flags +binding-flags-public+
                                       +binding-flags-static+
                                       +binding-flags-instance+
                                       +binding-flags-ignore-case+))))
    (unless info
      (error 'property-resolution-error :type type :property member-name))
    (let* ((staticp (%get-property (%net-vref (%invoke info nil '() "GetAccessors") 0)
                                   nil "IsStatic"))
           (property-type (%get-property info nil "PropertyType"))
           (property-type-name (%get-type-full-name property-type))
           (primitive-type (%get-primitive-type property-type-name))
           (lisp-type (%get-lisp-primitive-type property-type-name)))
      (multiple-value-bind (reader reader-ptr writer writer-ptr)
          (%get-accessor-trampolines info :property)
        (when reader
          (compile-reader-trampoline
           reader-ptr staticp primitive-type lisp-type name doc decls))
        (when writer
          (compile-writer-trampoline
           writer-ptr staticp primitive-type lisp-type `(setf ,name) doc decls))
        (%register-known name type-name member-name '() :property (cons reader writer))))))

(defun %compile-known-method (info name doc decls)
  (declare (type dotnet-object info)
           (type (or symbol cons) name)
           (type (or null string) doc)
           (type list decls))
  (let* ((staticp (%get-property info nil "IsStatic"))
         (parameters (%invoke info nil '() "GetParameters"))
         (return-type (%get-property info nil "ReturnType"))
         (void-type (%get-type-by-name "System.Void" t nil))
         (voidp (%invoke return-type nil '() "Equals" void-type))
         (iter (%make-param-array-iterator parameters)))
    (multiple-value-bind (delegate fptr) (%get-delegate-trampoline info '())
      (compile-method-trampoline fptr staticp voidp iter name doc decls)
      delegate)))

(defun %filter-know-generic-methods (candidates type-args)
  (declare (type dotnet-object candidates)
           (list type-args))
  (let* ((initial (bike-vector-to-list candidates))
         (type-args (mapcar (lambda (n) (%get-type-by-name n t nil)) type-args))
         (argc (length type-args))
         (defs (remove-if (lambda (m)
                            (not (%get-property m nil "IsGenericMethodDefinition")))
                          initial))
         (candidates (remove-if (lambda (md)
                                  (/= argc (%array-length
                                            (%invoke md nil '() "GetGenericArguments"))))
                                defs))
         (instances (mapcar (lambda (md)
                              (apply #'%invoke md nil '() "MakeGenericMethod" type-args))
                            candidates))
         (mb-type (%get-type-by-name "System.Reflection.MethodBase" t nil)))
    (%list-to-bike-vector instances mb-type)))

(defun %ensure-known-method (name type type-name member-name type-args arg-types doc decls)
  (declare (type symbol name)
           (type dotnet-type type)
           (type string type-name member-name)
           (type list arg-types type-args decls)
           (type (or null string) doc))
  (let* ((argc (length arg-types))
         (args-vector (%make-vector-of (%get-type-by-name "System.Type" t nil) argc)))
    (loop :for i :from 0
          :for arg-type :in arg-types
          :do (setf (%net-vref args-vector i) (%get-type-by-name arg-type t nil)))
    (let* ((binding-flags (%binding-flags +binding-flags-public+
                                          +binding-flags-static+
                                          +binding-flags-instance+
                                          +binding-flags-ignore-case+
                                          +binding-flags-invoke-method+))
           (candidates (%invoke type nil '() "GetMember"
                                member-name
                                (%%enum-to-object
                                 (%get-type-by-name "System.Reflection.MemberTypes" t nil)
                                 +member-type-method+)
                                binding-flags))
           (applicable (if type-args
                         (%filter-know-generic-methods candidates type-args)
                         candidates))
           (binder (%get-property (%get-type-by-name "System.Type" t nil) t "DefaultBinder"))
           (info (%invoke binder nil '() "SelectMethod"
                          binding-flags
                          applicable
                          args-vector
                          nil)))
      (unless info
        (error 'method-resolution-error :type type
                                        :method member-name
                                        :args (bike-vector-to-list args-vector)))
      (let ((delegate (%compile-known-method info name doc decls)))
        (%register-known name type-name member-name type-args :method delegate arg-types)))))

(defun %ensure-known-indexer (name type type-name member-name arg-types doc decls)
  (declare (type symbol name)
           (type string type-name member-name)
           (type list decls arg-types)
           (type (or null string) doc))
  (let* ((argc (length arg-types))
         (args-vector (%make-vector-of (%get-type-by-name "System.Type" t nil) argc)))
    (loop :for i :from 0
          :for arg-type :in arg-types
          :do (setf (%net-vref args-vector i) (%get-type-by-name arg-type t nil)))
    (let ((info (%invoke type nil '() "GetProperty"
                         member-name
                         (%binding-flags +binding-flags-public+
                                         +binding-flags-static+
                                         +binding-flags-instance+
                                         +binding-flags-ignore-case+)
                         nil
                         nil
                         args-vector
                         nil)))
      (unless info
        (error 'property-resolution-error :type type :property (cons member-name arg-types)))
      (let* ((accessors (cons (%get-property info nil "GetMethod")
                              (%get-property info nil "SetMethod")))
             (reader (car accessors))
             (writer (cdr accessors)))
        (when reader (setf reader (%compile-known-method reader name doc decls)))
        (when writer (setf writer (%compile-known-method writer (list 'setf name) doc decls)))
        (%register-known name type-name member-name '()
                         :property (cons reader writer) arg-types)))))

(defun %ensure-known (name type-name member-name type-args kind arg-types doc decls)
  (declare (type symbol name)
           (type string type-name member-name)
           (type list arg-types type-args)
           (type known-kind kind))
  (let ((type (%get-type-by-name type-name t nil)))
    (ecase kind
      (:method
          (%ensure-known-method name type type-name member-name type-args arg-types doc decls))
      (:property
       (if (endp arg-types)
         (%ensure-known-property name type type-name member-name doc decls)
         (%ensure-known-indexer name type type-name member-name arg-types doc decls)))
      (:field
       (%ensure-known-field name type type-name member-name doc decls)))
    (values)))

(defmacro defknown (name (type-name kind member-name &rest arg-types) &body docs-and-decls)
  (declare (type symbol name)
           (type known-kind kind)
           (type string-designator type-name)
           (type (or cons string-designator) member-name))
  "Establishes internal direct binding to .Net member"
  (multiple-value-bind
        (forms decls doc) (parse-body docs-and-decls :documentation t)
    (when (and (null doc)
               (= 1 (length forms))
               (stringp (car forms)))
      (setf doc (car forms)
            forms '()))
    (when forms (error "Unexpected forms in ~s: ~{~s~^ ~}" 'defknown forms))
    (let* ((type-name (string type-name))
           (genericp (consp member-name))
           (type-args (if genericp (mapcar #'string (cdr member-name)) '()))
           (member-name (string (if genericp (car member-name) member-name)))
           (arg-types (mapcar #'string arg-types)))
      `(eval-when (:load-toplevel :execute)
         (%ensure-known ',name ,type-name ,member-name ',type-args ',kind ',arg-types ,doc ',decls)
         ',name))))

(defun initialize-knowns ()
  (let ((old -knowns-))
    (setf -knowns- (make-hash-table :test #'equal))
    (when old
      (maphash (lambda (name entry)
                 (%ensure-known name
                                (%known-type-name entry)
                                (%known-member-name entry)
                                (%known-type-args entry)
                                (%known-kind entry)
                                (%known-arg-types entry)
                                (%known-doc entry)
                                (%known-decls entry)))
               old))
    (values)))

(uiop:register-image-restore-hook #'initialize-knowns (null -knowns-))

;;; vim: ft=lisp et
