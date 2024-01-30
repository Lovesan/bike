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

(in-readtable bike-syntax)

(defstruct (dynamic-type-cache (:constructor %make-dynamic-type-cache)
                               (:conc-name dtc-)
                               (:copier nil))
  (lock (make-rwlock) :type rwlock :read-only t)
  (assembly (required-slot) :type dotnet-object)
  (module (required-slot) :type dotnet-object)
  (type-count 0 :type (unsigned-byte 32))
  (classes '() :type list)
  (getter-delegate (required-slot) :type dotnet-object)
  (setter-delegate (required-slot) :type dotnet-object)
  (invoke-delegate (required-slot) :type dotnet-object)
  (proxy-interface-type (required-slot) :type dotnet-type))

(defstruct (type-builder-state (:constructor %make-type-builder-state)
                               (:conc-name tbs-)
                               (:copier nil))
  (type-builder (required-slot) :type dotnet-object)
  (getter-field (required-slot) :type dotnet-object)
  (setter-field (required-slot) :type dotnet-object)
  (context-field (required-slot) :type dotnet-object)
  (invoke-field (required-slot) :type dotnet-object))

(defstruct (method-parameter-info (:copier nil)
                                  (:conc-name mpi-)
                                  (:constructor make-mpi))
  (name (required-slot) :type symbol :read-only t)
  (dotnet-name (required-slot)
   :type simple-character-string
   :read-only t)
  (type (required-slot))
  (position 0 :type fixnum :read-only t)
  (direction :in :type (member :in :out :io)
   :read-only t)
  (ref-p nil :type boolean :read-only t))

(defstruct (generic-parameter-info (:copier nil)
                                   (:conc-name gpi-)
                                   (:constructor make-gpi
                                       (&key name
                                             (dotnet-name (camel-case-string
                                                           name :capitalize t))
                                             constraints)))
  (name (required-slot) :type symbol :read-only t)
  (dotnet-name (required-slot)
   :type simple-character-string
   :read-only t)
  (type nil)
  (constraints '() :type list :read-only t))

(define-global-var -dynamic-type-cache- nil)
(declaim (type (or null dynamic-type-cache) -dynamic-type-cache-))

(define-constant +dynamic-assembly-name+ "Bike.Dynamic"
  :test #'equal)

(define-constant +callable-proxy-context-field-name+
  "_context"
  :test #'equal)

(define-constant +callable-proxy-getter-field-name+
  "_getter"
  :test #'equal)

(define-constant +callable-proxy-setter-field-name+
  "_setter"
  :test #'equal)

(define-constant +callable-proxy-invoke-field-name+
  "_invoke"
  :test #'equal)

(defun make-dynamic-type-cache-assembly ()
  [:System.Reflection.Emit.AssemblyBuilder
   DefineDynamicAssembly
   (new 'System.Reflection.AssemblyName +dynamic-assembly-name+)
   #e(System.Reflection.Emit.AssemblyBuilderAccess RunAndCollect)])

(defun make-dynamic-type-cache-module (assembly)
  (declare (type dotnet-object assembly))
  [assembly DefineDynamicModule
            (or [assembly %FullName] +dynamic-assembly-name+)])

(defun make-dynamic-type-cache (getter setter invoke &optional classes)
  (let* ((asm (make-dynamic-type-cache-assembly))
         (module (make-dynamic-type-cache-module asm))
         (proxy-if-builder [module DefineType
                                   (format nil "~a.IDotNetCallableProxy"
                                           +dynamic-assembly-name+)
                                   #e(System.Reflection.TypeAttributes
                                      Public
                                      Abstract
                                      Interface
                                      AutoClass)]))
    (%make-dynamic-type-cache :assembly asm
                              :module module
                              :proxy-interface-type [proxy-if-builder CreateType]
                              :classes classes
                              :getter-delegate getter
                              :setter-delegate setter
                              :invoke-delegate invoke)))

(defun type-vector (&rest elements)
  (list-to-bike-vector elements :element-type :type))

(defun make-type-builder-state (name base-type interfaces)
  (declare (type simple-character-string name)
           (type (or null dotnet-type) base-type)
           (type list interfaces))
  (with-accessors ((lock dtc-lock)
                   (type-count dtc-type-count)
                   (module dtc-module))
      -dynamic-type-cache-
    (let* ((type-name (format nil "~a.<~a>$~a"
                              +dynamic-assembly-name+
                              name
                              (with-write-lock (lock) (incf type-count))))
           (type-builder
             [module DefineType
                     type-name
                     #e(System.Reflection.TypeAttributes AutoClass Public)
                     (or base-type [:object])
                     (list-to-bike-vector interfaces :element-type :type)])
           (attr-builder
             (new 'System.Reflection.Emit.CustomAttributeBuilder
                  [[:System.Runtime.CompilerServices.CompilerGeneratedAttribute]
                   GetConstructor
                   (empty-types)]
                  [:System.Array (Empty :object)])))
      [type-builder SetCustomAttribute attr-builder]
      (%make-type-builder-state
       :type-builder type-builder
       :getter-field [type-builder DefineField
                                   +callable-proxy-getter-field-name+
                                   [:(System.Func :object :object :string :object)]
                                   #e(System.Reflection.FieldAttributes Private)]
       :setter-field [type-builder DefineField
                                   +callable-proxy-setter-field-name+
                                   [:(System.Action :object :object :string :object)]
                                   #e(System.Reflection.FieldAttributes Private)]
       :context-field [type-builder DefineField
                                    +callable-proxy-context-field-name+
                                    [:object]
                                    #e(System.Reflection.FieldAttributes Private)]
       :invoke-field [type-builder DefineField
                                   +callable-proxy-invoke-field-name+
                                   [:(System.Func :object
                                                  :object
                                                  :string
                                                  (array :object)
                                                  :object)]
                                   #e(System.Reflection.FieldAttributes Private)]))))

(defun get-compare-exchange-method (type)
  (declare (type dotnet-type type))
  (let ((interlocked-type [:System.Threading.Interlocked])
        (method-name "CompareExchange")
        (binding-flags #e(System.Reflection.BindingFlags Public Static)))
    (if (or (value-type-p type)
            (bike-equals type [:object]))
      [interlocked-type GetMethod method-name binding-flags (type-vector
                                                             (resolve-type (list :ref type))
                                                             type
                                                             type)]
      (do-enumerable (mi (type-get-members interlocked-type
                                           method-name
                                           #e(System.Reflection.MemberTypes Method)
                                           binding-flags))
        (when (and (string= method-name (member-info-name mi))
                   (generic-method-definition-p mi))
          (let ((params (%method-parameters mi))
                (generic-params (method-generic-arguments mi)))
            (when (and (= 3 (%array-length params))
                       (= 1 (%array-length generic-params)))
              (return (make-generic-method mi type)))))))))

(defun get-cast-method (type)
  (make-generic-method [[:BikeInterop.TypeCaster] GetMethod "Cast"]
                       type))

(defmacro with-il-generator ((generator-var generator &rest label-names) &body body)
  `(let ((,generator-var ,generator))
     (declare (type dotnet-object ,generator-var))
     (macrolet ((opcode (name)
                  `(field 'System.Reflection.Emit.OpCodes ',name))
                (emit (opcode &rest args)
                  `(invoke ,',generator-var 'Emit (opcode ,opcode) ,@args))
                (emit-s1 (opcode arg) `(%emit-s1 ,',generator-var (opcode ,opcode) ,arg))
                (emit-u1 (opcode arg) `(%emit-u1 ,',generator-var (opcode ,opcode) ,arg))
                (emit-s2 (opcode arg) `(%emit-s2 ,',generator-var (opcode ,opcode) ,arg))
                (emit-u2 (opcode arg) `(%emit-u2 ,',generator-var (opcode ,opcode) ,arg))
                (emit-s4 (opcode arg) `(%emit-s4 ,',generator-var (opcode ,opcode) ,arg))
                (emit-u4 (opcode arg) `(%emit-u4 ,',generator-var (opcode ,opcode) ,arg))
                (emit-s8 (opcode arg) `(%emit-u8 ,',generator-var (opcode ,opcode) ,arg)))
       (flet ((declare-local (type)
                (invoke ,generator-var 'DeclareLocal type))
              (define-label ()
                (invoke ,generator-var 'DefineLabel))
              (mark-label (label)
                (invoke ,generator-var 'MarkLabel label)))
         (declare (ignorable (function declare-local)
                             (function define-label)
                             (function mark-label)))
         (let ,(loop :for name :in label-names
                     :collect `(,name (define-label)))
           ,@body)))))

(defun tbs-gen-unbox (gen type &optional (tmp [gen DeclareLocal [:object]]))
  (declare (type dotnet-object gen tmp)
           (type dotnet-type type))
  (with-il-generator (gen gen unbox unbox-end)
    (emit Stloc tmp)
    (emit Ldloc tmp)
    (emit Isinst type)
    (emit Brtrue unbox)
    (emit Ldloc tmp)
    (emit Call (get-cast-method type))
    (emit Br unbox-end)
    (mark-label unbox)
    (emit Ldloc tmp)
    (emit Unbox_Any type)
    (mark-label unbox-end)))

(defun tbs-gen-copy-array (gen from-local from-index to-local to-index)
  (declare (type dotnet-object gen from-local to-local)
           (type (signed-byte 32) from-index to-index))
  (let ((copy-method [[:System.Array] GetMethod "Copy" (type-vector [:array]
                                                                    [:int]
                                                                    [:array]
                                                                    [:int]
                                                                    [:int])])
        (min-method [[:System.Math] GetMethod "Min" (type-vector [:int]
                                                                 [:int])])
        (max-method [[:System.Math] GetMethod "Max" (type-vector [:int]
                                                                 [:int])]))
    (with-il-generator (gen gen)
      (emit Ldloc from-local)
      (emit-s4 Ldc_I4 from-index)
      (emit Ldloc to-local)
      (emit-s4 Ldc_I4 to-index)
      (emit Ldloc from-local)
      (emit Ldlen)
      (emit-s4 Ldc_I4 (- from-index))
      (emit Add)
      (emit Ldloc to-local)
      (emit Ldlen)
      (emit-s4 Ldc_I4 (- to-index))
      (emit Add)
      (emit call min-method)
      (emit Ldc_i4_0)
      (emit call max-method)
      (emit call copy-method))))

(defun tbs-gen-box-parameter
    (gen parameter &optional staticp)
  (declare (type dotnet-object gen)
           (type method-parameter-info parameter))
  (let ((type (mpi-type parameter)))
    (with-il-generator (gen gen)
      (tbs-gen-load-parameter gen parameter staticp)
      (when (mpi-ref-p parameter)
        (emit Ldobj type))
      (emit Box type)
      (emit Castclass [:object]))))

(defun tbs-gen-load-parameter (gen param &optional staticp)
  (declare (type dotnet-object gen)
           (type method-parameter-info param))
  (with-il-generator (gen gen)
    (let ((pos (mpi-position param)))
      (emit-s4 Ldarg (if staticp pos (1+ pos))))))

(defun tbs-gen-load-field (gen field-info &optional staticp)
  (declare (type dotnet-object gen field-info))
  (with-il-generator (gen gen)
    (unless staticp
      (emit Ldarg_0))
    (emit Ldfld field-info)))

(defun tbs-add-event-method (state event-name event-builder event-field addp)
  (declare (type type-builder-state state)
           (type simple-character-string event-name)
           (type dotnet-object event-builder event-field))
  (with-accessors ((type-builder tbs-type-builder))
      state
    (let* ((prefix (if addp "add_" "remove_"))
           (handler-type [event-field %FieldType])
           (combine-method
             [[:System.Delegate] GetMethod
              (if addp "Combine" "Remove")
              #e(System.Reflection.BindingFlags Public Static)
              (type-vector [:System.Delegate] [:System.Delegate])])
           (method-builder [type-builder DefineMethod
                                         (strcat prefix event-name)
                                         #e(System.Reflection.MethodAttributes
                                            HideBySig
                                            SpecialName
                                            Public
                                            Final
                                            Virtual
                                            NewSlot)
                                         [:void]
                                         (type-vector handler-type)])
           (compare-exchange (get-compare-exchange-method handler-type)))
      (unless compare-exchange
        (error "Internal error. No Interlocked.CompareExchange for ~s" handler-type))
      [method-builder DefineParameter
                      1
                      #e(System.Reflection.ParameterAttributes None)
                      "value"]
      (with-il-generator (gen [method-builder GetILGenerator] cmp-loop)
        ;; HandlerType fieldValue;
        ;; HandlerType tmp;
        ;; HandlerType combineResult;
        ;; fieldValue = this.EventName;
        ;; do
        ;; {
        ;;     tmp = fieldValue;
        ;;     combineResult = (HandlerType)Delegate.Combine(tmp, value);
        ;;     fieldValue = Interlocked.CompareExchange(ref this.EventName, combineResult, tmp);
        ;; } while ( fieldValue != tmp );
        (declare-local handler-type)
        (declare-local handler-type)
        (declare-local handler-type)
        (emit Ldarg_0)
        (emit Ldfld event-field)
        (emit Stloc_0)
        (mark-label cmp-loop)
        (emit Ldloc_0)
        (emit Stloc_1)
        (emit Ldloc_1)
        (emit Ldarg_1)
        (emit Call combine-method)
        (emit Castclass handler-type)
        (emit Stloc_2)
        (emit Ldarg_0)
        (emit Ldflda event-field)
        (emit Ldloc_2)
        (emit Ldloc_1)
        (emit Call compare-exchange)
        (emit StLoc_0)
        (emit Ldloc_0)
        (emit Ldloc_1)
        (emit Ceq)
        (emit Brfalse_S cmp-loop)
        (emit Ret))
      (if addp
        [event-builder SetAddOnMethod method-builder]
        [event-builder SetRemoveOnMethod method-builder])
      method-builder)))

(defun tbs-add-event (state name handler-type raise-method-name)
  (declare (type type-builder-state state)
           (type simple-character-string name)
           (type dotnet-type handler-type)
           (type (or null simple-character-string) raise-method-name))
  (with-accessors ((type-builder tbs-type-builder))
      state
    (let* ((event-builder
             [type-builder DefineEvent
                           name
                           #e(System.Reflection.EventAttributes None)
                           handler-type])
           (event-field
             [type-builder DefineField
                           name
                           handler-type
                           #e(System.Reflection.FieldAttributes Private)]))
      (tbs-add-event-method state name event-builder event-field t)
      (tbs-add-event-method state name event-builder event-field nil)
      (when raise-method-name
        (let* ((handler-invoke [handler-type GetMethod "Invoke"])
               (handler-parameters (method-parameters handler-invoke))
               (handler-parameter-types (mapcar #'parameter-type handler-parameters))
               (handler-return-param (method-return-parameter handler-invoke))
               (raise-builder [type-builder DefineMethod
                                            raise-method-name
                                            #e(System.Reflection.MethodAttributes
                                               HideBySig
                                               SpecialName
                                               Public)
                                            (method-return-type handler-invoke)
                                            (list-to-bike-vector handler-parameter-types
                                                                 :element-type :type)]))
          [raise-builder DefineParameter 0
                         (parameter-attributes handler-return-param)
                         (parameter-name handler-return-param)]
          (loop :for i :from 1
                :for param :in handler-parameters :do
                [raise-builder DefineParameter i
                               (parameter-attributes param)
                               (parameter-name param)])
          (with-il-generator (gen [raise-builder GetILGenerator] handler-is-null)
            (declare-local handler-type)
            (emit Ldarg_0)
            (emit Ldfld event-field)
            (emit Stloc_0)
            (emit Ldloc_0)
            (emit Brfalse handler-is-null)
            (emit Ldloc_0)
            (loop :for i :from 1 :to (length handler-parameters)
                  :do (if (< i 256)
                        (emit-u1 Ldarg_S i)
                        (emit-u2 Ldarg i)))
            (emit Callvirt handler-invoke)
            (mark-label handler-is-null)
            (emit Ret))
          [event-builder SetRaiseMethod raise-builder]))
      event-builder)))

(defun tbs-add-property-method (state property-name property-builder readerp)
  (declare (type type-builder-state state)
           (type simple-character-string property-name)
           (type dotnet-object property-builder))
  (with-accessors ((type-builder tbs-type-builder)
                   (getter-field tbs-getter-field)
                   (setter-field tbs-setter-field)
                   (context-field tbs-context-field))
      state
    (let* ((property-type (property-type property-builder))
           (prefix (if readerp "get_" "set_"))
           (builder [type-builder DefineMethod
                                  (strcat prefix property-name)
                                  #e(System.Reflection.MethodAttributes
                                     HideBySig
                                     SpecialName
                                     Public
                                     Final
                                     Virtual
                                     NewSlot)
                                  (if readerp property-type [:void])
                                  (if readerp
                                    (empty-types)
                                    (type-vector property-type))]))
      (with-il-generator (gen [builder GetILGenerator] do-unbox)
        (emit Ldarg_0)
        (emit Ldfld (if readerp getter-field setter-field))
        (emit Ldarg_0)
        (emit Ldarg_0)
        (emit Ldfld context-field)
        (emit Ldstr property-name)
        (if readerp
          (progn
            (emit Callvirt [(field-type getter-field) GetMethod "Invoke"])
            (cond ((value-type-p property-type)
                   (declare-local [:object])
                   (emit Stloc_0)
                   (emit Ldloc_0)
                   (emit Isinst property-type)
                   (emit Brtrue_S do-unbox)
                   (emit Ldloc_0)
                   (emit Tailcall)
                   (emit Call (get-cast-method property-type))
                   (emit Ret)
                   (mark-label do-unbox)
                   (emit Ldloc_0)
                   (emit Unbox_any property-type)
                   (emit Ret))
                  (t (emit Castclass property-type)
                     (emit Ret))))
          (progn
            (emit Ldarg_1)
            (when (value-type-p property-type)
              (emit Box property-type))
            (emit Tailcall)
            (emit Callvirt [(field-type setter-field) GetMethod "Invoke"])
            (emit Ret))))
      (if readerp
        [property-builder SetGetMethod builder]
        [property-builder SetSetMethod builder])
      builder)))

(defun tbs-add-property (state name type has-reader has-writer)
  (declare (type type-builder-state state)
           (type simple-character-string name)
           (type dotnet-type type))
  (with-accessors ((type-builder tbs-type-builder))
      state
    (let* ((property-builder [type-builder DefineProperty
                                           name
                                           #e(System.Reflection.PropertyAttributes None)
                                           type
                                           (empty-types)]))
      (when has-reader
        (tbs-add-property-method state name property-builder t))
      (when has-writer
        (tbs-add-property-method state name property-builder nil))
      property-builder)))

(defun tbs-add-constructor (state base-constructor base-callable-p)
  (declare (type type-builder-state state)
           (type dotnet-object base-constructor))
  (with-accessors ((type-builder tbs-type-builder)
                   (getter-field tbs-getter-field)
                   (setter-field tbs-setter-field)
                   (context-field tbs-context-field)
                   (invoke-field tbs-invoke-field))
      state
    (let* ((base-parameters (if base-callable-p
                              (nthcdr 4 (method-parameters base-constructor))
                              (method-parameters base-constructor)))
           (builder [type-builder DefineConstructor
                                  #e(System.Reflection.MethodAttributes Public)
                                  #e(System.Reflection.CallingConventions HasThis)
                                  (list-to-bike-vector
                                   (list* [:object]
                                          (field-type getter-field)
                                          (field-type setter-field)
                                          (field-type invoke-field)
                                          (mapcar #'parameter-type base-parameters))
                                   :element-type :type)])
           (param-attrs #e(System.Reflection.ParameterAttributes None)))
      [builder DefineParameter 1 param-attrs "$context"]
      [builder DefineParameter 2 param-attrs "$getter"]
      [builder DefineParameter 3 param-attrs "$setter"]
      [builder DefineParameter 4 param-attrs "$invoke"]
      (loop :for i :from 5
            :for param :in base-parameters
            :for base-param-attrs = (parameter-attributes param)
            :do [builder DefineParameter i base-param-attrs (parameter-name param)])
      (with-il-generator (gen [builder GetILGenerator])
        (emit Ldarg_0)
        (when base-callable-p
          (emit Ldarg_1)
          (emit Ldarg_2)
          (emit Ldarg_3)
          (emit-u1 Ldarg_S 4))
        (loop :for i :from 5
              :for param :in base-parameters
              :do (if (< i 256)
                    (emit-u1 Ldarg_S i)
                    (emit-u2 Ldarg i)))
        (emit Call base-constructor)
        (emit Ldarg_0)
        (emit Ldarg_1)
        (emit Stfld context-field)
        (emit Ldarg_0)
        (emit Ldarg_2)
        (emit Stfld getter-field)
        (emit Ldarg_0)
        (emit Ldarg_3)
        (emit Stfld setter-field)
        (emit Ldarg_0)
        (emit-u1 Ldarg_S 4)
        (emit Stfld invoke-field)
        (emit Ret))
      builder)))

(defun tbs-resolve-type (type generic-parameters)
  (declare (type list generic-parameters))
  (labels ((resolve (ast assembly)
             (cond
               ((symbolp ast)
                (gpi-type (find ast generic-parameters :key #'gpi-name)))
               ((dotnet-type-p ast) ast)
               ((stringp ast)
                (%type-entry-type (%intern-string-ast ast assembly)))
               ((consp ast)
                (case (first ast)
                  (* (make-pointer-type (resolve (second ast) assembly)))
                  (:ref (make-ref-type (resolve (second ast) assembly)))
                  (:array (let* ((element-type (resolve (second ast) assembly))
                                 (mz-vector-p (eq (third ast) '*))
                                 (rank (if mz-vector-p 1 (third ast))))
                            (if (or mz-vector-p (> rank 1))
                              (make-array-type* element-type rank)
                              (make-array-type element-type))))
                  (:qualified (resolve (second ast) (third ast)))
                  ;; generic type
                  (t (let ((definition (resolve (first ast) assembly))
                           (args (loop :for arg :in (rest ast)
                                       :collect (resolve arg nil))))
                       (apply #'make-generic-type definition args))))))))
    (with-type-table-lock (:read)
      (if generic-parameters
        (resolve type nil)
        (%type-entry-type (%intern-type-ast type nil))))))

(defun tbs-initialize-generic-parameters (generic-parameters
                                          generic-parameter-builders)
  (declare (type list generic-parameters generic-parameter-builders))
  (loop :for gpi :in generic-parameters
        :for gpb :in generic-parameter-builders :do
          (setf (gpi-type gpi) gpb))
  (loop :for gpi :in generic-parameters
        :for gpb = (gpi-type gpi) :do
          (loop :with constraints = '()
                :for c :in (gpi-constraints gpi) :do
                  (etypecase c
                    ((eql :in) (push 'Contravariant constraints))
                    ((eql :out) (push 'Covariant constraints))
                    ((eql :new) (push 'DefaultConstructorConstraint constraints))
                    ((eql :struct) (push 'NotNullableValueTypeConstraint constraints))
                    ((eql :class) (push 'ReferenceTypeConstraint constraints))
                    (cons (destructuring-bind (name value &rest values) c
                            (ecase name
                              (:base-type
                               [gpb SetBaseTypeConstraint
                                    (tbs-resolve-type value generic-parameters)])
                              (:interfaces
                               [gpb SetInterfaceConstraints
                                    (list-to-bike-vector
                                     (mapcar (lambda (v)
                                               (tbs-resolve-type v generic-parameters))
                                             (cons value values))
                                     :element-type :type)])))))
                :finally
                   (when constraints
                     [gpb SetGenericParameterAttributes
                          (apply 'enum 'System.Reflection.GenericParameterAttributes
                                 constraints)]))))

(defun tbs-add-method (state name generic-parameters return-type
                       parameters params-array-parameter)
  (declare (type type-builder-state state)
           (type simple-character-string name)
           (type list generic-parameters parameters))
  (with-accessors ((type-builder tbs-type-builder)
                   (invoke-field tbs-invoke-field)
                   (context-field tbs-context-field))
      state
    (let* ((builder [type-builder DefineMethod
                                  name
                                  #e(System.Reflection.MethodAttributes
                                     Public
                                     HideBySig
                                     Final
                                     Virtual
                                     NewSlot)])
           (gp-builders (when generic-parameters
                          (bike-vector-to-list
                           (apply #'invoke builder 'DefineGenericParameters
                                  (mapcar #'gpi-dotnet-name generic-parameters)))))
           (argc (length parameters))
           (argc/plain (if params-array-parameter (1- argc) argc))
           (parameter-types (make-list argc))
           (invoke-method [(field-type invoke-field) GetMethod "Invoke"])
           voidp)
      (tbs-initialize-generic-parameters generic-parameters gp-builders)
      (setf return-type (tbs-resolve-type return-type generic-parameters)
            voidp (bike-equals return-type [:void]))
      (dolist (p parameters)
        (let ((type (tbs-resolve-type (mpi-type p) generic-parameters)))
          (setf (mpi-type p)
                type
                (elt parameter-types (mpi-position p))
                (if (mpi-ref-p p)
                  (make-ref-type type)
                  type))))
      [builder SetReturnType return-type]
      (apply #'invoke builder 'SetParameters parameter-types)
      (dolist (p parameters)
        (let* ((dir (mpi-direction p))
               (refp (mpi-ref-p p))
               (pos (mpi-position p))
               (name (mpi-dotnet-name p))
               (attributes (enum 'System.Reflection.ParameterAttributes
                                 (if (and refp (eq dir :in)) 'In 0)
                                 (if (eq dir :out) 'Out 0)))
               (pb [builder DefineParameter (1+ pos) attributes name]))
          (when (eq p params-array-parameter)
            (let* ((attr-type [:System.ParamArrayAttribute])
                   (cab (new 'System.Reflection.Emit.CustomAttributeBuilder
                             [attr-type GetConstructor (empty-types)]
                             [:System.Array (Empty :object)])))
              [pb SetCustomAttribute cab]))))
      (with-il-generator (gen [builder GetILGenerator])
        (let* ((argc-local (declare-local [:int]))
               (args-local (declare-local [:(array :object)]))
               (rv-local (declare-local [:object]))
               (tmp-local (declare-local [:object])))
          (emit-s4 Ldc_I4 argc/plain)
          (emit Stloc argc-local)
          (when params-array-parameter
            (tbs-gen-load-parameter gen params-array-parameter)
            (emit Ldlen)
            (emit Ldloc argc-local)
            (emit Add)
            (emit Stloc argc-local))
          (emit Ldloc argc-local)
          (emit Newarr [:object])
          (emit Stloc args-local)
          (dolist (p parameters)
            (when (and (not (eq p params-array-parameter))
                       (member (mpi-direction p) '(:in :io)))
              (let* ((pos (mpi-position p)))
                (emit Ldloc args-local)
                (emit-s4 Ldc_i4 pos)
                (tbs-gen-box-parameter gen p)
                (emit Stelem_Ref))))
          (when params-array-parameter
            (let ((params-array-local
                    (declare-local
                     (mpi-type params-array-parameter))))
              (tbs-gen-load-parameter gen params-array-parameter)
              (emit Stloc params-array-local)
              (tbs-gen-copy-array
               gen params-array-local 0 args-local argc/plain)))
          (tbs-gen-load-field gen invoke-field)
          (emit Ldarg_0)
          (tbs-gen-load-field gen context-field)
          (emit Ldstr name)
          (emit Ldloc args-local)
          (emit Callvirt invoke-method)
          (if voidp
            (emit Pop)
            (emit Stloc rv-local))
          (dolist (p parameters)
            (when (member (mpi-direction p) '(:io :out))
              (let* ((pos (mpi-position p))
                     (type (mpi-type p)))
                (tbs-gen-load-parameter gen p)
                (emit Ldloc args-local)
                (emit-s4 Ldc_I4 pos)
                (emit Ldelem_Ref)
                (tbs-gen-unbox gen type tmp-local)
                (emit Stind_Ref))))
          (unless voidp
            (emit Ldloc rv-local)
            (tbs-gen-unbox gen return-type rv-local))
          (emit Ret))
        return-type))))

(defun tbs-create-type (state)
  (declare (type type-builder-state state))
  [(tbs-type-builder state) CreateType])

;;; vim: ft=lisp et
