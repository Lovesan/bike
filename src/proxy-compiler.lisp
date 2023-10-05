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
  (classes '() :type list))

(defstruct (type-builder-state (:constructor %make-type-builder-state)
                               (:conc-name tbs-)
                               (:copier nil))
  (type-builder (required-slot) :type dotnet-object)
  (getter-field (required-slot) :type dotnet-object)
  (setter-field (required-slot) :type dotnet-object)
  (context-field (required-slot) :type dotnet-object))

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

(defun make-dynamic-type-cache-assembly ()
  [:System.Reflection.Emit.AssemblyBuilder
   DefineDynamicAssembly
   (new 'System.Reflection.AssemblyName +dynamic-assembly-name+)
   #e(System.Reflection.Emit.AssemblyBuilderAccess RunAndCollect)])

(defun make-dynamic-type-cache-module (assembly)
  (declare (type dotnet-object assembly))
  [assembly DefineDynamicModule
            (or [assembly %FullName] +dynamic-assembly-name+)])

(defun make-dynamic-type-cache (&optional classes)
  (let* ((asm (make-dynamic-type-cache-assembly))
         (module (make-dynamic-type-cache-module asm)))
    (%make-dynamic-type-cache :assembly asm
                              :module module
                              :classes classes)))

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
                                   [:(System.Func :object :string :object)]
                                   #e(System.Reflection.FieldAttributes Private)]
       :setter-field [type-builder DefineField
                                   +callable-proxy-setter-field-name+
                                   [:(System.Action :object :string :object)]
                                   #e(System.Reflection.FieldAttributes Private)]
       :context-field [type-builder DefineField
                                    +callable-proxy-context-field-name+
                                    [:object]
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

(defmacro with-il-generator ((generator &rest label-names) &body body)
  (with-gensyms (generator-var)
    `(let ((,generator-var ,generator))
       (declare (type dotnet-object ,generator-var))
       (macrolet ((emit (opcode &rest args)
                    `(invoke ,',generator-var
                             'Emit
                             (field 'System.Reflection.Emit.OpCodes ',opcode)
                             ,@args)))
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
             ,@body))))))

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
      (with-il-generator ([method-builder GetILGenerator] cmp-loop)
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
          (with-il-generator ([raise-builder GetILGenerator] handler-is-null)
            (declare-local handler-type)
            (emit Ldarg_0)
            (emit Ldfld event-field)
            (emit Stloc_0)
            (emit Ldloc_0)
            (emit Brfalse handler-is-null)
            (emit Ldloc_0)
            (loop :for i :from 1 :to (length handler-parameters)
                  :do (if (< i 256)
                        (emit Ldarg_S i)
                        (emit Ldarg i)))
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
      (with-il-generator ([builder GetILGenerator] do-unbox)
        (emit Ldarg_0)
        (emit Ldfld (if readerp getter-field setter-field))
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
                   (context-field tbs-context-field))
      state
    (let* ((null-arg-ex-ctr [[:System.ArgumentNullException] GetConstructor
                             (type-vector [:string])])
           (base-parameters (if base-callable-p
                              (nthcdr 3 (method-parameters base-constructor))
                              (method-parameters base-constructor)))
           (builder [type-builder DefineConstructor
                                  #e(System.Reflection.MethodAttributes Public)
                                  #e(System.Reflection.CallingConventions HasThis)
                                  (list-to-bike-vector
                                   (list* [:object]
                                          (field-type getter-field)
                                          (field-type setter-field)
                                          (mapcar #'parameter-type base-parameters))
                                   :element-type :type)])
           (param-attrs #e(System.Reflection.ParameterAttributes None)))
      [builder DefineParameter 1 param-attrs "context"]
      [builder DefineParameter 2 param-attrs "getter"]
      [builder DefineParameter 3 param-attrs "setter"]
      (loop :for i :from 4
            :for param :in base-parameters
            :do [builder DefineParameter i param-attrs (parameter-name param)])
      (with-il-generator ([builder GetILGenerator]
                          setter-not-null
                          getter-not-null)
        (emit Ldarg_0)
        (when base-callable-p
          (emit Ldarg_1)
          (emit Ldarg_2)
          (emit Ldarg_3))
        (loop :for i :from 4
              :for param :in base-parameters
              :do (if (< i 256)
                    (emit Ldarg_S i)
                    (emit Ldarg i)))
        (emit Call base-constructor)
        (unless base-callable-p
          (emit Ldarg_2)
          (emit Brtrue_S getter-not-null)
          (emit Ldstr "getter")
          (emit Newobj null-arg-ex-ctr)
          (emit Throw)
          (mark-label getter-not-null)
          (emit Ldarg_3)
          (emit Brtrue_S setter-not-null)
          (emit Ldstr "setter")
          (emit Newobj null-arg-ex-ctr)
          (emit Throw)
          (mark-label setter-not-null))
        (emit Ldarg_0)
        (emit Ldarg_1)
        (emit Stfld context-field)
        (emit Ldarg_0)
        (emit Ldarg_2)
        (emit Stfld getter-field)
        (emit Ldarg_0)
        (emit Ldarg_3)
        (emit Stfld setter-field)
        (emit Ret))
      builder)))

(defun tbs-create-type (state)
  (declare (type type-builder-state state))
  [(tbs-type-builder state) CreateType])

;;; vim: ft=lisp et