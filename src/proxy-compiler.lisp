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

(defconstant +proxy-callback-opcode-invoke+ 0)
(defconstant +proxy-callback-opcode-get-property+ 1)
(defconstant +proxy-callback-opcode-set-property+ 2)

(defconstant +param-info-in+ #x00000)
(defconstant +param-info-out+ #x10000)
(defconstant +param-info-io+ #x20000)
(defconstant +param-info-direction-mask+ #x30000)
(defconstant +param-info-release-handle+ #x40000)

(defstruct (dynamic-type-cache (:constructor %make-dynamic-type-cache)
                               (:conc-name dtc-)
                               (:copier nil))
  (lock (make-rwlock) :type rwlock :read-only t)
  (assembly (required-slot) :type dotnet-object)
  (module (required-slot) :type dotnet-object)
  (type-count 0 :type (unsigned-byte 32))
  (classes '() :type list)
  (proxy-interface-type (required-slot) :type dotnet-type))

(defstruct (type-builder-state (:constructor %make-type-builder-state)
                               (:conc-name tbs-)
                               (:copier nil))
  (type-builder (required-slot) :type dotnet-object)
  (callback-field (required-slot) :type dotnet-object)
  (context-field (required-slot) :type dotnet-object)
  (lisp-object-field (required-slot) :type dotnet-object))

(defstruct (method-parameter-info
            (:copier nil)
            (:conc-name mpi-)
            (:constructor make-mpi (name
                                    dotnet-name
                                    type
                                    &key (position 0)
                                         (trampoline-position position)
                                         (direction :in)
                                         ((:ref ref-p) nil))))
  (name (required-slot) :type symbol :read-only t)
  (dotnet-name (required-slot)
   :type simple-character-string
   :read-only t)
  (type (required-slot))
  (position 0 :type fixnum :read-only t)
  (trampoline-position 0 :type fixnum)
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

(define-constant +callable-proxy-lisp-object-field-name+
  "_lispObject"
  :test #'equal)

(define-constant +callable-proxy-callback-field-name+
  "_callback"
  :test #'equal)

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
                (emit-s8 (opcode arg) `(%emit-s8 ,',generator-var (opcode ,opcode) ,arg))
                (emit-u8 (opcode arg) `(%emit-u8 ,',generator-var (opcode ,opcode) ,arg))
                (emit-calli (cconv return-type parameter-types)
                  `(%emit-calli ,',generator-var
                                (opcode Calli)
                                ,cconv
                                ,return-type
                                ,parameter-types)))
       (flet ((declare-local (type &optional pinned)
                (invoke ,generator-var 'DeclareLocal type pinned))
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

(defun get-invoke-method (type)
  (declare (type dotnet-type type))
  (type-get-method-by-name type "Invoke"
                           #e(System.Reflection.BindingFlags Public Instance)))

(defun get-cast-method (type)
  (make-generic-method
   (type-get-method-by-name [:BikeInterop.TypeCaster] "Cast"
                            #e(System.Reflection.BindingFlags Public Static))
   type))

(defun get-box-object-method ()
  (type-get-method-by-name [:BikeInterop.Externals] "BoxObject"
                           #e(System.Reflection.BindingFlags Public NonPublic Static)))

(defun get-unbox-object-method ()
  (type-get-method-by-name [:BikeInterop.Externals] "UnboxObject"
                           #e(System.Reflection.BindingFlags Public NonPublic Static)))

(defun get-full-type-code-method ()
  (type-get-method-by-name [:BikeInterop.TypeCodeExtensions] "GetFullTypeCode"
                           #e(System.Reflection.BindingFlags Public NonPublic Static)))

(defun get-string-pinnable-reference-method ()
  (type-get-method-by-name [:string] "GetPinnableReference"
                           #e(System.Reflection.BindingFlags Public Instance)))

(defun get-free-handle-method ()
  (type-get-method-by-name [:BikeInterop.Externals] "FreeHandle"
                           #e(System.Reflection.BindingFlags Public Static)))

(defun get-lisp-object-create-method ()
  (type-get-method-by-name [:BikeInterop.LispObject] "Create"
                           #e(System.Reflection.BindingFlags Public Static)))

(defun get-lisp-exception-constructor ()
  (type-get-constructor [:BikeInterop.LispException]
                        #e(System.Reflection.BindingFlags Public Instance)
                        (type-vector [:BikeInterop.LispObject])))

(defun get-target-invocation-exception-constructor ()
  (type-get-constructor [:System.Reflection.TargetInvocationException]
                        #e(System.Reflection.BindingFlags Public Instance)
                        (type-vector [:System.Exception])))

(defun make-dynamic-type-cache-assembly ()
  [:System.Reflection.Emit.AssemblyBuilder
   DefineDynamicAssembly
   (new 'System.Reflection.AssemblyName +dynamic-assembly-name+)
   #e(System.Reflection.Emit.AssemblyBuilderAccess RunAndCollect)])

(defun make-dynamic-type-cache-module (assembly)
  (declare (type dotnet-object assembly))
  [assembly DefineDynamicModule
            (or [assembly %FullName] +dynamic-assembly-name+)])

(defun make-proxy-interface-type (module)
  (declare (type dotnet-object module))
  [[module DefineType (strcat +dynamic-assembly-name+ ".IDotNetCallableProxy")
           #e(System.Reflection.TypeAttributes Public
                                               Abstract
                                               Interface
                                               AutoClass)]
   CreateType])

(defun make-dynamic-type-cache ()
  (let* ((asm (make-dynamic-type-cache-assembly))
         (module (make-dynamic-type-cache-module asm)))
    (%make-dynamic-type-cache
     :assembly asm
     :module module
     :proxy-interface-type (make-proxy-interface-type module))))

(defun %clear-dynamic-type-cache (cache)
  (declare (type dynamic-type-cache cache))
  (with-accessors ((lock dtc-lock)
                   (assembly dtc-assembly)
                   (module dtc-module)
                   (classes dtc-classes)
                   (type-count dtc-type-count)
                   (proxy-interface-type dtc-proxy-interface-type))
      cache
    (let* ((new-assembly (make-dynamic-type-cache-assembly))
           (new-module (make-dynamic-type-cache-module new-assembly))
           (new-proxy-interface-type (make-proxy-interface-type new-module)))
      (with-write-lock (lock)
        (setf assembly new-assembly
              module new-module
              proxy-interface-type new-proxy-interface-type
              classes classes
              type-count type-count))
      (values))))

(defun tbs-set-custom-attribute (member-builder attribute-type &rest args)
  (declare (type dotnet-object member-builder)
           (type dotnet-type attribute-type))
  (let* ((arg-types (mapcar #'bike-type-of args))
         (constructor (select-constructor attribute-type args arg-types))
         (ca-builder (new 'System.Reflection.Emit.CustomAttributeBuilder
                          constructor
                          (list-to-bike-vector args))))
    [member-builder SetCustomAttribute ca-builder]
    ca-builder))

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
                     (list-to-bike-vector interfaces :element-type :type)]))
      (tbs-set-custom-attribute type-builder
                                [:System.Runtime.CompilerServices.CompilerGeneratedAttribute])
      (%make-type-builder-state
       :type-builder type-builder
       :callback-field [type-builder DefineField
                                     +callable-proxy-callback-field-name+
                                     [:System.IntPtr]
                                     #e(System.Reflection.FieldAttributes Private)]
       :context-field [type-builder DefineField
                                    +callable-proxy-context-field-name+
                                    [:System.IntPtr]
                                    #e(System.Reflection.FieldAttributes Private)]
       :lisp-object-field [type-builder DefineField
                                        +callable-proxy-lisp-object-field-name+
                                        [:BikeInterop.LispObject]
                                        #e(System.Reflection.FieldAttributes Private)]))))

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

(defun tbs-define-method-signature
    (method-builder return-type generic-parameters parameters
     &optional params-array-parameter string-as-lpwstr-p)
  (declare (type dotnet-object method-builder)
           (type dotnet-type-designator return-type)
           (type list generic-parameters parameters)
           (type (or null method-parameter-info) params-array-parameter))
  (let ((gp-builders (when generic-parameters
                       (bike-vector-to-list
                        (apply #'invoke method-builder 'DefineGenericParameters
                               (mapcar #'gpi-dotnet-name generic-parameters)))))
        (parameter-types (make-list (length parameters))))
    (tbs-initialize-generic-parameters generic-parameters gp-builders)
    (setf return-type (tbs-resolve-type return-type generic-parameters))
    (loop :for p :in parameters
          :for type = (tbs-resolve-type (mpi-type p) generic-parameters)
          :for pos = (mpi-position p)
          :for refp = (mpi-ref-p p)
          :do (setf (mpi-type p) type
                    (elt parameter-types pos) (if refp
                                                (make-ref-type type)
                                                type)))
    (apply #'invoke method-builder 'SetParameters parameter-types)
    [method-builder SetReturnType return-type]
    (loop :for p :in parameters
          :for dir = (mpi-direction p)
          :for pos = (mpi-position p)
          :for name = (mpi-dotnet-name p)
          :for type = (mpi-type p)
          :for refp = (mpi-ref-p p)
          :for attrs = (enum 'System.Reflection.ParameterAttributes
                             (if (and refp (eq dir :in)) 'In 0)
                             (if (eq dir :out) 'Out 0))
          :for pb = [method-builder DefineParameter (1+ pos) attrs name]
          :when (eq p params-array-parameter) :do
            (tbs-set-custom-attribute
             pb
             [:System.ParamArrayAttribute])
          :when (and string-as-lpwstr-p (bike-equals type [:string]) (not refp)) :do
            (tbs-set-custom-attribute
             pb
             [:System.Runtime.InteropServices.MarshalAsAttribute]
             #e(System.Runtime.InteropServices.UnmanagedType LPWStr)))
    (values return-type parameter-types)))

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

(defun tbs-gen-load-parameter (gen param &optional staticp)
  (declare (type dotnet-object gen)
           (type method-parameter-info param))
  (with-il-generator (gen gen)
    (let ((pos (mpi-position param)))
      (emit-s4 Ldarg (if staticp pos (1+ pos))))))

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

(defun tbs-gen-load-field (gen field-info &optional staticp)
  (declare (type dotnet-object gen field-info))
  (with-il-generator (gen gen)
    (unless staticp
      (emit Ldarg_0))
    (emit Ldfld field-info)))

(defun tbs-gen-count-argc (gen parameters params-array-parameter)
  (declare (type dotnet-object gen)
           (type list parameters)
           (type (or null method-parameter-info) params-array-parameter))
  (with-il-generator (gen gen)
    (let* ((argc (length parameters))
           (argc/plain (if params-array-parameter (1- argc) argc))
           (argc-local (declare-local [:int])))
      ;; argc = argc/plain
      (emit-s4 Ldc_I4 argc/plain)
      (emit Stloc argc-local)
      (when params-array-parameter
        ;; argc += paramsArray.Length
        (tbs-gen-load-parameter gen params-array-parameter)
        (emit Ldlen)
        (emit Ldloc argc-local)
        (emit Add)
        (emit Stloc argc-local))
      (values argc-local argc/plain))))

(defun tbs-gen-stackalloc-args (gen parameters params-array-parameter)
  (declare (type dotnet-object gen)
           (type list parameters)
           (type (or null method-parameter-info) params-array-parameter))
  (with-il-generator (gen gen)
    (multiple-value-bind (argc-local argc/plain)
        (tbs-gen-count-argc gen parameters params-array-parameter)
      (let* ((argptr-local (declare-local [:(* System.IntPtr)]))
             (tcptr-local (declare-local [:(* :int)])))
        ;; IntPtr* argptr = stackalloc IntPtr[argc];
        ;; int* tcptr = stackalloc int[argc];
        (emit Ldloc argc-local)
        (emit Conv_U)
        (emit Sizeof [:System.IntPtr])
        (emit Mul_Ovf_Un)
        (emit Localloc)
        (emit Stloc argptr-local)
        (emit Ldloc argc-local)
        (emit Conv_U)
        (emit Sizeof [:int])
        (emit Mul_Ovf_Un)
        (emit Localloc)
        (emit Stloc tcptr-local)
        (values argc-local argc/plain argptr-local tcptr-local)))))

(defun tbs-gen-box-params-array (gen params-array-parameter
                                 argc/plain
                                 argptr-local
                                 tcptr-local
                                 tmpobj-local)
  (declare (type dotnet-object gen argptr-local tcptr-local tmpobj-local)
           (type (unsigned-byte 32) argc/plain)
           (type method-parameter-info params-array-parameter))
  ;;  `params' array elements must always end up at the
  ;;     end of the trampoline argument/info memory blocks
  (with-il-generator (gen gen loop-label end-label)
    (let* ((element-type (element-type-of (mpi-type params-array-parameter)))
           (len-local (declare-local [:int]))
           (tmpi-local (declare-local [:int])))
      ;; int len, i;
      ;; len = paramsArray.Length
      (tbs-gen-load-parameter gen params-array-parameter)
      (emit Ldlen)
      (emit Conv_I4)
      (emit Stloc len-local)
      ;; i = 0
      (emit Ldc_I4_0)
      (emit Stloc tmpi-local)
      ;; while (i < len)
      ;; {
      (mark-label loop-label)
      (emit Ldloc tmpi-local)
      (emit Ldloc len-local)
      (emit Bge end-label)
      ;; tmpobj = (object)paramsArray[i];
      (tbs-gen-load-parameter gen params-array-parameter)
      (emit Ldloc tmpi-local)
      (emit Ldelem element-type)
      (emit Box element-type)
      (emit Stloc tmpobj-local)
      ;; argptr[(i + argc/plain)*sizeof(IntPtr)] = BoxObject(tmpobj);
      (emit Ldloc tmpi-local)
      (emit-s4 Ldc_I4 argc/plain)
      (emit Add)
      (emit Sizeof [:System.IntPtr])
      (emit Mul)
      (emit Ldloc argptr-local)
      (emit Add)
      (emit Ldloc tmpobj-local)
      (emit Call (get-box-object-method))
      (emit Stind_I)
      ;; tcptr[(i + argc/plain)*sizeof(int)] = tmpobj.GetFullTypeCode() | ParamDirection.In;
      (emit Ldloc tmpi-local)
      (emit-s4 Ldc_I4 argc/plain)
      (emit Add)
      (emit Sizeof [:int])
      (emit Mul)
      (emit Ldloc tcptr-local)
      (emit Add)
      (emit Ldloc tmpobj-local)
      (emit Call (get-full-type-code-method))
      (emit-s4 Ldc_I4 +param-info-in+)
      (emit Or)
      (emit Stind_I4)
      ;; i += 1;
      (emit Ldloc tmpi-local)
      (emit Ldc_I4_1)
      (emit Add)
      (emit Stloc tmpi-local)
      (emit Br loop-label)
      ;; }
      (mark-label end-label))))

(defun tbs-gen-box-args (gen parameters params-array-parameter)
  (declare (type dotnet-object gen)
           (type list parameters)
           (type (or list method-parameter-info) params-array-parameter))
  (with-il-generator (gen gen)
    (multiple-value-bind (argc-local argc/plain argptr-local tcptr-local)
        (tbs-gen-stackalloc-args gen parameters params-array-parameter)
      (let ((box-method (get-box-object-method))
            (type-code-method (get-full-type-code-method))
            (tmpobj-local (declare-local [:object])))
        ;; object tmpobj;
        (loop :for p :in parameters
              :for tpos = (mpi-trampoline-position p)
              :for dir = (mpi-direction p)
              :unless (eq p params-array-parameter) :do
                (if (eq dir :out)
                  (progn
                    ;; tcptr[tpos*sizeof(int)] = ParamDirection.Out
                    (emit Ldloc tcptr-local)
                    (emit-s4 Ldc_I4 (* tpos (foreign-type-size :int32)))
                    (emit Add)
                    (emit-s4 Ldc_I4 +param-info-out+)
                    (emit Stind_I4))
                  (progn
                    ;; tmpobj = (obj)argN;
                    (tbs-gen-box-parameter gen p)
                    (emit Stloc tmpobj-local)
                    ;; argptr[tpos*sizeof(IntPtr)] = BoxObject(tmpobj);
                    (emit Ldloc argptr-local)
                    (emit-s4 Ldc_I4 (* tpos +pointer-size+))
                    (emit Add)
                    (emit Ldloc tmpobj-local)
                    (emit Call box-method)
                    (emit Stind_I)
                    ;; tcptr[tpos*sizeof(int)] = tmpobj.GetFullTypeCode() | paramDir;
                    (emit Ldloc tcptr-local)
                    (emit-s4 Ldc_I4 (* tpos (foreign-type-size :int32)))
                    (emit Add)
                    (emit Ldloc tmpobj-local)
                    (emit Call type-code-method)
                    (emit-s4 Ldc_I4 (ecase dir
                                      (:in +param-info-in+)
                                      (:io +param-info-io+)))
                    (emit Or)
                    (emit Stind_I4))))
        (when params-array-parameter
          (tbs-gen-box-params-array gen params-array-parameter argc/plain
                                    argptr-local tcptr-local tmpobj-local))
        (values argc-local argptr-local tcptr-local tmpobj-local)))))

(defun tbs-gen-load-opcode (gen operation)
  (declare (type dotnet-object gen)
           (type (member :invoke :get-property :set-property) operation))
  (with-il-generator (gen gen)
    (let ((code (ecase operation
                  (:invoke +proxy-callback-opcode-invoke+)
                  (:get-property +proxy-callback-opcode-get-property+)
                  (:set-property +proxy-callback-opcode-set-property+))))
      (emit-s4 Ldc_I4 code))))

(defun tbs-gen-load-const-string-addr
    (gen string &optional (string-local [gen DeclareLocal [:string] t]))
  (declare (type dotnet-object gen)
           (type simple-character-string string)
           (type dotnet-object string-local))
  (with-il-generator (gen gen)
    (emit Ldstr string)
    (emit Stloc string-local)
    (emit Ldloc string-local)
    (emit Call (get-string-pinnable-reference-method))))

(defun tbs-gen-process-exception (gen exh-local is-dotnet-exception-local)
  (declare (type dotnet-object gen exh-local is-dotnet-exception-local))
  (with-il-generator (gen gen no-exception-label dotnet-exception-label)
    (emit Ldloc exh-local)
    (emit Brfalse no-exception-label)
    (emit Ldloc is-dotnet-exception-local)
    (emit Brtrue dotnet-exception-label)
    (emit Ldloc exh-local)
    (emit Call (get-lisp-object-create-method))
    (emit Newobj (get-lisp-exception-constructor))
    (emit Throw)
    (mark-label dotnet-exception-label)
    (emit Ldloc exh-local)
    (emit Call (get-unbox-object-method))
    (emit Castclass [:System.Exception])
    (emit Newobj (get-target-invocation-exception-constructor))
    (emit Throw)
    (mark-label no-exception-label)))

(defun tbs-gen-unbox-out-parameters (gen parameters params-array-parameter
                                     argptr-local tcptr-local tmpobj-local)
  (declare (type dotnet-object gen argptr-local tcptr-local tmpobj-local)
           (type list parameters)
           (type (or null method-parameter-info) params-array-parameter))
  (with-il-generator (gen gen)
    (let ((tmph-local (declare-local [:System.IntPtr]))
          (unbox-method (get-unbox-object-method))
          (free-handle-method (get-free-handle-method)))
      (loop :for p :in parameters
            :for tpos = (mpi-trampoline-position p)
            :for dir = (mpi-direction p)
            :for type = (mpi-type p)
            :unless (or (eq dir :in) (eq p params-array-parameter)) :do
              ;; tmph = argptr[tpos*sizeof(IntPtr)]
              (emit Ldloc argptr-local)
              (emit-s4 Ldc_I4 (* tpos +pointer-size+))
              (emit Add)
              (emit Ldind_I)
              (emit Stloc tmph-local)
              ;; tmpobj = UnboxObject(tmph)
              (emit Ldloc tmph-local)
              (emit Call unbox-method)
              (emit Stloc tmpobj-local)
              (let ((no-release-label (define-label)))
                ;; if(tcptr[tpos*sizeof(int)] & 0x40000)
                ;; {
                (emit Ldloc tcptr-local)
                (emit-s4 Ldc_I4 (* tpos (foreign-type-size :int32)))
                (emit Add)
                (emit Ldind_I4)
                (emit-s4 Ldc_i4 +param-info-release-handle+)
                (emit And)
                (emit Brfalse no-release-label)
                ;;   FreeHandle(tmph)
                (emit Ldloc tmph-local)
                (emit Call free-handle-method)
                ;; }
                (mark-label no-release-label))
              ;; argN = (ArgType)tmpobj
              (tbs-gen-load-parameter gen p)
              (emit Ldloc tmpobj-local)
              (tbs-gen-unbox gen type tmpobj-local)
              (emit Stobj type)))))

#+(or ecl)
(progn
  (defun tbs-gen-ecl-import-current-thread (gen thread-imported-local)
    (declare (type dotnet-object gen thread-imported-local))
    (with-il-generator (gen gen)
      (emit-s4 Ldc_I4 1)
      (emit Conv_I)
      (emit-s4 Ldc_I4 1)
      (emit Conv_I)
      (emit-s8 Ldc_I8 (pointer-address
                       (foreign-symbol-pointer "ecl_import_current_thread")))
      (emit Conv_I)
      (emit-calli #e(System.Runtime.InteropServices.CallingConvention Cdecl)
                  [:System.Boolean]
                  (type-vector [:System.IntPtr]
                               [:System.IntPtr]))
      (emit Stloc thread-imported-local)))
  (defun tbs-gen-ecl-release-current-thread (gen thread-imported-local)
    (declare (type dotnet-object gen thread-imported-local))
    (with-il-generator (gen gen no-import-label)
      (emit Ldloc thread-imported-local)
      (emit Brfalse no-import-label)
      (emit-s8 Ldc_I8 (pointer-address
                       (foreign-symbol-pointer "ecl_release_current_thread")))
      (emit-calli #e(System.Runtime.InteropServices.CallingConvention Cdecl)
                  [:void]
                  (empty-types))
      (mark-label no-import-label))))

(defun tbs-add-method (state operation name return-type &key generic-parameters
                                                             parameters
                                                             params-array-parameter
                                                             special-name
                                                             (virtual t)
                                                             sealed
                                                             (callback-name name))
  (declare (type type-builder-state state)
           (type (member :invoke :get-property :set-property) operation)
           (type simple-character-string name callback-name)
           (type list generic-parameters parameters)
           (type (or null method-parameter-info) params-array-parameter))
  (with-accessors ((type-builder tbs-type-builder)
                   (callback-field tbs-callback-field)
                   (context-field tbs-context-field))
      state
    (let* ((builder [type-builder DefineMethod
                                  name
                                  (enum 'System.Reflection.MethodAttributes
                                        'Public
                                        'HideBySig
                                        (if sealed 'Final 0)
                                        (if virtual 'Virtual 0)
                                        (if virtual 'NewSlot 0)
                                        (if special-name 'SpecialName 0))])
           (return-type (tbs-define-method-signature builder
                                                     return-type
                                                     generic-parameters
                                                     parameters
                                                     params-array-parameter))
           (voidp (bike-equals return-type [:void])))
      (with-il-generator (gen [builder GetILGenerator]
                              dont-release-rvh-label)
        (multiple-value-bind (argc-local argptr-local tcptr-local tmpobj-local)
            (tbs-gen-box-args gen parameters params-array-parameter)
          (let* ((rvh-local (declare-local [:System.IntPtr]))
                 (release-rvh-local (declare-local [:bool]))
                 (exh-local (declare-local [:System.IntPtr]))
                 (is-dotnet-exception-local (declare-local [:bool]))
                 (unbox-method (get-unbox-object-method))
                 #+(or ecl) (thread-imported-local (declare-local [:bool])))
            ;; ECL must import foreign threads
            #+(or ecl)
            (tbs-gen-ecl-import-current-thread gen thread-imported-local)
            (emit Ldarg_0)
            (emit Call (get-box-object-method))
            (tbs-gen-load-field gen context-field)
            (tbs-gen-load-opcode gen operation)
            (emit-s4 Ldc_I4 (if voidp 1 0))
            (tbs-gen-load-const-string-addr gen callback-name)
            (emit Ldloc argptr-local)
            (emit Ldloc tcptr-local)
            (emit Ldloc argc-local)
            (emit Ldloca release-rvh-local)
            (emit Ldloca exh-local)
            (emit Ldloca is-dotnet-exception-local)
            (tbs-gen-load-field gen callback-field)
            (emit-calli #e(System.Runtime.InteropServices.CallingConvention Stdcall)
                        [:System.IntPtr]
                        (type-vector [:System.IntPtr] ; proxy
                                     [:System.IntPtr] ; context
                                     [:int]           ; opCode
                                     [:bool]          ; voidp
                                     [:System.IntPtr] ; name
                                     [:System.IntPtr] ; argsPtr
                                     [:System.IntPtr] ; infoPtr
                                     [:int]           ; nArgs
                                     [:System.IntPtr] ; oReleaseRvHandle
                                     [:System.IntPtr] ; oException
                                     [:System.IntPtr])) ; oIsDotNetException
            (emit Stloc rvh-local)
            ;; ECL must release foreign threads
            #+(or ecl)
            (tbs-gen-ecl-release-current-thread gen thread-imported-local)
            ;; should an exception occur, neither return value nor out/ref params
            ;;  should be initialized
            (tbs-gen-process-exception gen exh-local is-dotnet-exception-local)
            (emit Ldloc rvh-local)
            (emit Call unbox-method)
            (if voidp
              (emit Pop)
              (emit Stloc tmpobj-local))
            (emit Ldloc release-rvh-local)
            (emit Brfalse dont-release-rvh-label)
            (emit Ldloc rvh-local)
            (emit Call (get-free-handle-method))
            (mark-label dont-release-rvh-label)
            (tbs-gen-unbox-out-parameters gen parameters params-array-parameter
                                          argptr-local tcptr-local tmpobj-local)
            (unless voidp
              (emit Ldloc tmpobj-local)
              (tbs-gen-unbox gen return-type tmpobj-local))
            (emit Ret)
            builder))))))

(defun tbs-fixup-setter-parameter-positions (value-param parameters)
  (declare (type list parameters))
  ;; .NET expects the `value' argument to be the last in the list.
  ;; However, it would mess up with the `params' array and the boxing/unboxing process,
  ;;   and moreover, most lisp setter functions expect `new-value' argument
  ;;   to be the first one. (as in #'(setf gethash), #'(setf slot-value) etc.)
  (setf (mpi-trampoline-position value-param) 0)
  (dolist (p parameters)
    (unless (eq p value-param)
      (incf (mpi-trampoline-position p))))
  parameters)

(defun tbs-add-property (state name type &key getterp
                                              setterp
                                              parameters
                                              params-array-parameter)
  (declare (type type-builder-state state)
           (type simple-character-string name)
           (type dotnet-type type)
           (type list parameters params-array-parameter))
  (with-accessors ((type-builder tbs-type-builder))
      state
    (let* ((property-builder [type-builder DefineProperty
                                           name
                                           #e(System.Reflection.PropertyAttributes None)
                                           type
                                           (list-to-bike-vector
                                            (mapcar #'mpi-type parameters)
                                            :element-type :type)]))
      (when parameters
        (tbs-set-custom-attribute property-builder
                                  [:System.Runtime.CompilerServices.IndexerNameAttribute]
                                  name))
      (when getterp
        (let ((builder (tbs-add-method state :get-property
                                       (simple-character-string (strcat "get_" name))
                                       type
                                       :special-name t
                                       :parameters parameters
                                       :params-array-parameter params-array-parameter
                                       :callback-name name)))
          (property-builder-set-get-method property-builder builder)))
      (when setterp
        (let* ((value-param (make-mpi 'value "value" type
                                      :position (length parameters)))
               (parameters (cons value-param parameters)))
          (tbs-fixup-setter-parameter-positions value-param parameters)
          (let ((builder (tbs-add-method state :set-property
                                         (simple-character-string (strcat "set_" name))
                                         [:void]
                                         :special-name t
                                         :parameters parameters
                                         :params-array-parameter params-array-parameter
                                         :callback-name name)))
            (property-builder-set-set-method property-builder builder))))
      property-builder)))

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

(defun tbs-add-constructor (state base-constructor base-callable-p)
  (declare (type type-builder-state state)
           (type dotnet-object base-constructor))
  (with-accessors ((type-builder tbs-type-builder)
                   (context-field tbs-context-field)
                   (callback-field tbs-callback-field)
                   (lisp-object-field tbs-lisp-object-field))
      state
    (let* ((base-parameters (if base-callable-p
                              (nthcdr 2 (method-parameters base-constructor))
                              (method-parameters base-constructor)))
           (builder [type-builder DefineConstructor
                                  #e(System.Reflection.MethodAttributes Public)
                                  #e(System.Reflection.CallingConventions HasThis)
                                  (list-to-bike-vector
                                   (list* (field-type context-field)
                                          (field-type callback-field)
                                          (mapcar #'parameter-type base-parameters))
                                   :element-type :type)])
           (param-attrs #e(System.Reflection.ParameterAttributes None)))
      [builder DefineParameter 1 param-attrs "$context"]
      [builder DefineParameter 2 param-attrs "$callback"]
      (loop :for i :from 3
            :for param :in base-parameters
            :for base-param-attrs = (parameter-attributes param)
            :do [builder DefineParameter i base-param-attrs (parameter-name param)])
      (with-il-generator (gen [builder GetILGenerator])
        (emit Ldarg_0)
        (when base-callable-p
          (emit Ldarg_1)
          (emit Ldarg_2))
        (let ((i 3))
          (dolist (param base-parameters)
            (declare (ignore param))
            (if (< i 256)
              (emit-u1 Ldarg_S i)
              (emit-u2 Ldarg i))
            (incf i)))
        (emit Call base-constructor)
        (emit Ldarg_0)
        (emit Ldarg_1)
        (emit Stfld context-field)
        (emit Ldarg_0)
        (emit Ldarg_2)
        (emit Stfld callback-field)
        (emit Ldarg_0)
        (emit Ldarg_1)
        (emit Call (get-lisp-object-create-method))
        (emit Stfld lisp-object-field)
        (emit Ret))
      builder)))

(defun tbs-create-type (state)
  (declare (type type-builder-state state))
  [(tbs-type-builder state) CreateType])

;;; vim: ft=lisp et
