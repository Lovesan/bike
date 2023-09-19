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

(declaim (ftype (function (dotnet-object) dotnet-type) %bike-type-of))
(defknown %bike-type-of (System.Object :method GetType)
    "Retrieves .Net type of an object")

(defknown %get-hash-code (System.Object :method GetHashCode)
    "Retrieves hash code of an object")

(declaim (ftype (function (dotnet-object) string) %to-string))
(defknown %to-string (System.Object :method ToString)
    "Converts an object to a string")

(defknown %enum-to-object (System.Enum :method ToObject System.Type System.Int64)
    "Converts a enum to object")

(declaim (ftype (function (string boolean boolean) (or null dotnet-type)) %get-type))
(defknown %get-type (System.Type :method GetType System.String System.Boolean System.Boolean))

(defun get-type (name &optional (errorp t) error-value)
  (declare (type string-designator name))
  "Retrieves a type by NAME"
  (or (%get-type (string name) errorp t)
      error-value))

(defknown type-base-type (System.Type :property BaseType)
    "Retrieves base type for a type")

(declaim (ftype (function (dotnet-type) dotnet-type) element-type-of))
(defknown element-type-of (System.Type :method GetElementType)
    "Returns an element type for a TYPE")

(declaim (ftype (function (t t) boolean) bike-equals))
(defknown bike-equals (System.Object :method Equals System.Object System.Object)
    "Tests for .Net object equality")

(declaim (ftype (function (string) dotnet-object) %load-assembly))
(defknown %load-assembly (System.Reflection.Assembly :method Load System.String))

(defun load-assembly (assembly-string)
  (declare (type string-designator assembly-string))
  "Loads an assembly designated by assembly string"
  (%load-assembly (string assembly-string)))

(declaim (ftype (function (string) dotnet-object) %load-assembly-from))
(defknown %load-assembly-from (System.Reflection.Assembly :method LoadFrom System.String)
    "Loads an assembly from an assembly file")

(defun load-assembly-from (path)
  (declare (type (or pathname string) path))
  "Loads an assembly designated by assembly string"
  (%load-assembly-from (native-path (uiop:truenamize path))))

(declaim (ftype (function (dotnet-object) dotnet-object) %assembly-exported-types))
(defknown %assembly-exported-types (System.Reflection.Assembly :method GetExportedTypes))

(defun assembly-exported-types (assembly)
  (declare (type dotnet-object assembly))
  "Returns a list of types exported from ASSEMBLY"
  (bike-vector-to-list (%assembly-exported-types assembly)))

(declaim (ftype (function (dotnet-type) (or null string)) %type-full-name))
(defknown %type-full-name (System.Type :property FullName)
    "Retrieves full name of a type")

;; While we have already defined %get-type-full name,
;;   that function does not handle exceptions and would lead to
;;   a probable runtime crash should an object handle represent something other
;;   than a System.Type object.
(defun type-full-name (type)
  (declare (type dotnet-type type))
  "Retrieves full name of a type"
  (or (%type-full-name type)
      ;; FullName property may be null on some types
      (%to-string type)))

(defknown type-name (System.Type :property Name)
    "Retrieves name of a type")

(defknown type-primitive-p (System.Type :property IsPrimitive)
    "Determines whether a type is a primitive type")

(defknown type-assembly-qualified-name (System.Type :property AssemblyQualifiedName)
    "Retrieves assembly qualified name of a type")

(defknown generic-type-p (System.Type :property IsGenericType)
    "Tests whether a TYPE is a generic type")

(defknown generic-type-definition-p (System.Type :property IsGenericTypeDefinition)
    "Tests whether a TYPE is a generic type definition")

(defknown generic-type-definition-of (System.Type :method GetGenericTypeDefinition)
    "Returns a generic type definition for a generic TYPE")

(defknown type-generic-parameter-p (System.Type :property IsGenericParameter)
    "Determines whether type represents a generic parameter in generic
type or method definition.")

(declaim (ftype (function (dotnet-type) dotnet-object) %type-generic-arguments))
(defknown %type-generic-arguments (System.Type :method GetGenericArguments))

(defun type-generic-arguments (type)
  (declare (type dotnet-type type))
  "Returns generic type type arguments"
  (bike-vector-to-list (%type-generic-arguments type)))

(declaim (ftype (function (dotnet-object dotnet-type) (or null dotnet-object))
                member-custom-attrubute))
(defknown member-custom-attrubute
    (System.Attribute :method GetCustomAttribute System.Reflection.MemberInfo System.Type)
    "Retrieves custom attribute of a member info")

(defknown member-all-custom-attributes
    (System.Reflection.Memberinfo :method GetCustomAttributes System.Boolean)
    "Retrieves custom attributes for a MemberInfo")

(defun compiler-generated-member-p (info)
  (declare (type dotnet-object info))
  "Returns T in case of the MemberInfo designated by INFO
 is compiler-generated"
  (let ((attr-type (get-type 'System.Runtime.CompilerServices.CompilerGeneratedAttribute)))
    (not (null (member-custom-attrubute info attr-type)))))

(defknown array-type-rank (System.Type :method GetArrayRank)
    "Gets a number of dimensions of an array type")

(defknown array-type-sz-p (System.Type :property IsSZArray)
    "Returns T when an array type is a SZArray type")

(declaim (ftype (function (dotnet-object) boolean) dynamic-assembly-p))
(defknown dynamic-assembly-p (System.Reflection.Assembly :property IsDynamic)
    "Return non-NIL in case of assembly being dynamic")

(declaim (ftype (function (dotnet-type) dotnet-object) type-assembly))
(defknown type-assembly (System.Type :property Assembly)
    "Returns an assembly which type originated from")

(defun transient-type-p (type)
  (declare (type dotnet-type type))
  "Returns non-NIL in case of the TYPE is coming from a dynamic assembly"
  (dynamic-assembly-p (type-assembly type)))

(defknown pointer-type-p (System.Type :property IsPointer)
    "Returns non-NIL in case of the TYPE is a pointer type")

(defknown array-type-p (System.Type :property IsArray)
    "Returns non-NIL in case of the TYPE is an array type")

(defknown ref-type-p (System.Type :property IsByRef)
    "Returns non-NIL in case of the TYPE is an ref type")

(defknown enum-type-p (System.Type :property IsEnum)
    "Returns non-NIL in case of the TYPE is an enum type")

(defknown exception-message (System.Exception :property Message)
    "Returns exception message")

(defknown exception-stack-trace (System.Exception :property StackTrace)
    "Returns exception stack trace")

(defknown make-array-type (System.Type :method MakeArrayType)
    "Creates an array type from a TYPE")

(defknown make-array-type* (System.Type :method MakeArrayType System.Int32)
    "Creates an array type from a TYPE")

(defknown make-pointer-type (System.Type :method MakePointerType)
    "Creates a pointer type from a TYPE")

(defknown make-ref-type (System.Type :method MakeByRefType)
    "Creates a ref type from a TYPE")

(defknown make-generic-type (System.Type :method MakeGenericType "System.Type[]")
    "Creates a generic type from a type definition")

(declaim (ftype (function (dotnet-type dotnet-type) boolean) bike-subclass-p))
(defknown bike-subclass-p (System.Type :method IsSubclassOf System.Type)
    "Returns non-NIL in case of a type being a subclass of another type")

(defknown assignable-from-p (System.Type :method IsAssignableFrom System.Type)
    "Returns non-NIL in case of an instance of a type is assignable from a TYPE")

(defun delegate-type-p (type)
  (declare (type dotnet-type type))
  (let ((delegate-type (%get-type "System.Delegate" t nil)))
    (or (bike-equals type delegate-type)
        (bike-subclass-p type delegate-type))))

(declaim (ftype (function (dotnet-type) dotnet-object) get-enum-values))
(defknown get-enum-values (System.Type :method GetEnumValues)
    "Retrieves enum values of a type")

(declaim (ftype (function (dotnet-type t) string) get-enum-name))
(defknown get-enum-name (System.Type :method GetEnumName System.Object)
    "Retrieves a name for enum value")

(declaim (ftype (function (dotnet-type) dotnet-type) enum-underlying-type))
(defknown enum-underlying-type (System.Type :method GetEnumUnderlyingType)
    "Retrieves an underlying type for enum type")

(declaim (ftype (function (t dotnet-type) t) convert-type))
(defknown convert-type (System.Convert :method ChangeType System.Object System.Type)
    "Converts object type to another type")

(defun get-enum-hash-table (type)
  (declare (type dotnet-type type))
  "Retrieves a hash table for enum type"
  (let ((table (make-hash-table :test #'equal))
        (values (get-enum-values type))
        (underlying (enum-underlying-type type)))
    (do-bike-vector (val values)
      (let ((name (string-upcase (get-enum-name type val))))
        (setf (gethash name table)
              (convert-type val underlying))))
    table))

(defknown member-info-name (System.Reflection.MemberInfo :property Name)
    "Returns member name")

(defknown member-info-member-type (System.Reflection.MemberInfo :property MemberType)
    "Returns kind of a member that member info represents")

(declaim (ftype (function (dotnet-object) (or null dotnet-object))
                member-info-declaring-type))
(defknown member-info-declaring-type (System.Reflection.MemberInfo :property DeclaringType)
    "Returns declaring type of a member info")

(defknown method-public-p (System.Reflection.MethodBase :property IsPublic)
    "Returns whether method is public")

(defknown method-private-p (System.Reflection.MethodBase :property IsPrivate)
    "Returns whether method is private")

(defknown method-protected-p (System.Reflection.MethodBase :property IsFamily)
    "Returns whether method is protected")

(defknown method-internal-p (System.Reflection.MethodBase :property IsAssembly)
    "Returns whether method is internal")

(defknown method-protected-or-internal-p
    (System.Reflection.MethodBase :property IsFamilyOrAssembly)
    "Returns whether method is protected internal")

(declaim (ftype (function (dotnet-object) (or null dotnet-object))
                method-base-definition))
(defknown method-base-definition (System.Reflection.MethodInfo :method GetBaseDefinition)
    "Returns base definition of a method")

(defknown method-attributes (System.Reflection.MethodBase :property Attributes)
    "Retrieves method attributes")

(defknown method-virtual-p (System.Reflection.MethodBase :property IsVirtual)
    "Designates whether a method is virtual")

(defknown method-abstract-p (System.Reflection.MethodBase :property IsAbstract)
    "Designates whether a method is abstract")

(defknown method-sealed-p (System.Reflection.MethodBase :property IsFinal)
    "Designates whether a method is sealed")

(defun method-overriden-p (method-info)
  (declare (type dotnet-object method-info))
  (not (bike-equals (member-info-declaring-type (method-base-definition method-info))
                    (member-info-declaring-type method-info))))

(declaim (ftype (function (dotnet-object) boolean) method-static-p))
(defknown method-static-p (System.Reflection.MethodInfo :property IsStatic)
    "Designates whether a method is static")

(defknown field-static-p (System.Reflection.FieldInfo :property IsStatic)
    "Designates whether a field is a static one")

(defknown field-public-p (System.Reflection.FieldInfo :property IsPublic)
    "Designates whether a field is public")

(defknown field-private-p (System.Reflection.FieldInfo :property IsPrivate)
    "Designates whether a field is private")

(defknown field-protected-p (System.Reflection.FieldInfo :property IsFamily)
    "Designates whether a field is protected")

(defknown field-internal-p (System.Reflection.FieldInfo :property IsAssembly)
    "Designates whether a field is internal")

(defknown field-protected-or-internal-p
    (System.Reflection.FieldInfo :property IsFamilyOrAssembly)
    "Designates whether a field is protected internal")

(defknown field-readonly-p (System.Reflection.FieldInfo :property IsInitOnly)
    "Designates whether a field is a readonly one")

(defknown field-type (System.Reflection.FieldInfo :property FieldType)
    "Retrieves field type")

(defknown field-raw-constant-value
    (System.Reflection.FieldInfo :method GetRawConstantValue)
    "Gets constant field value")

(defknown field-attributes (System.Reflection.FieldInfo :property Attributes)
    "Retrieves field attributes")

(defknown field-const-p (System.Reflection.FieldInfo :property IsLiteral)
    "Whether field is a compile-time constant")

(defknown property-type (System.Reflection.PropertyInfo :property PropertyType)
    "Retrieves property type")

(declaim (ftype (function (dotnet-object) dotnet-object) %property-accessors))
(defknown %property-accessors (System.Reflection.PropertyInfo :method GetAccessors))

(defun property-static-p (info)
  (declare (type dotnet-object info))
  "Returns non-NIL if a property is static"
  (let ((accessors (%property-accessors info)))
    (do-bike-vector (mi accessors)
      (when (method-static-p mi)
        (return t)))))

(declaim (ftype (function (dotnet-object) dotnet-object) %property-index-parameters))
(defknown %property-index-parameters
    (System.Reflection.PropertyInfo :method GetIndexParameters))

(defknown property-attributes (System.Reflection.PropertyInfo :property Attributes)
    "Retrieves property attributes")

(defun property-indexer-p (property-info)
  (declare (type dotnet-object property-info))
  (plusp (%array-length (%property-index-parameters property-info))))

(declaim (ftype (function (dotnet-object) dotnet-object) %type-fields))
(defknown %type-fields (System.Type :method GetFields))

(defun type-fields (type)
  (declare (type dotnet-type type))
  "Retrieves fields from a TYPE"
  (bike-vector-to-list (%type-fields type)))

(defknown %get-type-fields
    (System.Type :method GetFields "System.Reflection.BindingFlags"))

(declaim (ftype (function (dotnet-object) dotnet-object) %type-properties))
(defknown %type-properties (System.Type :method GetProperties))

(defun type-properties (type)
  (declare (type dotnet-type type))
  "Retrieves properties from a TYPE"
  (bike-vector-to-list (%type-properties type)))

(defknown %get-type-properties
    (System.Type :method GetProperties "System.Reflection.BindingFlags"))

(declaim (ftype (function (dotnet-object) dotnet-object) %type-methods))
(defknown %type-methods (System.Type :method GetMethods))

(defun type-methods (type)
  (declare (type dotnet-type type))
  "Retrieves methods from a TYPE"
  (bike-vector-to-list (%type-methods type)))

(declaim (ftype (function (dotnet-object) dotnet-object) %type-events))
(defknown %type-events (System.Type :method GetEvents))

(defun type-events (type)
  (declare (type dotnet-type type))
  "Retrieves events from a TYPE"
  (bike-vector-to-list (%type-events type)))

(declaim (ftype (function (dotnet-object) dotnet-object) %method-parameters))
(defknown %method-parameters (System.Reflection.MethodBase :method GetParameters))

(defknown method-return-parameter (System.Reflection.MethodInfo :property ReturnParameter)
    "Retrieves return parameter")

(defknown parameter-name (System.Reflection.ParameterInfo :property Name)
    "Retrieves parameter name")

(defknown parameter-type (System.Reflection.ParameterInfo :property ParameterType)
    "Retrieves parameter type")

(declaim (ftype (function (dotnet-object) (signed-byte 32)) parameter-position))
(defknown parameter-position (System.Reflection.ParameterInfo :property Position)
    "Retrieves parameter position")

(defun method-parameters (info)
  (declare (type dotnet-object info))
  "Retrieves method parameters"
  (sort (bike-vector-to-list (%method-parameters info)) #'< :key #'parameter-position))

(defun property-index-parameters (info)
  (declare (type dotnet-object info))
  "Retrieves index parameters from property"
  (sort (bike-vector-to-list (%property-index-parameters info)) #'< :key #'parameter-position))

(defknown parameter-out-p (System.Reflection.ParameterInfo :property IsOut)
    "Returns non-NIL in case of parameter being out parameter")

(defknown parameter-in-p (System.Reflection.ParameterInfo :property IsIn)
    "Returns non-NIL in case of parameter being in parameter")

(defknown parameter-optional-p (System.Reflection.ParameterInfo :property IsOptional)
    "Returns non-NIL in case of parameter being optional")

(defknown parameter-default-value (System.Reflection.ParameterInfo :property DefaultValue)
    "Returns default value of a parameter")

(defknown parameter-default-value-p (System.Reflection.ParameterInfo :property HasDefaultValue)
    "Returns whether a parameter has default value")

(declaim (ftype (function (dotnet-object dotnet-type) (or null dotnet-object))
                parameter-custom-attrubute))
(defknown parameter-custom-attrubute
    (System.Attribute :method GetCustomAttribute System.Reflection.ParameterInfo System.Type)
    "Retrieves custom attribute of a parameter info")

(defun params-array-p (info)
  (declare (type dotnet-object info))
  "Returns non-NIL in case of INFO being a params[] parameter"
  (let ((attr-type (%get-type "System.ParamArrayAttribute" t nil)))
    (not (null (parameter-custom-attrubute info attr-type)))))

(defknown default-binder (System.Type :property DefaultBinder)
    "Returns a default reflection binder")

(defknown bind-to-method (System.Reflection.Binder :method BindToMethod
                                                   System.Reflection.BindingFlags
                                                   "System.Reflection.MethodBase[]"
                                                   "System.Object[]&"
                                                   "System.Reflection.ParameterModifier[]"
                                                   System.Globalization.CultureInfo
                                                   "System.String[]"
                                                   "System.Object&")
    "Binds to a method using supplied arguments")

(defknown type-get-members (System.Type :method GetMember
                                        System.String
                                        System.Reflection.MemberTypes
                                        System.Reflection.BindingFlags)
    "Gets a .Net array of members which satisfy name, member types and binding flags")

(defknown type-get-field (System.Type :method GetField
                                      System.String
                                      System.Reflection.BindingFlags)
    "Retrieves type field")

(defknown type-get-property (System.Type :method GetProperty
                                         System.String
                                         System.Reflection.BindingFlags
                                         System.Reflection.Binder
                                         System.Type
                                         "System.Type[]"
                                         "System.Reflection.ParameterModifier[]"))

(defknown empty-types (System.Type :field EmptyTypes)
    "Returns empty type array")

(defknown property-get-method (System.Reflection.PropertyInfo :property GetMethod)
    "Returns getter accessor for property")

(defknown property-set-method (System.Reflection.PropertyInfo :property SetMethod)
    "Returns setter accessor for property")

(defknown property-readable-p (System.Reflection.PropertyInfo :property CanRead)
    "Returns T when property has a getter")

(defknown property-writable-p (System.Reflection.PropertyInfo :property CanWrite)
    "Returns T when property has a setter")

(defknown method-return-type (System.Reflection.MethodInfo :property ReturnType)
    "Gets return type of a method")

(declaim (ftype (function (dotnet-type) dotnet-object) type-default-members))
(defknown type-default-members (System.Type :method GetDefaultMembers)
    "Gets members on which DefaultMemberAttribute is assigned")

(defun type-indexers (type)
  (declare (type dotnet-type type))
  "Returns a list of indexers on a type"
  (let ((members (type-default-members type))
        (property-info-type (%get-type "System.Reflection.PropertyInfo" t nil)))
    (loop :for i :below (%array-length members)
          :for info = (%net-vref members i)
          :when (bike-subclass-p (%bike-type-of info) property-info-type)
            :collect info)))

(defknown select-property (System.Reflection.Binder :method SelectProperty
                                                    System.Reflection.BindingFlags
                                                    "System.Reflection.PropertyInfo[]"
                                                    System.Type
                                                    "System.Type[]"
                                                    "System.Reflection.ParameterModifier[]"))

(defknown generic-method-definition-p
    (System.Reflection.MethodBase :property IsGenericMethodDefinition)
    "Returns non-NIL when a method is a generic method definition")

(defknown method-generic-arguments
    (System.Reflection.MethodBase :method GetGenericArguments)
    "Returns generic type arguments of a method")

(defknown make-generic-method
    (System.Reflection.MethodInfo :method MakeGenericMethod "System.Type[]")
    "Returns generic method instance")

(defknown type-constructors (System.Type :method GetConstructors)
    "Returns a .Net array of type constructors")

(defknown type-interfaces (System.Type :method GetInterfaces)
    "Returns a .Net array of interfaces implemented by a type")

(defknown type-namespace (System.Type :property Namespace)
    "Returns namespace of a type")

(defknown %to-disposable (BikeInterop.TypeCaster :method (Cast System.IDisposable)
                                                 System.Object)
    "Casts an object to IDisposable")

(defknown %dispose (System.IDisposable :method Dispose)
    "Disposes an object")

(declaim (ftype (function (dotnet-object) dotnet-object) %get-enumerator))
(defknown %get-enumerator (System.Collections.IEnumerable :method GetEnumerator)
    "Returns an enumerator for an IEnumerable")

(declaim (ftype (function (dotnet-object) t) %enumerator-current))
(defknown %enumerator-current (System.Collections.IEnumerator :property Current)
    "Returns current element of an IEnumerator")

(declaim (ftype (function (dotnet-object) boolean) %enumerator-move-next))
(defknown %enumerator-move-next (System.Collections.IEnumerator :method MoveNext)
    "Advances the IEnumerator")

(defknown gc-collect (System.GC :method Collect)
    "Invokes .Net garbage collector")

(defknown gc-wait-for-pending-finalizers (System.GC :method WaitForPendingFinalizers)
    "Waits for pending finalizers")

(defknown intptr->int64 (System.IntPtr :method ToInt64)
    "Converts IntPtr to 64-bit integer")

(defknown intptr->pointer (System.IntPtr :method ToPointer)
    "Converts IntPtr to pointer")

;;; vim: ft=lisp et
