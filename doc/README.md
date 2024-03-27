# bike - A .NET interface for Common Lisp

* [Basic Concepts](#basic-concepts)
* [Basic API](#basic-api)
* [Arrays and Enumerable](#arrays-and-enumerable)
* [Type Resolution](#type-resolution)
* [Reflection API](#reflection-api)
* [Exception Handling](#exception-handling)
* [Namespace Management](#namespace-management)
* [Type Aliases](#type-aliases)
* [Assembly and Type Cache Management](#assembly-and-type-cache-management)
* [Direct Memory Access](#direct-memory-access)
* [Printer Facility](#printer-facility)
* [Apropos Facility](#apropos-facility)
* [.NET Callable Classes](#callable-classes)
* [Custom Syntax](#custom-syntax)
* [Conditions](#conditions)
* [Limitations](#limitations)

## Basic Concepts

### .NET objects

A dotnet object is a concept used by the .NET runtime. In .NET, not unlike in Lisp, everything is an object, everything has a class, etc.

.NET distinguishes between two categories of objects. The first one includes objects of reference types. Like `System.String` instances.
The second category includes objects of value types. Like `System.Int32` instances.

In .NET, objects of value types are passed around by value (i.e. by copying), and so do not hold referential identity.

Since Lisp does not have a concept of such partitioning, and most lisp objects, including defstruct instances, are passed by copying
a reference to the actual data, the library does also treat most objects as objects of reference types. And performs boxing of value type instances.

To be able to be usable from Lisp, a boxed .NET object is then converted to a GCHandle, and then to an IntPtr value, which
is then put into an instance of `DOTNET-OBJECT` structure class(or its subclasses) on the Lisp side.

There are some exceptions to this rule, however. The most primitive object types are passed by value without boxing, whenever possible.

These types include:

* `System.Char`
* `System.Boolean`
* `System.Byte`
* `System.SByte`
* `System.Int16`
* `System.UInt16`
* `System.Int32`
* `System.UInt32`
* `System.Int64`
* `System.UInt64`
* `System.Single`
* `System.Double`
* `System.IntPtr`
* `System.UIntPtr`

Note that large integers and floats could still be boxed(in the generic sense, not in the sense of the `DOTNET-OBJECT` structure)
 on the Lisp side by the Lisp implementation, that depends on the implementation and the processor architecture

Given all this, the library is unable, and probably will never be able, to handle `ref struct`s.
Should an object of such type be spotted by the library, the interop layer throws a .NET exception of type `BikeInterop.RefStructParameterException`.

### Type specifier

One of the most important concepts used by the library is the type specifier or typespec for short.

A type specifier is a form with a specific syntax. A type specifier argument can either be evaluated or not, that depends on the context.

The most basic type specifier is a string object, which designates a type by its name. For example:
````lisp
"System.DateTime"
````
Any `string designator` can be used instead of a string. That means symbols and characters could also be used as type specifiers.

Type specifiers are case-insensitive.
````lisp
System.DateTime ; designates the same type as the above string, regardless of the (readtable-case *readtable*) value
````

String designators can actually include anything accepted by `Type.GetType` method from .NET.
Those complex string type definitions are parsed into Lisp type specifiers in most cases, however.

String designators can also point to type aliases, as defined by `USE-TYPE-ALIAS` function.
Some aliases are pre-defined for several primitive .NET types, for ex. `:int` actually points to `System.Int32`.

The second most simple type specifier is an object of type `DOTNET-TYPE`, which wraps a `System.Type` object from .NET itself.

There are also complex type specifiers.

#### Array typespec

An array type specifier is a list of form
````lisp
(ARRAY element-type &optional (rank 1))
````
The `ARRAY` name here is any string designator, which could be converted to the string `"ARRAY"`.

Element type can be any type specifier except for the ref typespec (see below).

Array rank can designate the number of array dimensions. It could be any integer between 1 and 32 inclusively.

Array rank can also be the `*` symbol. In this case, an MZ array type is assumed.
MZ arrays can have variable dimensions and variable lower bounds.

#### Pointer typespec

A pointer type specifier is a list of form
````lisp
(* inner-type)
````
It designates a pointer type from dotnet. Note that inner type cannot be a by-ref type.

#### Reference type

A reference type is a list of form
````lisp
(REF inner-type)
````

A by-ref value represents a managed strongly-typed pointer to some .NET object.
It is used for `out` and `ref`(i.e. in/out) parameters, for example.

Speaking of type specifiers, `REF` can only be a top-level typespec.
I.e. no inner complex types can include inner `REF` types.

#### Qualified type name

````lisp
(QUALIFIED full-name assembly-string)
````

Qualified type specifiers designates a specific type from a specific assembly.

An example:
````lisp
(:qualified
 "System.Int32"
 "System.Private.CoreLib, Version=8.0.0.0, Culture=neutral, PublicKeyToken=7cec85d7bea7798e")
````

#### Generic type specifier

````lisp
(generic-type-definition-name arg1-typespec &rest other-arg-type-specifiers)
````

A generic typespec designates an instantiated generic type. For example:

````lisp
(System.Collections.Generic.List :int)
````
equals to the following type designator from C#
````csharp
System.Collections.Generic.List<int>
````

#### Full syntax

````
typespec ::= simple-typespec | complex-typespec

simple-typespec ::= string-designator

complex-typespec ::= array-typespec | pointer-typespec | by-ref-typespec | qualified-typespec | generic-typespec

array-typespec ::= (ARRAY typespec [rank])

pointer-typespec ::= (* typespec)

by-ref-typespec ::= (REF typespec)

qualified-typespec ::= (QUALIFIED full-name assembly-string)

generic-typespec ::= (generic-type-definition-name typespec typespec*)
````

### Method designator

A method designator is either a `string designator` representing method name
or a list of form:

````lisp
(method-name generic-argument1 &rest generic-arguments)
````

Where `METHOD-NAME` is, again, a string designator, and each of the generic arguments
must be a type specifier that is used for the generic method instantiation.

Method designators are case-insensitive.

### Assembly designator

An assembly designator is either a string, which represents assembly name, or a .NET object
of type `System.Reflection.Assembly`.

Assembly designators are case-insensitive.

Actually, most of the things described below are case-insensitive.

## Basic API

### Function: **NEW**

Creates an instance of the specified `TYPE`.
In case of the `TYPE` being a delegate type, first,
 and only, argument, must be a lisp function-like
 object.

#### Syntax
````lisp
(new type &rest constructor-args) ; => an object
````

### Function: **INVOKE**

Invokes a method designated by `METHOD` on a `TARGET` which can
 either be a type specifier, in which case a static method is
 invoked, or an instance, which would lead to instance method
 invocation.

#### Syntax
````lisp
(invoke target method &rest args) ; => value
````

### Function: **PROPERTY**

Retrieves a value of property named `NAME` from a `TARGET`, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access.

#### Syntax
````lisp
(property target name) ; => property value
````

### Function: **(SETF PROPERTY)**

Changes a value of property named `NAME` of a `TARGET`, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access.

#### Syntax
````lisp
(setf (property target name) new-value) ; => new-value
````

### Function: **REF**

Retrieves a value of an indexer from a `TARGET`, which must be an instance.

#### Syntax
````lisp
(ref target index &rest indices) ; => indexer value
````

### Function: **(SETF REF)**

Changes a value of an indexer from a `TARGET`, which must be an instance.

#### Syntax
````lisp
(setf (ref target index &rest indices) new-value) ; => new-value
````

### Function: **FIELD**

Retrieves a value of field named `NAME` from `TARGET`, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access.

#### Syntax
````lisp
(field target name) ; => field value
````

### Function: **(SETF FIELD)**

Changes a value of field named `NAME` of a `TARGET`, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access.

#### Syntax
````lisp
(setf (field target name) new-value) ; => new-value
````

### Function: **EVENT-ADD**

Subscribes to an event named `NAME` on a `TARGET`, which
  can either be a type specifier, in which case a static event
  is accessed, or an instance, which would lead to an instance event
  subscription.

`HANDLER` can either be a .NET delegate or lisp function-like object, in
  which case a delegate is allocated for it behind the scenes.
  Note that in the latter case, you won't be able to unsubscribe from an event.

#### Syntax
````lisp
(event-add target name handler)
````

### Function: **ENUM**

Converts `LOGIOR`'ed `VALUE` and `VALUES` to enum object of specified `ENUM-TYPE`

#### Syntax
````lisp
(enum enum-type value &rest values)
````

### Function: **EVENT-REMOVE**

Unsubscribes from an event named `NAME` on a `TARGET`, which
  can either be a type specifier, in which case a static event
  is accessed, or an instance, which would lead to an instance event
  reference.

`HANDLER` must be a .NET delegate object.

#### Syntax
````lisp
(event-remove target name handler)
````

### Function: **BOX**

Makes a boxed representation of an `OBJECT`, optionally converting it into a `TYPE`.

#### Syntax
````lisp
(box object &optional type) ; => boxed object
````

### Function: **UNBOX**

Attempts to unbox an `OBJECT` into lisp object.

#### Syntax
````lisp
(unbox object) ; => unboxed object
````

### Function: **BIKE-EQUALS**

Tests for .Net object equality, i.e. it is a wrapper for `Object.Equals` method.

#### Syntax
````lisp
(bike-equals object1 object2) ; => T or NIL
````

### Function: **BIKE-TYPE-OF**

Retrieves .Net type of an `OBJECT`.

#### Syntax
````lisp
(bike-type-of object) ; => an instance of DOTNET-TYPE
````

### Function: **BIKE-TYPE-P**

Tests whether an `OBJECT`(or its boxed representation) belongs to a dotnet `TYPE`.

#### Syntax
````lisp
(bike-type-p object type) ; => T or NIL
````

### Function: **DOTNET-OBJECT-P**

Tests whether an `OBJECT` is a `DOTNET-OBJECT`, which is a wrapper structure for .Net objects.

#### Syntax
````lisp
(dotnet-object-p object) ; => T or NIL
````

### Function: **DOTNET-TYPE-P**

Tests whether an `OBJECT` is a `DOTNET-TYPE`, which is a wrapper structure for .Net types.

#### Syntax
````lisp
(dotnet-type-p object) ; => T or NIL
````

### Function: **DOTNET-DELEGATE-P**

Tests whether an `OBJECT` is a `DOTNET-DELEGATE`, which is a wrapper structure for .Net delegates.

#### Syntax
````lisp
(dotnet-delegate-p object) ; => T or NIL
````

### Function: **DOTNET-EXCEPTION-P**

Tests whether an `OBJECT` is a `DOTNET-EXCEPTION`, which is a wrapper structure for .Net exceptions.

#### Syntax
````lisp
(dotnet-exception-p object) ; => T or NIL
````

### Macro: **WITH-DISPOSABLE**

Binds `VAR` to an IDisposable `OBJECT` during the execution of `BODY` forms.
 `OBJECT` is disposed right before `BODY` forms exit.

#### Syntax
````lisp
(with-disposable (var object) &body body) ; => results of the BODY forms execution
````

### Macro: **WITH-DISPOSABLES**

Binds variables to IDisposable objects during the execution of `BODY` forms.
 Variables are bound by means of `LET` form.
 Objects are disposed right before `BODY` forms exit.

#### Syntax
````lisp
(with-disposables (&rest specs) &body body) ; => results of the BODY forms execution
````

### Macro: **WITH-DISPOSABLES\***

Binds variables to IDisposable objects during the execution of `BODY` forms.
 Variables are bound in a sequence, as in `LET*` form.
 Objects are disposed right before `BODY` forms exit.

#### Syntax
````lisp
(with-disposables* (&rest specs) &body body) ; => results of the BODY forms execution
````

## Arrays and Enumerable

### Function: **DNVREF**

Accesses a .Net `VECTOR` (an array of rank 1) at `INDEX`.

#### Syntax
````lisp
(dnvref vector index) ; => object at the specified index
````

### Function: **(SETF DNVREF)**

Accesses a .Net `VECTOR` (an array of rank 1) at `INDEX`.

#### Syntax
````lisp
(setf (dnvref vector index) new-value) ; => new-value
````

### Function: **DNAREF**

Accesses a .Net `ARRAY` at `INDICES`.

#### Syntax
````lisp
(dnaref array &rest indices) ; => object residing at indices
````

### Function: **(SETF DNAREF)**

Accesses a .Net `ARRAY` at `INDICES`.

#### Syntax
````lisp
(setf (dnaref array &rest indices) new-value) ; => new-value
````

### Function: **BIKE-VECTOR-TO-LIST**

Collects elements of a .Net `VECTOR` into a list, starting from `START` index and below `END` index.

#### Syntax
````lisp
(bike-vector-to-list vector &key (start 0) end) ; => a list
````

### Function: **LIST-TO-BIKE-VECTOR**

Converts a `LIST` to a one-dimensional .Net array with the specified `ELEMENT-TYPE`.
 `START` and `END` designate `LIST` bounds and the resulting vector size, correspondingly

#### Syntax
````lisp
(list-to-bike-vector list &key (start 0) end (element-type "System.Object")) ; => a .Net vector
````

### Macro: **DO-BIKE-VECTOR**

Evaluates `BODY` forms in a loop where `ELT-VAR` is subsequently bound to
 each element of a `VECTOR`, which should evaluate to .Net array of rank 1.
 Returns `RESULT` form.

#### Syntax
````lisp
(do-bike-vector (elt-var vector &optional result) &body body) ; => RESULT form value
````

### Macro: **DO-ENUMERABLE**

A direct analog of C# `foreach` statement.
 Binds `VAR` to each element of an `ENUMERABLE` and executes `BODY` forms on each iteration.
 Returns a result of an execution of `RESULT` form.

#### Syntax
````lisp
(do-enumerable (var enumerable &optional result) &body body) ; => results of BODY forms execution
````

## Type Resolution

### Function: **RESOLVE-TYPE**

Resolves a .Net type designated by `TYPE` specifier from an internal type cache.

`ASSEMBLY`, unless `NIL`, can either be an assembly object, or a string, designating an assembly.

#### Syntax
````lisp
(resolve-type type &key (errorp t) error-value assembly)
;; => a DOTNET-TYPE object or ERROR-VALUE value, depending on whether the type exists and whether ERRORP argument is NIL.
````

### Function: **GET-TYPE**

Directly asks .NET for a type metaobject by name.

#### Syntax
````lisp
(get-type name &optional (errorp t) error-value)
;; => a DOTNET-TYPE object or ERROR-VALUE value, depending on whether the type exists and whether ERROP argument is NIL.
````

### Function **BIKE-SUBCLASS-P**

Returns non-NIL in case of the type being a subclass of another type.
The function only accepts `DOTNET-TYPE` objects, and not type designators.

#### Syntax
````lisp
(bike-subclass-p type-object1 type-object2) ; => T or NIL
````

## Reflection API

Sometimes, as in the case of COM interop, or dynamic languages running on .NET, the usual invocation API,
which performs member resolution, runtime compilation and aggressive caching, may not be suitable for object invocation.

So, these are the functions which perform invocation using .NET reflection API.
The results and parameter processing of these functions may differ from the usual API.

### Function **REFLECTION-NEW**

Using reflection, creates an instance of the specified `TYPE`.
In case of the `TYPE` being a delegate type, first,
 and only, argument, must be a lisp function-like
 object.

#### Syntax
````lisp
(reflection-new type &rest constructor-args) ; => an instance of the TYPE
````

### Function **REFLECTION-INVOKE**

Using reflection, invokes a method designated by `METHOD` on a `TARGET` which can
 either be a type specifier, in which case a static method is
 invoked, or an instance, which would lead to instance method
 invocation.

#### Syntax
````lisp
(reflection-invoke target method &rest args) ; => result
````

### Function **REFLECTION-PROPERTY**

Using reflection, retrieves a value of property named `NAME` from a `TARGET`, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access.

#### Syntax
````lisp
(reflection-property target name) ; => property value
````

### Function **(SETF REFLECTION-PROPERTY)**

Using reflection, changes a value of property named `NAME` of a `TARGET`, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access.

#### Syntax
````lisp
(setf (reflection-property target name) new-value) ; => new-value
````

### Function **REFLECTION-REF**

Using reflection, retrieves a value of an indexer from a `TARGET`, which
 must be an instance.

#### Syntax
````lisp
(reflection-ref target index &rest indices) ; => indexer value
````

### Function **(SETF REFLECTION-REF)**

Using reflection, changes a value of an indexer from a `TARGET`, which
 must be an instance.

#### Syntax
````lisp
(setf (reflection-ref target index &rest indices) new-value) ; => new-value
````

### Function **REFLECTION-FIELD**

Using reflection, retrieves a value of field named `NAME` from `TARGET`, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access.

#### Syntax
````lisp
(reflection-field target name) ; => field value
````

### Function **(SETF REFLECTION-FIELD)**

Using reflection, changes a value of field named `NAME` of a `TARGET`, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access.

#### Syntax
````lisp
(setf (reflection-field target name) new-value) ; => new-value
````

## Exception Handling

Exceptions are marshaled on Lisp/.Net boundaries.

Lisp errors coming from callbacks are wrapped into `BikeInterop.LispException` objects.

.NET exceptions coming from callbacks are wrapped into `TargetInvocationException` by the interop layer.

.NET exceptions coming to Lisp are wrapped into `DOTNET-EXCEPTION` instances, which are then wrapped into `DOTNET-ERROR` condition objects.

These are the macros that are specifically designed to work with `DOTNET-EXCEPTION` objects directly.

These macros mimic Lisp condition system and project a similar system to .Net exceptions.

Note that because of exception marshaling, stack unwinding is always performed, and it is performed up to the closest .Net/Lisp boundary.

### Macro: **EXCEPTION-BIND**

Executes `BODY` forms in a dynamic context where the given handler bindings are in
effect. Each handler must take the exception being signalled as an argument.
The bindings are searched first to last in the event of a thrown exception

#### Syntax:
````lisp
(exception-bind (&rest (exception-type handler)) &body body) ; => Unless NLX occurs, the results of BODY forms execution
````

### Macro: **EXCEPTION-CASE**

Executes `FORM` in a context with handlers established for the exception types. A
peculiar property allows type to be `:NO-EXCEPTION`. If such a clause occurs, and
form returns normally, all its values are passed to this clause as if by
`MULTIPLE-VALUE-CALL`. The `:NO-EXCEPTION` clause accepts more than one var
specification.

#### Syntax:
````lisp
(exception-case form &rest (exception-type (&optional var) &body handler-body)) ; => Unless NLX occurs, the results of FORM execution
````

### Function: **DOTNET-ERROR**

Constructs an exception object of the specified .Net `TYPE`
  using constructor `ARGS` and signals a `DOTNET-ERROR` (using `ERROR` function) which holds this exception object.

#### Syntax:
````lisp
(dotnet-error type &rest args) ; => non-local exit
````

## Namespace management

.NET does not actually have a first-class concept of a namespace. Type namespace is simply a prefix in its name.

Nevertheless, the library provides some related functionality for convenience.

For example, the library allows you to "use" a namespace so that you won't need to type the full name of a type that belongs to that namespace.

Note that, however, unlike with CL packages and readtables, the list of used namespaces is global, and is not tied to files and compilation units.

### Function: **USE-NAMESPACE**

Adds a namespace (or a list of namespaces) to the list of used namespaces.

Each namespace must be a `string designator`.

### Syntax:
````lisp
(use-namespace namespace-or-namespaces)
````

### Function: **UNUSE-NAMESPACE**

Removes a namespace (or a list of namespaces) from the list of used namespaces

Each namespace must be a `string designator`.

### Syntax:
````lisp
(unuse-namespace namespace-or-namespaces)
````

### Function: **UNUSE-ALL-NAMESPACES**

Clears the list of used namespaces.

### Syntax:
````lisp
(unuse-all-namespaces)
````

### Function: **NAMESPACE-USED-P**

Tests whether a `NAMESPACE` is already in the list of used namespaces.

The `NAMESPACE` must be a `string designator`.

### Syntax:
````lisp
(namespace-used-p namespace) ; => generalized boolean
````

### Function: **GET-USED-NAMESPACES**

Returns a list of used namespaces.

### Syntax:
````lisp
(get-used-namespaces) ; => a list of strings
````

### Function: **NORMALIZED-TYPE-NAME**

Returns a convenient string representation of a type name stripped of used namespaces.

The function only accepts `DOTNET-TYPE` objects, not the type designators.

### Syntax:
````lisp
(normalized-type-name type-object) ; => a string
````

## Type Aliases

A type alias is simply a `string designator` which refers to another type.

The library allows you to define custom type aliases, to save the typing time, for example.

The library itself also defines some type aliases which mimic C# ones:

* `:object` -> `System.Object`
* `:string` -> `System.String`
* `:char` -> `System.Char`
* `:bool` -> `System.Boolean`
* `:float` -> `System.Single`
* `:double` -> `System.Double`
* `:byte` -> `System.Byte`
* `:sbyte` -> `System.SByte`
* `:short` -> `System.Int16`
* `:ushort` -> `System.UInt16`
* `:int` -> `System.Int32`
* `:uint` -> `System.UInt32`
* `:long` -> `System.Int64`
* `:ulong` -> `System.UInt64`
* `:decimal` -> `System.Decimal`
* `:type` -> `System.Type`
* `:void` -> `System.Void`

### Function: **USE-TYPE-ALIAS**

Adds an `ALIAS` for the `TYPE` in the type cache. `TYPE` designator must
 not contain actual type objects if the current lisp image is to be restored.

### Syntax:
````lisp
(use-type-alias alias type)
````

### Function: **UNUSE-TYPE-ALIAS**

Removes an `ALIAS` from the current type cache.

### Syntax:
````lisp
(use-type-alias alias)
````

### Function: **UNUSE-ALL-TYPE-ALIASES**

Remove all type aliases from the type cache.

### Syntax:
````lisp
(unuse-all-type-aliases)
````

## Assembly and Type Cache Management

All the types the library is working with must be available through the internal type cache.

`RESOLVE-TYPE` function, which analyzes a type designator into a type object, and is used everywhere throughout the library,
won't be able to work correctly unless the cache is properly populated.

The library attempts to restore the cache on image restore, therefore, the image restore must happen within
at least the same assembly environment as it has been dumped(i.e. all the assemblies loaded, must be present on image restore).
The only exceptions are the dynamic types from dynamic assemblies, which are discarded by the type cache.

The library currently uses default assembly load context, and so does not allow for assembly unloading(you can experiment with additional load contexts, however).

### Special Variable: **\*DEFAULT-ASSEMBLIES\***

A list of additional assembly names that are loaded by default, including on lisp image restore.

#### Default Value: \'()

### Function: **GET-LOADED-ASSEMBLIES**

Returns a list of currently loaded assemblies.

#### Syntax:
````lisp
(get-loaded-assemblies) ; => a list of assembly objects
````

### Function: **LOAD-ASSEMBLY**

Loads an assembly designated by `ASSEMBLY-STRING` which designates an assembly name.

N.B.: The function does not import the assembly into the type cache. You should do that manually.

#### Syntax:
````lisp
(load-assembly assembly-string) ; => assembly
````

### Function: **LOAD-ASSEMBLY-FROM**

Loads an assembly by `ASSEMBLY-PATH` which designates a filesystem path to the assembly file.

N.B.: The function does not import the assembly into the type cache. You should do that manually.

#### Syntax:
````lisp
(load-assembly-from assembly-path) ; => assembly
````

### Function: **IMPORT-ASSEMBLY**

Imports an assembly designated by `ASSEMBLY-DESIGNATOR` into the type cache.

#### Syntax:
````lisp
(import-assembly assembly-designator) ; => assembly
````

### Function: **IMPORT-ASSEMBLY-FROM**

Loads an assembly from the filesystem by `ASSEMBLY-PATH` and imports it into the type cache.

#### Syntax:
````lisp
(import-assembly-from assembly-path) ; => assembly
````

### Function: **IMPORT-LOADED-ASSEMBLIES**

Imports all currently loaded assemblies into the type cache.

Useful when loading big frameworks with lots of dependencies.

#### Syntax:
````lisp
(import-loaded-assemblies)
````

### Function: **CLEAR-TYPE-CACHE**

Clears the type cache and restores it to current defaults.

Re-imports all the default and currently loaded assemblies.

Resets type aliases and clears the list of used namespaces.

#### Syntax:
````lisp
(clear-type-cache)
````

## Direct Memory Access

The library provides you the ability to directly access the memory of several .NET object types, namely arrays and strings amongst others.

This is achieved by utilization of pinned GCHandles.

### Macro **WITH-FIXED**

A direct analog of C# `fixed` statement.

Executes `BODY` forms in a dynamic environment where `POINTER-VAR` is bound to the pinned data of an `OBJECT`.

#### Syntax:
````lisp
(with-fixed (pointer-var object) &body body) ; => results of the BODY forms execution
````

### Macro **WITH-FIXEDS**

Executes `BODY` forms in a dynamic environment where
 `BINDINGS` are bound to the pinned data of corresponding objects.

`BINDINGS` are processed as in `LET` operator.

#### Syntax:
````lisp
(with-fixeds (&rest bindings) &body body) ; => results of the BODY forms execution
````

### Macro **WITH-FIXEDS\***

Executes `BODY` forms in a dynamic environment where `BINDINGS` are bound to the pinned data of corresponding objects.

`BINDINGS` are processed as in `LET*` operator.

#### Syntax:
````lisp
(with-fixeds* (&rest bindings) &body body) ; => results of the BODY forms execution
````

## Printer Facility

The library provides an extensible printer facility, based on the Lisp pretty printer.

It allows you to redefine text representation of .NET objects.

It does take .Net type hierarchy into account, and also allows you to define printer methods on interfaces,
which are used in case none is defined for the specific class hierarchy.

### Special Variable: **\*PRINT-DOTNET-OBJECT\***

Non-NIL value forces printing of .Net object contents.

#### Default Value: T

### Special Variable: **\*PRINT-ENUMERABLE\***

Non-NIL value forces printing of IEnumerable contents.

#### Default Value: T

### Special Variable: **\*PRINT-DOTNET-TYPE-NAMESPACES\***

Value of NIL forces omitting printing namespaces while printing type names.

Value of :NORMALIZE omits printing namespaces whenever these are currently used by means of `USE-NAMESPACE` function.

Other values force the printing of type namespaces.

#### Default Value: :NORMALIZE

### Special Variable: **\*PRINT-DOTNET-TYPE-PARAMETERS\***

Non-NIL value forces printing of generic type parameters.

#### Default Value: T

### Special Variable: **\*PRINT-DOTNET-TYPE-QUALIFIED\***

Non-NIL value forces printing of assembly qualified type names whenever possible.

#### Default Value: NIL

### Special Variable: **\*PRINT-DOTNET-TYPE-POINTER\***

Non-NIL value forces printing of pointer type name suffix (i.e. `*`).

#### Default Value: T

### Special Variable: **\*PRINT-DOTNET-TYPE-REF\***

Non-NIL value forces printing of ByRef type name suffix (i.e. `&`).

#### Default Value: T

### Function: **WRITE-TYPE-NAME**

Outputs name of a `TYPE` to the specified `STREAM`, defaulting to `*STANDARD-OUTPUT*`

`:NAMESPACES` - whether to print type namespaces. `NIL` - do not print namespaces. `:NORMALIZE` - omit used namespaces. Other value - print all namespaces.

`:PARAMETERS` - whether to print generic type parameters

`:QUALIFIED` - whether to print assembly-qualified type names

`:POINTER` - whether to print `*` suffix for pointer types

`:REF` - whether to print `&` suffix for ByRef types

`:STREAM` - stream to output to

#### Syntax:
````lisp
(write-type-name type &key (namespaces *print-dotnet-type-namespaces*)
                           (parameters *print-dotnet-type-parameters*)
                           (qualified *print-dotnet-type-qualified*)
                           (pointer *print-dotnet-type-pointer*)
                           (ref *print-dotnet-type-ref*)
                           (stream *standard-output*))
;; => TYPE
````

### Macro: **PPRINT-DOTNET-OBJECT**

`PRINT-UNREADABLE-OBJECT` analog for .Net objects

Output `OBJECT` to `STREAM` with `#<` prefix, `>` suffix, optionally
      with object-type prefix and object-identity suffix,
      and post-type suffix and pre-identity prefix and executing the
      code in `BODY` to provide possible further output.

#### Syntax:
````lisp
(pprint-dotnet-object (object stream &key type
                                          identity
                                          (prefix (when type " "))
                                          (suffix (when identity " ")))
  &body body)
;; => results of BODY forms exectuion
````

### Function: **SET-DOTNET-OBJECT-PRINTER**

Installs a print function designated by `PRINTER` for a .Net `TYPE`.

`PRINTER` function must accept two arguments - an `OBJECT` to print, and a `STREAM` to print to.

#### Syntax:
````lisp
(set-dotnet-object-printer type printer)
````

### Macro: **DEFINE-DOTNET-OBJECT-PRINTER**

A macro version of `SET-DOTNET-OBJECT-PRINTER`.

Installs a printer function for a .Net `TYPE`.

The function accepts two arguments - an `OBJECT` to print, and a `STREAM` to print to.

#### Syntax:
````lisp
(define-dotnet-object-printer type (object stream) &body body)
````

## Apropos Facility

The library allows for convenient querying for .NET types and type members.

### Macro: **DO-TYPES**

Evaluates `BODY` forms in a dynamic environment where `TYPE-VAR` is subsequently bound
  to each of the types found during the search based on the following filter arguments:

`:SEARCH` - a string-designator or `NIL`.
          Designates a substring to search for in the type name(excluding namespace).

`:ASSEMBLY` - Designates a specific assembly to search in. Can be a name,
            a `System.Reflection.Assembly` object, or `NIL`, in which case
            all loaded assemblies are searched.

`:NAMESPACE` - Designates a namespace to search for types. In case of it being null,
             all loaded namespaces are processed.

`:EXPORTED-ONLY` - Defaults to `T`. In case of it being non-null, only exported(i.e. public)
                 types of assemblies are processed.
                 Otherwise, all of the defined types are processed.

`TYPE-VAR` may be a symbol or a list of form (`TYPE-VAR` `RESULT-FORM`) where `RESULT-FORM`
  designates a form to be evaluated and returned at the end of the iteration.

#### Syntax:
````lisp
(do-types (type-var &key search assembly namespace (exported-only t)) &body body)
;; => the result of the evaluation of the RESULT-FORM
````

### Function: **TYPE-APROPOS**

Briefly describes all types found according to the search
  given the following filter arguments:

`STRING-DESIGNATOR` - A substring to search for in the type name(excluding namespace).
                    Can be `NIL`, in which case all type names are processed.

`:ASSEMBLY` - Designates a specific assembly to search in. Can be a name,
            a `System.Reflection.Assembly` object, or `NIL`, in which case
            all loaded assemblies are searched.

`:NAMESPACE` - Designates a namespace to search for types. In case of it being null,
             all loaded namespaces are processed.

`:EXPORTED-ONLY` - Defaults to `T`. In case of it being non-null, only exported(i.e. public)
                 types of assemblies are processed.
                 Otherwise, all of the defined types are processed.

#### Syntax:
````lisp
(type-apropos string-designator &key assembly namespace (exported-only t))
````

### Function: **TYPE-APROPOS-LIST**

Like `TYPE-APROPOS`, but returns a list of types instead of describing them.

#### Syntax:
````lisp
(type-apropos-list string-designator &key assembly namespace (exported-only t))
;; => a list of types
````

### Function: **NAMESPACE-APROPOS**

Briefly describes all namespaces found according to the search
  given the following filter arguments:

`STRING-DESIGNATOR` - A substring to search for in the namespace name.
                    Can be `NIL`, in which case all names are processed.

`:ASSEMBLY` - Designates a specific assembly to search in. Can be a name,
            a `System.Reflection.Assembly` object, or `NIL`, in which case
            all loaded assemblies are searched.

`:EXPORTED-ONLY` - Defaults to `T`. In case of it being non-null, only exported(i.e. public)
                 types of assemblies are processed.
                 Otherwise, all of the defined types are processed.

#### Syntax:
````lisp
(namespace-apropos string-designator &key assembly (exported-only t))
````

### Function: **NAMESPACE-APROPOS-LIST**

Briefly describes all namespaces found according to the search
  given the following filter arguments:

`STRING-DESIGNATOR` - A substring to search for in the namespace name.
                    Can be `NIL`, in which case all names are processed.

`:ASSEMBLY` - Designates a specific assembly to search in. Can be a name,
            a `System.Reflection.Assembly` object, or `NIL`, in which case
            all loaded assemblies are searched.

`:EXPORTED-ONLY` - Defaults to `T`. In case of it being non-null, only exported(i.e. public)
                 types of assemblies are processed.
                 Otherwise, all of the defined types are processed.

#### Syntax:
````lisp
(namespace-apropos-list string-designator &key assembly (exported-only t))
;; => a list of strings
````

### Macro: **DO-MEMBERS**

Evaluates `BODY` forms in a dynamic environment where `VAR` is subsequently bound
  to each of the members of the `TYPE` found during the search based on the following
  filter arguments:

`:SEARCH` - a string-designator or `NIL`.
          Designates a substring to search for in the member name.

`:INSTANCE` - whether to search for instance members.

`:STATIC` - whether to search for static members.

`:PUBLIC` - whether to search for public members.

`:NON-PUBLIC` - whether to search for non-public members(i.e. private and protected).

`:DECLARED-ONLY` - whether to only include members declared in the type directly and
                 omit inherited ones.

`:CONSTRUCTORS` - whether to search for constructors.

`:EVENTS` - whether to search for events.

`:FIELDS` - whether to search for fields.

`:METHODS` - whether to search for methods.

`:PROPERTIES` - whether to search for properties.

`:CUSTOM` - whether to search for custom type members.

`:NESTED-TYPES` - whether to search for nested types.

`VAR` can either be a symbol or a list of form (`VAR` `RESULT-FORM`) where `RESULT-FORM`
  designates a form to be evaluated and returned at the end of the iteration.

#### Syntax:
````lisp
(do-members (var type &key search
                           (instance t)
                           (static t)
                           (public t)
                           non-public
                           declared-only
                           (constructors t)
                           (events t)
                           (fields t)
                           (methods t)
                           (properties t)
                           (custom t)
                           (nested-types t))
  &body body)
;; => the result of the execution of the RESULT-FORM
````

### Function: **MEMBER-APROPOS**

Briefly describes members of the `TYPE` found during the search based on the following
  filter arguments:

`STRING-DESIGNATOR` - A substring to search for in the member name.
                    Can be `NIL`, in which case all names are processed.

`:INSTANCE` - whether to search for instance members.

`:STATIC` - whether to search for static members.

`:PUBLIC` - whether to search for public members.

`:NON-PUBLIC` - whether to search for non-public members(i.e. private and protected).

`:DECLARED-ONLY` - whether to only include members declared in the type directly and
                 omit inherited ones.

`:CONSTRUCTORS` - whether to search for constructors.

`:EVENTS` - whether to search for events.

`:FIELDS` - whether to search for fields.

`:METHODS` - whether to search for methods.

`:PROPERTIES` - whether to search for properties.

`:CUSTOM` - whether to search for custom type members.

`:NESTED-TYPES` - whether to search for nested types.

#### Syntax:
````lisp
(member-apropos type string-designator &key (instance t)
                                            (static t)
                                            (public t)
                                            non-public
                                            declared-only
                                            (constructors t)
                                            (events t)
                                            (fields t)
                                            (methods t)
                                            (properties t)
                                            (custom t)
                                            (nested-types t))
````

### Function: **MEMBER-APROPOS-LIST**

Like `MEMBER-APROPOS` but returns a list of members instead of describing them.

#### Syntax:
````lisp
(member-apropos-list type string-designator &key (instance t)
                                                 (static t)
                                                 (public t)
                                                 non-public
                                                 declared-only
                                                 (constructors t)
                                                 (events t)
                                                 (fields t)
                                                 (methods t)
                                                 (properties t)
                                                 (custom t)
                                                 (nested-types t))
;; => a list of member info objects
````

### Function: **CONSTRUCTOR-APROPOS**

Like `MEMBER-APROPOS` but only describes constructors and does not allow for name search.

#### Syntax:
````lisp
(constructor-apropos type &key (instance t) (static t) (public t) non-public declared-only)
````

### Function: **CONSTRUCTOR-APROPOS-LIST**

Like `CONSTRUCTOR-APROPOS` but returns a list of constructors instead of describing them.

#### Syntax:
````lisp
(constructor-apropos-list type &key (instance t) (static t) (public t) non-public declared-only)
;; => a list of constructor info objects
````

### Function: **EVENT-APROPOS**

Like `MEMBER-APROPOS` but only describes events.

#### Syntax:
````lisp
(event-apropos type string-designator &key (instance t) (static t) (public t) non-public declared-only)
````

### Function: **EVENT-APROPOS-LIST**

Like `EVENT-APROPOS` but returns a list of events instead of describing them.

#### Syntax:
````lisp
(event-apropos-list type string-designator &key (instance t) (static t) (public t) non-public declared-only)
;; => a list of event info objects
````

### Function: **FIELD-APROPOS**

Like `MEMBER-APROPOS` but only describes fields.

#### Syntax:
````lisp
(field-apropos type string-designator &key (instance t) (static t) (public t) non-public declared-only)
````

### Function: **FIELD-APROPOS-LIST**

Like `FIELD-APROPOS` but returns a list of fields instead of describing them.

#### Syntax:
````lisp
(field-apropos-list type string-designator &key (instance t) (static t) (public t) non-public declared-only)
;; => a list of field info objects
````

### Function: **METHOD-APROPOS**

Like `MEMBER-APROPOS` but only describes methods.

#### Syntax:
````lisp
(method-apropos type string-designator &key (instance t) (static t) (public t) non-public declared-only)
````

### Function: **METHOD-APROPOS-LIST**

Like `METHOD-APROPOS` but returns a list of methods instead of describing them.

#### Syntax:
````lisp
(method-apropos-list type string-designator &key (instance t) (static t) (public t) non-public declared-only)
;; => a list of method info objects
````

### Function: **PROPERTY-APROPOS**

Like `MEMBER-APROPOS` but only describes properties.

#### Syntax:
````lisp
(property-apropos type string-designator &key (instance t) (static t) (public t) non-public declared-only)
````

### Function: **PROPERTY-APROPOS-LIST**

Like `PROPERTY-APROPOS` but returns a list of properties instead of describing them.

#### Syntax:
````lisp
(property-apropos-list type string-designator &key (instance t) (static t) (public t) non-public declared-only)
;; => a list of property info objects
````

### Function: **CUSTOM-MEMBER-APROPOS**

Like `MEMBER-APROPOS` but only describes custom type members.

#### Syntax:
````lisp
(custom-member-apropos type string-designator &key (instance t) (static t) (public t) non-public declared-only)
````

### Function: **CUSTOM-MEMBER-APROPOS-LIST**

Like `CUSTOM-MEMBER-APROPOS` but returns a list of custom type members instead of describing them.

#### Syntax:
````lisp
(custom-member-apropos-list type string-designator &key (instance t) (static t) (public t) non-public declared-only)
;; => a list of custom member objects
````

### Function: **NESTED-TYPE-APROPOS**

Like `MEMBER-APROPOS` but only describes nested types.

#### Syntax:
````lisp
(nested-type-apropos type string-designator &key (instance t) (static t) (public t) non-public declared-only)
````

### Function: **NESTED-TYPE-APROPOS-LIST**

Like `NESTED-TYPE-APROPOS` but returns a list of nested types instead of describing them.

#### Syntax:
````lisp
(nested-type-apropos-list type string-designator &key (instance t) (static t) (public t) non-public declared-only)
;; => a list of nested type metaobjects
````

## Callable Classes

The library allows for the definition of special CLOS classes, which generate proxy types on the .NET side.

These proxy types delegate their member invocation to the Lisp code.

This is achieved by utilization of `System.Reflection.Emit` infrastructure at run-time.

Currently, only instance member generation is supported. Attributes on classes, members and parameters are not supported yet.

The callable class metamodel can be extended by the user. Take a look at `DOTNET-SLOT-DEFINITION` class and its subclasses.

TODO: The metamodel description.

### Macro: **DEFINE-DOTNET-CALLABLE-CLASS**

Defines a CLOS class, which members could be invoked by .NET code.

#### Syntax:
````lisp
(define-dotnet-callable-class class-name-and-options (&rest superclasses) &body doc-and-slots)
````
<details>
<summary> Full syntax </summary>

````
class-name-and-options ::= name | (name class-option*)

class-option ::= (:BASE-TYPE typespec) | (:INTERFACES typespec*) | CLOS-class-option

superclasses ::= name*

doc-and-slots ::= [ docstring ] slot*

slot ::= event-slot | property-slot | method-slot | indexer-slot | CLOS-slot

event-slot ::= (:EVENT event-name-and-options typespec CLOS-slot-option*)

event-name-and-options ::= name | (name-pair [ raise-method-option ])

raise-method-option ::= :RAISE-METHOD-DOTNET-NAME dotnet-name

property-slot ::= auto-property-slot | long-form-property-slot

auto-property-slot ::= (:PROPERTY auto-property-name-and-options typespec CLOS-slot-option*)

auto-property-name-and-options ::= name | (name-pair { getter-option | setter-option }*)

name-pair ::= name | dotnet-name | { name dotnet-name } | { dotnet-name name }

getter-option ::= :GETTER { T | NIL | function-name }

setter-option ::= :SETTER { T | NIL | function-name }

long-form-property-slot ::= (:PROPERTY long-form-property-name-and-options typespec property-accessors)

long-form-property-name-and-options ::= name | (name-pair [ defmethodp-option ] CLOS-slot-option*)

defmethodp-option ::= :DEFMETHODP generalized-boolean

property-accessors ::= { getter-form | setter-form } [ { getter-form | setter-form } ]

getter-form ::= existing-getter-form | getter-definition-form

existing-getter-form ::= ({ :GET | :GETTER } function-name)

getter-definition-form ::= ({ :GET | :GETTER } function-name accessor-body)

setter-form ::= existing-setter-form | setter-definition-form

existing-setter-form ::= ({ :SET | :SETTER } function-name)

setter-definition-form ::= ({ :SET | :SETTER } function-name accessor-body)

method-slot ::= ({ :METHOD | :DEFMETHOD } method-name-options-and-generic-parameters return-type parameter-list function-body)

return-type ::= method-parameter-typespec

method-name-options-and-generic-parameters ::= name | (method-name-and-options generic-parameter*)

method-name-and-options ::= name | (name-pair { defmethodp-option | function-name-option }* CLOS-slot-option*)

function-name-option ::= :FUNCTION-NAME name

generic-parameter ::= name | (name generic-parameter-qualifier*)

generic-parameter-qualifier ::= :IN | :OUT | :NEW | :STRUCT | :CLASS | (:BASE-TYPE typespec) | (:INTERFACES typespec*)

parameter-list ::= (parameter* [ &REST params-array-parameter ])

parameter ::= (name typespec parameter-option*)

parameter-option ::= { :DIRECTION { :IN | :OUT | :IO } } | { :DOTNET-NAME dotnet-name } | { :REF generalized-boolean }

params-array-parameter ::= (name (array typespec) [ :DOTNET-NAME dotnet-name ])

indexer-slot ::= (:indexer [ indexer-name-and-options ] typespec indexer-parameter-list property-accessors)

indexer-name-and-options ::= name | (name-pair [ defmethodp-option ] CLOS-slot-option*)

indexer-parameter-list ::= (parameter parameter* [ &REST params-array-parameter ])

````

* *name* - a symbol

* *dotnet-name* - a string

* *function-name* - either a symbol or a list of form `(SETF name)`

* *function-body* - as in `DEFUN` or `DEFMETHOD`

* *accessor-body* - as in `DEFUN` or `DEFMETHOD`, but must contain at least one form

* *generalized-boolean* - any object. `NIL` designates false value.

* *CLOS-class-option* - any `DEFCLASS` option, such as `:DEFAULT-INITARGS`, except for `:DOCUMENTATION`

* *CLOS-slot-option* - any CLOS slot option, such as `:INITARG` or `:ACCESSOR`

* *CLOS-slot* - a usual CLOS slot, not prefixed by a keyword

* *typespec* - a valid bike typespec

* *method-parameter-typespec* - a bike typespec, with the exception that generic parameter names are also allowed

* *docstring* - a string

</details>

### Class: **DOTNET-PROXY-OBJECT**

Base class for objects which contain proxies to dotnet objects.

Instances of this class can mimic _true_ .NET objects and could be passed to .NET methods.

#### Reader methods:

* **DOTNET-PROXY-OBJECT-VALUE** - returns a .NET proxy object(of type `DOTNET-OBJECT`).

### Type: **DOTNET-OBJECT\***

#### Definition:
````lisp
(deftype dotnet-object* () '(or dotnet-object dotnet-proxy-object))
````

### Class: **DOTNET-CALLABLE-OBJECT**

Base class for objects which could be invoked by .NET code.

#### Reader methods:

* **DOTNET-CALLABLE-OBJECT-PROXY** - returns a .NET proxy object(of type `DOTNET-OBJECT`).

### Class: **DOTNET-PROXY-CLASS**

Base metaclass for classes that rely on a proxy .NET type.

#### Reader methods:

* **DOTNET-PROXY-CLASS-OBJECT-TYPE** - returns a .NET proxy type(of type `DOTNET-TYPE`).

### Class: **DOTNET-CALLABLE-CLASS**

Base metaclass for dotnet callable classes.

#### Reader methods:

* **DOTNET-CALLABLE-CLASS-PROXY-TYPE** - returns a .NET proxy type(of type `DOTNET-TYPE`).

### Function: **DOTNET-CALLABLE-OBJECT-PROXY-INITIALIZED-P**

Returns Non-NIL when the object's proxy is initialized and ready to be passed to .NET code.

#### Syntax:
````lisp
(dotnet-callable-object-proxy-initialized-p object) ; => T or NIL
````

### Function: **DOTNET-CALLABLE-PROXY-P**

Return non-NIL in case of an `OBJECT` being a callable proxy of a `DOTNET-CALLABLE-OBJECT` instance.

#### Syntax:
````lisp
(dotnet-callable-proxy-p object) ; => T or NIL
````

### Function: **DOTNET-CALLABLE-PROXY-TYPE-P**

Returns non-NIL in case of a `DOTNET-TYPE` is a proxy type for a `DOTNET-CALLABLE-CLASS`.

#### Syntax:
````lisp
(dotnet-callable-proxy-type-p dotnet-type) ; => T or NIL
````

### Function: **DOTNET-CALLABLE-PROXY-OBJECT**

Retrieves an object associated with the `PROXY`.

#### Syntax:
````lisp
(dotnet-callable-proxy-object proxy) ; => an object
````

### Function: **UNWRAP-DOTNET-CALLABLE-PROXY**

Returns an underlying object of a dotnet callable proxy in case an `OBJECT` is a proxy.

#### Syntax:
````lisp
(unwrap-dotnet-callable-proxy object) ; => an object or NIL
````

## Custom Syntax

The library exposes a named readtable `BIKE-SYNTAX`, which allows for the following reader extensions:

````lisp
[:typespec MethodDesignator . args] ; == (invoke 'typespec 'MethodDesignator . args)

[object MethodDesignator . args] ; == (invoke object 'MethodDesignator . args)

[:typespec %PropertyName] ; == (property 'typespec 'PropertyName)

[object %PropertyName] ; == (property object 'PropertyName)

[:typespec $FieldName] ; == (field 'typespec 'FieldName)

[object $FieldName] ; == (field object 'FieldName)

#[object index . indices] ; == (ref object index . indices)

#e(EnumTypeName Value1 Value2 . Values) ; == (enum 'EnumTypeName 'Value1 'Value2 . Values) ; no value is evaluated

[:typespec +EventName handler] ; == (event-add 'typespec 'EventName handler)

[object +EventName handler] ; == (event-add object 'EventName handler)

[:typespec -EventName handler] ; == (event-remove 'typespec 'EventName handler)

[object -EventName handler] ; == (event-remove object 'EventName handler)
````

## Conditions

### Condition Class: **DOTNET-ERROR**

Represents a .NET exception.

#### Reader methods:

* **DOTNET-ERROR-OBJECT**

### Condition Class: **BIKE-CONDITION**

Represents a generic condition.

### Condition Class: **BIKE-ERROR**

Represents an erroneous condition.

### Condition Class: **BIKE-WARNING**

Represents a subnormal but not erroneous condition.

### Condition Class: **INVALID-ASSEMBLE-DESIGNATOR**

Represents a condition in which the library encountered an invalid assembly designator.

#### Reader methods:

* **INVALID-ASSEMBLY-DESIGNATOR-DATUM**

### Condition Class: **TYPE-RESOLUTION-ERROR**

Represents a condition when a type resolution has failed for one reason or another.

#### Reader methods:

* **TYPE-RESOLUTION-ERROR-DATUM**

### Condition Class: **INVALID-TYPE-DESIGNATOR**

Represents a condition when an object supplied does not represent a proper type specifier.

#### Reader methods:

* **INVALID-TYPE-DESIGNATOR-DATUM**

### Condition Class: **INVALID-TYPE-AST**

Represents a condition when an interned typespec AST is ill-formed.

#### Reader methods:

* **INVALID-TYPE-AST-DATUM**

### Condition Class: **INVALID-TYPE-NAME**

Represents a condition when a type name supplied is invalid for one reason or another.

#### Reader methods:

* **INVALID-TYPE-NAME-DATUM**

### Condition Class: **INVALID-REF-TYPE**

Represents a condition when a `:REF` typespec occurs in a place where it is not allowed. (like e.g. in a property type)

#### Reader methods:

* **INVALID-REF-TYPE-DATUM**

### Condition Class: **INNER-REF-TYPE-ERROR**

Represents a condition when the typespec parser encountered a `:REF` typespec despite it not being a "top-level" one.

#### Reader methods:

* **INNER-REF-TYPE-ERROR-DATUM**

### Condition Class: **INNER-QUALIFIED-TYPE-ERROR**

Represents a condition when the typespec parser encountered an unexpected `:QUALIFIED` typespec.

#### Reader methods:

* **INNER-QUALIFIED-TYPE-ERROR-DATUM**

### Condition Class: **TYPE-NAME-PARSER-ERROR**

Represents an error that occurred during the type name parsing.

#### Reader methods:

* **TYPE-NAME-PARSER-ERROR-CHARACTER**
* **TYPE-NAME-PARSER-ERROR-POSITION**
* **TYPE-NAME-PARSER-ERROR-STRING**

### Condition Class: **TYPE-NAME-UNEXPECTED-TOKEN-ERROR**

Represents a condition when the type name parser encountered an unexpected token.

#### Reader methods:

* **TYPE-NAME-UNEXPECTED-TOKEN-ERROR-VALUE**
* **TYPE-NAME-UNEXPECTED-TOKEN-ERROR-TOKEN**

### Condition Class: **GENERIC-ARGUMENT-COUNT-MISMATCH**

Represents a condition when the type name parser encountered a mismatch between generic parameter count and the number embedded into the generic definition name.

### Condition Class: **TYPE-NAME-PARSER-EOF**

Represents a condition when the type name parser encountered an unexpected end of input.

### Condition Class: **ENUM-RESOLUTION-ERROR**

Represents a condition when the library is unable to resolve an Enum type.

#### Reader methods:

* **ENUM-RESOLUTION-ERROR-DATUM**

### Condition Class: **MEMBER-RESOLUTION-ERROR**

Represents a condition when the library is unable to resolve a specific member of a class.

#### Reader methods:

* **MEMBER-RESOLUTION-ERROR-TYPE**
* **MEMBER-RESOLUTION-ERROR-STATIC-P**
* **MEMBER-RESOLUTION-ERROR-MEMBER**
* **MEMBER-RESOLUTION-ERROR-ARGS**
* **MEMBER-RESOLUTION-ERROR-ACCESSOR-KIND**
* **MEMBER-RESOLUTION-ERROR-MEMBER-KIND**

### Condition Class: **FIELD-RESOLUTION-ERROR**

Represents a condition when the library is unable to resolve a specific field of a class.

#### Reader methods:

* **FIELD-RESOLUTION-ERROR-FIELD**

### Condition Class: **PROPERTY-RESOLUTION-ERROR**

Represents a condition when the library is unable to resolve a specific property of a class.

#### Reader methods:

* **PROPERTY-RESOLUTION-ERROR-PROPERTY**

### Condition Class: **EVENT-RESOLUTION-ERROR**

Represents a condition when the library is unable to resolve a specific event of a class.

#### Reader methods:

* **EVENT-RESOLUTION-ERROR-EVENT**

### Condition Class: **INDEXER-RESOLUTION-ERROR**

Represents a condition when the library is unable to resolve an indexer of a class.

### Condition Class: **METHOD-RESOLUTION-ERROR**

Represents a condition when the library is unable to resolve a specific method of a class.

#### Reader methods:

* **METHOD-RESOLUTION-ERROR-METHOD**
* **METHOD-RESOLUTION-ERROR-ARGS**

### Condition Class: **CONSTRUCTOR-RESOLUTION-ERROR**

Represents a condition when the library is unable to resolve a type constructor.

### Condition Class: **ACCESSOR-RESOLUTION-ERROR**

Represents a condition when the library is unable to resolve a proper accessor for an event or a property.

#### Reader methods:

* **ACCESSOR-RESOLUTION-ERROR-ACCESSOR-KIND**
* **ACCESSOR-RESOLUTION-ERROR-MEMBER-KIND**
* **ACCESSOR-RESOLUTION-ERROR-MEMBER**

### Condition Class: **BIKE-READER-ERROR**

Represents a reader error which happened during custom syntax parsing.

#### Reader methods:

* **BIKE-READER-ERROR-MESSAGE**

### Condition Class: **DUPLICATE-DOTNET-NAME**

Represents a condition when there exists a duplicate dotnet member name in a class.

#### Reader methods:

* **DUPLICATE-DOTNET-NAME-VALUE**
* **DUPLICATE-DOTNET-NAME-CLASS**

### Condition Class: **DUPLICATE-INDEXER**

Represents a condition when there's a duplicate indexer slot defined on a class.

#### Reader methods:

* **DUPLICATE-INDEXER-CLASS**

### Condition Class: **DOTNET-SLOT-MISSING**

Represents a condition when a proxy trampoline realizes that there is no slot of the specified dotnet name defined on a class.

#### Reader methods:

* **DOTNET-SLOT-MISSING-CLASS**
* **DOTNET-SLOT-MISSING-NAME**

### Condition Class: **DELEGATE-TYPE-EXPECTED**

Represents a condition when an `:EVENT` slot handler type is not a delegate type.

#### Reader methods:

* **DELEGATE-TYPE-EXPECTED-DATUM**

### Condition Class: **INTERFACE-TYPE-EXPECTED**

Represents a condition when there's a non-interface type in an `(:INTERFACES)` option of a `DOTNET-CALLABLE-CLASS`.

#### Reader methods:

* **INTERFACE-TYPE-EXPECTED-DATUM**

### Condition Class: **SEALED-INHERITANCE**

Represents a condition when there's an attempt to inherit from a sealed type.

#### Reader methods:

* **SEALED-INHERITANCE-TYPE**

### Condition Class: **PARAMETER-DIRECTION-MISMATCH**

Represents a condition when an `:IO` or `:OUT` parameter is defined to not be passed by reference.

#### Reader methods:

* **PARAMETER-DIRECTION-MISMATCH-DATUM**

### Condition Class: **INVALID-PARAMS-ARRAY-DEFINITION**

Represents a condition when a `&rest` method or indexer parameter is ill-formed.

#### Reader methods:

* **INVALID-PARAMS-ARRAY-DEFINITION-DATUM**

### Condition Class: **DUPLICATE-PARAMETER-NAME**

Represents a condition when an indexer or method argument list contains duplicate names.

#### Reader methods:

* **DUPLICATE-PARAMETER-NAME-DATUM**
* **DUPLICATE-PARAMETER-NAME-VALUE**

### Condition Class: **INVALID-GENERIC-CONSTRAINT**

Represents a condition when a `DOTNET-CALLABLE-CLASS` initializer encountered an invalid constraint on a generic method argument of a generic method.

#### Reader methods:

* **INVALID-GENERIC-CONSTRAINT-MESSAGE**
* **INVALID-GENERIC-CONSTRAINT-LIST**

### Condition Class: **DOTNET-SLOT-WRITE-ATTEMPT**

Represents a condition when there's an attempt to invoke `(SETF SLOT-VALUE)` on a dotnet slot.

#### Reader methods:

* **DOTNET-SLOT-WRITE-ATTEMPT-OBJECT**
* **DOTNET-SLOT-WRITE-ATTEMPT-SLOT-NAME**
* **DOTNET-SLOT-WRITE-ATTEMPT-VALUE**

### Condition Class: **METHOD-SLOT-WRITE-ATTEMPT**

Represents a condition when there's an attempt to invoke `(SETF SLOT-VALUE)` on a .NET method slot.

#### Reader methods:

* **METHOD-SLOT-WRITE-ATTEMPT-OBJECT**
* **METHOD-SLOT-WRITE-ATTEMPT-SLOT-NAME**
* **METHOD-SLOT-WRITE-ATTEMPT-VALUE**

### Condition Class: **INDEXER-SLOT-WRITE-ATTEMPT**

Represents a condition when there's an attempt to invoke `(SETF SLOT-VALUE)` on a .NET indexer slot.

#### Reader methods:

* **INDEXER-SLOT-WRITE-ATTEMPT-OBJECT**
* **INDEXER-SLOT-WRITE-ATTEMPT-SLOT-NAME**
* **INDEXER-SLOT-WRITE-ATTEMPT-VALUE**

### Condition Class: **DOTNET-SLOT-MAKUNBOUND-ATTEMPT**

Represents a condition when there's an attempt to invoke `SLOT-MAKUNBOUND` on a .NET slot.

#### Reader methods:

* **DOTNET-SLOT-MAKUNBOUND-ATTEMPT-OBJECT**
* **DOTNET-SLOT-MAKUNBOUND-ATTEMPT-SLOT-NAME**

### Condition Class: **METHOD-SLOT-MAKUNBOUND-ATTEMPT**

Represents a condition when there's an attempt to invoke `SLOT-MAKUNBOUND` on a .NET method slot.

#### Reader methods:

* **METHOD-SLOT-MAKUNBOUND-ATTEMPT-OBJECT**
* **METHOD-SLOT-MAKUNBOUND-ATTEMPT-SLOT-NAME**

### Condition Class: **PROPERTY-SLOT-MAKUNBOUND-ATTEMPT**

Represents a condition when there's an attempt to invoke `SLOT-MAKUNBOUND` on a .NET-only property slot.

#### Reader methods:

* **PROPERTY-SLOT-MAKUNBOUND-ATTEMPT-OBJECT**
* **PROPERTY-SLOT-MAKUNBOUND-ATTEMPT-SLOT-NAME**

### Condition Class: **INDEXER-SLOT-MAKUNBOUND-ATTEMPT**

Represents a condition when there's an attempt to invoke `SLOT-MAKUNBOUND` on a .NET indexer slot.

#### Reader methods:

* **INDEXER-SLOT-MAKUNBOUND-ATTEMPT-OBJECT**
* **INDEXER-SLOT-MAKUNBOUND-ATTEMPT-SLOT-NAME**

### Condition Class: **DOTNET-CALLABLE-OBJECT-ORPHAN-PROXY**

Represents a condition in which the library detected that a proxy outlived its `DOTNET-CALLABLE-OBJECT`.

#### Reader methods:

* **DOTNET-CALLABLE-OBJECT-ORPHAN-PROXY-VALUE**
* **DOTNET-CALLABLE-OBJECT-ORPHAN-PROXY-OPERATION**
* **DOTNET-CALLABLE-OBJECT-ORPHAN-PROXY-MEMBER-NAME**
* **DOTNET-CALLABLE-OBJECT-ORPHAN-PROXY-ARGUMENTS**

## Limitations

### Runtime code generation

The library relies heavily on runtime compilation and code generation, both on the Lisp side and the .NET side.

This means it won't be usable in a restricted environment like __iOS__.

This also means that ECL and similar implementations would constantly invoke their C compiler program, so keep that in mind.

### What has been done cannot be undone

.NET runtime is not as dynamic as Common Lisp implementations.

.NET runtime, once loaded, can not be unloaded from the process without proper deinitialization.
Moreover, it can not be loaded once again, after it has been shut down.

To solve both problems, the library simply does not expose any API for shutting the .NET runtime down.

So, if something goes south, your best bet is a Lisp image restart.
