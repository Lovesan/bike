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

(in-package #:cl-user)

(uiop:define-package #:bike
  (:use #:cl #:global-vars #:cffi #:bike-internals #:named-readtables)
  (:shadowing-import-from #:closer-mop
                          #:defclass
                          #:defgeneric
                          #:defmethod
                          #:slot-definition
                          #:standard-slot-definition
                          #:direct-slot-definition
                          #:effective-slot-definition
                          #:standard-direct-slot-definition
                          #:standard-effective-slot-definition
                          #:direct-slot-definition-class
                          #:effective-slot-definition-class
                          #:compute-effective-slot-definition
                          #:validate-superclass
                          #:finalize-inheritance
                          #:standard-class
                          #:standard-object
                          #:slot-definition-name
                          #:slot-definition-initfunction
                          #:slot-definition-initform
                          #:slot-definition-allocation
                          #:slot-definition-initargs
                          #:slot-definition-type
                          #:class-name
                          #:class-slots
                          #:compute-slots
                          #:class-direct-slots
                          #:class-finalized-p
                          #:ensure-finalized
                          #:slot-value-using-class
                          #:slot-boundp-using-class
                          #:slot-makunbound-using-class
                          #:fix-slot-initargs
                          #:class-precedence-list
                          #:subclassp)
  (:import-from #:uiop
                #:strcat
                #:register-image-restore-hook
                #:register-image-dump-hook
                #:probe-file*
                #:make-pathname*
                #:merge-pathnames*
                #:pathname-directory-pathname
                #:get-pathname-defaults
                #:lisp-implementation-directory
                #:inter-directory-separator
                #:directory*
                #:*wild*
                #:string-prefix-p
                #:string-suffix-p)
  (:import-from #:alexandria
                #:with-gensyms
                #:once-only
                #:symbolicate
                #:string-designator
                #:removef
                #:hash-table-alist
                #:define-constant
                #:non-negative-fixnum
                #:positive-fixnum
                #:parse-body
                #:ensure-list
                #:when-let
                #:when-let*
                #:hash-table-keys
                #:switch
                #:eswitch)
  (:import-from #:cl-ppcre
                #:regex-replace)
  (:export
   ;; some internals reexports
   #:native-path

   ;; FPU
   #:disable-fpu-exceptions

   ;; types and stuff
   #:*default-assemblies*
   #:resolve-type
   #:load-assembly
   #:load-assembly-from
   #:import-assembly
   #:import-assembly-from
   #:get-loaded-assemblies
   #:import-loaded-assemblies
   #:clear-type-cache

   ;; namespaces
   #:use-namespace
   #:unuse-namespace
   #:unuse-all-namespaces
   #:namespace-used-p
   #:get-used-namespaces
   #:normalized-type-name

   ;; type aliases
   #:use-type-alias
   #:unuse-type-alias
   #:unuse-all-type-aliases

   ;; lisp type definitions
   #:dotnet-type-designator
   #:dotnet-method-designator

   ;; objects
   #:dotnet-object
   #:dotnet-object-p
   #:dotnet-type
   #:dotnet-type-p
   #:dotnet-exception
   #:dotnet-exception-p
   #:dotnet-delegate
   #:dotnet-delegate-p
   #:dotnet-proxy-object
   #:dotnet-proxy-object-value
   #:dotnet-callable-object
   #:dotnet-callable-object-proxy
   #:dotnet-callable-object-proxy-initialized-p
   #:dotnet-callable-proxy-object
   #:dotnet-callable-proxy-type-p
   #:dotnet-callable-proxy-p
   #:unwrap-dotnet-callable-proxy
   #:dotnet-object*

   ;; conditions
   #:bike-condition
   #:bike-warning
   #:bike-error
   #:dotnet-error
   #:dotnet-error-object
   #:invalid-assembly-designator
   #:invalid-assembly-designator-datum
   #:invalid-type-designator
   #:invalid-type-designator-datum
   #:type-resolution-error
   #:type-resolution-error-datum
   #:invalid-type-ast
   #:invalid-type-ast-datum
   #:inner-ref-type-error
   #:inner-ref-type-error-datum
   #:invalid-type-name
   #:invalid-type-name-datum
   #:invalid-ref-type
   #:invalid-ref-type-datum
   #:inner-qualified-type-error
   #:inner-qualified-type-error-datum
   #:type-name-parser-error
   #:type-name-parser-error-string
   #:type-name-parser-error-character
   #:type-name-parser-error-position
   #:type-name-unexpected-token-error
   #:type-name-unexpected-token-error-value
   #:type-name-parser-error-value
   #:type-name-unexpected-token-error-token
   #:type-name-parser-error-token
   #:generic-argument-count-mismatch
   #:typename-parser-eof
   #:enum-resolution-error
   #:enum-resolution-error-datum
   #:member-resolution-error
   #:member-resolution-error-type
   #:member-resolution-error-field
   #:member-resolution-error-property
   #:member-resolution-error-method
   #:member-resolution-error-args
   #:member-resolution-error-accessor-kind
   #:member-resolution-error-member-kind
   #:member-resolution-error-member
   #:field-resolution-error
   #:field-resolution-error-field
   #:property-resolution-error
   #:property-resolution-error-property
   #:event-resolution-error
   #:event-resolution-error-event
   #:indexer-resolution-error
   #:method-resolution-error
   #:method-resolution-error-method
   #:method-resolution-error-args
   #:constructor-resolution-error
   #:accessor-resolution-error
   #:accessor-resolution-error-accessor-kind
   #:accessor-resolution-error-member-kind
   #:accessor-resolution-error-member
   #:enum-resolution-error
   #:bike-reader-error
   #:bike-reader-error-message
   #:duplicate-dotnet-name
   #:duplicate-dotnet-name-value
   #:duplicate-dotnet-name-class
   #:duplicate-indexer
   #:duplicate-indexer-class
   #:delegate-type-expected
   #:delegate-type-expected-datum
   #:interface-type-expected
   #:interface-type-expected-datum
   #:sealed-inheritance
   #:sealed-inheritance-type
   #:parameter-direction-mismatch
   #:parameter-direction-mismatch-datum
   #:invalid-params-array-definition
   #:invalid-params-array-definition-datum
   #:duplicate-parameter-name
   #:duplicate-parameter-name-value
   #:duplicate-parameter-name-datum
   #:invalid-generic-constraint
   #:invalid-generic-constraint-message
   #:invalid-generic-constraint-list
   #:dotnet-slot-write-attempt
   #:dotnet-slot-write-attempt-object
   #:dotnet-slot-write-attempt-slot-name
   #:dotnet-slot-write-attempt-value
   #:method-slot-write-attempt
   #:method-slot-write-attempt-object
   #:method-slot-write-attempt-slot-name
   #:method-slot-write-attempt-value
   #:indexer-slot-write-attempt
   #:indexer-slot-write-attempt-object
   #:indexer-slot-write-attempt-slot-name
   #:indexer-slot-write-attempt-value
   #:dotnet-slot-makunbound-attempt
   #:dotnet-slot-makunbound-attempt-object
   #:dotnet-slot-makunbound-attempt-slot-name
   #:method-slot-makunbound-attempt
   #:method-slot-makunbound-attempt-object
   #:method-slot-makunbound-attempt-slot-name
   #:property-slot-makunbound-attempt
   #:property-slot-makunbound-attempt-object
   #:property-slot-makunbound-attempt-slot-name
   #:indexer-slot-makunbound-attempt
   #:indexer-slot-makunbound-attempt-object
   #:indexer-slot-makunbound-attempt-slot-name
   #:dotnet-callable-object-orphan-proxy
   #:dotnet-callable-object-orphan-proxy-value
   #:dotnet-callable-object-orphan-proxy-operation
   #:dotnet-callable-object-orphan-proxy-member-name
   #:dotnet-callable-object-orphan-proxy-arguments
   #:dotnet-slot-missing
   #:dotnet-slot-missing-class
   #:dotnet-slot-missing-name

   ;; reflection api
   #:reflection-invoke
   #:reflection-field
   #:reflection-property
   #:reflection-ref
   #:reflection-new

   ;; api
   #:enum
   #:get-type
   #:bike-type-of
   #:bike-equals
   #:bike-subclass-p
   #:new
   #:invoke
   #:field
   #:property
   #:ref
   #:event-add
   #:event-remove
   #:box
   #:unbox
   #:bike-type-p
   #:with-fixed
   #:with-fixeds
   #:with-fixeds*
   #:with-disposable
   #:with-disposables
   #:with-disposables*
   #:do-enumerable
   #:exception-bind
   #:exception-case

   ;; arrays
   #:do-bike-vector
   #:dnvref
   #:dnaref
   #:list-to-bike-vector
   #:bike-vector-to-list

   ;; syntax
   #:bike-syntax

   ;; printer
   #:*print-dotnet-object*
   #:*print-enumerable*
   #:*print-dotnet-type-namespaces*
   #:*print-dotnet-type-parameters*
   #:*print-dotnet-type-qualified*
   #:*print-dotnet-type-pointer*
   #:*print-dotnet-type-ref*
   #:pprint-dotnet-object
   #:write-type-name
   #:set-dotnet-object-printer
   #:define-dotnet-object-printer

   ;; proxy classes and metaclasses
   #:dotnet-proxy-class
   #:dotnet-proxy-class-object-type
   #:dotnet-callable-class
   #:dotnet-callable-class-proxy-type
   #:dotnet-slot-definition
   #:slot-definition-dotnet-name
   #:property-slot-definition
   #:slot-definition-property-type
   #:slot-definition-getter
   #:slot-definition-setter
   #:event-slot-definition
   #:slot-definition-handler-type
   #:callable-event-slot-definition
   #:slot-definition-raise-method-dotnet-name
   #:method-slot-definition
   #:slot-definition-return-type
   #:slot-definition-parameters
   #:slot-definition-rest-parameter
   #:slot-definition-generic-parameters
   #:slot-definition-function-name
   #:indexer-slot-definition
   #:direct-dotnet-slot-definition
   #:direct-property-slot-definition
   #:direct-event-slot-definition
   #:direct-callable-event-slot-definition
   #:direct-method-slot-definition
   #:direct-indexer-slot-definition
   #:effective-dotnet-slot-definition
   #:effective-property-slot-definition
   #:effective-event-slot-definition
   #:effective-callable-event-slot-definition
   #:effective-method-slot-definition
   #:effective-indexer-slot-definition

   ;; proxy macros
   #:define-dotnet-callable-class

   ;; apropos
   #:do-types
   #:type-apropos
   #:type-apropos-list
   #:namespace-apropos
   #:namespace-apropos-list
   #:do-members
   #:member-apropos
   #:member-apropos-list
   #:constructor-apropos
   #:constructor-apropos-list
   #:event-apropos
   #:event-apropos-list
   #:field-apropos
   #:field-apropos-list
   #:method-apropos
   #:method-apropos-list
   #:property-apropos
   #:property-apropos-list
   #:custom-member-apropos
   #:custom-member-apropos-list
   #:nested-type-apropos
   #:nested-type-apropos-list))

;;; vim: ft=lisp et
