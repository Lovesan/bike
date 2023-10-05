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
                #:register-image-dump-hook)
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
                #:when-let*)
  (:export
   ;; some internals reexports
   #:native-path

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
   #:delegate-type-expected
   #:delegate-type-expected-datum
   #:interface-type-expected
   #:interface-type-expected-datum
   #:sealed-inheritance
   #:sealed-inheritance-type

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
   #:direct-dotnet-slot-definition
   #:direct-property-slot-definition
   #:direct-event-slot-definition
   #:direct-callable-event-slot-definition
   #:effective-dotnet-slot-definition
   #:effective-property-slot-definition
   #:effective-event-slot-definition
   #:effective-callable-event-slot-definition))

;;; vim: ft=lisp et
