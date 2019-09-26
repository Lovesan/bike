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
  (:use #:cl #:cffi #:bike-internals)
  (:import-from #:alexandria
                #:with-gensyms
                #:symbolicate
                #:string-designator
                #:removef
                #:hash-table-alist
                #:define-constant
                #:non-negative-fixnum
                #:positive-fixnum
                #:parse-body
                #:ensure-list)
  (:export

   ;; types and stuff
   #:*default-assemblies*
   #:resolve-type
   #:current-app-domain
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
   #:with-disposable
   #:with-disposables
   #:with-disposables*
   #:do-enumerable
   #:exception-bind
   #:exception-case

   ;; arrays
   #:do-bike-vector
   #:dnvref
   #:list-to-bike-vector
   #:bike-vector-to-list))

;;; vim: ft=lisp et
