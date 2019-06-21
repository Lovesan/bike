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
                #:non-negative-fixnum)
  (:export
   
   ;; types and stuff
   #:import-type
   #:resolve-type
   #:load-assembly
   #:import-assembly
   #:get-loaded-assemblies
   #:import-loaded-assemblies
   #:clear-type-cache
   #:use-namespace
   #:unuse-namespace
   #:unuse-all-namespaces
   #:use-type-alias
   #:unuse-type-alias
   #:unuse-all-type-aliases

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
   #:invalid-type-designator
   #:invalid-type-designator-datum
   #:type-resolution-error
   #:type-resolution-error-datum   


   ;; api
   #:new
   #:invoke
   #:field
   #:property
   #:box
   #:unbox

   ;; arrays
   #:do-bike-vector
   #:dnvref))

;;; vim: ft=lisp et






