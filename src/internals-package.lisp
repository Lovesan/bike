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

(uiop:define-package #:bike-internals
  (:use #:cl #:global-vars #:uiop #:cffi #:split-sequence #:flexi-streams #:cl-ppcre)
  (:import-from #:alexandria
                #:string-designator
                #:define-constant
                #:non-negative-fixnum
                #:with-gensyms
                #:once-only)
  (:export
   ;; .NET runtime location
   #:find-coreclr
   #:+coreclr-library-file+
   #-windows
   #:+system-native-library-file+

   ;; Interop library
   #:find-interop
   #:build-interop
   #:+interop-library-file+

   ;; FFI
   #:+pointer-size+
   #:+pointer-bits+
   #:lpwstr
   #:lpastr
   #:dnchar
   #:size-t
   #:define-foreign-library-once
   #:use-foreign-library-once
   #+windows
   #:kernel32

   ;; Conditions
   #:slot-initializer-missing
   #:slot-initializer-missing-message
   #:required-slot

   ;; Read/Write lock
   #:rwlock
   #:rwlockp
   #:make-rwlock
   #:rwlock-name
   #:with-read-lock
   #:with-write-lock

   ;; String utils
   #:make-simple-character-string
   #:simple-character-string
   #:simple-character-string-upcase
   #:camel-case-string
   #:lisp-case-string
   #:whitespace-char-p

   ;; Pathnames
   #:native-path
   #:get-exe-path))

;;; vim: ft=lisp et
