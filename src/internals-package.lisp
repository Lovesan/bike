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
  (:use #:cl #:uiop #:cffi #:split-sequence #:flexi-streams #:cl-ppcre)
  (:export #:find-coreclr
           #:find-interop
           #:build-interop
           #:get-exe-path
           #:+coreclr-library-file+
           #:+interop-library-file+
           #:+pointer-size+
           #:+pointer-bits+
           #:lpwstr
           #:lpastr
           #:dnchar
           #:size-t
           #:native-path

           #:slot-initializer-missing
           #:slot-initializer-missing-message
           #:required-slot

           #:rwlock
           #:rwlockp
           #:make-rwlock
           #:with-read-lock
           #:with-write-lock)
  (:import-from #:alexandria
                #:define-constant
                #:non-negative-fixnum
                #:with-gensyms))

;;; vim: ft=lisp et
