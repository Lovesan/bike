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

(asdf:defsystem #:bike
  :version "0.6.1"
  :description "Common Lisp .Net Core Interop"
  :author "Dmitry Ignatiev <lovesan.ru at gmail.com>"
  :maintainer "Dmitry Ignatiev <lovesan.ru at gmail.com>"
  :licence "MIT"
  :defsystem-depends-on (#:bike-internals)
  :depends-on (#:bike-internals)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "libraries")
                             (:file "ffi")
                             (:file "host")
                             (:file "handle-table")
                             (:file "object")
                             (:file "conditions")
                             (:file "api-low")
                             (:file "callbacks")
                             (:file "trampolines")
                             (:file "known")
                             (:file "api-known")
                             (:file "print")
                             (:file "type")
                             (:file "type-name-parser")
                             (:file "type-resolution")
                             (:file "assemblies")
                             (:file "enum")
                             (:file "api-reflection")
                             (:file "invocation-cache")
                             (:file "members")
                             (:file "api"))))
  :in-order-to  ((test-op (test-op #:bike-tests))))

;;; vim: ft=lisp et
