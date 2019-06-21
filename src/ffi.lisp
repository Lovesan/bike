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

(defcfun (coreclr-initialize
          "coreclr_initialize"
          :library coreclr
          :convention :stdcall)
    :uint
  (exe-path (:string :encoding :utf-8))
  (app-domain-name (:string :encoding :utf-8))
  (property-count :int)
  (property-keys :pointer)
  (property-values :pointer)
  (host-handle :pointer)
  (domain-id :pointer))

(defcfun (coreclr-shutdown-2
          "coreclr_shutdown_2"
          :library coreclr
          :convention :stdcall)
    :uint
  (host-handle :pointer)
  (domain-id :uint)
  (exit-code :pointer))

(defcfun (coreclr-create-delegate
          "coreclr_create_delegate"
          :library coreclr
          :convention :stdcall)
    :uint
  (host-handle :pointer)
  (domain-id :uint)
  (entry-assembly (:string :encoding :utf-8))
  (entry-type (:string :encoding :utf-8))
  (entry-method (:string :encoding :utf-8))
  (delegate :pointer))

(defcfun (coreclr-execute-assembly
          "coreclr_execute_assembly"
          :library coreclr
          :convention :stdcall)
    :uint
  (host-handle :pointer)
  (domain-id :uint)
  (argc :int)
  (argv :pointer)
  (assembly-path (:string :encoding :utf-8))
  (exit-code :pointer))

;;; vim: ft=lisp et
