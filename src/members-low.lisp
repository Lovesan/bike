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

(defstruct (method-entry (:constructor %method-entry (info
                                                      return-type
                                                      type-arg-count
                                                      type-args
                                                      arg-count
                                                      args
                                                      arg-types
                                                      arg-type-count
                                                      delegate
                                                      delegate-pointer
                                                      callable
                                                      next))
                         (:predicate method-entry-p)
                         (:conc-name %method-entry-))
  "Represents method description"
  (info (required-slot) :type dotnet-object)
  (return-type (required-slot) :type dotnet-object)
  (type-arg-count 0 :type non-negative-fixnum :read-only t)
  (type-args '() :type list :read-only t)
  (arg-count 0 :type non-negative-fixnum :read-only t)
  (args '() :type list :read-only t)
  (arg-types '() :type list :read-only t)
  (arg-type-count 0 :type non-negative-fixnum :read-only t)
  (delegate nil :type (or null dotnet-delegate) :read-only t)
  (delegate-pointer (null-pointer) :type foreign-pointer :read-only t)
  (callable #'identity :type function :read-only t)
  (instances '() :type list)
  (next nil :type (or null method-entry)))

(defun %method-entry-generic-p (method-entry)
  (declare (type method-entry method-entry))
  (> (%method-entry-type-arg-count method-entry) 0))

(defstruct (generic-method-entry
            (:constructor %gme-entry (type-arg-count
                                      type-args
                                      delegate
                                      delegate-pointer
                                      callable
                                      next))
            (:predicate generic-method-entry-p)
            (:conc-name %gme-entry-))
  "Represents constructed generic method description"
  (type-arg-count 0 :type non-negative-fixnum
                    :read-only t)
  (type-args '() :type list :read-only t)
  (delegate nil :type (or null dotnet-delegate) :read-only t)
  (delegate-pointer (null-pointer) :type foreign-pointer :read-only t)
  (callable #'identity :type function)
  (next nil :type (or null generic-method-entry)))

(deftype parameter-direction () '(member :in :out :ref))

(defstruct (parameter-entry (:constructor %param-entry (info
                                                        name
                                                        type
                                                        primitive-type
                                                        optional-p
                                                        direction
                                                        position
                                                        params-p))
                            (:predicate parameter-entry-p)
                            (:conc-name %param-entry-))
  "Represents parameter description"
  (info (required-slot) :type dotnet-object :read-only t)
  (name (required-slot) :type string :read-only t)
  (primitive-type nil :type symbol :read-only t)
  (type (required-slot) :type dotnet-type :read-only t)
  (optional-p nil :type boolean :read-only t)
  (direction :in :type parameter-direction :read-only t)
  (position 0 :type non-negative-fixnum :read-only t)
  (params-p nil :type boolean :read-only t))

(defstruct (field-entry (:constructor %field-entry (info
                                                    name
                                                    staticp
                                                    primitive-type
                                                    type
                                                    reader
                                                    reader-delegate
                                                    reader-pointer
                                                    writer
                                                    writer-delegate
                                                    writer-pointer))
                        (:predicate field-entry-p)
                        (:conc-name %field-entry-))
  "Represents field description"
  (info (required-slot) :type dotnet-object :read-only t)
  (name "" :type string :read-only t)
  (staticp nil :type boolean :read-only t)
  (primitive-type nil :type symbol :read-only t)
  (type (required-slot) :type dotnet-object :read-only t)
  (reader nil :type (or null function) :read-only t)
  (reader-delegate nil :type (or null dotnet-object) :read-only t)
  (reader-pointer (null-pointer) :type foreign-pointer :read-only t)
  (writer nil :type (or null function) :read-only t)
  (writer-delegate nil :type (or null dotnet-object) :read-only t)
  (writer-pointer (null-pointer) :type foreign-pointer :read-only t))

(defstruct (property-entry (:constructor %property-entry (info
                                                          name
                                                          staticp
                                                          primitive-type
                                                          type
                                                          reader
                                                          reader-delegate
                                                          reader-pointer
                                                          writer
                                                          writer-delegate
                                                          writer-pointer))
                           (:predicate property-entry-p)
                           (:conc-name %property-entry-))
  "Represents non-indexer property description"
  (info (required-slot) :type dotnet-object :read-only t)
  (name "" :type string :read-only t)
  (staticp nil :type boolean :read-only t)
  (primitive-type nil :type symbol :read-only t)
  (type (required-slot) :type dotnet-object :read-only t)
  (reader nil :type (or null function) :read-only t)
  (reader-delegate nil :type (or null dotnet-object) :read-only t)
  (reader-pointer (null-pointer) :type foreign-pointer :read-only t)
  (writer nil :type (or null function) :read-only t)
  (writer-delegate nil :type (or null dotnet-object) :read-only t)
  (writer-pointer (null-pointer) :type foreign-pointer :read-only t))

(defstruct (indexer-entry (:constructor %indexer-entry (info
                                                        name
                                                        primitive-type
                                                        type
                                                        arg-count
                                                        args
                                                        reader
                                                        reader-delegate
                                                        reader-pointer
                                                        writer
                                                        writer-delegate
                                                        writer-pointer))
                          (:predicate indexer-entry-p)
                          (:conc-name %indexer-entry-))
  "Represents indexer property description"
  (info (required-slot) :type dotnet-object :read-only t)
  (name "" :type string :read-only t)
  (primitive-type nil :type symbol :read-only t)
  (type (required-slot) :type dotnet-object :read-only t)
  (arg-count 0 :type non-negative-fixnum)
  (args (required-slot) :type cons)
  (reader nil :type (or null function) :read-only t)
  (reader-delegate nil :type (or null dotnet-object) :read-only t)
  (reader-pointer (null-pointer) :type foreign-pointer :read-only t)
  (writer nil :type (or null function) :read-only t)
  (writer-delegate nil :type (or null dotnet-object) :read-only t)
  (writer-pointer (null-pointer) :type foreign-pointer :read-only t))

(define-constant +primitive-types+ '(("System.Char" . dnchar)
                                     ("System.Boolean" . :bool)
                                     ("System.Int8" . :int8)
                                     ("System.UInt8" . :uint8)
                                     ("System.Int16" . :int16)
                                     ("System.UInt16" . :uint16)
                                     ("System.Int32" . :int32)
                                     ("System.UInt32" . :uint32)
                                     ("System.Int64" . :int64)
                                     ("System.UInt64" . :uint64)
                                     ("System.Single" . :float)
                                     ("System.Double" . :double)
                                     ("System.IntPtr" . :pointer))
  :test #'equal)

;;; vim: ft=lisp et
