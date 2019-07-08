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

(defvar *default-assemblies* (%get-trusted-assembly-names)
  "A list of assembly names that are loaded by default,
 including on lisp image restore")

(declaim (type list *default-assembles*))

(defun load-assembly-from (path)
  (declare (type (or pathname string) path))
  "Loads an assembly from a file designated by PATH"
  (let* ((path (uiop:truename* path))
         (assembly-type-name "System.Reflection.Assembly")
         (assembly-type (resolve-type assembly-type-name)))
    (%invoke assembly-type t () "LoadFrom" (uiop:native-namestring path))))

(defun import-assembly (assembly-designator)
  (declare (type (or dotnet-object string-designator)
                 assembly-designator))
  "Loads an assembly designated by ASSEMBLY-STRING"
  (let* ((assembly (if (dotnet-object-p assembly-designator)
                     assembly-designator
                     (load-assembly assembly-designator)))
         (types (%invoke assembly nil () "GetExportedTypes")))
    (with-type-table (data ns lock)
      (with-write-lock (lock)
        (do-bike-vector (type types)
          (unless (compiler-generated-member-p type)
            (%ensure-type-entry type))))))
  (values))

(defun get-loaded-assemblies ()
  "Returns a list of currently loaded assemblies"
  (let* ((loader-ctx-type-name "System.Runtime.Loader.AssemblyLoadContext")
         (loader-ctx-type (resolve-type loader-ctx-type-name))
         (assemblies (%invoke loader-ctx-type t () "GetLoadedAssemblies")))
    (bike-vector-to-list assemblies)))

(defun import-loaded-assemblies ()
  "Imports all currently loaded assemblies into type cache"
  (dolist (assembly (get-loaded-assemblies))
    (unless (%get-property assembly nil "IsDynamic")
      (import-assembly assembly)))
  (values))

(defun %init-type-table (namespaces types aliases)
  (setf *type-table* (%type-table))
  ;; load default assemblies
  (dolist (assembly-string *default-assemblies*)
    (load-assembly assembly-string))
  (import-loaded-assemblies)
  (use-type-alias :object "System.Object")
  (use-type-alias :string "System.String")
  (use-type-alias :char "System.Char")
  (use-type-alias :bool "System.Boolean")
  (use-type-alias :float "System.Single")
  (use-type-alias :double "System.Double")
  (use-type-alias :byte "System.Byte")
  (use-type-alias :sbyte "System.SByte")
  (use-type-alias :short "System.Int16")
  (use-type-alias :ushort "System.UInt16")
  (use-type-alias :int "System.Int32")
  (use-type-alias :uint "System.UInt32")
  (use-type-alias :long "System.Int64")
  (use-type-alias :ulong "System.UInt64")
  (use-type-alias :decimal "System.Decimal")
  (with-type-table (data (ns namespaces) lock (new-aliases aliases))
    (with-write-lock (lock)
      (setf ns namespaces)
      (loop :for (alias . designator) :in aliases
            :do (setf (gethash alias new-aliases) designator))
      (loop :for (full-name . qname) :in types
            :do (resolve-type qname))))
  (values))

(defun %reload-type-table ()
  (multiple-value-bind (namespaces types aliases)
      (when *type-table*
        (with-type-table (data namespaces aliases)
          (values namespaces
                  (let ((types '()))
                    (maphash (lambda (full-name entry)
                               (with-type-entry ((qname qualified-name)) entry
                                 (when qname
                                   (push (cons full-name qname)
                                         types))))
                             data)
                    (nreverse types))
                  (hash-table-alist aliases))))
    (%init-type-table namespaces types aliases))
  (values))

(defun clear-type-cache ()
  "Clears type and namespace cache and restores it to current defaults."
  (%init-type-table '() '() '()))

;;; vim: ft=lisp et
