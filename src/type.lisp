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

(defstruct (type-table (:constructor %type-table ())
                       (:predicate type-table-p)
                       (:conc-name %type-table-))
  "Represents a storage for loaded types and used namespaces"
  (data (make-hash-table :test 'equal) :type hash-table
                                       :read-only t)
  (namespaces '() :type list)
  (aliases (make-hash-table :test 'equal) :type hash-table
                                          :read-only t)
  (lock (make-rwlock) :type rwlock
                      :read-only t))

(defstruct (type-entry (:constructor %type-entry (type qualified-name))
                       (:predicate type-entry-p)
                       (:conc-name %type-entry-))
  "Represents an entry in the type table"
  (type nil :type dotnet-type)
  (qualified-name nil :type (or null string)))

(deftype dotnet-type-designator ()
  '(or dotnet-type string-designator (cons string-designator list)))

#+sbcl
(sb-ext:defglobal *type-table* nil)

#-sbcl
(defvar *type-table* nil)

(defmacro with-type-table ((data-var ns-var lock-var
                            &optional (aliases-var (gensym)))
                           &body body)
  (with-gensyms (table)
    `(let ((,table *type-table*))
       (declare (type type-table ,table))
       (with-accessors ((,data-var %type-table-data)
                        (,ns-var %type-table-namespaces)
                        (,lock-var %type-table-lock)
                        (,aliases-var %type-table-aliases))
           ,table
         ,@body))))

(declaim (inline %net-symbolify))
(defun %mknetsym (what)
  (declare (type string-designator what))
  (string-upcase (string what)))

(defun use-namespace (namespace)  
  (declare (type string-designator namespace))
  "Adds a NAMESPACE to the list of used namespaces."
  (let ((namespace-prefix (uiop:strcat (%mknetsym namespace) ".")))
    (with-type-table (data namespaces lock)
      (with-write-lock (lock)
        (pushnew namespace-prefix namespaces :test #'equal))))
  (values))

(defun unuse-namespace (namespace)  
  (declare (type string-designator namespace))
  "Removes a NAMESPACE from the list of used namespaces."
  (let ((namespace-prefix (uiop:strcat (%mknetsym namespace) ".")))
    (with-type-table (data namespaces lock)
      (with-write-lock (lock)
        (removef namespaces namespace-prefix
                 :test #'equal))))
  (values))

(defun unuse-all-namespaces ()
  "Clears the list of used namespaces."
  (with-type-table (data namespaces lock)
    (with-write-lock (lock)
      (setf namespaces '()))))

(defun use-type-alias (alias type)
  (declare (type string-designator alias)
           (type dotnet-type-designator type))
  "Adds an ALIAS for TYPE in a type cache. TYPE designator must
 not contain actual type objects if current lisp image
 is to be restored."
  (with-type-table (data ns lock aliases)
    (with-write-lock (lock)
      (setf (gethash (%mknetsym alias) aliases) type)))
  (values))

(defun unuse-type-alias (alias)
  (declare (type string-designator alias))
  "Removes an alias from the current type cache."
  (with-type-table (data ns lock aliases)
    (with-write-lock (lock)
      (remhash (%mknetsym alias) aliases)))
  (values))

(defun unuse-all-type-aliases ()
  "Clears current type aliases cache."
  (with-type-table (data ns lock aliases)
    (with-write-lock (lock)
      (clrhash aliases)))
  (values))

(defun %register-type (type qualified)
  (declare (type dotnet-type type))
  (with-type-table (data ns lock)
    (let ((full-name (%invoke-member type "get_FullName")))
      (setf (gethash (%mknetsym full-name) data)
            (%type-entry
             type
             (when qualified
               (%invoke-member type "get_AssemblyQualifiedName"))))
      type)))

(defun %import-type (type assembly)
  (with-type-table (data ns lock)
    (cond ((dotnet-type-p type)
           (%register-type type assembly))
          ((typep type 'string-designator)
           (let ((type (%mknetsym type)))
             (%import-type
              (check-exception
               (if (dotnet-object-p assembly)
                 (%get-type-by-name type t assembly)
                 (%get-type-by-name type t nil)))
              assembly)))
          ((consp type)
           (multiple-value-bind (type types)
               (loop :with generic-type = (%mknetsym (first type))  
                     :for type-arg :in (rest type)
                     :collect (%import-type type-arg nil) :into types
                     :finally (return (values generic-type types)))             
             (%import-type
              (check-exception
               (%get-generic-type-by-name
                type t (and (dotnet-object-p assembly) assembly) types))
              assembly)))
          (t (error 'invalid-type-designator :datum type)))))

(defun import-type (type &optional assembly)
  (declare (type dotnet-type-designator type)
           (type (or dotnet-object boolean) assembly))
  "Imports a .Net type designated by a TYPE specifier.
ASSEMBLY can be either:
a) A .Net System.Reflection.Assembly object, in which case a type would be loaded from it, 
b) T, in which case resulting type would be imported with its fully qualified name
c) NIL, in which case a type would be registered only by its .FullName"
  (with-type-table (data ns lock)
    (with-write-lock (lock)
      (%import-type type assembly))))

(declaim (inline %%resolve-type))
(defun %%resolve-type (name)
  (with-type-table (data ns lock)
    (declare (type string name))
    (let ((entry (gethash name data)))
      (and entry (%type-entry-type entry)))))

(defun %resolve-generic-type (type args errorp)
  (declare (type string-designator type)
           (type list args))
  (if (endp args)
    (%resolve-type type errorp)
    (loop :for i :of-type fixnum :from 0
          :for arg :in args
          :for resolved = (%resolve-type arg errorp)
          :if (not resolved)
            :do (return)
          :else
            :collect resolved :into type-args
          :finally
             (let ((definition
                     (%resolve-type (format nil "~a`~D" type i) errorp)))
               (when definition
                 (return (check-exception
                          (%make-generic-type definition type-args))))))))

(defun %resolve-type (type errorp)
  (with-type-table (data ns lock aliases)
    (cond ((dotnet-type-p type) type)
          ((typep type 'string-designator)
           (let* ((type (%mknetsym type))
                  (alias (gethash type aliases)))
             (if alias
               (%resolve-type alias errorp)
               (or (%%resolve-type type)
                   (loop :for namespace :in ns
                         :for prefixed = (uiop:strcat namespace type)
                         :for resolved = (%%resolve-type prefixed)
                         :when resolved :do (return resolved)
                           :finally (when errorp
                                      (error 'type-resolution-error :datum type)))))))
          ((consp type)
           (destructuring-bind (type &rest rest) type
             (let ((def-type (%mknetsym type)))
               (if (string= def-type "ARRAY")
                 (destructuring-bind (elt-type &optional (rank 1)) rest
                   (declare (type (integer 1 32) rank))
                   (let ((elt-type (%resolve-type elt-type errorp)))
                     (when elt-type
                       (check-exception (%make-array-type elt-type rank)))))
                 (%resolve-generic-type def-type rest errorp)))))
          (t (when errorp (error 'invalid-type-designator :datum type))))))

(defun resolve-type (type &optional (errorp t) (error-value nil))
  (declare (type dotnet-type-designator type))
  "Resolves a .Net type designated by TYPE specifier from an
 internal type cache."
  (with-type-table (data ns lock)
    (with-read-lock (lock)
      (or (%resolve-type type errorp)
          error-value))))

(defun load-assembly (assembly-string)
  (declare (type string-designator assembly-string))
  "Loads an assembly designated by ASSEMBLY-STRING"
  (let* ((assembly-string (string assembly-string))
         (assembly-type-name "System.Reflection.Assembly")
         (assembly-type (or (resolve-type assembly-type-name nil)
                            (import-type assembly-type-name))))
    (check-exception
     (%invoke-static assembly-type
                     "Load"
                     assembly-string))))

(defun import-assembly (assembly-designator)
  (declare (type (or dotnet-object string-designator)
                 assembly-designator))
  "Loads an assembly designated by ASSEMBLY-STRING"
  (let* ((assembly (if (dotnet-object-p assembly-designator)
                     assembly-designator
                     (load-assembly assembly-designator)))
         (types (check-exception
                 (%invoke-member assembly "GetTypes"))))
    (with-type-table (data ns lock)
      (with-write-lock (lock)
        (dotimes (i (%array-length types))
          (%import-type (%net-vref types i)
                        assembly))))
    (values)))

(defun get-loaded-assemblies ()
  "Returns a list of currently loaded assemblies"
  (let* ((loader-ctx-type-name "System.Runtime.Loader.AssemblyLoadContext")
         (loader-ctx-type (or (resolve-type loader-ctx-type-name nil)
                              (import-type loader-ctx-type-name)))
         (assemblies (check-exception
                      (%invoke-static loader-ctx-type "GetLoadedAssemblies")))
         (result '()))
    (dotimes (i (%array-length assemblies))
      (push (%net-vref assemblies i) result))
    (nreverse result)))

(defun import-loaded-assemblies ()
  "Imports all currently loaded assemblies into type cache"
  (dolist (assembly (get-loaded-assemblies))
    (unless (check-exception
             (%invoke-member assembly "get_IsDynamic"))
      (import-assembly assembly)))
  (values))

(defun %init-type-table (namespaces types aliases)
  (setf *type-table* (%type-table))
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
  (with-type-table (data ns lock new-aliases)
    (with-write-lock (lock)
      (setf ns namespaces)
      (loop :for (alias . designator) :in aliases
            :do (setf (gethash alias new-aliases) designator))
      (loop :for (full-name . qname) :in types
            :do (if qname
                  (%import-type qname t)
                  (%import-type full-name nil)))))
  (values))

(defun %reload-type-table ()
  (multiple-value-bind (namespaces types aliases)
      (when *type-table*
        (with-type-table (data namespaces lock aliases)
          (values namespaces
                  (let ((types '()))
                    (maphash (lambda (full-name entry)
                               (let ((qname (%type-entry-qualified-name entry)))
                                 (push (cons full-name qname)
                                       types)))
                             data)
                    (nreverse types))
                  (hash-table-alist aliases))))
    (%init-type-table namespaces types aliases))
  (values))

(defun clear-type-cache ()
  "Clears type and namespace cache and restores it to current defaults."
  (%init-type-table '() '() '()))

;;; vim: ft=lisp et
