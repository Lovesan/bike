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

(define-constant +empty-dotnet-name+ (make-simple-character-string 0)
  :test #'equal)

(declaim (inline %call-ientry))
(defun %call-ientry-accessor (entry instance readp new-value)
  (declare (type invocation-entry entry)
           (type (or null dotnet-object) instance))
  (with-ientry (type name reader writer) entry
    (cond ((and readp reader instance)
           (funcall (the function reader) instance))
          ((and readp reader)
           (funcall (the function reader)))
          ((and writer instance)
           (funcall (the function writer) instance new-value))
          (writer (funcall (the function writer) new-value))
          (t (error 'accessor-resolution-error :type type
                                               :static-p (not instance)
                                               :member name
                                               :member-kind (ientry-kind entry)
                                               :kind (if readp :reader :writer))))))

(declaim (inline %ensure-ientry-accessor))
(defun %ensure-ientry-accessor (type info member-type name kind instance)
  (declare (type dotnet-type type)
           (type simple-character-string name)
           (type (member :field :property) kind)
           (type (or null dotnet-object) instance))
  (multiple-value-bind (reader-delegate reader-ptr
                        writer-delegate writer-ptr)
      (%get-accessor-trampolines info kind)
    (let* ((member-type-full-name (type-full-name member-type))
           (primitive-type (%get-primitive-type member-type-full-name))
           (lisp-type (%get-lisp-primitive-type member-type-full-name))
           (reader (when reader-delegate
                     (compile-reader-trampoline reader-ptr
                                                (not instance)
                                                primitive-type
                                                lisp-type)))
           (writer (when writer-delegate
                     (compile-writer-trampoline writer-ptr
                                                (not instance)
                                                primitive-type
                                                lisp-type))))
      (add-ientry type name kind
                  :reader reader
                  :reader-delegate reader-delegate
                  :writer writer
                  :writer-delegate writer-delegate))))

(declaim (inline %get-binding-flags))
(defun %get-binding-flags (&optional instancep methodp)
  (%%enum-to-object (resolve-type 'System.Reflection.BindingFlags)
                    (logior +binding-flags-public+
                            +binding-flags-ignore-case+
                            (if instancep
                              +binding-flags-instance+
                              (logior +binding-flags-static+
                                      +binding-flags-flatten-hierarchy+))
                            (if methodp
                              +binding-flags-invoke-method+
                              +binding-flags-default+))))

(defun %make-type-iterator (type instancep)
  (let ((list (if instancep
                (cons type (bike-vector-to-list (type-interfaces type)))
                (list type))))
    (lambda () (pop list))))

(defmacro do-type-iterator ((type-var type instancep &optional result) &body body)
  (with-gensyms (iter start)
    `(prog* ((,type-var ,type)
             (,iter (%make-type-iterator ,type-var ,instancep)))
        (declare (type (or null dotnet-type) ,type-var))
        ,start
        (setf ,type-var (funcall ,iter))
        (unless ,type-var (return ,result))
        (let ((,type-var ,type-var))
          ,@body)
        (go ,start))))

(defun %find-named-property (type name instancep &aux (empty-types (empty-types)))
  (declare (type dotnet-type type)
           (type simple-character-string name))
  (do-type-iterator (type type instancep)
    (let ((property (type-get-property type name
                                       (%get-binding-flags instancep)
                                       nil
                                       nil
                                       empty-types
                                       nil)))
      (when property (return property)))))

(defun %find-indexer (type arg-types)
  (declare (type dotnet-type type)
           (type dotnet-object arg-types))
  (do-type-iterator (type type t)
    (let* ((indexers (list-to-bike-vector (type-indexers type)
                                          :element-type 'System.Reflection.PropertyInfo))
           (selected (unless (zerop (%array-length indexers))
                       (select-property (default-binder)
                                        (%get-binding-flags t)
                                        indexers
                                        nil
                                        arg-types
                                        nil))))
      (when selected (return selected)))))

(defun %filter-generic-methods (candidates type-arg-count type-args)
  (declare (type dotnet-object candidates)
           (type positive-fixnum type-arg-count)
           (type list type-args))
  (let ((selected '()))
    (do-bike-vector (method candidates)
      (when (generic-method-definition-p method)
        (let ((args (method-generic-arguments method)))
          (when (= (%array-length args) type-arg-count)
            (push (apply #'make-generic-method method type-args) selected)))))
    (list-to-bike-vector selected :element-type 'System.Reflection.MethodBase)))

(defun %find-method (type name type-arg-count type-args args instancep)
  (declare (type dotnet-type type)
           (type simple-character-string name)
           (type non-negative-fixnum type-arg-count)
           (type list type-args)
           (type dotnet-object args))
  (do-type-iterator (type type instancep)
    (let* ((binding-flags (%get-binding-flags instancep t))
           (candidates (type-get-members
                        type name
                        (enum 'System.Reflection.MemberTypes 'Method)
                        binding-flags))
           (applicable (if type-args
                         (%filter-generic-methods candidates
                                                  type-arg-count
                                                  type-args)
                         candidates))
           (selected (ignore-errors
                      (bind-to-method (default-binder)
                                      binding-flags
                                      applicable
                                      args
                                      nil
                                      nil
                                      nil))))
      (when selected (return selected)))))

(defun %access-field (type name instance readp new-value)
  (declare (type dotnet-type type)
           (type simple-character-string name)
           (type (or null dotnet-object) instance))
  (let ((entry (get-ientry type name :field)))
    (unless entry
      (let ((info (type-get-field type name (%get-binding-flags instance))))
        (unless info
          (error 'field-resolution-error :type type
                                         :member name
                                         :static-p (not instance)))
        (setf entry (%ensure-ientry-accessor
                     type info (field-type info) name :field instance))))
    (%call-ientry-accessor entry instance readp new-value)))

(defun %access-property (type name instance readp new-value)
  (declare (type dotnet-type type)
           (type simple-character-string name)
           (type (or null dotnet-object) instance))
  (let ((entry (get-ientry type name :property)))
    (unless entry
      (let ((info (%find-named-property type name instance)))
        (unless info
          (error 'property-resolution-error :type type
                                            :member name
                                            :static-p (not instance)))
        (setf entry (%ensure-ientry-accessor
                     type info (property-type info) name :property instance))))
    (%call-ientry-accessor entry instance readp new-value)))

(defun %compile-method (info fptr &optional name doc decls)
  (declare (type dotnet-object info))
  (let* ((void-type (%get-type "System.Void" t nil))
         (return-type (method-return-type info))
         (voidp (bike-equals void-type return-type))
         (staticp (method-static-p info))
         (params (%method-parameters info))
         (iter (%make-param-array-iterator params)))
    (compile-method-trampoline fptr staticp voidp iter name doc decls)))

(defun %compile-constructor (info fptr &optional name doc decls)
  (declare (type dotnet-object info))
  (let* ((params (%method-parameters info))
         (iter (%make-param-array-iterator params)))
    (compile-method-trampoline fptr t nil iter name doc decls)))

(defun %access-indexer (instance readp new-value &rest args)
  (declare (type dotnet-object instance)
           (dynamic-extent args))
  (let* ((arg-type-count (length args))
         (type (%bike-type-of instance))
         (arg-types (mapcar #'bike-type-of args)))
    (let ((entry (get-ientry type +empty-dotnet-name+
                             :indexer :arg-type-count arg-type-count
                             :arg-types arg-types)))
      (unless entry
        (let ((info (%find-indexer type (list-to-bike-vector
                                         arg-types
                                         :element-type 'System.Type))))
          (unless info
            (error 'indexer-resolution-error :type type
                                             :member "this[*]"))
          (multiple-value-bind (reader-delegate
                                reader-ptr
                                writer-delegate
                                writer-ptr)
              (%get-accessor-trampolines info :indexer)
            (let (reader writer)
              (when reader-delegate
                (setf reader (%compile-method (property-get-method info) reader-ptr)))
              (when writer-delegate
                (setf writer (%compile-method (property-set-method info) writer-ptr)))
              (setf entry (add-ientry type +empty-dotnet-name+
                                      :indexer
                                      :arg-types arg-types
                                      :arg-type-count arg-type-count
                                      :reader reader
                                      :reader-delegate reader-delegate
                                      :writer writer
                                      :writer-delegate writer-delegate))))))
      (with-ientry (type reader writer) entry
        (cond ((and readp reader instance)
               (apply (the function reader) instance args))
              ((and readp reader)
               (apply (the function reader) args))
              ((and writer instance)
               (setf (cdr (last args)) (cons new-value nil))
               (apply (the function writer) instance args))
              (writer
               (setf (cdr (last args)) (cons new-value nil))
               (apply (the function writer) args))
              (t (error 'accessor-resolution-error
                        :type type
                        :static-p (not instance)
                        :member "this[*]"
                        :member-kind :indexer
                        :kind (if readp :reader :writer))))))))

(defun %invoke-method (type name instance type-args &rest args)
  (declare (type dotnet-type type)
           (type simple-character-string name)
           (type (or null dotnet-object) instance)
           (type list type-args args))
  (let* ((arg-type-count (length args))
         (type-arg-count (length type-args))
         (arg-types (mapcar #'bike-type-of args))
         (entry (get-ientry type name :method
                            :type-arg-count type-arg-count
                            :type-args type-args
                            :arg-type-count arg-type-count
                            :arg-types arg-types)))
    (unless entry
      (let* ((info (%find-method type name
                                 type-arg-count type-args
                                 (list-to-bike-vector args)
                                 instance)))
        (unless info
          (error 'method-resolution-error :type type
                                          :static-p (not instance)
                                          :member name
                                          :args (mapcar #'type-full-name arg-types)))
        (multiple-value-bind (delegate fptr)
            (%get-delegate-trampoline info '())
          (let ((callable (%compile-method info fptr)))
            (setf entry (add-ientry type name :method
                                    :type-arg-count type-arg-count
                                    :type-args type-args
                                    :arg-type-count arg-type-count
                                    :arg-types arg-types
                                    :reader callable
                                    :reader-delegate delegate))))))
    (with-ientry (reader) entry
      (if instance
        (apply (the function reader) (cons instance args))
        (apply (the function reader) args)))))

(defun %new (type &rest args)
  (declare (type dotnet-type type)
           (type list args))
  (let* ((arg-type-count (length args))
         (arg-types (mapcar #'bike-type-of args))
         (entry (get-ientry type +empty-dotnet-name+ :constructor
                            :arg-type-count arg-type-count
                            :arg-types arg-types)))
    (unless entry
      (let* ((applicable (type-constructors type))
             (args* (list-to-bike-vector args))
             (info (unless (zerop (%array-length applicable))
                     (bind-to-method (default-binder)
                                     (enum 'System.Reflection.BindingFlags
                                           'Public
                                           'CreateInstance
                                           'IgnoreCase
                                           'Instance)
                                     applicable
                                     args*
                                     nil
                                     nil
                                     nil))))
        (unless info
          (error 'constructor-resolution-error :type type
                                               :args (mapcar #'type-full-name arg-types)))
        (multiple-value-bind (delegate fptr)
            (%get-delegate-trampoline info '())
          (let ((callable (%compile-constructor info fptr)))
            (setf entry (add-ientry type +empty-dotnet-name+
                                    :constructor
                                    :arg-type-count arg-type-count
                                    :arg-types arg-types
                                    :reader callable
                                    :reader-delegate delegate))))))
    (with-ientry (reader) entry
      (apply (the function reader) args))))

;;; vim: ft=lisp et
