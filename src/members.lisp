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

(declaim (inline selection-binding-flags))
(defun selection-binding-flags (&optional instancep methodp optionalp)
  (%%enum-to-object (resolve-type 'System.Reflection.BindingFlags)
                    (logior +binding-flags-public+
                            +binding-flags-ignore-case+
                            (if instancep
                              +binding-flags-instance+
                              (logior +binding-flags-static+
                                      +binding-flags-flatten-hierarchy+))
                            (if methodp
                              +binding-flags-invoke-method+
                              +binding-flags-default+)
                            (if optionalp
                              +binding-flags-optional-param-binding+
                              +binding-flags-default+))))

(defun %make-type-hierarchy-iterator (type instancep)
  (let ((list (if instancep
                (cons type (bike-vector-to-list (type-interfaces type)))
                (list type))))
    (lambda () (pop list))))

(defmacro do-type-hierarchy ((type-var type instancep &optional result) &body body)
  (with-gensyms (iter start)
    `(prog* ((,type-var ,type)
             (,iter (%make-type-hierarchy-iterator ,type-var ,instancep)))
        (declare (type (or null dotnet-type) ,type-var))
        ,start
        (setf ,type-var (funcall ,iter))
        (unless ,type-var (return ,result))
        (let ((,type-var ,type-var))
          ,@body)
        (go ,start))))

(declaim (inline call-accessor-ientry))
(defun call-accessor-ientry (entry instance readp new-value)
  (declare (type invocation-entry entry)
           (type (or null dotnet-object*) instance))
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

(defun add-accessor-ientry (type info member-type name kind instancep)
  (declare (type dotnet-type type member-type)
           (type simple-character-string name)
           (type (member :field :property) kind))
  (multiple-value-bind (reader-delegate reader-ptr
                        writer-delegate writer-ptr)
      (%get-accessor-trampolines info kind)
    (let* ((member-type-full-name (type-full-name member-type))
           (primitive-type (%get-primitive-type member-type-full-name))
           (lisp-type (%get-lisp-primitive-type member-type-full-name))
           (reader (when reader-delegate
                     (compile-reader-trampoline reader-ptr
                                                (not instancep)
                                                primitive-type
                                                lisp-type)))
           (writer (when writer-delegate
                     (compile-writer-trampoline writer-ptr
                                                (not instancep)
                                                primitive-type
                                                lisp-type))))
      (add-ientry type name kind
                  :reader reader
                  :reader-delegate reader-delegate
                  :writer writer
                  :writer-delegate writer-delegate))))

(defun select-field (type name instancep &optional (errorp t) error-value)
  (declare (type dotnet-type type)
           (type simple-character-string name))
  (let ((info (type-get-field type name (selection-binding-flags instancep))))
    (or info
        (if errorp
          (error 'field-resolution-error :type type
                                         :name name
                                         :static-p (not instancep))
          error-value))))

(defun ensure-field-ientry (type name instancep)
  (declare (type dotnet-type type)
           (type simple-character-string name))
  (let ((entry (get-ientry type name :field)))
    (or entry
        (let ((info (select-field type name instancep)))
          (add-accessor-ientry type info (field-type info) name :field instancep)))))

(defun %access-field (type name instance readp new-value)
  (declare (type dotnet-type type)
           (type simple-character-string name)
           (type (or null dotnet-object*) instance))
  (call-accessor-ientry (ensure-field-ientry type name instance)
                        instance readp new-value))

(defun select-named-property (type name instancep
                              &optional (errorp t) error-value
                              &aux (empty-types (empty-types)))
  (declare (type dotnet-type type)
           (type simple-character-string name))
  (or
   (do-type-hierarchy (type type instancep)
     (let ((property (type-get-property type name
                                        (selection-binding-flags instancep)
                                        nil
                                        nil
                                        empty-types
                                        nil)))
       (when property (return property))))
   (if errorp
     (error 'property-resolution-error :type type
                                       :member name
                                       :static-p (not instancep))
     error-value)))

(defun ensure-property-ientry (type name instancep)
  (declare (type dotnet-type type)
           (type simple-character-string name))
  (let ((entry (get-ientry type name :property)))
    (or entry
        (let ((info (select-named-property type name instancep)))
          (add-accessor-ientry type info (property-type info)
                               name :property instancep)))))

(defun %access-property (type name instance readp new-value)
  (declare (type dotnet-type type)
           (type simple-character-string name)
           (type (or null dotnet-object*) instance))
  (call-accessor-ientry (ensure-property-ientry type name instance)
                        instance readp new-value))

(defun params-array-p (info)
  (declare (type dotnet-object info))
  "Returns non-NIL in case of INFO being a params[] parameter"
  (and (parameter-custom-attrubute
        info
        (resolve-type 'System.ParamArrayAttribute))
       t))

(defun make-param-array-iterator (parameters)
  (declare (type dotnet-object parameters))
  (let* ((count (%array-length parameters))
         (i 0))
    (lambda ()
      (when (< i count)
        (let* ((param (dnvref parameters i))
               (type (parameter-type param))
               (outp (parameter-out-p param))
               (refp (ref-type-p type))
               (name (parameter-name param)))
          (let* ((type (if refp (element-type-of type) type))
                 (type-name (type-full-name type)))
            (multiple-value-prog1
                (values (or name (string (gensym (string '#:arg))))
                        type
                        (%get-primitive-type type-name)
                        (%get-lisp-primitive-type type-name)
                        (if refp (if outp :out :ref) :in)
                        (params-array-p param))
              (incf i))))))))

(defun compile-method (info fptr &optional name doc decls)
  (declare (type dotnet-object info))
  (let* ((void-type (resolve-type :void))
         (return-type (method-return-type info))
         (voidp (bike-equals void-type return-type))
         (staticp (method-static-p info))
         (params (%method-parameters info))
         (iter (make-param-array-iterator params)))
    (compile-method-trampoline fptr staticp voidp iter name doc decls)))

(defun select-indexer (type arg-types &optional (errorp t) error-value)
  (declare (type dotnet-type type)
           (type list arg-types))
  (or
   (let ((arg-types (list-to-bike-vector arg-types :element-type :type)))
     (do-type-hierarchy (type type t)
       (let* ((indexers (list-to-bike-vector (type-indexers type)
                                             :element-type 'System.Reflection.PropertyInfo))
              (selected (unless (zerop (%array-length indexers))
                          (select-property (default-binder)
                                           (selection-binding-flags t nil)
                                           indexers
                                           nil
                                           arg-types
                                           nil))))
         (when selected (return selected)))))
   (if errorp
     (error 'indexer-resolution-error :type type
                                      :member "this[*]")
     error-value)))

(defun ensure-indexer-ientry (type args)
  (declare (type dotnet-type type)
           (type list args))
  (let* ((arg-count (length args))
         (arg-types (mapcar #'bike-type-of args))
         (entry (get-ientry type +empty-dotnet-name+ :indexer
                            :arg-type-count arg-count
                            :arg-types arg-types)))
    (or entry
        (let ((info (select-indexer type arg-types)))
          (multiple-value-bind (reader-delegate
                                reader-ptr
                                writer-delegate
                                writer-ptr)
              (%get-accessor-trampolines info :indexer)
            (let (reader writer)
              (when reader-delegate
                (setf reader (compile-method (property-get-method info) reader-ptr)))
              (when writer-delegate
                (setf writer (compile-method (property-set-method info) writer-ptr)))
              (add-ientry type +empty-dotnet-name+
                          :indexer
                          :arg-types arg-types
                          :arg-type-count arg-count
                          :reader reader
                          :reader-delegate reader-delegate
                          :writer writer
                          :writer-delegate writer-delegate)))))))

(defun %access-indexer (instance readp new-value args)
  (declare (type dotnet-object* instance)
           (dynamic-extent args))
  (let ((type (bike-type-of instance)))
    (with-ientry (type reader writer)
        (ensure-indexer-ientry type args)
      (cond
        ((and readp reader)
         (apply (the function reader) instance args))
        (writer
         (setf (cdr (last args)) (cons new-value nil))
         (apply (the function writer) instance args))
        (t (error 'accessor-resolution-error
                  :type type
                  :static-p (not instance)
                  :member "this[*]"
                  :member-kind :indexer
                  :kind (if readp :reader :writer)))))))

(defun filter-generic-methods (candidates type-arg-count type-args)
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

(defun select-method (type name type-arg-count type-args args arg-types instancep
                       &optional (errorp t) error-value)
  (declare (type dotnet-type type)
           (type simple-character-string name)
           (type non-negative-fixnum type-arg-count)
           (type list type-args args))
  (or
   (do-type-hierarchy (type type instancep)
     (let* ((args (list-to-bike-vector args))
            (binding-flags (selection-binding-flags instancep t))
            (candidates (type-get-members
                         type name
                         (enum 'System.Reflection.MemberTypes 'Method)
                         binding-flags))
            (applicable (if type-args
                          (filter-generic-methods candidates
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
       (when selected (return selected))))
   (if errorp
     (error 'method-resolution-error :type type
                                     :static-p (not instancep)
                                     :member name
                                     :args (mapcar #'type-full-name arg-types))
     error-value)))

(defun ensure-method-ientry (type name instancep type-args args)
  (declare (type dotnet-type type)
           (type simple-character-string name)
           (type list type-args args))
  (let* ((type-arg-count (length type-args))
         (arg-count (length args))
         (arg-types (mapcar #'bike-type-of args))
         (entry (get-ientry type name :method
                            :type-arg-count type-arg-count
                            :type-args type-args
                            :arg-type-count arg-count
                            :arg-types arg-types)))
    (or entry
        (let ((info (select-method type name
                                   type-arg-count type-args
                                   args arg-types
                                   instancep)))
          (multiple-value-bind (delegate fptr)
              (%get-delegate-trampoline info '())
            (add-ientry type name :method
                        :type-arg-count type-arg-count
                        :type-args type-args
                        :arg-type-count arg-count
                        :arg-types arg-types
                        :reader (compile-method info fptr)
                        :reader-delegate delegate))))))

(defun %invoke-method (type name instance type-args args)
  (declare (type dotnet-type type)
           (type simple-character-string name)
           (type (or null dotnet-object*) instance)
           (type list type-args args))
  (with-ientry (reader)
      (ensure-method-ientry type name instance type-args args)
    (if instance
      (apply (the function reader) instance args)
      (apply (the function reader) args))))

(defun select-event (type name instancep &optional (errorp t) error-value)
  (declare (type dotnet-type type)
           (type simple-character-string name))
  (or
   (do-type-hierarchy (type type instancep)
     (let ((event (type-get-event type
                                  name
                                  (selection-binding-flags instancep))))
       (when event (return event))))
   (if errorp
     (error 'event-resolution-error :type type
                                    :member name
                                    :static-p (not instancep))
     error-value)))

(defun ensure-event-ientry (type info name)
  (declare (type dotnet-type type)
           (type dotnet-object info)
           (type simple-character-string name))
  (let ((entry (get-ientry type name :event)))
    (or entry
        (let (add-method-delegate
              add-method-trampoline
              remove-method-delegate
              remove-method-trampoline)
          (when-let ((add-method (event-get-add-method info nil)))
            (multiple-value-bind (delegate fptr)
                (%get-delegate-trampoline add-method '())
              (setf add-method-delegate delegate
                    add-method-trampoline (compile-method add-method fptr))))
          (when-let ((remove-method (event-get-remove-method info nil)))
            (multiple-value-bind (delegate fptr)
                (%get-delegate-trampoline remove-method '())
              (setf remove-method-delegate delegate
                    remove-method-trampoline (compile-method remove-method fptr))))
          (add-ientry type name :event
                      :reader add-method-trampoline
                      :reader-delegate add-method-delegate
                      :writer remove-method-trampoline
                      :writer-delegate remove-method-delegate)))))

(defun %access-event (type name instance addp handler)
  (declare (type dotnet-type type)
           (type simple-character-string name)
           (type (or null dotnet-object*) instance)
           (type (or function-designator dotnet-delegate) handler))
  (let* ((info (select-event type name instance))
         (handler (if (and addp (not (dotnet-delegate-p handler)))
                    (let ((handler-type (event-handler-type info)))
                      (%get-delegate-for-lisp-function handler handler-type))
                    handler)))
    (with-ientry (type reader writer)
        (ensure-event-ientry type info name)
      (cond ((and addp reader instance)
             (funcall (the function reader) instance handler))
            ((and addp reader)
             (funcall (the function reader) handler))
            ((and writer instance)
             (funcall (the function writer) instance handler))
            (writer
             (funcall (the function writer) handler))
            (t (error 'accessor-resolution-error
                      :type type
                      :static-p (not instance)
                      :member name
                      :member-kind :event
                      :kind (if addp :add :remove)))))))

(defun select-constructor (type args arg-types)
  (declare (type dotnet-type type)
           (type list args arg-types))
  (let* ((applicable (type-constructors type))
         (args* (list-to-bike-vector args))
         (info (unless (zerop (%array-length applicable))
                 (ignore-errors
                  (bind-to-method (default-binder)
                                  (%%enum-to-object
                                   (resolve-type 'System.Reflection.BindingFlags)
                                   (logior +binding-flags-public+
                                           +binding-flags-create-instance+
                                           +binding-flags-ignore-case+
                                           +binding-flags-instance+))
                                  applicable
                                  args*
                                  nil
                                  nil
                                  nil)))))
    (unless info
      (error 'constructor-resolution-error
             :type type
             :args (mapcar #'type-full-name arg-types)))
    info))

(defun compile-constructor (info fptr &optional name doc decls)
  (declare (type dotnet-object info)
           (type foreign-pointer fptr))
  (let* ((params (%method-parameters info))
         (iter (make-param-array-iterator params)))
    (compile-method-trampoline fptr t nil iter name doc decls)))

(defun ensure-constructor-ientry (type args)
  (let* ((arg-types (mapcar #'bike-type-of args))
         (arg-type-count (length arg-types))
         (entry (get-ientry type +empty-dotnet-name+ :constructor
                            :arg-type-count arg-type-count
                            :arg-types arg-types)))
    (or entry
        (let ((info (select-constructor type args arg-types)))
          (multiple-value-bind (delegate fptr)
              (%get-delegate-trampoline info '())
            (add-ientry type +empty-dotnet-name+ :constructor
                        :arg-type-count arg-type-count
                        :arg-types arg-types
                        :reader (compile-constructor info fptr)
                        :reader-delegate delegate))))))

(defun %new (type args)
  (declare (type dotnet-type type)
           (type list args))
  (with-ientry (reader) (ensure-constructor-ientry type args)
    (apply (the function reader) args)))

;;; vim: ft=lisp et
