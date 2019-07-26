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

(defun invoke (target method &rest args)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type dotnet-method-designator method)
           (dynamic-extent args))
  "Invokes a method designated by METHOD on a TARGET which can
 either be a type specifier, in which case a static method is
 invoked, or an instance, which would lead to instance method
 invocation."
  (let* ((instancep (dotnet-object-p target))
         (type (if instancep
                 (%bike-type-of target)
                 (resolve-type target)))
         (genericp (consp method))
         (name (%mknetsym (if genericp (car method) method)))
         (type-args (when genericp
                      (unless (cdr method)
                        (error 'generic-argument-count-mismatch
                               :token name
                               :value ""
                               :position 0
                               :datum method))
                      (mapcar #'resolve-type (cdr method)))))
    (apply #'%invoke-method type (%mknetsym name) (and instancep target) type-args args)))

(defun property (target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Retrieves a value of property named NAME from a TARGET, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access."
  (let* ((instancep (dotnet-object-p target))
         (type (if instancep
                 (%bike-type-of target)
                 (resolve-type target))))
    (%access-property type (%mknetsym name) (and instancep target) t nil)))

(defun (setf property) (new-value target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Changes a value of property named NAME of a TARGET, which
 can either be a type specifier, in which case a static property
 is accessed, or an instance, which would lead to instance property
 access."
  (let* ((instancep (dotnet-object-p target))
         (type (if instancep
                 (%bike-type-of target)
                 (resolve-type target))))
    (%access-property type (%mknetsym name) (and instancep target) nil new-value)))

(defun ref (target index &rest indices)
  (declare (type dotnet-object target)
           (dynamic-extent indices))
  "Retrieves a value of an indexer from a TARGET, which
 must be an instance."
  (apply #'%access-indexer target t nil (cons index indices)))

(defun (setf ref) (new-value target index &rest indices)
  (declare (type dotnet-object target)
           (dynamic-extent indices))
  "Changes a value of an indexer from a TARGET, which
 must be an instance."
  (apply #'%access-indexer target nil new-value (cons index indices)))

(defun field (target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Retrieves a value of field named NAME from TARGET, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access."
  (let* ((instancep (dotnet-object-p target))
         (type (if instancep
                 (%bike-type-of target)
                 (resolve-type target))))
    (%access-field type (%mknetsym name) (and instancep target) t nil)))

(defun (setf field) (new-value target name)
  (declare (type (or dotnet-object dotnet-type-designator) target)
           (type string-designator name))
  "Changes a value of field named NAME of a TARGET, which
 can either be a type specifier, in which case a static field
 is accessed, or an instance, which would lead to instance field
 access."
  (let* ((instancep (dotnet-object-p target))
         (type (if instancep
                 (%bike-type-of target)
                 (resolve-type target))))
    (%access-field type (%mknetsym name) (and instancep target) nil new-value))
  new-value)

(defun new (type &rest args)
  (declare (type dotnet-type-designator type)
           (dynamic-extent args))
  "Creates an instance of the specified TYPE.
In case of the TYPE being a delegate type, first,
 and only, argument, must be a lisp function-like
 object."
  (let ((type (resolve-type type)))
    (if (delegate-type-p type)
      (let ((lisp-function (first args)))
        (declare (type (or symbol function) lisp-function))
        (%get-delegate-for-lisp-function lisp-function type))
      (apply #'%new type args))))

(defun unbox (object)
  "Attempts to unbox an OBJECT into lisp object"
  (if (dotnet-object-p object)
    (let ((code (%get-full-type-code (%dotnet-object-handle object))))
      (values (%unbox (%dotnet-object-handle object) code t)))
    object))

(defun box (object &optional (type nil typep))
  (declare (type (or null dotnet-type-designator) type))
  "Makes a boxed representation of an OBJECT"
  (let ((boxed (if (dotnet-object-p object)
                 object
                 (let* ((ptr (%box object))
                        (code (%get-full-type-code ptr)))
                   (%get-boxed-object ptr (ash code -8) nil)))))
    (if typep
      (invoke 'BikeInterop.TypeCaster (list 'Cast type) object)
      boxed)))

(defmacro with-disposable ((var object) &body body)
  "Binds VAR to an IDisposable OBJECT during the execution of BODY forms.
 OBJECT is disposed right before BODY forms exit."
  `(let ((,var (%to-disposable ,object)))
     (unwind-protect
          (let ((,var ,var))
            ,@body)
       (%dispose ,var))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %collect-disposable-bindings (specs seqp)
    (loop :for (var object) :in specs
          :for real-var = (if seqp var (gensym))
          :collect `(,real-var (%to-disposable ,object))
            :into bindings
          :collect `(,var ,real-var)
            :into inner-bindings
          :collect `(when ,real-var (%dispose ,real-var))
            :into cleanups
          :finally (return (values bindings
                                   inner-bindings
                                   cleanups))))
  (defun %expand-disposable-bindings (specs body &optional seqp)
    (multiple-value-bind (bindings inner-bindings cleanups)
        (%collect-disposable-bindings specs seqp)
      (labels ((expand (bindings cleanups)
                 (if (endp bindings)
                   `(,(if seqp 'let* 'let) ,inner-bindings
                     (declare (ignorable ,@(mapcar #'car inner-bindings)))
                     ,@body)
                   `(let (,(first bindings))
                      (unwind-protect
                           ,(expand (rest bindings)
                                    (rest cleanups))
                        ,(first cleanups))))))
        (expand bindings cleanups)))))

(defmacro with-disposables ((&rest specs) &body body)
  "Binds variables to IDisposable objects during the execution of BODY forms.
 Variables are bound by means of LET form.
 Objects are disposed right before BODY forms exit."
  (%expand-disposable-bindings specs body))

(defmacro with-disposables* ((&rest specs) &body body)
  "Binds variables to IDisposable objects during the execution of BODY forms.
 Variables are bound in a sequence, as in LET* form.
 Objects are disposed right before BODY forms exit."
  (%expand-disposable-bindings specs body t))

(defmacro do-enumerable ((var enumerable &optional result) &body body)
  "A direct analogue of C# `foreach' statement.
 Binds VAR to each element of an ENUMERABLE and executes BODY forms
  on each iteration.
 Returns a result of an execution of RESULT form."
  (with-gensyms (e start)
    `(prog ((,e (%get-enumerator ,enumerable))
            ,var)
        ,start
        (unless (%enumerator-move-next ,e) (return ,result))
        (setf ,var (%enumerator-current ,e))
        (let ((,var ,var))
          ,@body)
        (go ,start))))

(defmacro exception-bind ((&rest bindings) &body body)
  "(EXCEPTION-BIND ( {(exception-type handler)}* ) body)

Executes body in a dynamic context where the given handler bindings are in
effect. Each handler must take the exception being signalled as an argument.
The bindings are searched first to last in the event of a thrown exception"
  (with-gensyms (err err-type)
    (multiple-value-bind (bindings cases)
        (loop :for (type handler) :in bindings
              :for type-var = (gensym)
              :collect `(,type-var (resolve-type ',type))
                :into bindings
              :collect `((or (bike-equals ,err-type ,type-var)
                             (bike-subclass-p ,err-type ,type-var))
                         (funcall ,handler ,err))
                :into cases
              :finally (return (values bindings cases)))
      `(let ,bindings
         (handler-bind ((dotnet-error
                          (lambda (,err)
                            (declare (type dotnet-error ,err))
                            (let* ((,err (dotnet-error-object ,err))
                                   (,err-type (%bike-type-of ,err)))
                              (declare (ignorable ,err-type)
                                       (type dotnet-exception ,err))
                              (cond ,@cases)))))
           ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %get-no-error-handler (cases)
    (let (no-err-handler err-cases)
      (dolist (spec cases)
        (destructuring-bind (&whole form spec (&rest args) &body body)
            spec
          (if (member spec '(:no-error :no-exception))
            (progn (when no-err-handler
                     (error "~s case is already specified" :no-exception))
                   (setf no-err-handler `(lambda ,args ,@body)))
            (push form err-cases))))
      (values no-err-handler
              (nreverse err-cases))))
  (defun %process-exception-cases (error-return cases)
    (let ((flet-bindings)
          (handler-bindings))
      (dolist (spec cases)
        (destructuring-bind (typespec (&optional (var (gensym)))
                             &body body)
            spec
          (let* ((handler-var (gensym))
                 (handler `(,handler-var (,var)
                                         (declare (type dotnet-exception ,var)
                                                  (ignorable ,var))
                                         (return-from ,error-return (locally ,@body)))))
            (push handler flet-bindings)
            (push `(,typespec (function ,handler-var)) handler-bindings))))
      (values flet-bindings
              (nreverse handler-bindings)))))

(defmacro exception-case (form &body cases)
  "(EXCEPTION-CASE form { (exception-type ([var]) body) }* )

Executes FORM in a context with handlers established for the condition types. A
peculiar property allows type to be :NO-EXCEPTION. If such a clause occurs, and
form returns normally, all its values are passed to this clause as if by
MULTIPLE-VALUE-CALL. The :NO-EXCEPTION clause accepts more than one var
specification."
  (multiple-value-bind (no-err-handler cases)
      (%get-no-error-handler cases)
    (if no-err-handler
      (with-gensyms (normal-return)
        `(multiple-value-call ,no-err-handler
           (block ,normal-return
             (exception-case
                 (return-from ,normal-return ,form)
               ,@cases))))
      (with-gensyms (error-return)
        (multiple-value-bind
              (flet-bindings handler-bindings)
            (%process-exception-cases error-return cases)
          `(block ,error-return
             (flet ,flet-bindings
               (exception-bind ,handler-bindings ,form))))))))

;;; vim: ft=lisp et
