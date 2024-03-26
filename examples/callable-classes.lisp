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

(in-package #:bike-examples)

(uiop:define-package #:bike-examples/callable-classes
  (:use #:cl #:bike #:uiop #:named-readtables))

(in-package #:bike-examples/callable-classes)

(in-readtable bike-syntax)

(use-namespace '(System
                 System.Collections
                 System.Collections.Generic
                 System.Linq))

;;; An example of callable class usage.
;;; An implementation of read-only sequence interfaces for Lisp sequences.

(define-dotnet-callable-class (sequence-enumerator (:interfaces (IEnumerator :object))) ()
  "An implementation of IEnumerator<object> for CL sequences"
  ;; These 3 forms describe standard CLOS slots.
  ;; 1. A sequence which we enumerate
  (seq :initarg :sequence :reader se-sequence)
  ;; 2. An index inside the sequence
  (idx :initform -1 :reader se-index)
  ;; 3. An object which created this enumerator
  (parent :initarg :parent :reader se-parent)
  ;; .NET slot definitions start with a keyword.
  ;; The following is a .NET property slot definition in the `long' form.
  ;;  (there's also `short' form which are used for `auto' properties)
  ;; .NET name for the property is generated based on the lisp name,
  ;;   so for ex, `CURRENT' becomes `Current'
  (:property current :object
   ;; The first element of the accessor form must be one of:
   ;;   `:get', `:getter', `:set', `:setter'.
   ;; The first two are equivalent and are used to define property getters.
   ;; The latter two are also equivalent and are used to define property setters.
   ;; The second element in the accessor form is the name of a lisp function
   ;;   which would be generated for this accessor.
   ;; Other elements of the form represent `&body' of the function.
   (:get se-get-current (let ((index (se-index this))
                              ;; the macro interns `this' symbol into the current package
                              (len (length (se-sequence this))))
                          (cond
                            ((minusp index)
                             (dotnet-error 'InvalidOperationException
                                           "Enumeration has not started yet."))
                            ((>= index len)
                             (dotnet-error 'InvalidOperationException
                                           "Enumeration already finished."))
                            (t (elt (se-sequence this) index))))))
  ;; `:method' generates lisp functions using `defun'
  ;; `:defmethod' would utilize `defmethod'
  (:method move-next :bool ()
    "Advances enumerator position within sequence"
    (with-slots (seq idx) this
      (let ((len (length seq))
            (next-idx (1+ idx)))
        (cond ((>= next-idx len)
               (setf idx len)
               nil)
              (t (setf idx next-idx)
                 t)))))
  ;; You can also override the generated .NET method name
  (:method ((se-reset "Reset")) :void ()
    "Resets enumerator position within sequence"
    (with-slots (idx) this
      (setf idx -1)))
  (:defmethod dispose :void ()
    "Disposes an enumerator"
    (with-slots (live-enumerators) (se-parent this)
      (setf live-enumerators (remove this live-enumerators)))
    (values)))

(define-dotnet-callable-class (sequence-enumerable-base
                               (:interfaces IEnumerable)
                               ;; `defclass' options like `:default-initargs' go here
                               (:default-initargs
                                :sequence (error "Please supply a :sequence."))) ()
  "An implementation of IEnumerable for lisp sequences."
  ;; Collect lisp enumerator instances in a list
  ;;   to prevent them from being garbage collected while their proxies are in use.
  (live-enumerators :initform '())
  ;; The sequence we wrap.
  (seq :initarg :sequence :reader se-sequence)
  ;; The only method on the IEnumerable interface.
  (:defmethod get-enumerator IEnumerator ()
    "Retrieves an enumerator for the sequence."
    (with-slots (seq live-enumerators) this
      (let ((e (make-instance 'sequence-enumerator :parent this
                                                   :sequence seq)))
        (push e live-enumerators)
        e))))

(define-dotnet-callable-class (sequence-enumerable
                               (:interfaces (IEnumerable :object)))
    (sequence-enumerable-base)
  "An implemenetation of generic IEnumerable<object> for lisp sequences"
  (:defmethod get-enumerator (IEnumerator :object) ()
    "Retrieves an enumerator for the sequence."
    ;; Since we are using `:defmethod', we can use CLOS stuff here.
    (call-next-method)))

(define-dotnet-callable-class (read-only-collection
                               (:interfaces (IReadOnlyCollection :object)))
    (sequence-enumerable)
  "A read-only wrapper for a lisp sequence."
  ;; Define a generic function for the getter
  (:property (count :defmethodp t) :int
   (:get count-of (length (se-sequence this)))))

(define-dotnet-callable-class (read-only-list
                               (:interfaces (IReadOnlyList :object)))
    (read-only-collection)
  "A read-only wrapper for a lisp sequence."
  ;; Indexer slots represent properties which take arguments.
  ;; Those are defined as 'T this[..args...]{ get ... set ...}' in C#.
  ;; There can only be one indexer per class.
  ;; Default name for an indexer slot is `item', interned into the current package.
  ;; Indexer setters also intern `value' symbol into the current package.
  (:indexer :object ((idx :int))
   (:get rolref (elt (se-sequence this) idx))))

(defun bike-examples:callable-enumerable-example ()
  (let* ((list (list 1 2 3 4 5))
         (wrapper (make-instance 'read-only-list :sequence list))
         (selector (new '(Func :object :int) #'1+))
         (sum [:Enumerable Sum [:Enumerable (Select :object :int) wrapper selector]]))
    (assert (= 20 sum))
    sum))

;;; Here's another example of `define-dotnet-callable-class' syntax

(define-dotnet-callable-class (my-class
                               ;; interface list
                               (:interfaces)
                               ;; A class that proxy should inherit from.
                               (:base-type :object)
                               ;; you can also override the metaclass object.
                               ;; But remember that your metaclass
                               ;;   must inherit from `dotnet-callable-class'.
                               (:metaclass dotnet-callable-class)
                               ;; Other `defclass' options also go here.
                               (:default-initargs
                                :auto-property nil))
    ;; CLOS superclasses
    ()
  ;; Simple CLOS slot.
  (values :initform (make-hash-table :test #'equal) :accessor my-class-values)
  ;; Auto properties use CLOS slot as a backing field.
  ;; These props don't specify (:get ...) or (:set ...) forms.
  ;; .NET proxy uses `slot-value' function to access these properties.
  (:property (auto-property
              ;; Auto properties use `T' for getter and setter names
              :getter t
              ;; disable .Net setter
              :setter nil)
   ;; property type
   :object
   ;; Normal CLOS accessor
   :accessor my-class-auto-property
   ;; Normal CLOS initarg
   :initarg :auto-property
   ;; Normal CLOS initform. You can also use other slot definition parameters.
   :initform nil)
  ;; Event slots `hold' a list of delegates which subscribed to the event.
  ;; You can raise an event by acquiring its `slot-value' and funcall'ing it.
  (:event Raised EventHandler :accessor my-class-raised)
  (:method ((hairy-method "HairyMethod"
             ;; Name of the generated function
             :function-name hairy-method
             ;; whether to generate the function using `defmethod'
             :defmethodp nil
             ;; other CLOS slot arguments(except `:documentation') go here.
             ;; `:accessor's are not that useful though.
             ;; CLOS reader methods, when applied to method slots, return closures
             ;;   that enclose an object and accept the rest of the method arguments.
             ;; CLOS writer methods simply signal an error.
             :accessor my-class-hairy-method)
            ;; generic arguments
            ;; Each one can be a symbol, or a list
            ;;   of form (symbol ...constraints...)
            ;; Available constraints are:
            ;;  :in - contravariance
            ;;  :out - covariance
            ;;  :new - must have a public parameterless constructor
            ;;  :struct - must be a value type
            ;;  :class - must be a reference type
            ;;  (:interfaces ...ifaces..) - must implement interfaces
            ;;  (:base-type `type') - must inherit from `type'
            TArg)
    :string ((arg-count :int
                        ;; `:out' parameters must be set before method exits.
                        :direction :out
                        ;; The following designates that the parameter
                        ;;   is passed by reference.
                        ;; That's automatically the case if `:direction' is `:out' or `:io'
                        :ref t)
             (first-arg TArg)
             ;; `&rest' argument represents "params T[] args" parameter
             &rest (args (array TArg)))
    (let ((result (format nil "~a, ~{~a~^,~}" first-arg args)))
      (setf arg-count (1+ (length args)))
      ;; Actually, since Lisp is unable to pass parameters by reference,
      ;;   all the `:out' and `:io' parameters must be
      ;;   returned from the method as secondary return values,
      ;;   but the macro does that for you.
      result))
  ;; There should be at most one indexer on a class.
  (:indexer (item "Item"
                  ;; define generic methods for getter and setter
                  :defmethodp t
                  ;; other CLOS slot arguments go here
                  :documentation "My indexer"
                  ;; Reader methods return a pair(two values) of closures,
                  ;; that are similiar to the `:method' one.
                  ;; The first return value is for the getter,
                  ;;   and the second is for the setter.
                  :reader my-class-indexer)
   ;; parameter lambda list is similiar to the method's one
   :int ((name :string))
   (:get my-class-item (gethash name (my-class-values this)))
   ;; generated function name can also be of form `(setf ...)'
   (:set (setf my-class-item) (setf (gethash name (my-class-values this)) value))))

(defun bike-examples:callable-class-example ()
  (let ((obj (make-instance 'my-class)))
    ;; You can access callable class instances in the same manner as any other dotnet objects.
    ;; The only current exception are `:out' and `:io' parameters, which are not quite
    ;;   accessible through the library's public APIs.
    ;; .NET can utilize these perfectly, however.
    (setf #[obj "Name"] 123)
    ;; You can also use generated lisp accessors.
    (format t "obj[\"Name\"] == ~s" (my-class-item obj "Name"))
    t))

;;; vim: ft=lisp et
