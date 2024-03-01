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

(named-readtables:in-readtable bike-syntax)

(use-namespace 'System)
(use-namespace 'System.Reflection)
(use-namespace 'System.Collections.Generic)

(defun hello-syntax ()
  ;; Basic method syntax
  ;; The ':' right after square bracket implies that the
  ;;   first argument is not evaluated and the whole expression
  ;;   should be treated as static member(method in this case) access
  [:Console WriteLine "Hello, World!"]
  ;; Generic methods are invoked as in bike:invoke, i.e. you use tree of linked lists
  ;;  instead of string designator to specify a method.
  (let ((tuple [:Tuple (Create :string :string) "Hello" "World"]))
    ;; To access an instance member you should omit the leading ':' after the opening bracket.
    ;; In this case, the first form is evaluated
    [tuple ToString]))

(defun enum-syntax ()
  ;; Enum shortcut: #e(EnumTypeName EnumValue1 EnumValue2 ...)
  ;; Neither type name, nor values are evaluated, so this equals to
  ;;  (bike:enum 'EnumTypeName 'EnumValue1 'EnumValue2 ...)
  (let ((combined-flags #e(BindingFlags Static Public)))
    (format t "~s == #x~x~%" combined-flags (unbox combined-flags))))

(defun property-syntax ()
  ;; Property access is similiar to method access, but you should prefix
  ;;  property name with percent sign ('%').
  (let ((dt [:DateTime %Now]))
    (format t "Now is: ~a (total ticks: ~a)~%" dt [dt %Ticks])
    ;; Access of a static property of a generic class:
    [:(EqualityComparer :string) %Default]))

(defun field-syntax ()
  ;; Field syntax is similiar to property syntax but
  ;;  field name should start with the dollar sign ('$')
  (format t "Int32 max value: ~a~%" [:int $MaxValue]))

(defun indexer-syntax ()
  ;; Indexer syntax is implemented using sharpsign dispatch character
  ;; #[FORM Index1 Index2 ...]
  (let ((dict (new '(Dictionary :string :string))))
    ;; Note that the form could be SETF'ed as usual.
    ;;   This is also true for property and field forms described above.
    (setf #[dict "Hello"] "World!")
    dict))

(define-dotnet-callable-class example-object-with-event ()
  (:event raised EventHandler :accessor eowe-raised))

(defun event-syntax ()
  (let ((obj (make-instance 'example-object-with-event))
        (handler (new 'EventHandler (lambda (sender e)
                                      (declare (ignore sender e))
                                      (write-line "Event raised")))))
    ;; Event syntax is somewhat similiar to method invocation.
    ;; "+EventName" subscribes to an event, and the following line
    ;; actually equals to (event-add obj 'Raised handler)
    [obj +Raised handler]
    ;; "-EventName" unsubscribes from an event.
    ;; The following equals to (event-remove obj 'Raised handler)
    [obj -Raised handler]
    ;; Note that there's no syntax for event invocation, as RaiseMethod
    ;; is not guaranteed to be present in EventInfo,
    ;;   and for ex. C# compiler completely omits it while generating MSIL.
    ;; For dotnet-callable classes you can use event slot value for that, however.
    [obj +Raised handler]
    (let ((raise-method (eowe-raised obj)))
      (funcall raise-method obj (field 'EventArgs 'Empty)))
    obj))

;;; vim: ft=lisp et
