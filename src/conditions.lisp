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

(define-condition bike-condition (condition)
  ()
  (:documentation "Represents generic condition"))

(define-condition bike-error (bike-condition error)
  ()
  (:report "Represents an erroneous condition"))

(define-condition bike-warning (bike-condition warning)
  ()
  (:report "Represents a subnormal but not erroneous condition"))

(define-condition invalid-assembly-designator (bike-error)
  ((%datum :initarg :datum
           :reader invalid-assembly-designator-datum))
  (:report (lambda (c s)
             (format s "~&Invalid assembly designator: ~s"
                     (invalid-assembly-designator-datum c)))))

(define-condition type-resolution-error (bike-error)
  ((%datum :initarg :datum
           :reader type-resolution-error-datum
           :reader invalid-type-name-datum
           :reader invalid-type-designator-datum
           :reader invalid-type-ast-datum
           :reader inner-ref-type-error-datum
           :reader enum-resolution-error-datum
           :reader invalid-ref-type-datum
           :reader inner-qualified-type-error-datum))
  (:report (lambda (c s)
             (format s "~&Unable to resolve type: ~s"
                     (type-resolution-error-datum c)))))

(define-condition invalid-type-designator (type-resolution-error)
  ()
  (:report (lambda (c s)
             (format s "~&Invalid type designator: ~s"
                     (invalid-type-designator-datum c)))))

(define-condition invalid-type-ast (invalid-type-designator)
  ()
  (:report (lambda (c s)
             (format s "~&Invalid type ast: ~s"
                     (invalid-type-designator-datum c)))))

(define-condition invalid-type-name (invalid-type-designator)
  ()
  (:report (lambda (c s)
             (format s "~&Invalid type name: ~s"
                     (invalid-type-name-datum c)))))

(define-condition invalid-ref-type (invalid-type-designator)
  ()
  (:report (lambda (c s)
             (format s "~&Reference types are not allowed here: ~s"
                     (invalid-ref-type-datum c)))))

(define-condition inner-ref-type-error (invalid-ref-type)
  ()
  (:report (lambda (c s)
             (format s "~&Inner ref types are not allowed: ~s"
                     (inner-ref-type-error-datum c)))))

(define-condition inner-qualified-type-error (invalid-type-designator)
  ()
  (:report (lambda (c s)
             (format s "~&Inner qualified types are not allowed here: ~s~%"
                     (inner-qualified-type-error-datum c)))))

(define-condition type-name-parser-error (invalid-type-name)
  ((%c :initarg :character :reader type-name-parser-error-character)
   ($pos :initarg :position :reader type-name-parser-error-position))
  (:report (lambda (c s)
             (format s "~&Unexpected character ~s at position ~d in type name string ~s"
                     (type-name-parser-error-character c)
                     (1+ (type-name-parser-error-position c))
                     (type-name-parser-error-string c)))))

(define-condition type-name-unexpected-token-error (type-name-parser-error)
  ((%c :initarg :value
       :reader type-name-unexpected-token-error-value
       :reader type-name-parser-error-value)
   (%tok :initarg :token
         :reader type-name-unexpected-token-error-token
         :reader type-name-parser-error-token))
  (:report
   (lambda (c s &aux (token (type-name-parser-error-token c))
                     (value (type-name-parser-error-value c))
                     (literal (find token '((:identifier . "identifier")
                                            (:integer . "integer")
                                            (:eof . "end of string"))
                                    :key #'car)))
     (format s "~&Unexpected ~:[~;~:*~a ~]~:[~;~:*~s~]~_ at position ~d in type name ~s"
             (cdr literal)
             value
             (1+ (type-name-parser-error-position c))
             (type-name-parser-error-string c)))))

(define-condition generic-argument-count-mismatch (type-name-unexpected-token-error)
  ()
  (:report
   (lambda (c stream &aux (token (type-name-parser-error-token c))
                          (value (type-name-parser-error-value c)))
     (format
      stream
      "~&Generic type argument count mismatch for ~:[~;~:*~a~]~a~% at position ~d in name ~s"
      token
      value
      (1+ (type-name-parser-error-position c))
      (type-name-parser-error-string c)))))

(defgeneric type-name-parser-error-string (parser-error)
  (:method ((parser-error type-name-parser-error))
    (slot-value parser-error '%datum)))

(define-condition type-name-parser-eof (type-name-parser-error)
  ()
  (:report (lambda (c s)
             (format s "~&Unexpected end of string while tokenizing~% type name ~s"
                     (type-name-parser-error-string c))))
  (:default-initargs :character nil))

(define-condition enum-resolution-error (type-resolution-error)
  ()
  (:report (lambda (c s)
             (format s "~&~s is not an enum type"
                     (type-resolution-error-datum c)))))

(define-condition member-resolution-error (bike-error)
  ((%type :initarg :type :reader member-resolution-error-type)
   (%staticp :initarg :static-p :reader member-resolution-error-static-p))
  (:default-initargs :static-p nil))

(define-condition field-resolution-error (member-resolution-error)
  ((%field :initarg :field
           :initarg :member
           :reader field-resulution-error-field
           :reader member-resolution-error-member))
  (:report (lambda (c s)
             (format s "~&Unable to resolve ~:[~;static ~]field ~a~% of type ~s"
                     (member-resolution-error-static-p c)
                     (member-resolution-error-member c)
                     (member-resolution-error-type c)))))

(define-condition property-resolution-error (member-resolution-error)
  ((%property :initarg :property
              :initarg :member
              :reader property-resulution-error-property
              :reader member-resolution-error-member))
  (:report (lambda (c s)
             (format s "~&Unable to resolve ~:[~;static ~]property ~a~% of type ~s"
                     (member-resolution-error-static-p c)
                     (member-resolution-error-member c)
                     (member-resolution-error-type c)))))

(define-condition event-resolution-error (member-resolution-error)
  ((%event :initarg :event
           :initarg :member
           :reader event-resolution-error-event
           :reader member-resolution-error-member))
  (:report (lambda (c s)
             (format s "~&Unable to resolve ~:[~;static ~]event ~a~% of type ~s"
                     (member-resolution-error-static-p c)
                     (member-resolution-error-member c)
                     (member-resolution-error-type c)))))

(define-condition indexer-resolution-error (property-resolution-error)
  ())

(define-condition method-resolution-error (member-resolution-error)
  ((%method :initarg :method
            :initarg :member
            :reader method-resulution-error-method
            :reader member-resolution-error-member)
   (%args :initarg :args
          :reader method-resolution-error-args
          :reader member-resolution-error-args))
  (:report (lambda (c s)
             (format s "~&Unable to resolve ~:[~;static ~]method ~a(~{~a~^, ~})~% of type ~s"
                     (member-resolution-error-static-p c)
                     (member-resolution-error-member c)
                     (member-resolution-error-args c)
                     (member-resolution-error-type c)))))

(define-condition constructor-resolution-error (method-resolution-error)
  ()
  (:report (lambda (c s)
             (format s (concatenate
                        'string
                        "~&Unable to resolve constructor of"
                        " type ~s~% with arguments (~{~a~^, ~})")
                     (member-resolution-error-type c)
                     (member-resolution-error-args c))))
  (:default-initargs :method ".ctor"))

(define-condition accessor-resolution-error (member-resolution-error)
  ((%kind :initarg :kind
          :reader accessor-resolution-error-accessor-kind
          :reader member-resolution-error-accessor-kind
          :type (member :reader :writer :add :remove))
   (%member-kind :initarg :member-kind
                 :reader accessor-resolution-error-member-kind
                 :reader member-resolution-error-member-kind
                 :type (member :field :property :indexer :event))
   (%member :initarg :member
            :reader accessor-resolution-error-member
            :reader member-resolution-error-member))
  (:report (lambda (c s)
             (format s "~&Unable to resolve ~(~a~) on ~(~a~) ~s~% in type ~s"
                     (member-resolution-error-accessor-kind c)
                     (member-resolution-error-member-kind c)
                     (member-resolution-error-member c)
                     (member-resolution-error-type c)))))

(define-condition dotnet-error (bike-error)
  ((%object :reader dotnet-error-object
            :initarg :object))
  (:documentation "Represents a .Net Exception"))

(define-condition bike-reader-error (bike-error reader-error)
  ((%datum :initarg :message
           :reader bike-reader-error-message))
  (:report (lambda (c s)
             (format s "Reader error: ~a"
                     (bike-reader-error-message c)))))

(defun bike-reader-error (stream message)
  (error 'bike-reader-error :message message :stream stream))

(define-condition duplicate-dotnet-name (bike-error)
  ((value :initarg :value
          :reader duplicate-dotnet-name-value)
   (class :initarg :class
          :reader duplicate-dotnet-name-class))
  (:report (lambda (c s)
             (format s "~<Duplicate dotnet name ~s in class ~s~:>"
                     (list
                      (duplicate-dotnet-name-value c)
                      (duplicate-dotnet-name-class c))))))

(define-condition delegate-type-expected (bike-error)
  ((datum :initarg :type
          :initarg :datum
          :reader delegate-type-expected-datum))
  (:report (lambda (c s)
             (format s "~s is not a delegate type"
                     (delegate-type-expected-datum c)))))

(define-condition interface-type-expected (bike-error)
  ((datum :initarg :type
          :initarg :datum
          :reader interface-type-expected-datum))
  (:report (lambda (c s)
             (format s "~s is not an interface type"
                     (interface-type-expected-datum c)))))

(define-condition sealed-inheritance (bike-error)
  ((type :initarg :type
         :reader sealed-inheritance-type))
  (:report (lambda (c s)
             (format s "~s is sealed."
                     (sealed-inheritance-type c)))))

(define-condition parameter-direction-mismatch (bike-error)
  ((datum :initarg :datum
          :reader parameter-direction-mismatch-datum))
  (:report (lambda (c s)
             (format s "Parameter direction mismatch: ~s"
                     (parameter-direction-mismatch-datum c)))))

(define-condition invalid-params-array-definition (bike-error)
  ((datum :initarg :datum
          :reader invalid-params-array-definition-datum))
  (:report (lambda (c s)
             (format s "Invalid params array definition: ~s"
                     (invalid-params-array-definition-datum c)))))

(define-condition duplicate-parameter-name (bike-error)
  ((datum :initarg :datum
          :reader duplicate-parameter-name-datum)
   (value :initarg :value
          :reader duplicate-parameter-name-value))
  (:report (lambda (c s)
             (format s "~<Duplicate parameter name ~s in ~s~:>"
                     (list
                      (duplicate-parameter-name-value c)
                      (duplicate-parameter-name-datum c))))))

(defun duplicate-parameter-name (name datum)
  (error 'duplicate-parameter-name :value name
                                   :datum datum))

(define-condition invalid-generic-constraint (bike-error)
  ((message :initarg :message
            :reader invalid-generic-constraint-message)
   (list :initarg :list
         :reader invalid-generic-constraint-list))
  (:report (lambda (c s)
             (format s "~<Invalid generic constraint: ~:@_~a ~:@_Constraint list was: ~s~:>"
                     (list
                      (invalid-generic-constraint-message c)
                      (invalid-generic-constraint-list c))))))

(defun invalid-generic-constraint (list message &rest args)
  (error 'invalid-generic-constraint
         :list list
         :message (apply #'format nil message args)))

(define-condition method-slot-write-attempt (bike-error)
  ((object :initarg :object
           :reader method-slot-write-attempt-object)
   (slot-name :initarg :slot-name
              :reader method-slot-write-attempt-slot-name)
   (value :initarg :value
          :reader method-slot-write-attempt-value))
  (:report (lambda (c s)
             (format s #.(strcat "~<Cannot write to slot ~s of ~s~:@_"
                                 "  becase it is a .NET method slot~:@_"
                                 "Value was: ~s~:>")
                     (list (method-slot-write-attempt-slot-name c)
                           (method-slot-write-attempt-object c)
                           (method-slot-write-attempt-value c))))))

(define-condition method-slot-makunbound-attempt (bike-error)
  ((object :initarg :object
           :reader method-slot-makunbound-attempt-object)
   (slot-name :initarg :slot-name
              :reader method-slot-makunbound-attempt-slot-name))
  (:report (lambda (c s)
             (format s #.(strcat "~<Cannot unbind slot ~s of ~s~:@_"
                                 "  becase it is a .NET method slot~:@_"
                                 "~:>")
                     (list (method-slot-makunbound-attempt-slot-name c)
                           (method-slot-makunbound-attempt-object c))))))

(define-condition dotnet-callable-object-orphan-proxy ()
  ((value :initarg :value
          :reader dotnet-callable-object-orphan-proxy-value)
   (operation :initarg :operation
              :reader dotnet-callable-object-orphan-proxy-operation)
   (arguments :initarg :arguments
              :reader dotnet-callable-object-orphan-proxy-arguments))
  (:report (lambda (c s)
             (format s #.(strcat "Orphan proxy has been invoked.~%"
                                 "Operation was: ~s~%"
                                 "Arguments: ~s")
                     (dotnet-callable-object-orphan-proxy-operation c)
                     (dotnet-callable-object-orphan-proxy-arguments c)))))

;;; vim: ft=lisp et
