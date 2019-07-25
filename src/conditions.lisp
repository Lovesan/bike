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
           :reader enum-resolution-error-datum))
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

(define-condition inner-ref-type-error (invalid-type-ast)
  ()
  (:report (lambda (c s)
             (format s "~&Inner ref types are not allowed: ~s"
                     (invalid-type-designator-datum c)))))

(define-condition invalid-type-name (invalid-type-designator)
  ()
  (:report (lambda (c s)
             (format s "~&Invalid type name: ~s"
                     (invalid-type-name-datum c)))))

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
                     (unnamed (member token '(:identifier :integer))))
     (format s "~&Unexpected ~:[~(~a~)~;~s~] ~:[~;~:*~s ~]~% at position ~d in type name ~s"
             (not unnamed)
             (if unnamed token value)
             (when unnamed value)
             (1+ (type-name-parser-error-position c))
             (type-name-parser-error-string c)))))

(define-condition generic-argument-count-mismatch (type-name-unexpected-token-error)
  ()
  (:report
   (lambda (c stream &aux (token (type-name-parser-error-token c))
                          (value (type-name-parser-error-value c)))
     (format
      stream
      "~&Generic type argument count mismatch for ~a~a~% at position ~d in name ~s"
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
             (format s "~&Unable to resolve constructor of type ~s~% with arguments (~{~a~^, ~})"
                     (member-resolution-error-type c)
                     (member-resolution-error-args c))))
  (:default-initargs :method ".ctor"))

(define-condition accessor-resolution-error (member-resolution-error)
  ((%kind :initarg :kind
          :reader accessor-resolution-error-accessor-kind
          :reader member-resolution-error-accessor-kind
          :type (member :reader :writer))
   (%member-kind :initarg :member-kind
                 :reader accessor-resolution-error-member-kind
                 :reader member-resolution-error-member-kind
                 :type (member :field :property :indexer))
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

;;; vim: ft=lisp et
