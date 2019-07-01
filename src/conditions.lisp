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

(define-condition invalid-type-designator (bike-error)
  ((%datum :initarg :datum :reader invalid-type-designator-datum))
  (:report (lambda (c s)
             (format s "Invalid type designator: ~s"
                     (invalid-type-designator-datum c)))))

(define-condition type-resolution-error (bike-error)
  ((%datum :initarg :datum :reader type-resolution-error-datum))
  (:report (lambda (c s)
             (format s "Unable to resolve type: ~s"
                     (type-resolution-error-datum c)))))

(define-condition member-resolution-error (bike-error)
  ((%type :initarg :type :reader member-resolution-error-type)
   (%staticp :initarg :static-p :reader member-resolution-error-static-p)))

(define-condition field-resolution-error (member-resolution-error)
  ((%field :initarg :field
           :initarg :member
           :reader field-resulution-error-field
           :reader member-resolution-error-member))
  (:report (lambda (c s)
             (format s "Unable to resolve ~:[instance~;static~] field ~s in type ~s"
                     (member-resolution-error-static-p c)
                     (member-resolution-error-member c)
                     (member-resolution-error-type c)))))

(define-condition property-resolution-error (member-resolution-error)
  ((%property :initarg :property
              :initarg :member
              :reader property-resulution-error-property
              :reader member-resolution-error-member))
  (:report (lambda (c s)
             (format s "Unable to resolve ~:[instance~;static~] property ~s in type ~s"
                     (member-resolution-error-static-p c)
                     (member-resolution-error-member c)
                     (member-resolution-error-type c)))))

(define-condition indexer-resolution-error (member-resolution-error)
  ()
  (:report (lambda (c s)
             (format s "Unable to resolve indexer in type ~s"
                     (member-resolution-error-type c)))))

(define-condition method-resolution-error (member-resolution-error)
  ((%method :initarg :method
            :initarg :member
            :reader method-resulution-error-method
            :reader member-resolution-error-member)
   (%args :initarg :args
          :reader method-resolution-error-args
          :reader member-resolution-error-args))
  (:report (lambda (c s)
             (format s "Unable to resolve ~:[instance~;static~] method ~s ~s in type ~s"
                     (member-resolution-error-static-p c)
                     (member-resolution-error-member c)
                     (member-resolution-error-args c)
                     (member-resolution-error-type c)))))

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
             (format s "Unable to resolve ~(~a~) on ~(~a~) ~s in type ~s"
                     (member-resolution-error-accessor-kind c)
                     (member-resolution-error-member-kind c)
                     (member-resolution-error-member c)
                     (member-resolution-error-type c)))))

(define-condition dotnet-error (bike-error)
  ((%object :reader dotnet-error-object
            :initarg :object))
  (:documentation "Represents a .Net Exception"))

(defmacro check-exception (form)
  (with-gensyms (result exception handle)
    `(multiple-value-bind (,result ,exception)
         ,form
       (when ,exception
         (let ((,handle (%dotnet-exception-handle ,exception)))
           (if (%is-lisp-object ,handle)
             (error (%handle-table-get
                     (pointer-address
                      (%%unbox-lisp-object ,handle))))
             (error 'dotnet-error :object ,exception))))
       ,result)))

;;; vim: ft=lisp et
