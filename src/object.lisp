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

(defconstant +type-code-empty+ 0)
(defconstant +type-code-object+ 1)
(defconstant +type-code-db-null+ 2)
(defconstant +type-code-boolean+ 3)
(defconstant +type-code-char+ 4)
(defconstant +type-code-sbyte+ 5)
(defconstant +type-code-byte+ 6)
(defconstant +type-code-int16+ 7)
(defconstant +type-code-uint16+ 8)
(defconstant +type-code-int32+ 9)
(defconstant +type-code-uint32+ 10)
(defconstant +type-code-int64+ 11)
(defconstant +type-code-uint64+ 12)
(defconstant +type-code-single+ 13)
(defconstant +type-code-double+ 14)
(defconstant +type-code-decimal+ 15)
(defconstant +type-code-datetime+ 16)
(defconstant +type-code-string+ 18)

(defconstant +ext-type-code-lisp-object+ 1)
(defconstant +ext-type-code-type+ 2)
(defconstant +ext-type-code-delegate+ 3)
(defconstant +ext-type-code-exception+ 4)
(defconstant +ext-type-code-enum+ 5)

(macrolet ((frob (what &optional (include t))
             (let* ((what (symbolicate '#:dotnet- what))
                    (pct '#:%)
                    (%%ctr (symbolicate pct pct what))
                    (%ctr (symbolicate pct what))
                    (predicate (symbolicate what '- 'p))
                    (conc-name (symbolicate pct what '-))
                    (handle-accessor (symbolicate conc-name 'handle)))
               `(progn (declaim (inline ,%ctr ,%%ctr ,predicate ,handle-accessor))
                       (defstruct (,what ,@(when include `((:include dotnet-object)))
                                         (:constructor ,%%ctr (handle))
                                         (:conc-name ,conc-name)
                                         (:predicate ,predicate))
                         ,@(unless include
                             `((handle (null-pointer) :type foreign-pointer
                                                      :read-only t))))
                       (defun ,%ctr (pointer)
                         (declare (type foreign-pointer pointer))
                         (if (null-pointer-p pointer)
                           nil
                           (let ((object (,%%ctr pointer)))
                             (tg:finalize object (lambda () (%free-handle pointer)))
                             object)))))))
  (frob object nil)
  (frob type)
  (frob delegate)
  (frob exception))

;;; vim: ft=lisp et
