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

(in-package #:cl-user)

(uiop:define-package #:bike-tests
  (:use #:cl #:bike #:fiveam)
  (:export #:bike-suite))

(in-package #:bike-tests)

(def-suite bike-suite
  :description "Tests")

(in-suite bike-suite)

(test test-hello
  (is (eq nil (invoke 'System.Console 'WriteLine "Hello, World!"))))

(test box-unbox-int
  (is (eql 4 (unbox (box 4 'int)))))

(test test-datetime-rountrip
  (let* ((culture (property 'System.Globalization.CultureInfo 'InvariantCulture))
         (dt (property 'System.DateTime 'Now))
         (str (invoke dt 'ToString "o" culture)))
    (is (invoke :object 'Equals
                dt
                (invoke 'System.DateTime 'ParseExact str "o" culture)))))

(test test-exception
  (let ((ex nil))
    (handler-case
        (dnvref (new '(:array int) 0) 1)
      (dotnet-error (e)
        (setf ex (dotnet-error-object e))))
    (let ((type-name (and ex (property (invoke ex 'GetType)
                                       'FullName))))
      (print type-name)
      (is (equal "System.IndexOutOfRangeException" type-name)
          "Wrong exception type. Got: ~a" type-name))))

(test test-callback
  (let* ((x 1)
         (delegate (new 'System.Action (lambda () (incf x)))))
    (invoke delegate 'invoke)
    (is (= x 2))))

;;; vim: ft=lisp et
