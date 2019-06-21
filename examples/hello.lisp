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

(import-assembly 'System.Runtime.InteropServices.RuntimeInformation)

(use-namespace 'System.Runtime.InteropServices)

(defun hello ()
  (invoke 'System.Console 'WriteLine "Hello, World!"))

(defun hello-datetime ()
  (let ((now (property 'System.DateTime 'Now))
        (user (property 'System.Environment 'UserName)))
    (format t "Hello, ~a! Now is ~a"
            user (invoke now 'ToString "g"))))

(defun hello-callback ()
  (let* ((os (property 'RuntimeInformation 'OSDescription))
         (delegate (new '(System.Action :string)
                        (lambda (who)
                          (format t "Hello ~a!~%You are running .Net Core~% inside ~a ~a~% on ~a"
                                  who
                                  (lisp-implementation-type)
                                  (lisp-implementation-version)                                  
                                  os))))
         (user (property 'System.Environment 'UserName)))
    (invoke delegate 'invoke user)))

;;; vim: ft=lisp et
