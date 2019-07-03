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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *coreclr-location*)
  (defvar *interop-location*)
  (defun init-coreclr-search-location ()
    (let ((lib (find-coreclr)))
      (if lib
        (progn (setf *coreclr-location* lib)
               (pushnew (uiop:pathname-directory-pathname lib)
                        *foreign-library-directories*))
        (error "Unable to find CoreCLR")))
    (let ((lib (find-interop t)))
      (if lib
        (progn (setf *interop-location* lib)
               (pushnew (uiop:pathname-directory-pathname lib)
                        *foreign-library-directories*))
        (error "Unable to find BikeInterop.dll")))
    (values))
  (uiop:register-image-restore-hook 'init-coreclr-search-location))

(define-foreign-library coreclr
  (t #.+coreclr-library-file+))

(use-foreign-library coreclr)

(defun get-trusted-platform-assemblies ()
  "Retrieves a list of pathnames of trusted platform assemblies"
  (uiop:directory* (uiop:make-pathname*
                    :name uiop:*wild*
                    :type "dll"
                    :defaults *coreclr-location*)))

(defun %get-tpa-string ()
  (format nil (uiop:strcat "~{~a~^" (uiop:inter-directory-separator) "~}~a~a")
          (mapcar #'uiop:native-namestring
                  (get-trusted-platform-assemblies))
          (uiop:inter-directory-separator)
          (uiop:native-namestring *interop-location*)))

(defun %get-trusted-assembly-names ()
  (loop :with tpa-dlls = (get-trusted-platform-assemblies)
        :for full-pathname :in tpa-dlls
        :for name = (pathname-name full-pathname)
        :when (uiop:string-prefix-p "System." name)
          :collect name))

;;; vim: ft=lisp et
