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

(in-package #:bike-internals)

(define-constant +coreclr-library-file+
  #+coreclr-windows
  "coreclr.dll"
  #+coreclr-macos
  "libcoreclr.dylib"
  #-(or coreclr-windows coreclr-macos)
  "libcoreclr.so"
  :test #'equal)

(define-constant +interop-library-file+ "BikeInterop.dll"
  :test #'equal)

(define-condition slot-initializer-missing (cell-error)
  ((message :initarg :message :initform nil :reader slot-initializer-missing-message))
  (:report (lambda (err stream)
             (let ((message (slot-initializer-missing-message err))
                   (name (ignore-errors (cell-error-name err))))
               (format stream
                       "Initializer for slot ~:[~;~:*~s ~]is missing.~:[~; ~:*~a~]"
                       name
                       message)
               err))))

(defmacro required-slot (&key name message)
  `(error 'slot-initializer-missing
          :name ,name
          :message (or ,message "Slot is required.")))

(defvar *interop-build-dir*
  (uiop:native-namestring
   (merge-pathnames*
    (make-pathname*
     :directory '(:relative "BikeInterop" "bin" "netstandard2.0"))
    (pathname-directory-pathname #.(current-lisp-file-pathname)))))

(defun %find-by-command ()
  (let* ((out (with-output-to-string (out)
                (progn
                  (ignore-errors
                   (run-program '("dotnet" "--list-runtimes")
                                :output out)))))
         (runtimes (loop :with list = (split-sequence #\newline out)
                         :with re = "Microsoft.NETCore.App ([0-9.]+) \\[(.*)\\]"
                         :with scanner = (create-scanner re)
                         :for line :in list
                         :for match = (nth-value 1 (scan-to-strings scanner line))
                         :when match
                           :collect (cons (elt match 0) (elt match 1))))
         (latest (first (sort runtimes #'string> :key #'car))))
    (when latest
      (let* ((runtimes-dir (ensure-directory-pathname (cdr latest)))
             (latest-dir (ensure-directory-pathname
                          (merge-pathnames* (car latest) runtimes-dir))))
        (probe-file*
         (merge-pathnames* +coreclr-library-file+ latest-dir))))))

(defun %get-flexi-stream (in)
  #+coreclr-windows
  (make-flexi-stream
   in :external-format (make-external-format :code-page :id (get-oemcp)))
  #-coreclr-windows
  (make-flexi-stream
   in :external-format (make-external-format :utf8)))

(defun build-interop (&key rebuild
                           (enable-task-hack #+coreclr-sbcl-task-hack t
                                             #-coreclr-sbcl-task-hack nil))
  (let* ((dir (pathname-parent-directory-pathname
               (pathname-parent-directory-pathname
                *interop-build-dir*)))
         (proj "BikeInterop.csproj"))
    (with-current-directory (dir)
      (unwind-protect
           (run-program (remove
                         nil
                         (list "dotnet" "build" "-c" "Release"
                               (when rebuild
                                 "--no-incremental")
                               (when enable-task-hack
                                 "/p:DefineConstants=ENABLE_TASK_HACK")
                               proj))
                        :output (pathname "build.log")
                        :error-output (pathname "build.log"))
        (ignore-errors
         (with-open-file (stream "build.log" :element-type '(unsigned-byte 8)
                                             :if-does-not-exist :create)
           (with-open-stream (in (%get-flexi-stream stream))
             (loop :for line = (read-line in nil)
                   :while line :do (write-line line *error-output*)))))))))


(defun %find-interop ()
  (or (and (directory-exists-p *interop-build-dir*)
           (probe-file* (merge-pathnames* +interop-library-file+
                                          *interop-build-dir*)))
      (probe-file* (merge-pathnames* +interop-library-file+
                                     (get-pathname-defaults)))
      (probe-file* (merge-pathnames* +interop-library-file+
                                     (lisp-implementation-directory)))
      (probe-file* (merge-pathnames* +interop-library-file+
                                     (pathname-parent-directory-pathname
                                      (get-exe-path))))))

(defun find-interop (&optional build)
  (or (%find-interop)
      (and build (progn (build-interop)
                        (%find-interop)))))

(defun find-coreclr ()
  "Returns full path to coreclr library or NIL if not found"
  (or (probe-file* (merge-pathnames* +coreclr-library-file+
                                     (get-pathname-defaults)))
      (probe-file* (merge-pathnames* +coreclr-library-file+
                                     (lisp-implementation-directory)))
      (probe-file* (merge-pathnames* +coreclr-library-file+
                                     (pathname-parent-directory-pathname
                                      (get-exe-path))))
      (%find-by-command)))

;;; vim: ft=lisp et
