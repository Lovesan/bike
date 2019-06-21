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

(uiop:define-package #:bike-internals
  (:use #:cl #:uiop #:cffi #:split-sequence #:flexi-streams #:cl-ppcre)
  (:export #:find-coreclr
           #:find-interop
           #:build-interop
           #:get-exe-path
           #:+coreclr-library-file+
           #:+interop-library-file+
           #:+pointer-size+
           #:+pointer-bits+
           #:lpwstr
           #:rwlock
           #:rwlockp
           #:make-rwlock
           #:with-read-lock
           #:with-write-lock)
  (:import-from #:alexandria
                #:define-constant
                #:non-negative-fixnum
                #:with-gensyms))

(in-package #:bike-internals)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond ((os-windows-p)
         (pushnew :coreclr-windows *features*))
        ((os-macosx-p)
         (pushnew :coreclr-macos *features*))
        ((os-unix-p)
         (pushnew :coreclr-unix *features*)))
  #+linux
  (pushnew :coreclr-linux *features*)
  #+(and sbcl windows)
  (when (string< (uiop:lisp-version-string)
                 "1.5.4")
    (pushnew :coreclr-sbcl-task-hack *features*)))

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

(define-constant +pointer-size+ (foreign-type-size :pointer))

(define-constant +pointer-bits+ (* 8 +pointer-size+))

(defvar *interop-build-dir*
  (merge-pathnames*
   (make-pathname*
    :directory '(:relative "BikeInterop" "bin" "netstandard2.0"))
   (pathname-directory-pathname #.(current-lisp-file-pathname))))

(cffi:defctype lpwstr (:string :encoding :utf-16/le))

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


#+coreclr-windows
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library kernel32
    (t "kernel32.dll"))
  (use-foreign-library kernel32)
  (defcfun (get-oemcp "GetOEMCP" :convention :stdcall
                                 :library kernel32)
      :uint)
  (defcfun (get-last-error "GetLastError" :convention :stdcall
                                          :library kernel32)
      :uint32)
  (defcfun (get-module-file-name
            "GetModuleFileNameW" :convention :stdcall
                                 :library kernel32)
      :uint32
    (module :pointer)
    (buffer :pointer)
    (size :uint32)))

#+corecl-macos
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defcfun (ns-get-executable-path "_NSGetExecutablePath")
      :int
    (buf :pointer)
    (bufsize :uint32)))

(defun get-exe-path ()
  #+coreclr-windows
  (let* ((size 260) ;; MAX_PATH
         (buf (foreign-alloc :char :count size)))
    (unwind-protect
         (loop :for rv = (get-module-file-name (null-pointer)
                                               buf
                                               size)
               :for last-error = (get-last-error) :do
                 (unless (= last-error 122)
                   (return (values (foreign-string-to-lisp
                                    buf :encoding :utf-16/le))))
                 (foreign-free buf)
                 (setf size (* size 2)
                       buf (foreign-alloc :char :count size))
                 (when (null-pointer-p buf)
                   (error "Unable to allocate buffer")))
      (unless (null-pointer-p buf)
        (foreign-free buf))))
  #+coreclr-macos
  (let ((size 260)
        (buf (foreign-alloc :char :count size)))
    (unwind-protect
         (loop :for rv = (ns-get-executable-path buf size) :do
           (when (zerop rv)
             (return (values (foreign-string-to-lisp buf))))
           (foreign-free buf)
           (setf size (* size 2)
                 buf (foreign-alloc :char :count size))
           (when (null-pointer-p buf)
             (error "Unable to allocate buffer")))
      (unless (null-pointer-p buf)
        (foreign-free buf))))
  #+coreclr-linux
  (native-namestring (truename* "/proc/self/exe"))
  #-(or coreclr-windows coreclr-macos coreclr-linux)
  (let ((argv0 (first (raw-command-line-arguments))))
    (unless argv0
      (error "Unable to get executable name"))
    argv0))

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
