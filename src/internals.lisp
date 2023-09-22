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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-global-var -interop-build-dir-
    (native-path
     (merge-pathnames*
      (make-pathname*
       :directory '(:relative "BikeInterop" "bin" "netstandard2.0"))
      (pathname-directory-pathname #.(current-lisp-file-pathname))))))

;; Clear interop assembly on rebuild
(eval-when (:compile-toplevel :execute)
  (unless (find-package '#:bike)
    (delete-directory-tree (parse-native-namestring -interop-build-dir-)
                           :validate t
                           :if-does-not-exist :ignore)))

(defun %version-compare (left right)
  (labels ((parse (c)
             ;; Given .Net version naming convention,
             ;;   3.0.0 is actually greater then 3.0.0-rc1
             (or (ignore-errors (parse-integer c))
                 -1))
           (split (str &aux (end (length str)))
             (loop :with start = 0
                   :with components = '()
                   :for i :below end
                   :for c = (char str i)
                   :when (member c '(#\. #\-))
                     :do (push (subseq str start i)
                               components)
                         (setf start (1+ i))
                   :finally (unless (= start end)
                              (push (subseq str start end) components))
                            (return (nreverse (mapcar #'parse components)))))
           (cmp (a b) (cond ((> a b) 1) ((< a b) -1) (t 0)))
           (pad (list other-length)
             (let ((length (length list)))
               (if (< length other-length)
                 (append list (make-list (- other-length length)
                                         :initial-element 0))
                 list))))
    (loop :with v1 = (split left)
          :with v2 = (split right)
          :with v1-len = (length v1)
          :with v2-len = (length v2)
          :for left :on (pad v1 v2-len)
          :and right :on (pad v2 v1-len)
          :for a = (car left)
          :for b = (car right)
          :do (cond ((< a b) (return -1))
                    ((> a b) (return 1)))
          :finally (return (cond (left (cmp a 0))
                                 (right (cmp 0 b))
                                 (t 0))))))

(declaim (inline %version>))
(defun %version> (left right)
  (= 1 (%version-compare left right)))

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
         (latest (loop :with max :of-type cons = (first runtimes)
                       :for spec :of-type cons :in (rest runtimes)
                       :for ver :of-type string = (car spec)
                       :when (%version> ver (car max)) :do (setf max spec)
                         :finally (return max))))
    (when latest
      (let* ((runtimes-dir (ensure-directory-pathname (cdr latest)))
             (latest-dir (ensure-directory-pathname
                          (merge-pathnames* (car latest) runtimes-dir))))
        (probe-file*
         (merge-pathnames* +coreclr-library-file+ latest-dir))))))

(defun %get-flexi-stream (in)
  ;;; Recent .Net versions seem to use UTF-8 as a default encoding
  (make-flexi-stream in :external-format (make-external-format :utf8)))

(defun build-interop (&key rebuild
                           (enable-task-hack #+coreclr-sbcl-task-hack t
                                             #-coreclr-sbcl-task-hack nil))
  (let* ((dir (pathname-parent-directory-pathname
               (pathname-parent-directory-pathname
                -interop-build-dir-)))
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
  (or (probe-file* (merge-pathnames* +interop-library-file+
                                     (pathname-directory-pathname
                                      (get-exe-path))))
      (probe-file* (merge-pathnames* +interop-library-file+
                                     (lisp-implementation-directory)))
      (probe-file* (merge-pathnames* +interop-library-file+
                                     (get-pathname-defaults)))
      (and (directory-exists-p -interop-build-dir-)
           (probe-file* (merge-pathnames* +interop-library-file+
                                          -interop-build-dir-)))))

(defun find-interop (&optional build)
  (or (%find-interop)
      (and build (progn (build-interop)
                        (%find-interop)))))

(defun find-coreclr ()
  "Returns full path to coreclr library or NIL if not found"
  (or (probe-file* (merge-pathnames* +coreclr-library-file+
                                     (pathname-directory-pathname
                                      (get-exe-path))))
      (probe-file* (merge-pathnames* +coreclr-library-file+
                                     (lisp-implementation-directory)))
      (probe-file* (merge-pathnames* +coreclr-library-file+
                                     (get-pathname-defaults)))
      (%find-by-command)))

;;; vim: ft=lisp et
