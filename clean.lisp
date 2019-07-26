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

(let* ((current-file #.(uiop:current-lisp-file-pathname))
       (current-dir (or (and current-file
                             (uiop:pathname-directory-pathname
                              current-file))
                        (uiop:getcwd)))
       (bindir (uiop:merge-pathnames*
                (uiop:make-pathname*
                 :directory '(:relative "src" "BikeInterop" "bin"))
                current-dir))
       (objdir (uiop:merge-pathnames*
                (uiop:make-pathname*
                 :directory '(:relative "src" "BikeInterop" "obj"))
                current-dir))
       (examples-bindir (uiop:merge-pathnames*
                         (uiop:make-pathname*
                          :directory '(:relative "examples" "BikeExamples" "bin"))
                         current-dir))
       (examples-objdir (uiop:merge-pathnames*
                         (uiop:make-pathname*
                          :directory '(:relative "examples" "BikeExamples" "obj"))
                         current-dir))
       (aspnet-assemblies (uiop:merge-pathnames*
                           (uiop:make-pathname*
                            :directory '(:relative "examples" "AspNetMvcAssemblies"))
                           current-dir)))
  (dolist (dir (list bindir objdir examples-bindir examples-objdir aspnet-assemblies))
    (let ((dir (uiop:probe-file* dir)))
      (when dir
        (handler-case
            (uiop:delete-directory-tree dir :validate t
                                            :if-does-not-exist :ignore)
          (error (e) (format *error-output* "~a~%" e)))))))

;;; vim: ft=lisp et
