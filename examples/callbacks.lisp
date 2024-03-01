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

(use-namespace 'System)
(use-namespace 'System.Diagnostics)
(use-namespace 'System.Text)
(use-namespace 'System.IO)
(use-namespace 'System.Linq)
(use-namespace 'BikeExamples)

(defvar *examples-project-dir*
  (native-path
   (merge-pathnames*
    (make-pathname* :directory '(:relative "BikeExamples"))
    (pathname-directory-pathname #.(current-lisp-file-pathname)))))

(defun make-process-output-handler (stream)
  (declare (type stream stream))
  (new 'DataReceivedEventHandler
       (lambda (sender e)
         (declare (ignore sender))
         (let ((str (property e 'Data)))
           (when str (write-line str stream))))))

(defun build-examples-assembly ()
  "Builds examples project"
  (let* ((project-file (native-path
                        (make-pathname*
                         :name "BikeExamples"
                         :type "csproj"
                         :defaults *examples-project-dir*)))
         (process (new 'Process))
         (start-info (property process 'StartInfo))
         (args (property start-info 'ArgumentList))
         (encoding (property 'System.Text.Encoding 'UTF8)))
    (dolist (arg (list "build" "-c" "Release" project-file))
      (invoke args 'Add arg))
    (setf (property start-info 'FileName) "dotnet"
          (property start-info 'UseShellExecute) nil
          (property start-info 'WorkingDirectory) *examples-project-dir*
          (property start-info 'RedirectStandardOutput) T
          (property start-info 'RedirectStandardError) T
          (property start-info 'StandardOutputEncoding) encoding
          (property start-info 'StandardErrorEncoding) encoding
          (property process 'EnableRaisingEvents) T)
    (event-add process 'OutputDataReceived
            (make-process-output-handler *standard-output*))
    (event-add process 'ErrorDataReceived
            (make-process-output-handler *error-output*))
    (invoke process 'Start)
    (invoke process 'BeginOutputReadLine)
    (invoke process 'BeginErrorReadLine)
    (invoke process 'WaitForExit)
    (let ((rv (property process 'ExitCode)))
      (unless (zerop rv)
        (error "Unable to publish project file: ~a" rv))
      project-file)))

(defun ensure-examples-assembly ()
  (let ((bin-path (merge-pathnames*
                   (make-pathname*
                    :name "BikeExamples"
                    :type "dll"
                    :directory '(:relative "bin"))
                   *examples-project-dir*)))
    (unless (probe-file bin-path)
      (build-examples-assembly))
    (import-assembly-from bin-path)))

(ensure-examples-assembly)

(defun simple-callback ()
  "Passes simple callback to .Net code"
  (let ((delegate (new 'Action (lambda () (write-line "Hello, World!")))))
    (invoke 'ExampleClass 'InvokeAction delegate)))

(defun enumerable-callbacks ()
  "Passes callbacks into LINQ extension methods"
  ;; TODO: make the ability to use extension methods directly, like in C#
  (let* ((range (invoke 'Enumerable 'Range 1 10))
         (filter (new '(Func :int :bool) #'evenp))
         (aggregator (new '(Func :int :int :int) #'+)))
    (write-line "Aggregating even numbers from range 1..10 using #'+ function")
    (invoke 'Enumerable '(Aggregate :int)
            (invoke 'Enumerable '(Where :int) range filter)
            aggregator)))

;;; vim: ft=lisp et
