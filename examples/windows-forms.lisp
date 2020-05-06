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

(uiop:define-package #:bike-examples/system-windows-forms
  (:use #:cl #:bike #:uiop))

(in-package #:bike-examples/system-windows-forms)

(named-readtables:in-readtable bike-syntax)

(import-assembly 'System.Windows.Forms)
(import-loaded-assemblies)

(use-namespace 'System)
(use-namespace 'System.Threading)

(defun init-controls (form)
  [:System.Windows.Forms.Application EnableVisualStyles]
  (let ((button (new 'System.Windows.Forms.Button)))
    (setf [form %Text] "bike Windows.Forms"
          [form %ShowInTaskbar] t
          [form %StartPosition] #e(System.Windows.Forms.FormStartPosition CenterScreen)
          [form %Size] (new 'System.Drawing.Size 640 480)
          [button %Text] "Press me"
          [button %Font] (new 'System.Drawing.Font "Arial" 16.0e0)
          [button %Dock] #e(System.Windows.Forms.DockStyle Fill)
          [button %Parent] form)
    [button add_Click
            (new 'EventHandler
                 (lambda (s e)
                   (declare (ignore s e))
                   [:System.Windows.Forms.MessageBox Show
                                                     form
                                                     "Hello, World!"
                                                     "bike"]))]))

(defun bike-examples:hello-windows-forms ()
  (bt:make-thread
   (lambda ()
     ;; Now, this is the VERY important thing. WinForms stuff
     ;;   must run inside single-threaded COM apartment.
     ;; The below statement is roughly equialent to [STAThread] attribute
     ;;   on a thread function.
     ;; You can also set an apartment state for the main thread,
     ;;   but remember that you can only set an apartment state once
     ;;     (well, not really[there's OleUninitialize etc],
     ;;       but .NET will go insane if you do this).
     [[:Thread %CurrentThread] SetApartmentState #e(ApartmentState STA)]
     (let ((form (new 'System.Windows.Forms.Form)))
       (init-controls form)
       [form Show]
       [:System.Windows.Forms.Application Run form]))))

;;; vim: ft=lisp et
