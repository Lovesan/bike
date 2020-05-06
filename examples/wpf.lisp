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

(uiop:define-package #:bike-examples/wpf
  (:use #:cl #:bike #:uiop))

(in-package #:bike-examples/wpf)

(named-readtables:in-readtable bike-syntax)

(defvar *xaml-path*
  (native-path
   (merge-pathnames*
    (pathname-directory-pathname #.(current-lisp-file-pathname))
    "WpfUserControl.xaml")))

(import-assembly 'PresentationFramework)
(import-loaded-assemblies)

(use-namespace 'System)
(use-namespace 'System.IO)
(use-namespace 'System.Threading)

(defun load-xaml (path)
  (with-disposable (stream [:File OpenRead (native-path path)])
    [:System.Windows.Markup.XamlReader Load stream]))

(defun reload-content (window)
  (let* ((control (load-xaml *xaml-path*))
         (button [control FindName "button"]))
    [button add_Click
            (new 'System.Windows.RoutedEventHandler
                 (lambda (s e)
                   (declare (ignore s e))
                   [:System.Windows.MessageBox Show
                                               window  "Hello, World!"
                                               "bike"
                                               [:System.Windows.MessageBoxButton $OK]
                                               [:System.Windows.MessageBoxImage $Information]]))]
    (setf [window %Title] "bike WPF window"
          [window %ShowActivated] t
          [window %ShowInTaskbar] t
          [window %WindowStartupLocation] #e(System.Windows.WindowStartupLocation CenterScreen)
          [window %Content] control)))

(defun bike-examples:hello-wpf ()
  (bt:make-thread
   (lambda ()
     ;; Now, this is the VERY important thing. WPF stuff
     ;;   must run inside single-threaded COM apartment.
     ;; The below statement is roughly equialent to [STAThread] attribute
     ;;   on a thread function.
     ;; You can also set an apartment state for the main thread,
     ;;   but remember that you can only set an apartment state once
     ;;     (well, not really[there's OleUninitialize etc],
     ;;       but .NET will go insane if you do this).
     [[:Thread %CurrentThread] SetApartmentState #e(ApartmentState STA)]
     (let ((window (new 'System.Windows.Window)))
       (reload-content window)
       [window ShowDialog]))))

;;; vim: ft=lisp et
