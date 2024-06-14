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
  (define-global-var -coreclr-location- nil)
  (define-global-var -interop-location- nil)
  (defun init-coreclr-search-location ()
    (let ((lib (find-coreclr)))
      (if lib
        (let ((runtime-dir (pathname-directory-pathname lib)))
          (setf -coreclr-location- lib)
          ;; Add runtime directory to DLL search path
          (add-default-library-directory runtime-dir))
        (error "Unable to find CoreCLR")))
    (let ((lib (find-interop t)))
      (if lib
        (let ((interop-dir (pathname-directory-pathname lib)))
          (setf -interop-location- lib)
          ;; Add interop directory to DLL search path
          (add-default-library-directory interop-dir))
        (error "Unable to find BikeInterop.dll")))
    (values))
  (register-image-restore-hook 'init-coreclr-search-location))

(define-foreign-library-once coreclr #.+coreclr-library-file+)

(use-foreign-library-once coreclr :default-directories-only t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %get-related-app-path (name)
    (regex-replace
     "(?i)Microsoft\\.NetCore\\.App"
     (native-path (pathname-directory-pathname -coreclr-location-))
     name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-global-var -aspnet-sdk-dir- nil)
  (defun initialize-aspnet-search-location ()
    (let* ((aspnet-sdk-dir (pathname
                            (%get-related-app-path "Microsoft.AspNetCore.App"))))
      (if (probe-file* (make-pathname* :name "Microsoft.AspNetCore"
                                       :type "dll"
                                       :defaults aspnet-sdk-dir))
        ;; Add Asp.Net Core to DLL search path
        (add-default-library-directory (setf -aspnet-sdk-dir- aspnet-sdk-dir))
        (setf -aspnet-sdk-dir- nil))))
  (register-image-restore-hook 'initialize-aspnet-search-location))

#+coreclr-windows
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-global-var -desktop-sdk-dir- nil)
    (defun initialize-wpfgfx-search-location ()
      (let* ((desktop-sdk-dir (pathname
                               (%get-related-app-path "Microsoft.WindowsDesktop.App"))))
        (if (probe-file*
             (make-pathname* :name "wpfgfx_cor3"
                             :type "dll"
                             :defaults desktop-sdk-dir))
          ;; Add Desktop SDK dir to DLL search path
          (add-default-library-directory (setf -desktop-sdk-dir- desktop-sdk-dir))
          (setf -desktop-sdk-dir- nil))))
    (register-image-restore-hook 'initialize-wpfgfx-search-location))

  ;; These libs should be pre-loaded, otherwise WPF would be unable
  ;;   to find them, for who knows what reasons
  (define-foreign-library-once vcruntime "vcruntime140_cor3.dll")
  (define-foreign-library-once d3dcompiler "D3DCompiler_47_cor3.dll")
  (define-foreign-library-once wpfgfx "wpfgfx_cor3.dll")

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun load-wpfgfx ()
      (when -desktop-sdk-dir-
        (load-foreign-library-once 'vcruntime :default-directories-only t)
        (load-foreign-library-once 'd3dcompiler :default-directories-only t)
        (load-foreign-library-once 'wpfgfx :default-directories-only t)))
    (register-image-restore-hook 'load-wpfgfx)))

#-coreclr-windows
(progn
  (define-foreign-library-once system-native #.+system-native-library-file+)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-global-var -has-system-native- nil)
    (defun initialize-system-native-search ()
      (let ((path (merge-pathnames*
                   +system-native-library-file+
                   (pathname-directory-pathname -coreclr-location-))))
        (when (probe-file* path)
          (setf -has-system-native- t))))
    (register-image-restore-hook 'initialize-system-native-search))
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun load-system-native ()
      (when -has-system-native-
        (load-foreign-library-once 'system-native)))
    (register-image-restore-hook 'load-system-native))
  (defun init-native-aux-signals ()
    (when -has-system-native-
      (let ((fp (foreign-symbol-pointer
                 "SystemNative_InitializeTerminalAndSignalHandling"
                 :library 'system-native)))
        (when fp
          (foreign-funcall-pointer fp ()))))))

(defun %get-app-paths ()
  (let ((directories
          (delete-duplicates
           (mapcar #'native-path
                   (remove nil
                           (list
                            (pathname-directory-pathname -coreclr-location-)
                            #+windows
                            -desktop-sdk-dir-
                            -aspnet-sdk-dir-
                            (pathname-directory-pathname -interop-location-)
                            (get-pathname-defaults)
                            (lisp-implementation-directory)
                            (pathname-directory-pathname (get-exe-path)))))
           :test #'equalp)))
    (format nil (strcat "~{~a~^" (inter-directory-separator) "~}")
            (nreverse directories))))

(defun get-trusted-platform-assemblies ()
  "Retrieves a list of pathnames of trusted platform assemblies"
  (let ((defaults (directory* (make-pathname*
                               :name *wild*
                               :type "dll"
                               :defaults -coreclr-location-))))
    (when -aspnet-sdk-dir-
      (setf defaults (append defaults
                             (directory*
                              (make-pathname*
                               :name *wild*
                               :type "dll"
                               :defaults -aspnet-sdk-dir-)))))
    ;; On Windows, we want to default to Desktop SDK, should
    ;;  it be installed, to be able to use WinForms/WPF.
    ;; How's the addition of the desktop SDK dir to APP/NI
    ;;  paths is not enough?
    ;; Well, the thing is, those folders contain different
    ;;  WindowsBase.dll assembly, amongst others, and should we
    ;;  only include the coreclr directory for TPA assemblies, then WindowsBase
    ;;  would be not the WindowsBase the PresentationCore.dll etc are looking for.
    ;; So hence we want to default to the one that is present inside Desktop SDK dir.
    ;; It wont hurt apps that won't use WPF/WinForms, since Desktop SDK assemblies are
    ;;  simply extended ones of the defaults.
    #+coreclr-windows
    (if -desktop-sdk-dir-
      (loop :with desktop-asms = (directory*
                                  (make-pathname*
                                   :name *wild*
                                   :type "dll"
                                   :defaults -desktop-sdk-dir-))
            :for asm :in defaults
            :for replacement = (find-if (lambda (name)
                                          (string-equal (pathname-name asm)
                                                        name))
                                        desktop-asms
                                        :key #'pathname-name)
            :if replacement :collect replacement
              :else :collect asm)
      defaults)
    #-coreclr-windows
    defaults))

(defun %get-tpa-string ()
  (format nil (strcat "~{~a~^" (inter-directory-separator) "~}~a~a")
          (mapcar #'native-path (get-trusted-platform-assemblies))
          (inter-directory-separator)
          (native-path -interop-location-)))

(defun %get-trusted-assembly-names ()
  (loop :with tpa-dlls = (get-trusted-platform-assemblies)
        :for full-pathname :in tpa-dlls
        :for name = (pathname-name full-pathname)
        :when (and (string-prefix-p "System." name)
                   (not (string-suffix-p name ".Native")))
          :collect name))

;;; vim: ft=lisp et
