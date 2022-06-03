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
                        *foreign-library-directories*
                        :test #'equalp))
        (error "Unable to find CoreCLR")))
    (let ((lib (find-interop t)))
      (if lib
        (progn (setf *interop-location* lib)
               (pushnew (uiop:pathname-directory-pathname lib)
                        *foreign-library-directories*
                        :test #'equalp))
        (error "Unable to find BikeInterop.dll")))
    (values))
  (uiop:register-image-restore-hook 'init-coreclr-search-location))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library coreclr
    (t #.+coreclr-library-file+))

  (unless (foreign-library-loaded-p 'coreclr)
    (use-foreign-library coreclr)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %get-related-app-path (name)
    (cl-ppcre:regex-replace
     "(?i)Microsoft\\.NetCore\\.App"
     (native-path (uiop:pathname-directory-pathname *coreclr-location*))
     name)))

#+coreclr-windows
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar *desktop-sdk-dir* nil)
    (defun initialize-wpfgfx-search-location ()
      (let* ((desktop-sdk-dir (pathname
                               (%get-related-app-path "Microsoft.WindowsDesktop.App"))))
        (if (uiop:probe-file*
             (uiop:make-pathname* :name "wpfgfx_cor3.dll"
                                  :defaults desktop-sdk-dir))
          (pushnew (setf *desktop-sdk-dir* desktop-sdk-dir)
                   *foreign-library-directories*
                   :test #'equalp)
          (setf *desktop-sdk-dir* nil))))
    (uiop:register-image-restore-hook 'initialize-wpfgfx-search-location))

  (eval-when (:compile-toplevel :load-toplevel :execute)
    ;; These libs should be pre-loaded, otherwise WPF would be unable
    ;;   to find them, for who knows what reasons

    (define-foreign-library vcruntime
      (t "vcruntime140_cor3.dll"))
    (define-foreign-library d3dcompiler
      (t "D3DCompiler_47_cor3.dll"))
    (define-foreign-library wpfgfx
      (t "wpfgfx_cor3.dll"))
    (defun load-wpfgfx ()
      (when *desktop-sdk-dir*
        (unless (foreign-library-loaded-p 'vcruntime)
          (use-foreign-library vcruntime))
        (unless (foreign-library-loaded-p 'd3dcompiler)
          (use-foreign-library d3dcompiler))
        (unless (foreign-library-loaded-p 'wpfgfx)
          (use-foreign-library wpfgfx))))
    (uiop:register-image-restore-hook 'load-wpfgfx)))

#-coreclr-windows
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar *has-libsystem-native* nil)
    (define-foreign-library libsystem-native
      #+coreclr-macos
      (t "libSystem.Native.dylib")
      #-coreclr-macos
      (t "libSystem.Native.so"))
    (defun initialize-libsystem-native-search ()
      (let ((path (uiop:merge-pathnames*
                   #+coreclr-macos
                   "libSystem.Native.dylib"
                   #-coreclr-macos
                   "libSystem.Native.so"
                   (uiop:pathname-directory-pathname *coreclr-location*))))
        (when (uiop:probe-file* path)
          (setf *has-libsystem-native* t))))
    (uiop:register-image-restore-hook 'initialize-libsystem-native-search))
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun load-libsystem-native ()
      (when *has-libsystem-native*
        (use-foreign-library libsystem-native)))
    (uiop:register-image-restore-hook 'load-libsystem-native))
  (defun init-native-aux-signals ()
    (when *has-libsystem-native*
      (let ((fp (foreign-symbol-pointer
                 "SystemNative_InitializeTerminalAndSignalHandling"
                 :library 'libsystem-native)))
        (when fp
          (foreign-funcall-pointer fp ()))))))

(defun %get-app-paths ()
  (let ((directories
          (delete-duplicates
           (mapcar #'native-path
                   (remove nil
                           (list
                            (uiop:pathname-directory-pathname *coreclr-location*)
                            #+windows
                            *desktop-sdk-dir*
                            (uiop:pathname-directory-pathname *interop-location*)
                            (uiop:get-pathname-defaults)
                            (uiop:lisp-implementation-directory)
                            (uiop:pathname-directory-pathname (get-exe-path)))))
           :test #'equalp)))
    (format nil (uiop:strcat "~{~a~^" (uiop:inter-directory-separator) "~}")
            (nreverse directories))))

(defun get-trusted-platform-assemblies ()
  "Retrieves a list of pathnames of trusted platform assemblies"
  (let ((defaults (uiop:directory* (uiop:make-pathname*
                                    :name uiop:*wild*
                                    :type "dll"
                                    :defaults *coreclr-location*))))
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
    (if *desktop-sdk-dir*
      (loop :with desktop-asms = (uiop:directory*
                                  (uiop:make-pathname*
                                   :name uiop:*wild*
                                   :type "dll"
                                   :defaults *desktop-sdk-dir*))
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
  (format nil (uiop:strcat "~{~a~^" (uiop:inter-directory-separator) "~}~a~a")
          (mapcar #'native-path (get-trusted-platform-assemblies))
          (uiop:inter-directory-separator)
          (native-path *interop-location*)))

(defun %get-trusted-assembly-names ()
  (loop :with tpa-dlls = (get-trusted-platform-assemblies)
        :for full-pathname :in tpa-dlls
        :for name = (pathname-name full-pathname)
        :when (and (uiop:string-prefix-p "System." name)
                   (not (uiop:string-suffix-p name ".Native")))
          :collect name))

;;; vim: ft=lisp et
