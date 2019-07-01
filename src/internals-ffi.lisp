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

(in-package #:bike-internals)

(define-constant +pointer-size+ (foreign-type-size :pointer))

(define-constant +pointer-bits+ (* 8 +pointer-size+))

(defctype lpwstr (:string :encoding :utf-16/le))

(define-foreign-type dotnet-char-type ()
  ()
  (:actual-type :uint16)
  (:simple-parser dnchar))

(defmethod translate-to-foreign (char (type dotnet-char-type))
  (logand #xFFFF (char-code char)))

(defmethod translate-from-foreign (int (type dotnet-char-type))
  (code-char int))

(defmethod expand-to-foreign (char (type dotnet-char-type))
  `(logand #xFFFF (char-code ,char)))

(defmethod expand-from-foreign (int (type dotnet-char-type))
  `(code-char ,int))

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
    (size :uint32))
  (defcfun (multi-byte-to-wide-char
            "MultiByteToWideChar" :convention :stdcall
            :library kernel32)
      :int
    (code-page :uint)
    (flags :uint32)
    (mbstr :pointer)
    (cbmbstr :int)
    (wstr :pointer)
    (cchwstr :int))
  (defcfun (wide-char-to-multi-byte
            "WideCharToMultiByte" :convention :stdcall
            :library kernel32)
      :int
    (code-page :uint)
    (flags :uint32)
    (wstr :pointer)
    (cchwstr :int)
    (mbstr :pointer)
    (cbmbstr :int)
    (p-default-char (:pointer :char))
    (p-used-default-char (:pointer :bool))))

#+corecl-macos
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defcfun (ns-get-executable-path "_NSGetExecutablePath")
      :int
    (buf :pointer)
    (bufsize :uint32)))

(defun get-exe-path ()
  "Retrieves the path to current executable file"
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


;; i hope that no one who is in his mind
;;   uses anything else for paths on Linux/Unix systems
#-coreclr-windows
(defctype lpastr (:string :encoding :utf-8))

#+coreclr-windows
(progn
  (defconstant +cp-thread-acp+ 3)

  ;; we shouldn't bother writing type translator optimizers
  ;;  because of it is being used only on CoreCLR loading
  (define-foreign-type windows-ansi-string-type ()
    ()
    (:actual-type :pointer)
    (:simple-parser lpastr))

  (defmacro %error-cannot-translate-acp-string ()
    `(error "Unable to translate Windows ACP string. Last error: ~D" (get-last-error)))

  (defmethod translate-to-foreign (string (type windows-ansi-string-type))
    (declare (type string string))
    (with-foreign-string (ptr string :encoding :utf-16/le)
      (let* ((length (length string))
             (rv (wide-char-to-multi-byte +cp-thread-acp+
                                          0
                                          ptr
                                          length
                                          (null-pointer)
                                          0
                                          (null-pointer)
                                          (null-pointer))))
        (when (zerop rv) (%error-cannot-translate-acp-string))
        (let* ((buffer (foreign-alloc :uint8 :count (1+ rv) :initial-element 0))
               (rv (wide-char-to-multi-byte +cp-thread-acp+
                                            0
                                            ptr
                                            length
                                            buffer
                                            (1+ rv)
                                            (null-pointer)
                                            (null-pointer))))
          (when (zerop rv) (%error-cannot-translate-acp-string))
          buffer))))

  (defmethod translate-from-foreign (pointer (type windows-ansi-string-type))
    (declare (type foreign-pointer pointer))
    (let ((rv (multi-byte-to-wide-char +cp-thread-acp+
                                       0
                                       pointer
                                       -1
                                       (null-pointer)
                                       0)))
      (when (zerop rv) (%error-cannot-translate-acp-string))
      (with-foreign-pointer (ptr (* (1+ rv) (foreign-type-size :uint16)) size)
        (let ((rv2 (multi-byte-to-wide-char +cp-thread-acp+
                                            0
                                            pointer
                                            -1
                                            ptr
                                            rv)))
          (when (zerop rv2) (%error-cannot-translate-acp-string))
          (foreign-string-to-lisp ptr :count size :max-chars (1- rv2) :encoding :utf-16/le)))))

  (defmethod free-translated-object (pointer (type windows-ansi-string-type) param)
    (declare (ignore param))
    (foreign-free pointer)))


;;; vim: ft=lisp et
