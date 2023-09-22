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

#+coreclr-64-bit
(progn
  (defctype size-t :uint64))

#+coreclr-32-bit
(progn
  (defctype size-t :uint32))

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
  (define-constant +invalid-handle-value+ (make-pointer (ldb (byte 64 0) -1))
    :test #'pointer-eq)
  (defconstant +generic-read+ #x80000000)
  (defconstant +file-share-read+ #x00000001)
  (defconstant +file-share-write+ #x00000002)
  (defconstant +file-share-delete+ #x00000004)
  (defconstant +open-existing+ 3)
  (defconstant +file-attributes-normal #x00000080)
  (defconstant +max-path+ 260)
  (define-constant +dos-path-prefixes+ '("\\\\?\\" "\\\\.\\")
    :test #'equal)

  (define-foreign-library kernel32
    (t "kernel32.dll"))
  (use-foreign-library kernel32)

  (defcfun (create-file "CreateFileW" :convention :stdcall
                                      :library kernel32)
      :pointer
    (file-name lpwstr)
    (access :uint32)
    (share-mode :uint32)
    (security-attributes :pointer)
    (create-disposition :uint32)
    (flags :uint32)
    (template :pointer))
  (defcfun (close-handle "CloseHandle" :convention :stdcall
                                       :library kernel32)
      :boolean
    (handle :pointer))
  (defcfun (get-oemcp "GetOEMCP" :convention :stdcall
                                 :library kernel32)
      :uint)
  (defcfun (get-last-error "GetLastError" :convention :stdcall
                                          :library kernel32)
      :uint32)
  (defcfun (%get-module-file-name
            "GetModuleFileNameW" :convention :stdcall
            :library kernel32)
      :uint32
    (module :pointer)
    (buffer :pointer)
    (size :uint32))
  (defcfun (%get-final-path-name-by-handle "GetFinalPathNameByHandleW"
                                           :convention :stdcall
                                           :library kernel32)
      :uint32
    (handle :pointer)
    (buffer :pointer)
    (len :uint32)
    (flags :uint32))
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
    (p-used-default-char (:pointer :bool)))
  (defcfun (get-full-path-name
            "GetFullPathNameW" :convention :stdcall
            :library kernel32)
      :int
    (file-name lpwstr)
    (buffer-length :uint32)
    (buffer :pointer)
    (file-part :pointer)))

#+coreclr-macos
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defcfun (ns-get-executable-path "_NSGetExecutablePath")
      :int
    (buf :pointer)
    (bufsize :uint32)))

#+coreclr-windows
(progn
  (declaim (inline alloc-lpwstr))
  (defun alloc-lpwstr (length)
    (declare (type (unsigned-byte 32) length))
    "Allocates null-terminated LPWSTR which can hold LENGTH characters."
    (let ((ptr (foreign-alloc :uint16 :count (1+ length))))
      (when (null-pointer-p ptr)
        (error "Unable to allocate null-terminated LPWSTR of length ~a"
               length))
      (setf (mem-aref ptr :uint16 length) 0)
      ptr))

  (declaim (inline read-lpwstr))
  (defun read-lpwstr (pointer)
    (declare (type foreign-pointer pointer))
    (values (foreign-string-to-lisp pointer :encoding :utf-16/le)))

  (defun signal-last-error (prefix)
    (declare (type string prefix))
    (error (strcat prefix " Error: #x~8,'0X")
           (get-last-error)))

  (defun get-module-file-name (&optional module)
    (declare (type (or null foreign-pointer) module))
    (let* ((module (or module (null-pointer)))
           (size +max-path+)
           (buf (alloc-lpwstr size)))
      (unwind-protect
           (loop :for rv = (%get-module-file-name module buf size)
                 :for last-error = (get-last-error)
                 :unless (= last-error 122)
                   :return (read-lpwstr buf)
                 :else :do
                   (foreign-free buf)
                   (setf size (* size 2)
                         buf (alloc-lpwstr size)))
        (foreign-free buf))))

  (defun get-final-path-name-by-handle (handle)
    (declare (type foreign-pointer handle))
    (let* ((size +max-path+)
           (buf (alloc-lpwstr size)))
      (unwind-protect
           (loop :for rv = (%get-final-path-name-by-handle handle buf (1+ size) 0)
                 :when (zerop rv)
                   :do (signal-last-error "Unable to get final path name.")
                 :when (> rv size)
                   :do (foreign-free buf)
                       (setf size (* size 2)
                             buf (alloc-lpwstr size))
                 :else :return (read-lpwstr buf))
        (foreign-free buf))))

  ;; Most lisp implementations do not handle UNC-like Windows paths well
  (defun strip-dos-path-prefix (path)
    (declare (type string path))
    (dolist (prefix +dos-path-prefixes+ path)
      (when (eql 0 (search prefix path :test #'char=))
        (let* ((prefix-len (length prefix))
               (unc-suffix "UNC\\")
               (unc-pos (search unc-suffix path :start2 prefix-len :test #'char-equal)))
          (return
            (if (eql prefix-len unc-pos)
              (uiop:strcat "\\\\" (subseq path (+ prefix-len (length unc-suffix))))
              (subseq path prefix-len)))))))

  (defun get-final-path-name (filename)
    (declare (type string filename))
    (let ((handle (create-file filename
                               +generic-read+
                               (logior +file-share-read+
                                       +file-share-write+
                                       +file-share-delete+)
                               (null-pointer)
                               +open-existing+
                               0
                               (null-pointer))))
      (when (pointer-eq handle +invalid-handle-value+)
        (signal-last-error (format nil "Unable to open file ~s." filename)))
      (unwind-protect
           (get-final-path-name-by-handle handle)
        (close-handle handle)))))

(defun get-exe-path ()
  "Retrieves the path to current executable file"
  #+coreclr-windows
  (strip-dos-path-prefix
   (get-final-path-name
    (get-module-file-name)))
  #+coreclr-macos
  (let* ((size 260)
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
  (let ((argv0 (argv0)))
    (unless argv0
      (error "Unable to get executable name"))
    argv0))

(defun native-path (path)
  "Retrieves native namestring for the file or directory"
  (declare (type (or pathname string) path))
  #+coreclr-windows
  (let* ((path (uiop:native-namestring path))
         (count (get-full-path-name path 0 (null-pointer) (null-pointer))))
    (with-foreign-object (ptr :uint16 count)
      (let ((rv (get-full-path-name path count ptr (null-pointer))))
        (when (zerop rv)
          (error "Unable to get native path. Error: #x~8.'0X" (get-last-error)))
        (read-lpwstr ptr))))
  #-coreclr-windows
  (uiop:native-namestring path))

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
