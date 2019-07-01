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

(defun %get-delegate (host domain-id name)
  (with-foreign-object (pp :pointer)
    (let ((rv (coreclr-create-delegate host
                                       domain-id
                                       "BikeInterop"
                                       "BikeInterop.Externals"
                                       name
                                       pp)))
      (if (zerop rv)
        (mem-ref pp :pointer)
        (error "Unable to get interop delegate '~a'" name)))))

(defun %do-initialize-coreclr (domain-name)
  (declare (type string domain-name))
  (let* ((exe (get-exe-path))
         (tpa (convert-to-foreign (%get-tpa) 'lpastr)))
    (unwind-protect
         (with-foreign-string (propkey "TRUSTED_PLATFORM_ASSEMBLIES"
                                       :encoding :ascii)
           (with-foreign-objects ((keys :pointer)
                                  (vals :pointer)
                                  (host :pointer)
                                  (domain-id :uint))
             (setf (mem-ref keys :pointer) propkey
                   (mem-ref vals :pointer) tpa)
             (let ((rv (coreclr-initialize exe
                                           domain-name
                                           1
                                           keys
                                           vals
                                           host
                                           domain-id)))
               (unless (zerop rv)
                 (error "Unable to initialize coreclr: HRESULT ~8,'0X" rv))
               (%coreclr-host :handle (mem-ref host :pointer)
                              :domain-id (mem-ref domain-id :uint)
                              :domain-name domain-name))))
      (free-converted-object tpa 'lpastr nil))))

(defun init-coreclr (&optional (domain-name "CommonLisp"))
  (let* ((host (%do-initialize-coreclr domain-name))
         (host-handle (%coreclr-host-handle host))
         (domain-id (%coreclr-host-domain-id host)))
    (macrolet ((frob (name foreign-name)
                 (let ((conc-name (symbolicate '%coreclr-host- name)))
                   `(setf (,conc-name host)
                          (%get-delegate host-handle domain-id ,foreign-name)))))
      (frob free-handle "FreeHandle")
      (frob get-type-by-name "GetTypeByName")
      (frob get-generic-type-by-name "GetGenericTypeByName")
      (frob make-generic-type "MakeGenericType")
      (frob make-array-type "MakeArrayType")
      (frob is-delegate-type "IsDelegateType")
      (frob get-field "GetField")
      (frob set-field "SetField")
      (frob get-property "GetProperty")
      (frob set-property "SetProperty")
      (frob get-index "GetIndex")
      (frob set-index "SetIndex")
      (frob invoke-constructor "InvokeConstructor")
      (frob invoke "Invoke")
      (frob invoke-delegate "InvokeDelegate")
      (frob box-boolean "BoxBoolean")
      (frob box-char "BoxChar")
      (frob box-uint8 "BoxUInt8")
      (frob box-int8 "BoxInt8")
      (frob box-uint16 "BoxUInt16")
      (frob box-int16 "BoxInt16")
      (frob box-uint32 "BoxUInt32")
      (frob box-int32 "BoxInt32")
      (frob box-uint64 "BoxUInt64")
      (frob box-int64 "BoxInt64")
      (frob box-intptr "BoxIntPtr")
      (frob box-single "BoxSingle")
      (frob box-double "BoxDouble")
      (frob box-string "BoxString")
      (frob unbox-boolean "UnboxBoolean")
      (frob unbox-char "UnboxChar")
      (frob unbox-uint8 "UnboxUInt8")
      (frob unbox-int8 "UnboxInt8")
      (frob unbox-uint16 "UnboxUInt16")
      (frob unbox-int16 "UnboxInt16")
      (frob unbox-uint32 "UnboxUInt32")
      (frob unbox-int32 "UnboxInt32")
      (frob unbox-uint64 "UnboxUInt64")
      (frob unbox-int64 "UnboxInt64")
      (frob unbox-intptr "UnboxIntPtr")
      (frob unbox-single "UnboxSingle")
      (frob unbox-double "UnboxDouble")
      (frob unbox-string "UnboxString")
      (frob get-string-length "GetStringLength")
      (frob install-callbacks "InstallCallbacks")
      (frob box-lisp-object "BoxLispObject")
      (frob unbox-lisp-object "UnboxLispObject")
      (frob is-lisp-object "IsLispObject")
      (frob is-type "IsType")
      (frob get-delegate-for-lisp-function "GetDelegateForLispFunction")
      (frob vector-get "VectorGet")
      (frob vector-set "VectorSet")
      (frob array-length "ArrayLength")
      (frob convert-to "ConvertTo")
      (frob get-delegate-trampoline "GetDelegateTrampoline")
      (frob get-accessor-trampolines "GetAccessorTrampolines")
      (frob get-full-type-code "GetFullTypeCode")
      (frob get-type-of "GetTypeOf")
      (frob get-type-full-name "GetTypeFullName")
      (frob get-type-assembly-qualified-name "GetTypeAssemblyQualifiedName")
      (setf *coreclr-host* host)
      (%install-callbacks (callback free-lisp-handle)
                          (callback apply))
      t)))

(defun shutdown-coreclr ()
  (when *coreclr-host*
    (with-foreign-object (exit-code :int)
      (let ((rv (coreclr-shutdown-2 (%coreclr-host-handle *coreclr-host*)
                                    (%coreclr-host-domain-id *coreclr-host*)
                                    exit-code)))
        (if (zerop rv)
          (prog1 (mem-ref exit-code :int)
            (setf *coreclr-host* nil))
          (error "Unable to shut down coreclr: HRESULT ~8,'0X" rv))))))

(uiop:register-image-restore-hook 'init-coreclr (not *coreclr-host*))
(uiop:register-image-restore-hook '%reload-type-table (not *type-table*))

;;; vim: ft=lisp et
