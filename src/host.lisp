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

(defun %get-delegate (host-handle domain-id name)
  (with-foreign-object (pp :pointer)
    (let ((rv (coreclr-create-delegate host-handle
                                       domain-id
                                       "BikeInterop"
                                       "BikeInterop.Externals"
                                       name
                                       pp)))
      (if (zerop rv)
        (mem-ref pp :pointer)
        (error "Unable to get interop delegate '~a'" name)))))

(defmacro define-coreclr-host
    (name (handle-slot domain-id-slot domain-name-slot vtable-slot)
     &body delegates)
  (let* ((var-name (symbolicate '+ name '+))
         (index-var-name (symbolicate '+ name '- 'indices '+))
         (predicate (symbolicate name '- 'p))
         (conc-name (symbolicate '% name '-))
         (constructor (symbolicate '% name))
         (delegate-count (length delegates))
         (vtable-accessor (symbolicate conc-name vtable-slot)))
    `(progn (#+sbcl sb-ext:defglobal #-sbcl defvar ,var-name nil)
            (defstruct (,name (:constructor ,constructor (,handle-slot
                                                          ,domain-id-slot
                                                          ,domain-name-slot))
                              (:copier nil)
                              (:predicate ,predicate)
                              (:conc-name ,conc-name))
              "Represents initialized CoreCLR host"
              (,handle-slot (required-slot) :type foreign-pointer :read-only t)
              (,domain-id-slot (required-slot) :type (unsigned-byte 32)
                                               :read-only t)
              (,domain-name-slot (required-slot) :type string
                                                 :read-only t)
              (,vtable-slot
               (foreign-alloc :pointer :count ,delegate-count)
               :type foreign-pointer
               :read-only t))
            (declaim (type (or null ,name) ,var-name))
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (#+sbcl sb-ext:defglobal #-sbcl defvar ,index-var-name (make-hash-table :test #'eq))
              (clrhash ,index-var-name)
              ,@(loop :for i :from 0
                      :for form :in delegates
                      :collect `(setf (gethash ',(car form) ,index-var-name) ,i)))
            (defun ,name (,handle-slot ,domain-id-slot ,domain-name-slot)
              (declare (type foreign-pointer ,handle-slot)
                       (type (unsigned-byte 32) ,domain-id-slot)
                       (type string ,domain-name-slot))
              "Initializes CoreCLR host"
              (let* ((,name (,constructor ,handle-slot ,domain-id-slot ,domain-name-slot))
                     (,vtable-slot (,vtable-accessor ,name)))
                ,@(loop :for i :from 0
                        :for form :in delegates
                        :collect
                        (destructuring-bind
                            (lisp-name foreign-name) form
                          (declare (ignore lisp-name))
                          `(setf (mem-aref ,vtable-slot :pointer ,i)
                                 (%get-delegate ,handle-slot
                                                ,domain-id-slot
                                                ,foreign-name))))
                (setf ,var-name ,name)
                (values)))
            (defmacro hostcall (name &rest args-and-types)
              (let ((index (gethash name ,index-var-name)))
                (unless index (error "Undefined host call: ~s" name))
                `(foreign-funcall-pointer
                  (mem-aref (,',vtable-accessor (the ,',name ,',var-name)) :pointer ,index)
                  (:convention :stdcall)
                  ,@args-and-types))))))

(define-coreclr-host coreclr-host (handle domain-id domain-name vtable)
  (free-handle "FreeHandle")
  (get-type-of "GetTypeOf")
  (get-type-by-name "GetTypeByName")
  (get-type-full-name "GetTypeFullName")
  (get-full-type-code "GetFullTypeCode")
  (box-boolean "BoxBoolean")
  (box-char "BoxChar")
  (box-uint8 "BoxUInt8")
  (box-int8 "BoxInt8")
  (box-uint16 "BoxUInt16")
  (box-int16 "BoxInt16")
  (box-uint32 "BoxUInt32")
  (box-int32 "BoxInt32")
  (box-uint64 "BoxUInt64")
  (box-int64 "BoxInt64")
  (box-intptr "BoxIntPtr")
  (box-single "BoxSingle")
  (box-double "BoxDouble")
  (box-string "BoxString")
  (unbox-boolean "UnboxBoolean")
  (unbox-char "UnboxChar")
  (unbox-uint8 "UnboxUInt8")
  (unbox-int8 "UnboxInt8")
  (unbox-uint16 "UnboxUInt16")
  (unbox-int16 "UnboxInt16")
  (unbox-uint32 "UnboxUInt32")
  (unbox-int32 "UnboxInt32")
  (unbox-uint64 "UnboxUInt64")
  (unbox-int64 "UnboxInt64")
  (unbox-intptr "UnboxIntPtr")
  (unbox-single "UnboxSingle")
  (unbox-double "UnboxDouble")
  (unbox-string "UnboxString")
  (get-string-length "GetStringLength")
  (box-lisp-object "BoxLispObject")
  (unbox-lisp-object "UnboxLispObject")
  (is-lisp-object "IsLispObject")
  (pin-object "PinObject")
  (array-get "ArrayGet")
  (array-set "ArraySet")
  (vector-get "VectorGet")
  (vector-set "VectorSet")
  (array-length "ArrayLength")
  (get-field "GetField")
  (set-field "SetField")
  (get-property "GetProperty")
  (set-property "SetProperty")
  (get-index "GetIndex")
  (set-index "SetIndex")
  (invoke-constructor "InvokeConstructor")
  (invoke "Invoke")
  (get-delegate-for-lisp-function "GetDelegateForLispFunction")
  (get-delegate-trampoline "GetDelegateTrampoline")
  (get-accessor-trampolines "GetAccessorTrampolines")
  (enum-to-object "EnumToObject")
  (make-vector-of "MakeVectorOf")
  (make-array-of "MakeArrayOf")
  (install-callbacks "InstallCallbacks")
  (initialize-corefx-signals "InitializeCoreFxSignals")
  (get-loaded-assemblies "GetLoadedAssemblies"))

(defun initialize-coreclr (&optional (domain-name "CommonLisp"))
  (declare (type string domain-name))
  (let* ((exe (get-exe-path))
         (tpa (convert-to-foreign (%get-tpa-string) 'lpastr))
         (assembly-dirs (%get-app-paths))
         (app-paths (convert-to-foreign assembly-dirs 'lpastr))
         (app-ni-paths (convert-to-foreign assembly-dirs 'lpastr)))
    (unwind-protect
         (with-foreign-strings ((tpa-key "TRUSTED_PLATFORM_ASSEMBLIES"
                                         :encoding :ascii)
                                (app-paths-key "APP_PATHS"
                                               :encoding :ascii)
                                (app-ni-paths-key "APP_NI_PATHS"
                                                  :encoding :ascii))
           (with-foreign-objects ((keys :pointer 3)
                                  (vals :pointer 3)
                                  (host :pointer)
                                  (domain-id :uint))
             (setf (mem-aref keys :pointer 0) tpa-key
                   (mem-aref keys :pointer 1) app-paths-key
                   (mem-aref keys :pointer 2) app-ni-paths-key
                   (mem-aref vals :pointer 0) tpa
                   (mem-aref vals :pointer 1) app-paths
                   (mem-aref vals :pointer 2) app-ni-paths
                   (mem-ref domain-id :uint) 0
                   (mem-ref host :pointer) (null-pointer))
             (locally (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
               (let ((rv (coreclr-initialize exe
                                             domain-name
                                             3
                                             keys
                                             vals
                                             host
                                             domain-id)))
                 (declare (type (unsigned-byte 32) rv))
                 #+coreclr-restore-signals
                 (progn
                   ;; save coreclr signals
                   (dotimes (i +nsig+)
                     ;; Some of them would of course be ours lisp signals reestablished earlier
                     (sigaction i (null-pointer) (sigaction-address +dotnet-sigactions+ i)))
                   #+sbcl
                   (foreign-funcall "restore_sbcl_signals")
                   #-sbcl
                   (restore-lisp-sigactions))
                 (unless (zerop rv)
                   (error "Unable to initialize coreclr: HRESULT ~8,'0X" rv))))
             (coreclr-host (mem-ref host :pointer)
                           (mem-ref domain-id :uint)
                           domain-name)
             (values)))
      (free-converted-object tpa 'lpastr nil)
      (free-converted-object app-paths 'lpastr nil)
      (free-converted-object app-ni-paths 'lpastr nil))))

(uiop:register-image-restore-hook #'initialize-coreclr (not +coreclr-host+))

(defun shutdown-coreclr ()
  (let ((host +coreclr-host+))
    (if host
      (with-foreign-object (pcode :int)
        (setf +coreclr-host+ nil)
        (let ((rv (coreclr-shutdown-2 (%coreclr-host-handle host)
                                      (%coreclr-host-domain-id host)
                                      pcode)))
          (unless (zerop rv)
            (error "Unable to shutdown coreclr: HRESULT ~8,'0X" rv))
          (mem-ref pcode :int)))
      0)))

#+coreclr-restore-signals
(progn

  (defun initialize-dotnet-sigactions ()
    "Initializes CoreFX sigactions, namely for SIGCHLD, SIGCONT, SIGINT, SIGQUIT.
 We utilize CoreFX handlers for some/all of this, but use lisp signal masks for others"
    ;; first, force initialization of corefx signal handlers
    (init-native-aux-signals)
    (hostcall initialize-corefx-signals :void)

    (setf +new-sigactions+ (foreign-alloc '(:struct sigaction) :count +nsig+))
    (dotimes (i +nsig+)
      (sigaction i (null-pointer) (sigaction-address +new-sigactions+ i))
      (unless (pointer-eq (foreign-slot-value (sigaction-address +new-sigactions+ i)
                                              '(:struct sigaction)
                                              'handler)
                          (foreign-slot-value (sigaction-address +lisp-sigactions+ i)
                                              '(:struct sigaction)
                                              'handler))
        (sigaction i (null-pointer) (sigaction-address +dotnet-sigactions+ i))))
    (foreign-funcall "memcpy" :pointer +new-sigactions+
                              :pointer +lisp-sigactions+
                              size-t (* +nsig+ (foreign-type-size '(:struct sigaction))))

    (disable-all-posix-signal-handling)

    (flet ((collect-signals (from)
             (loop :for i :below +nsig+
                   :for saddr = (sigaction-address from i)
                   :for handler = (foreign-slot-value saddr '(:struct sigaction) 'handler)
                   :unless (or (pointer-eq handler (make-pointer +sig-dfl+))
                               (pointer-eq handler (make-pointer +sig-ign+)))
                     :collect (cons i handler))))
      (let* ((lisp-signals (collect-signals +lisp-sigactions+))
             (dotnet-signals (collect-signals +dotnet-sigactions+))
             (changed-dotnet-signals
               (remove-if (lambda (sig)
                            (not
                             (let ((dsig (find (car sig) lisp-signals
                                               :key #'car)))
                               (and dsig
                                    (not (pointer-eq (cdr sig)
                                                     (cdr dsig)))))))
                          dotnet-signals))
             (new-dotnet-signals (remove-if (lambda (sig)
                                              (find sig lisp-signals :key #'car))
                                            dotnet-signals :key #'car)))
        (declare (ignore changed-dotnet-signals))
        (dolist (sig new-dotnet-signals)
          (let ((sig (car sig)))
            (foreign-funcall "memcpy" :pointer (sigaction-address +lisp-sigactions+ sig)
                                      :pointer (sigaction-address +dotnet-sigactions+ sig)
                                      size-t (foreign-type-size '(:struct sigaction)))))))



    ;; only use SIGCHLD for now, dotnet background thread perfectly handles it
    (dolist (sig (list +sigchld+))
      (setf (foreign-slot-value (sigaction-address +new-sigactions+ sig)
                                '(:struct sigaction) 'handler)
            (foreign-slot-value (sigaction-address +dotnet-sigactions+ sig)
                                '(:struct sigaction) 'handler)))

    ;; init signals from new sigactions
    (dotimes (i +nsig+)
      (let ((addr (sigaction-address +new-sigactions+ i)))
        (sigaction i addr (null-pointer)))))

  (uiop:register-image-restore-hook #'initialize-dotnet-sigactions
                                    (null-pointer-p +new-sigactions+)))

;;; vim: ft=lisp et
