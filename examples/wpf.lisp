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
  (:use #:cl #:bike #:uiop #:named-readtables)
  (:shadowing-import-from #:closer-mop
                          #:defclass
                          #:defmethod
                          #:slot-value-using-class
                          #:slot-definition-name))

(in-package #:bike-examples/wpf)

(in-readtable bike-syntax)

;; XAML file we use to instantiate top-level control in a window
(defvar *xaml-path*
  (native-path
   (merge-pathnames*
    (pathname-directory-pathname #.(current-lisp-file-pathname))
    "WpfUserControl.xaml")))

;; some global state
(defvar *dispatcher* nil)
(defvar *window* nil)
;; view model object requires to be alive during the UI session
;;   because of callable class limitations:
;; Generated .Net proxies only hold weak references to callable class instances,
;;   to avoid memory leaks, so a proxy could be orphaned, i.e. it can
;;   outlive its parent. This, of course, prevents access to object slots
;;   and methods(because there is no object anymore).
;;   Under normal circumstances, should such a proxy be encountered,
;;   an error is signaled. But sometimes, as in WPF case, we don't care
;;   that much, and can invoke the `continue' restart.
;;   Nevertheless, we must retain the object reference somewhere.
(defvar *vm* nil)

;; import WPF assemblies
(import-assembly 'PresentationFramework)
(import-loaded-assemblies)

;; use some namespaces
(use-namespace 'System)
(use-namespace 'System.Collections.Generic)
(use-namespace 'System.IO)
(use-namespace 'System.Threading)
(use-namespace 'System.Windows)
(use-namespace 'System.Windows.Input)
(use-namespace '(System.ComponentModel System.Collections.ObjectModel))

;; Delegate command pattern implementation
(define-dotnet-callable-class (delegate-command (:interfaces ICommand)) ()
  "A Lisp implementation of Delegate Commmand pattern."
  (can-execute-callback :initarg :can-execute-callback
                        :initform (constantly t)
                        :accessor can-execute-callback)
  (execute-callback :initarg :execute-callback
                    :initform (constantly nil)
                    :accessor execute-callback)
  ;; This slot 'holds' event handler wrapper, which
  ;;   could be obtained by means of 'slot-value'.
  ;; It can then be funcall'ed to invoke
  ;;   delegates subscribed on the event.
  (:event can-execute-changed EventHandler)
  ;; :defmethod designates that generic methods are generated
  ;;   on the lisp side.
  ;; :method would generate usual functions.
  ;;  Names, unless specified, are generated using
  ;;  the `camel-case-string' function from `bike-internals'
  ;;  package.
  (:defmethod ((can-execute-p "CanExecute")) :bool ((param :object))
    "Checks whether command can begin execution"
    ;; Notice that the macro interns `this' symbol
    ;;   in the current package.
    (funcall (can-execute-callback this) param))
  ;; Execute and CanExecute methods simply delegate
  ;;   their code to external callbacks.
  ;; A common pattern in WPF and Avalonia.
  (:defmethod execute :void ((param :object))
    "Executes command"
    (funcall (execute-callback this) param)))

(define-dotnet-callable-class
    (view-model-base (:interfaces INotifyPropertyChanged)) ()
  "Base class for view models"
  ;; PropertyChangedEventHandler accepts two arguments:
  ;;   1) a `sender' :object (commonly used for passing `this' around), and
  ;;   2) an object of type PropertyChangedEventArgs,
  ;;      which holds property name
  (:event property-changed PropertyChangedEventHandler
   :reader vm-property-changed))

(define-dotnet-callable-class symbol-view-model (view-model-base)
  "View model for symbols"
  (symbol :accessor svm-symbol :initarg :symbol)
  ;; :property slots are visible on .Net side as object properties.
  (:property (name "SymbolName") :string :initarg :name :accessor svm-name))

(define-dotnet-callable-class package-view-model (view-model-base)
  "View model for packages"
  (package :initarg :package :accessor pvm-package)
  (:property (name "PackageName")
   :string :initarg :name
           :accessor pvm-name))

(define-dotnet-callable-class example-view-model
    (view-model-base)
  "View model class for WPF example"
  ;; this slot holds a list of package-view-model objects for all packages
  (packages :initarg :packages
            :initform nil
            :accessor evm-packages)
  ;; this slot holds a list of symbol-view-model objects
  ;;   for external symbols of the selected package
  (symbols :initform nil
           :accessor evm-symbols)
  ;; an observable collection which wraps packages slot
  (:property (packages-view "Packages" :setter nil) (ObservableCollection :object)
   :initform (new '(ObservableCollection :object))
   :accessor evm-packages-view)
  ;; an observable collection which wraps symbols slot
  (:property (symbols-view "Symbols" :setter nil) (ObservableCollection :object)
   :accessor evm-symbols-view
   :initform (new '(ObservableCollection :object)))
  ;; currently selected package. Most of the time, it is set by ListView.
  (:property selected-package :object
   :initform nil
   :accessor evm-selected-package)
  ;; currently selected symbol. Most of the time, it is set by ListView.
  (:property selected-symbol :object
   :initform nil)
  ;; symbol description string
  (:property (symbol-description :setter nil) :string
   :accessor evm-symbol-description)
  ;; a command for refresh button
  (:property (refresh-command :setter nil) ICommand
   :initarg :refresh-command)
  (:property search :string
   :initarg nil
   :accessor evm-search))

(defun make-delegate-command (execute-callback &optional can-execute-callback)
  (declare (type (or symbol function) execute-callback can-execute-callback))
  "Creates an instance of delegate-command"
  (make-instance 'delegate-command
                 :execute-callback execute-callback
                 :can-execute-callback (or can-execute-callback
                                           (constantly t))))

(defun symbol-full-name (symbol)
  (declare (type symbol symbol))
  "Returns full name of a symbol"
  (let ((package (symbol-package symbol))
        (keyword-package (find-package :keyword))
        (name (symbol-name symbol)))
    (with-output-to-string (out)
      (cond ((null package) (write-string "#:" out))
            ((eq package keyword-package) (write-string ":" out))
            (t (write-string (package-name package) out)
               (multiple-value-bind (symbol status)
                   (find-symbol name package)
                 (declare (ignore symbol))
                 (write-string (if (eq status :external) ":" "::")
                               out))))
      (write-string name out))))

(defun make-symbol-view-model (symbol &optional full-name)
  (declare (type symbol symbol))
  "Creates an instance of a view model representing a symbol"
  (let ((name (if full-name
                (symbol-full-name symbol)
                (symbol-name symbol))))
    (make-instance 'symbol-view-model
                   :symbol symbol
                   :name name)))

(defun make-package-view-model (package)
  "Creates an instance of a view model representing a package"
  (make-instance 'package-view-model
                 :package (find-package package)
                 :name (package-name package)))

(defun get-package-vms ()
  "Retrieves view models for all existing packages"
  (mapcar #'make-package-view-model
          (sort (list-all-packages)
                #'string<
                :key #'package-name)))

(defgeneric get-symbol-vms (designator)
  (:documentation "Retrieves list of symbol view models based on DESIGNATOR"))

(defmethod get-symbol-vms ((package package))
  "Returns view models for external symbols in a package"
  (loop :for s :being :the :external-symbols :in package
        :collect s :into result
        :finally (return (mapcar #'make-symbol-view-model
                                 (sort result #'string<
                                       :key #'symbol-name)))))

(defmethod get-symbol-vms ((search string))
  "Returns view models for the search string"
  (loop :for symbol :in (apropos-list search)
        :for (s status) = (multiple-value-list
                           (find-symbol (symbol-name symbol)
                                        (symbol-package symbol)))
        :when (and s (eq status :external))
          :collect (make-symbol-view-model symbol t) :into vms
        :finally (return (sort vms #'string<
                               :key #'svm-name))))

(defun refresh-packages (evm)
  "Refreshes list of existing packages."
  (let* ((evm (unwrap-dotnet-callable-proxy evm)) ;; get actual object
         (packages-oc (evm-packages-view evm))
         (symbols-oc (evm-symbols-view evm))
         (package-vms (get-package-vms))) ;; get vms for all packages
    ;; clear observable collections and description slot
    [packages-oc Clear]
    [symbols-oc Clear]
    (setf (evm-symbol-description evm) nil
          (evm-search evm) nil)
    ;; add package view models to observable collection
    (dolist (pvm package-vms)
      [packages-oc Add pvm])
    ;; store package view models inside the main VM
    (setf (evm-packages evm) package-vms
          ;; select first package for display
          (evm-selected-package evm) (first package-vms))))

(defun make-example-view-model ()
  "Creates an instance of main view model"
  (let* ((package-vms (get-package-vms)) ;; view models for all packages
         ;; delegate command for refresh button:
         (refresh-command (make-delegate-command 'refresh-packages))
         (vm (make-instance 'example-view-model
                            :packages package-vms
                            :refresh-command refresh-command)))
    (refresh-packages vm)
    vm))

(defun on-package-selected (evm package-vm)
  "Invoked on selected package change."
  (when package-vm
    (with-slots (package) package-vm
      (with-slots (symbols-view symbols) evm
        ;; Clear observable collection
        [symbols-view Clear]
        ;; Compute external symbols of a package
        (setf symbols (get-symbol-vms package))
        ;; Add view models to observable collection
        (dolist (s symbols)
          [symbols-view Add s])))))

(defun on-symbol-changed (evm symbol-vm)
  "Invoked on selected symbol change."
  (when symbol-vm
    ;; set symbol description slot for UI
    (setf (evm-symbol-description evm)
          (with-output-to-string (out)
            (describe (svm-symbol symbol-vm) out)))))

(defun on-search-changed (evm search)
  "Invoked when search text has changed"
  (with-slots (selected-package symbols-view symbols) evm
    (unless [:string IsNullOrWhitespace search]
      ;; Clear package selection
      (setf selected-package nil)
      ;; Clear observable collection
      [symbols-view Clear]
      ;; Compute apropos-list
      (setf symbols (get-symbol-vms search))
      ;; Add view models to observable collection
      (dolist (s symbols)
        [symbols-view Add s]))))

(defmethod (setf slot-value-using-class) :after
    (new-value
     (class dotnet-callable-class)
     (object example-view-model)
     (slotd effective-property-slot-definition))
  (handler-case
      (let* ((dotnet-name (slot-definition-dotnet-name slotd))
             ;; (setf slot-value) may be called by make-instance before
             ;; an object is fully initialized
             (proxy-present-p (dotnet-callable-object-proxy-initialized-p object))
             ;; slot-value for event slot returns a wrapper function which
             ;;  represents current state of a multicast event delegate.
             ;; Or NIL.
             ;; So basically, this function is used to invoke event subscribers.
             (handler (and proxy-present-p (vm-property-changed object))))
        (case (slot-definition-name slotd)
          (selected-package
           ;; Something(e.g. the ListView) changed the selected package
           (on-package-selected object (unwrap-dotnet-callable-proxy new-value)))
          (selected-symbol
           ;; Something(e.g. the ListView) changed the selected symbol
           (on-symbol-changed object (unwrap-dotnet-callable-proxy new-value)))
          (search
           (on-search-changed object new-value)))
        (when handler
          (funcall handler object (new 'PropertyChangedEventArgs dotnet-name))))
    (error (e) (format t "~&~a~%" e))))

(defun load-xaml (path)
  "Loads an object from XAML file."
  (with-disposable (stream [:File OpenRead (native-path path)])
    [:System.Windows.Markup.XamlReader Load stream]))

(defun call-with-dispatcher (function)
  "Executes a function in the context of current WPF dispatcher."
  (let ((dispatcher *dispatcher*))
    (unless dispatcher
      (error "Dispatcher not running"))
    ;; Execute some code in UI thread. Useful for on-the-fly reloading
    [dispatcher Invoke (new '(Func :object) function)]))

(defmacro with-dispatcher (&body body)
  "Executes BODY forms in the context of current WPF dispatcher."
  `(call-with-dispatcher
    (lambda (&aux (*print-readably* nil))
      (handler-case (progn ,@body)
        (error (e) (format *error-output* "~&~a~%" e))))))

;; This method could be called after editing the XAML file
;;   while the UI thread is still running.
;; This allows for rapid UI prototyping.
(defun reload-content ()
  "Reloads toplevel WPF control from XAML file."
  (with-dispatcher
    (let* ((window *window*)
           (control (load-xaml *xaml-path*)))
      ;; Set the control as the toplevel content for the window.
      ;; Also, set some window properties.
      (setf [window %Content] control
            [window %Title] "WPF Package Explorer for Common Lisp"
            [window %ShowActivated] t
            [window %ShowInTaskbar] t
            [window %WindowStartupLocation] #e(WindowStartupLocation CenterScreen)))))

(defun bike-examples:hello-wpf ()
  (when *dispatcher*
    (error "UI thread is already running"))
  (bt2:make-thread
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
     ;; WPF frequently causes floating-point exceptions when float traps are unmasked.
     ;; So, just disable them for the UI thread.
     (cffi:foreign-funcall "_fpreset")
     ;; Create a window and set global variables
     (let ((window (new 'System.Windows.Window))
           (*print-readably* nil)
           (*print-pretty* t)
           ;; create a view model for the window
           (view-model (make-example-view-model)))
       (setf *dispatcher* [window %Dispatcher]
             *window* window
             *vm* view-model
             ;; set our view model as DataContext for the window.
             [window %DataContext] view-model)
       (unwind-protect
            (handler-case
                (handler-bind ((dotnet-callable-object-orphan-proxy
                                 ;; Stale proxies may originate from class redefenitions
                                 ;;   during file recompilation or from
                                 ;;   stale references in controls.
                                 ;; It is almost safe to ignore such proxies
                                 ;;   in WPF context.
                                 ;; The results of `continue' restart invocation
                                 ;;   would be nulls, default values and invalid casts
                                 ;;   all around, but WPF can handle such
                                 ;;   situations(mostly).
                                 (lambda (e &aux (*print-dotnet-object* nil))
                                   (format *debug-io*
                                           "~&Stale proxy ~s for op ~s (~{~s~^ ~})~%"
                                           (dotnet-callable-object-orphan-proxy-value e)
                                           (dotnet-callable-object-orphan-proxy-operation e)
                                           (dotnet-callable-object-orphan-proxy-arguments e))
                                   (continue e))))
                  (progn
                    ;; load XAML content and show window
                    (reload-content)
                    [window ShowDialog]))
              (error (e) (format *error-output* "~&~a~%" e)))
         (setf *dispatcher* nil
               *vm* nil
               *window* nil
               [window %DataContext] nil
               [window %Content] nil))))))

;;; vim: ft=lisp et
