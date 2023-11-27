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

(uiop:define-package #:bike-examples/aspnet-mvc
  (:use #:cl #:bike #:uiop)
  (:import-from #:alexandria
                #:define-constant)
  (:import-from #:named-readtables
                #:in-readtable))

(in-package #:bike-examples/aspnet-mvc)

(in-readtable bike-syntax)

(defvar *host* nil)

(defvar *default-hello-server-port* 5000)

;; Some global state.
;; Class instances are required to be alive
;;  while their proxies are in use by the ASP .NET framework.
(defvar *server-port*)
(defvar *controller-model-convention*)
(defvar *action-model-convention*)
(defvar *feature-provider*)
(defvar *controller-activator*)
(defvar *controller*)
(defvar *logger-provider*)

;;; Import ASP.NET assemblies

(import-assembly 'Microsoft.AspNetCore)
(import-assembly 'Microsoft.AspNetCore.Server.Kestrel)
(import-assembly 'Microsoft.AspNetCore.Server.Kestrel.Core)
(import-assembly 'Microsoft.AspNetCore.Server.Kestrel.Transport.Sockets)
(import-assembly 'Microsoft.AspNetCore.Mvc)
(import-assembly 'Microsoft.AspNetCore.Mvc.Core)
(import-assembly 'Microsoft.Extensions.Configuration)
(import-assembly 'Microsoft.Extensions.DependencyInjection)
(import-loaded-assemblies)

;;; Use a bunch of namespaces
;;;   for the purpose of not writing qualified names of types.

(use-namespace '(System
                 System.Collections.Generic
                 System.Net.Http
                 System.Threading
                 System.Threading.Tasks
                 System.Web
                 Microsoft.AspNetCore.Builder
                 Microsoft.Extensions.DependencyInjection
                 Microsoft.AspNetCore.Routing
                 Microsoft.Extensions.Logging
                 Microsoft.AspNetCore.Connections
                 Microsoft.AspNetCore.Server.Kestrel.Core
                 Microsoft.AspNetCore.Server.Kestrel.Transport.Sockets
                 Microsoft.AspNetCore.Hosting.Server
                 Microsoft.AspNetCore.Mvc
                 Microsoft.AspNetCore.Mvc.ApplicationParts
                 Microsoft.AspNetCore.Mvc.ApplicationModels
                 Microsoft.AspNetCore.Mvc.Controllers))

;; Asp.Net MVC controller
(define-dotnet-callable-class (example-controller
                               (:base-type . ControllerBase))
    ()
  ;; Echo the 'Hello' message to client
  (:method index :string ((name :string))
    (format nil "Hello~:[~;, ~:*~a~]!" name)))

;; Custom controller feature provider is required
;;  because of .NET proxy class for controller is dynamically generated,
;;  i.e. MVC cannot scavenge it from some assembly
(define-dotnet-callable-class (example-feature-provider
                               (:interfaces (IApplicationFeatureProvider ControllerFeature)))
    ()
  (:method ((efp-populate-feature "PopulateFeature")) :void
    ((parts (IEnumerable ApplicationPart))
     (feature ControllerFeature))
    (declare (ignore parts))
    ;; Mark `example-controller' class as MVC controller
    (let ((class (find-class 'example-controller)))
      [[feature %Controllers] Add [class GetTypeInfo]])))

;; Custom controller model convention is required because
;;   .NET proxy class for controller is dynamically generated.
(define-dotnet-callable-class (example-controller-model-convention
                               (:interfaces IControllerModelConvention))
    ()
  (:method ((ecmc-apply "Apply")) :void ((ctrl ControllerModel))
    (when (bike-equals (find-class 'example-controller)
                       [ctrl %ControllerType])
      ;; Set controller name
      (setf [ctrl %ControllerName] "Home")
      ;; The below equals to settings [Route("/")] attribute on the controller class
      (let ((selector-model (new 'SelectorModel)))
        (setf [selector-model %AttributeRouteModel]
              (new 'AttributeRouteModel (new 'RouteAttribute "/")))
        [[ctrl %Selectors] Add selector-model]))))

;; Custom action model convention.
;; The below stuff could actually be done in the corresponding
;;   controller model convention method.
(define-dotnet-callable-class (example-action-model-convention
                               (:interfaces IActionModelConvention))
    ()
  (:method ((eamc-apply "Apply")) :void ((am ActionModel))
    (when (and (bike-equals (find-class 'example-controller)
                            [[am %Controller] %ControllerType])
               (equalp [am %ActionName] "Index"))
      ;; The below equals to setting [HttpGet("{name}")] attribute
      ;;   on the 'Index' method
      (let ((selector-model (new 'SelectorModel)))
        (setf [selector-model %AttributeRouteModel]
              (new 'AttributeRouteModel (new 'HttpGetAttribute "{name}")))
        [[am %Selectors] Add selector-model]))))

;; Custom controller activator is required because
;;   dotnet-callable-class proxies are sort of incompatible
;;   with dependency injection, because their constructors
;;   contain special arguments, required for interoperability with Lisp code.
(define-dotnet-callable-class (example-controller-activator
                               (:interfaces IControllerActivator))
    ()
  (:method ((eca-create "Create")) :object ((ctx ControllerContext))
    (declare (ignore ctx))
    *controller*)
  (:method ((eca-release "Release")) :void ((ctx ControllerContext) (ctrl :object))
    (declare (ignore ctx ctrl))))

;; Custom logger class. Logs to Lisp stderr stream.
(define-dotnet-callable-class (example-logger (:interfaces ILogger))
    ()
  (:method ((el-begin-scope "BeginScope") TState) IDisposable ((state TState))
    (declare (ignore state))
    nil)
  (:method ((el-enabled-p "IsEnabled")) :bool ((level LogLevel))
    (declare (ignore level))
    t)
  ;; N.B.: a generic method definition
  (:method ((el-log "Log") TState) :void ((level LogLevel)
                                          (eid EventId)
                                          (state TState)
                                          (ex Exception)
                                          (formatter (Func TState Exception :string)))
    (declare (ignore eid))
    (format *error-output* "~&[~a] ~a~%" [level ToString] [formatter Invoke state ex])))

;; Custom logger provider which creates loggers descibed above.
(define-dotnet-callable-class (example-logger-provider (:interfaces ILoggerProvider))
    ()
  ;; Keep loggers in a hash table, in a thread-safe way.
  (loggers :accessor elp-loggers :initform (make-hash-table :test #'equal))
  (lock :accessor elp-lock :initform (bt2:make-lock))
  (:defmethod create-logger ILogger ((category :string))
    (bt2:with-lock-held ((elp-lock this))
      (let ((loggers (elp-loggers this)))
        (or (gethash category loggers)
            (setf (gethash category loggers) (make-instance 'example-logger))))))
  (:defmethod dispose :void ()
    (bt2:with-lock-held ((elp-lock this))
      (clrhash (elp-loggers this)))))

(defun bike-examples:start-hello-host
    (&optional (server-port *default-hello-server-port*))
  (declare (type (integer 1 65535) server-port))
  "Starts Kestrel server listener on the specified SERVER-IP"
  ;; Ensure our host is not running first
  (when *host* (bike-examples:stop-hello-host))
  ;; set global state
  (setf *controller* (make-instance 'example-controller)
        *controller-activator* (make-instance 'example-controller-activator)
        *feature-provider* (make-instance 'example-feature-provider)
        *controller-model-convention* (make-instance 'example-controller-model-convention)
        *action-model-convention* (make-instance 'example-action-model-convention)
        *logger-provider* (make-instance 'example-logger-provider)
        *server-port* server-port)
  (let* ((app-opts (new 'WebApplicationOptions)) ;; empty options
         ;; Create a host builder.
         ;; Use CreateEmptyBuilder to configure framework from scratch
         (builder [:WebApplication CreateEmptyBuilder app-opts])
         ;; retrieve Dependency Injection container
         (services [builder %Services]))
    ;; Add logger provider to DI container
    [:ServiceCollectionServiceExtensions
     (AddSingleton ILoggerProvider) services *logger-provider*]
    ;; Add Kestrel to DI container
    [:ServiceCollectionServiceExtensions
     (AddSingleton IServer KestrelServer) services]
    ;; Add Kestrel transport to DI container
    [:ServiceCollectionServiceExtensions
     (AddSingleton IConnectionListenerFactory SocketTransportFactory) services]
    ;; Add custom controller activator to DI container
    [::ServiceCollectionServiceExtensions
     (AddSingleton IControllerActivator) services *controller-activator*]
    ;; Configure Kestrel
    [:OptionsServiceCollectionExtensions
     (Configure KestrelServerOptions)
     [builder %Services]
     (new '(Action KestrelServerOptions) 'configure-kestrel)]
    ;; Configure MVC
    (let* ((mvc [:MvcServiceCollectionExtensions
                 AddMvc services (new '(Action MvcOptions) 'configure-mvc)])
           (part-mgr [mvc %PartManager]))
      ;; Configure MVC features
      [[part-mgr %FeatureProviders] Add *feature-provider*])
    ;; Build the host and start it
    (let ((app [builder Build]))
      (setf *host* app)
      ;; Add controllers middleware
      [:ControllerEndpointRouteBuilderExtensions MapControllers app]
      ;; start the app
      [[app StartAsync [:CancellationToken %None]] Wait])))

(defun bike-examples:stop-hello-host ()
  "Stops Kestrel server listener"
  (when *host*
    [[*host* StopAsync [:CancellationToken %None]] Wait]
    [*host* Dispose]
    (setf *host* nil)))

(defun configure-mvc (opts)
  ;; Add custom controller convention
  [:ApplicationModelConventionExtensions
   Add [opts %Conventions] *controller-model-convention*]
  ;; Add custom action convention
  [:ApplicationModelConventionExtensions
   Add [opts %Conventions] *action-model-convention*])

(defun configure-kestrel (opts)
  "Configures Kestrel server"
  ;; Allow for listening on any IP on the specified port (5000 by default)
  [opts ListenAnyIp *server-port*]
  ;; Allow synchronous IO, for convenience
  (setf [opts %AllowSynchronousIO] t))

;;; The below function could be used for testing server response.
;;; You can however, use curl for that, or even open the page in browser

(defun bike-examples:get-hello-response (&optional who (port *default-hello-server-port*))
  (declare (type (integer 1 65535) port))
  "Retrieves a string message from running web host"
  (let* ((client (new 'HttpClient))
         (who (when who [:HttpUtility UrlEncode [(box who) ToString]]))
         (url  [(new 'UriBuilder "http" "localhost" port (or who "")) %Uri])
         (stdout *standard-output*))
    ;; Do not use "Result" property directly, it may cause a deadlock, especially
    ;;  if not all the required type members are cached yet
    [[client GetStringAsync url] ContinueWith
     (new '(Action Task)
          (lambda (task)
            [client Dispose]
            (format stdout "~a" [task %Result])))]))

;;; vim: ft=lisp et!
