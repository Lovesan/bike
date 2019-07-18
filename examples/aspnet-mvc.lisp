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
                #:define-constant))

(in-package #:bike-examples/aspnet-mvc)

(defvar *host* nil)

(defvar *default-hello-server-port* 5000)

(defvar *server-port*)

;;; This example consists of two parts.
;;;
;;; The first one is responsible for loading a set of AspNet Core
;;;  assemblies, which are of interest to us, into a directory
;;;  named 'AspNetMvcAssemblies' by means of creating a temporary
;;;  .csproj file which lists the dependencies we need, and invoking
;;;  dotnet command afterwards.
;;; We then import all this assemblies.
;;;
;;; The second part is actually responsible for setting up an
;;;  AspNet Mvc framework on top of Kestrel server, and running it.

(define-constant +tmp-project-name+ "AspNetMvcAssemblies"
  :test #'equal)

(defvar *deps-directory*
  (native-namestring
   (merge-pathnames*
    (make-pathname* :directory (list :relative +tmp-project-name+))
    (pathname-directory-pathname #.(current-lisp-file-pathname)))))

(use-namespace 'System)
(use-namespace 'System.Diagnostics)
(use-namespace 'System.Text)
(use-namespace 'System.IO)

(defun write-project-file (&optional (version "2.2.0"))
  (declare (type string version))
  "Creates a temporary project file which is used for collecting
 AspNet assemblies required by us."
  ;; Manually create a .csproj file xml body
  (let ((xml (strcat "<Project Sdk=\"Microsoft.NET.Sdk\">"
                     "<PropertyGroup>"
                     "<TargetFramework>netstandard2.0</TargetFramework>"
                     "<OutputType>Library</OutputType>"
                     "</PropertyGroup>"
                     "<ItemGroup>"
                     (strcat
                      "<PackageReference "
                      "Include=\"Microsoft.AspNetCore.Mvc\" version=\""
                      version
                      "\"/>")
                     (strcat
                      "<PackageReference "
                      "Include=\"Microsoft.AspNetCore.Server.Kestrel\" version=\""
                      version
                      "\"/>")
                     "</ItemGroup>"
                     "</Project>"))
        (encoding (new 'UTF8Encoding :false :false))
        (file (native-namestring
               (make-pathname :defaults *deps-directory*
                              :name +tmp-project-name+
                              :type "csproj"))))
    (ensure-directories-exist *deps-directory*)
    (invoke 'File 'WriteAllText file xml encoding)
    file))

(defun make-output-handler (stream)
  (declare (type stream stream))
  (new 'DataReceivedEventHandler
       (lambda (sender e)
         (declare (ignore sender))
         (let ((str (property e 'Data)))
           (when str (write-line str stream))))))

(defun publish-project (project-file)
  (declare (type (or pathname string) project-file))
  "Builds and publishes a temporary project,
 which gives us all the required AspNet assemblies"
  (let* ((process (new 'Process))
         (start-info (property process 'StartInfo))
         (args (property start-info 'ArgumentList)))
    (dolist (arg (list "publish"
                       "-c" "Release"
                       "-o" *deps-directory*
                       project-file))
      (invoke args 'Add arg))
    (setf (property start-info 'FileName) "dotnet"
          (property start-info 'UseShellExecute) nil
          (property start-info 'WorkingDirectory) *deps-directory*
          (property start-info 'RedirectStandardOutput) T
          (property start-info 'RedirectStandardError) T
          (property process 'EnableRaisingEvents) T)
    (invoke process "add_Exited"
            (new 'EventHandler
                 (lambda (sender e)
                   (declare (ignore sender e))
                   (cleanup-temporary-project))))
    (invoke process "add_OutputDataReceived"
            (make-output-handler *standard-output*))
    (invoke process "add_ErrorDataReceived"
            (make-output-handler *error-output*))
    (invoke process 'Start)
    (invoke process 'BeginOutputReadLine)
    (invoke process 'BeginErrorReadLine)
    (invoke process 'WaitForExit)
    (let ((rv (property process 'ExitCode)))
      (unless (zerop rv)
        (error "Unable to publish project file: ~a" rv))
      project-file)))

(defun cleanup-temporary-project ()
  "Cleans up a temporary project build"
  ;; delete temporary project files and directories
  (let ((bin (native-namestring
              (merge-pathnames*
               (make-pathname :directory '(:relative "bin"))
               *deps-directory*)))
        (obj (native-namestring
              (merge-pathnames*
               (make-pathname :directory '(:relative "obj"))
               *deps-directory*))))
    (format *error-output* "Deleting ~s~%" bin)
    (invoke 'Directory 'Delete bin t)
    (format *error-output* "Deleting ~s~%" obj)
    (invoke 'Directory 'Delete obj t)
    (do-bike-vector (file (invoke 'Directory 'GetFiles
                                  *deps-directory*
                                  (strcat +tmp-project-name+ "*.*")))
      (format *error-output* "Deleting ~s~%" file)
      (delete-file file))))

(defun import-microsoft-assemblies ()
  "Imports all the assemblies prefixed with 'Microsoft.' from the temp project dir."
  (loop :for path :in (directory* (make-pathname*
                                   :type "dll"
                                   :name *wild*
                                   :defaults *deps-directory*))
        :for name = (pathname-name path)
        :when (string-prefix-p "Microsoft." name)
          :do (handler-case
                  (import-assembly (load-assembly-from (native-namestring path)))
                (error (e) (format *error-output* "~a~%" e)))))

(defun ensure-asp-net-assemblies ()
  "Loads and imports all required AspNet Mvc assemblies."
  (unless (invoke 'Directory 'Exists *deps-directory*)
    (let ((file (write-project-file)))
      (publish-project file)))
  (import-microsoft-assemblies))

(ensure-asp-net-assemblies)

;;; Now, to the actual part of this example.
;;; First, use a bunch of namespaces
;;;   for the purpose of not writing qualified names of types.

(use-namespace 'System.Net.Http)
(use-namespace 'System.Threading)
(use-namespace 'System.Threading.Tasks)
(use-namespace 'System.Web)
(use-namespace 'Microsoft.AspNetCore.Hosting)
(use-namespace 'Microsoft.AspNetCore.Http)
(use-namespace 'Microsoft.AspNetCore.Routing)
(use-namespace 'Microsoft.Extensions.Configuration)
(use-namespace 'Microsoft.Extensions.DependencyInjection)
(use-namespace 'Microsoft.AspNetCore.Builder)
(use-namespace 'Microsoft.AspNetCore.Server.Kestrel)
(use-namespace 'Microsoft.AspNetCore.Server.Kestrel.Core)

(defun bike-examples:start-hello-host
    (&optional (server-port *default-hello-server-port*))
  (declare (type (integer 1 65535) server-port))
  "Starts Kestrel server listener on the specified SERVER-IP"
  ;;; Ensure our host is not running first
  (when *host* (bike-examples:stop-hello-host))
  (let ((builder (new 'WebHostBuilder)) ;; create a host builder
        (*server-port* server-port))
    ;; Now we need to do two things, first - configure Kestrel as
    ;;   our server.
    ;; Second - configure services available through AspNet dependency injections
    (setf builder (invoke 'WebHostBuilderKestrelExtensions 'UseKestrel builder
                          (new '(Action KestrelServerOptions)
                               'configure-kestrel))
          builder (invoke builder 'ConfigureServices
                          (new '(Action WebHostBuilderContext IServiceCollection)
                               'configure-services)))
    ;; Build the host and start it
    (let ((host (invoke builder 'Build)))
      (invoke host 'Start)
      (setf *host* host))))

(defun bike-examples:stop-hello-host ()
  "Stops Kestrel server listener"
  (when *host*
    (invoke (invoke *host* 'StopAsync (property 'CancellationToken 'None))
            'Wait)
    (setf *host* nil)))

(defun configure-kestrel (opts)
  "Configures Kestrel server"
  ;; Allow for listening on any IP on the specified port (5000 by default)
  (invoke opts 'ListenAnyIp *server-port*))

(defun configure-services (ctx services)
  (declare (ignore ctx))
  "Configures AspNet Core services"
  ;; First configure MVC
  (invoke 'MvcServiceCollectionExtensions 'AddMvc services)
  ;; Now the next thing is required because we do not use custom Startup
  ;;  class as usual C# applications do.
  ;; Instead, we use DelegateStartup instance and do configuration using callbacks.
  (invoke 'ServiceCollectionServiceExtensions '(AddSingleton IStartup) services
          (new '(Func IServiceProvider IStartup) 'get-delegate-startup)))

(defun get-delegate-startup (provider)
  (declare (type dotnet-object provider))
  "Retrieves DelegateStartup instance for delegate-based configuration"
  ;; First, retrieve IServiceProviderFactory<IServiceCollections> instance,
  ;;  which is the required first argument of DelegateStartup constructor
  (let* ((type (resolve-type '(IServiceProviderFactory IServiceCollection)))
         (factory (invoke provider 'GetService type)))
    ;; Create a DelegateStartup and pass our configuration callback to it
    (new 'DelegateStartup factory
         (new '(Action IApplicationBuilder) 'configure-pipeline))))

(defun configure-pipeline (app)
  (declare (type dotnet-object app))
  "A callback which configures AspNet pipeline.
 APP argument represents IApplicationBuilder instance."
  ;; We don't need a lot of configuration here, simply
  ;;  configure MVC, and make use of our route configuration callback
  (invoke 'MvcApplicationBuilderExtensions 'UseMvc app
          (new '(Action IRouteBuilder) 'configure-routes)))

(defun configure-routes (builder)
  (declare (type dotnet-object builder))
  "Configures AspNet MVC routes. BUILDER represents IRouteBuilder"
  ;; Map a single route for GET /{name}, and supply our custom route handler
  (invoke 'RequestDelegateRouteBuilderExtensions 'MapGet builder
          "/{name=None}"
          (new '(Func HttpRequest HttpResponse RouteData Task)
               'process-request)))

(defun process-request (request response route-data)
  (declare (type dotnet-object request response route-data)
           (ignore request))
  "Processes a single HTTP request.
 REQUEST parameter represents an instance of HttpRequest.
 RESPONSE parameter is an instance of HttpResponse, which we would modify.
 ROUTE-DATA represent a collection of route data parameters(a RouteData instance)."
  (handler-case
      ;; First, retrieve the 'name' route parameter, which we have configured
      ;;  in our CONFIGURE-ROUTES handler
      ;; In case of it represents a string which equals to 'None',
      ;;  we instead utilize current user name.
      (let* ((route-arg (ref (property route-data 'Values) "name"))
             (who (if (string-equal route-arg "None")
                    (property 'Environment 'UserName)
                    route-arg)))
        ;; Set response content type
        (setf (property response 'ContentType) "text/plain; encoding=utf-8")
        ;; Write a string to response stream. Note that the extension method
        ;;   which we are making use of, returns a Task instance
        (invoke 'HttpResponseWritingExtensions 'WriteAsync response
                (format nil "Hello from AspNet.Mvc, ~a!~%Now is ~a~%"
                        who
                        (invoke (property 'DateTime 'Now) 'ToString))
                (property 'CancellationToken 'None)))
    (error (e)
      ;; Handle error in case of one occurs and return completed task
      (format *error-output* "~a~%" e)
      (property 'Task 'CompletedTask))))

;;; The below function could be used for testing server response.
;;; You can however, use curl for that, or even open the page in browser

(defun bike-examples:get-hello-response (&optional who (port *default-hello-server-port*))
  (declare (type (integer 1 65535) port))
  "Retrieves a string message from running web host"
  (let* ((client (new 'HttpClient))
         (who (when who (invoke 'HttpUtility 'UrlEncode (invoke (box who) 'ToString))))
         (url (property (new 'UriBuilder "http" "localhost" port (or who ""))
                        'Uri))
         (stdout *standard-output*))
    ;; Do not use "Result" property directly, it may cause a deadlock, especially
    ;;  if not all the required type members are cached yet
    (invoke (invoke client 'GetStringAsync url) 'ContinueWith
            (new '(Action Task)
                 (lambda (task)
                   (invoke client 'Dispose)
                   (format stdout "~a" (property task 'Result)))))))

;;; vim: ft=lisp et!
