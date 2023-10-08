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

(defun %process-method-trampoline-args (args-iter)
  (let (params-type need-cleanups funargs arg-decls boxeds withs
        initializers hostargs results cleanups)
    (loop
      (multiple-value-bind (name type primitive-type lisp-type dir params-p)
          (funcall args-iter)
        (unless name (return (values params-type need-cleanups
                                     (nreverse funargs) arg-decls boxeds withs
                                     (nreverse initializers) (nreverse hostargs)
                                     (nreverse results) cleanups)))
        (if params-p
          (setf params-type (%invoke type nil '() "GetElementType"))
          (with-gensyms (pname tcptr cleanup
                         need-cleanup boxed boxed-old)
            (setf name (make-symbol (string-upcase name)))
            (unless (eq dir :out)
              (push name funargs)
              (when lisp-type (push `(type ,lisp-type ,name) arg-decls)))
            (if primitive-type
              (ecase dir
                (:in
                 (push primitive-type hostargs)
                 (push name hostargs))
                (:ref
                 (push `(,pname ,primitive-type) withs)
                 (push `(setf (mem-ref ,pname ,primitive-type) ,name) initializers)
                 (push :pointer hostargs)
                 (push pname hostargs)
                 (push `(mem-ref ,pname ,primitive-type) results))
                (:out
                 (push `(,pname ,primitive-type) withs)
                 (push :pointer hostargs)
                 (push pname hostargs)
                 (push `(mem-ref ,pname ,primitive-type) results)))
              (ecase dir
                (:in
                 (push `(,boxed-old (null-pointer)) boxeds)
                 (push need-cleanup need-cleanups)
                 (push `(multiple-value-bind
                              (,boxed ,cleanup) (%box ,name)
                          (setf ,boxed-old ,boxed)
                          (when ,cleanup (setf ,need-cleanup t)))
                       initializers)
                 (push :pointer hostargs)
                 (push boxed-old hostargs)
                 (push `(when ,need-cleanup (%free-handle ,boxed-old)) cleanups))
                (:ref
                 (push `(,boxed-old (null-pointer)) boxeds)
                 (push need-cleanup need-cleanups)
                 (push `(,pname :pointer) withs)
                 (push `(,tcptr :int) withs)
                 (push `(multiple-value-bind
                              (,boxed ,cleanup) (%box ,name)
                          (setf ,boxed-old ,boxed)
                          (when ,cleanup (setf ,need-cleanup ,t))
                          (setf (mem-ref ,pname :pointer) ,boxed))
                       initializers)
                 (push :pointer hostargs)
                 (push pname hostargs)
                 (push :pointer hostargs)
                 (push tcptr hostargs)
                 (push `(let ((,boxed (mem-ref ,pname :pointer)))
                          (multiple-value-bind (,name ,cleanup)
                              (%unbox ,boxed (mem-ref ,tcptr :int))
                            (when ,cleanup (%free-handle ,boxed))
                            ,name))
                       results)
                 (push `(when ,need-cleanup (%free-handle ,boxed-old)) cleanups))
                (:out
                 (push `(,pname :pointer) withs)
                 (push `(,tcptr :int) withs)
                 (push :pointer hostargs)
                 (push pname hostargs)
                 (push :pointer hostargs)
                 (push tcptr hostargs)
                 (push `(let ((,boxed (mem-ref ,pname :pointer)))
                          (multiple-value-bind (,name ,cleanup)
                              (%unbox ,boxed (mem-ref ,tcptr :int))
                            (when ,cleanup (%free-handle ,boxed))
                            ,name))
                       results))))))))))

(defun %make-method-trampoline-lambda (fptr staticp voidp args-iter doc decls)
  (declare (type foreign-pointer fptr)
           (type boolean staticp voidp)
           (type function args-iter))
  (multiple-value-bind
        (params-type need-cleanups funargs arg-decls boxeds
         withs initializers hostargs results cleanups)
      (%process-method-trampoline-args args-iter)
    (with-gensyms (rest rv result rvtc ex cleanup)
      (let ((this (make-symbol (string '#:this))))
        `(lambda (,@(unless staticp `(,this)) ,@funargs ,@(when params-type `(&rest ,rest)))
           ,@(when doc `(,doc))
           (declare ,@arg-decls)
           ,@(unless staticp `((declare (type dotnet-object* ,this))))
           ,@decls
           (let (,@(unless staticp
                     `((,this (dotnet-object-handle ,this))))
                 ,@(when params-type
                     `((,rest (%list-to-bike-vector ,rest ,params-type))))
                 ,@boxeds
                 ,@need-cleanups)
             (with-foreign-objects (,@(unless voidp `((,rv :pointer)
                                                      (,rvtc :int)))
                                    ,@withs
                                    (,ex :pointer))
               ,@initializers
               (foreign-funcall-pointer
                ,fptr
                (:convention :stdcall)
                ,@(unless staticp `(:pointer ,this))
                ,@hostargs
                ,@(when params-type `(:pointer (%dotnet-object-handle ,rest)))
                ,@(unless voidp `(:pointer ,rv :pointer ,rvtc))
                :pointer ,ex
                :void)
               (multiple-value-prog1
                   (values ,@(unless voidp
                               `((let ((,rv (mem-ref ,rv :pointer))
                                       (,rvtc (mem-ref ,rvtc :int)))
                                   (multiple-value-bind
                                         (,result ,cleanup) (%unbox ,rv ,rvtc)
                                     (when ,cleanup (%free-handle ,rv))
                                     ,result))))
                           ,@results)
                 ,@cleanups
                 (%transform-exception (mem-ref ,ex :pointer))))))))))

(defun compile-method-trampoline (fptr staticp voidp args-iter &optional name doc decls)
  (declare (type foreign-pointer fptr)
           (type boolean staticp voidp)
           (type function args-iter)
           (type (or null symbol) name))
  "Compiles a delegate trampoline for a .Net method"
  (compile name (%make-method-trampoline-lambda fptr staticp voidp args-iter doc decls)))

(defun %make-reader-trampoline (fptr staticp primitive-type lisp-type doc decls)
  (let ((this (make-symbol (string '#:this))))
    (with-gensyms (ex rv result cleanup type-code)
      `(lambda (,@(unless staticp `(,this)))
         ,@(when doc `(,doc))
         ,@(unless staticp `((declare (type dotnet-object* ,this))))
         ,@decls
         (with-foreign-objects (,@(unless primitive-type
                                    `((,type-code :int)))
                                (,ex :pointer))
           (let* ((,rv (foreign-funcall-pointer
                        ,fptr
                        (:convention :stdcall)
                        ,@(unless staticp `(:pointer (dotnet-object-handle ,this)))
                        ,@(unless primitive-type `(:pointer ,type-code))
                        :pointer ,ex
                        ,(or primitive-type :pointer)))
                  (,ex (mem-ref ,ex :pointer)))
             ,@(if primitive-type
                 `((%transform-exception ,ex)
                   ,rv)
                 `((multiple-value-bind
                         (,result ,cleanup) (%unbox ,rv (mem-ref ,type-code :int))
                     (when ,cleanup (%free-handle ,rv))
                     (%transform-exception ,ex)
                     ,(if lisp-type
                        `(the ,lisp-type ,result)
                        result))))))))))

(defun compile-reader-trampoline (fptr staticp primitive-type lisp-type &optional name doc decls)
  (declare (type foreign-pointer fptr)
           (type boolean staticp)
           (type symbol primitive-type)
           (type (or null symbol) name))
  "Compiles a trampoline for a .Net reader method"
  (compile name (%make-reader-trampoline fptr staticp primitive-type lisp-type doc decls)))

(defun %make-writer-trampoline (fptr staticp primitive-type lisp-type doc decls)
  (let ((this (make-symbol (string '#:this))))
    (with-gensyms (value ex cleanup boxed)
      `(lambda (,@(unless staticp `(,this)) ,value)
         ,@(when doc `(,doc))
         ,@(when lisp-type `((declare (type ,lisp-type ,value))))
         ,@(unless staticp `((declare (type dotnet-object* ,this))))
         ,@decls
         (with-foreign-objects ((,ex :pointer))
           (,@(if primitive-type
                '(progn)
                `(multiple-value-bind (,boxed ,cleanup)
                     (%box ,value)))
            (foreign-funcall-pointer
             ,fptr
             (:convention :stdcall)
             ,@(unless staticp `(:pointer (dotnet-object-handle ,this)))
             ,@(if primitive-type
                 `(,primitive-type ,value)
                 `(:pointer ,boxed))
             :pointer ,ex
             :void)
            (let ((,ex (mem-ref ,ex :pointer)))
              ,@(unless primitive-type
                  `((when ,cleanup (%free-handle ,boxed))))
              (%transform-exception ,ex))))))))

(defun compile-writer-trampoline (fptr staticp primitive-type lisp-type &optional name doc decls)
  (declare (type foreign-pointer fptr)
           (type boolean staticp)
           (type symbol primitive-type)
           (type (or null symbol) name))
  "Compiles a trampoline for a .Net writer method"
  (compile name (%make-writer-trampoline fptr staticp primitive-type lisp-type doc decls)))

;;; vim: ft=lisp et
