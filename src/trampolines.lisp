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

(defun %process-method-trampoline-args (args)
  (declare (type list args))
  (loop :with params-arg = (find-if #'%param-entry-params-p args)
        :with args = (remove t args :key #'%param-entry-params-p)
        :for arg :in args
        :for name = (make-symbol (%mknetsym (%param-entry-name arg)))
        :for pname = (gensym (string '#:ptr))
        :for tcptr = (gensym (string '#:typecode))
        :for cleanup = (gensym (string '#:cleanup))
        :for need-cleanup = (gensym (string ':need-cleanup))
        :for boxed = (gensym (string '#:boxed))
        :for boxed-old = (gensym (string '#:boxed-old))
        :for primitive = (%param-entry-primitive-type arg)
        :for dir = (%param-entry-direction arg)
        :if (not (eq dir :out))
          :collect name :into funargs
        :if (and primitive (eq dir :in))
          :append (list primitive name) :into hostargs
        :if (and primitive (not (eq dir :in)))
          :append (list :pointer pname) :into hostargs
          :and :collect (list pname primitive) :into withs
          :and :collect `(mem-ref ,pname ,primitive) :into results
        :if (and primitive (eq dir :ref))
          :collect `(setf (mem-ref ,pname ,primitive) ,name) :into initializers
        :if (and (not primitive) (member dir '(:in :ref)))
          :collect (list boxed-old '(null-pointer)) :into boxeds
          :and :collect (list need-cleanup nil) :into need-cleanups
          :and :collect `(when ,need-cleanup (%free-handle ,boxed-old)) :into cleanups
        :if (and (not primitive) (eq dir :in))
          :append (list :pointer boxed-old) :into hostargs
          :and :collect `(multiple-value-bind
                               (,boxed ,cleanup) (%box ,name)
                           (setf ,boxed-old ,boxed)
                           (when ,cleanup (setf ,need-cleanup ,t))) :into initializers
        :if (and (not primitive) (eq dir :ref))
          :collect `(multiple-value-bind
                          (,boxed ,cleanup) (%box ,name)
                      (setf ,boxed-old ,boxed)
                      (when ,cleanup (setf ,need-cleanup ,t))
                      (setf (mem-ref ,pname :pointer) ,boxed)) :into initializers
        :if (and (not primitive) (member dir '(:ref :out)))
          :collect (list pname :pointer) :into withs
          :and :append (list :pointer pname) :into hostargs
          :and :collect (list tcptr :int) :into withs
          :and :append (list :pointer tcptr) :into hostargs
          :and :collect `(let ((,boxed (mem-ref ,pname :pointer)))
                           (multiple-value-bind (,name ,cleanup)
                               (%unbox ,boxed ,tcptr)
                             (when ,cleanup (%free-handle ,boxed))
                             ,name)) :into results
        :finally (return (values params-arg need-cleanups
                                 funargs boxeds withs
                                 initializers hostargs results cleanups))))

(defun %make-method-trampoline-lambda (fptr staticp voidp args)
  (declare (type foreign-pointer fptr)
           (type boolean staticp voidp)
           (type list args))
  (multiple-value-bind
        (params-arg need-cleanups funargs boxeds
         withs initializers hostargs results cleanups)
      (%process-method-trampoline-args args)
    (with-gensyms (rest rv result rvtc ex cleanup)
      (let ((this (make-symbol (string '#:this)))
            (params-type (and params-arg
                              (reflection-invoke (%param-entry-type params-arg)
                                                 "GetElementType"))))
        `(lambda (,@(unless staticp `(,this)) ,@funargs ,@(when params-arg `(&rest ,rest)))
           (declare (optimize (speed 0) (space 0) (safety 0) (debug 0)))
           (let (,@(unless staticp `((,this (%dotnet-object-handle ,this))))
                 ,@(when params-arg `((,rest (list-to-bike-vector ,rest :element-type ,params-type))))
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
                ,@(when params-arg `(:pointer (%dotnet-object-handle ,rest)))
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

(defun compile-method-trampoline (fptr staticp voidp args &optional name)
  (declare (type foreign-pointer fptr)
           (type boolean staticp voidp)
           (type list args)
           (type (or null symbol) name))
  "Compiles a delegate trampoline for a .Net method"
  (compile name (%make-method-trampoline-lambda fptr staticp voidp args)))

(defun %make-reader-trampoline (fptr staticp primitive-type)
  (let ((this (make-symbol (string '#:this))))
    (with-gensyms (ex rv result cleanup type-code)
      `(lambda (,@(unless staticp `(,this)))
         (with-foreign-objects (,@(unless primitive-type
                                    `((,type-code :int)))
                                (,ex :pointer))
           (let* ((,rv (foreign-funcall-pointer
                        ,fptr
                        (:convention :stdcall)
                        ,@(unless staticp `(:pointer (%dotnet-object-handle ,this)))
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
                     ,result)))))))))

(defun compile-reader-trampoline (fptr staticp primitive-type &optional name)
  (declare (type foreign-pointer fptr)
           (type boolean staticp)
           (type symbol primitive-type)
           (type (or null symbol) name))
  "Compiles a trampoline for a .Net reader method"
  (compile name (%make-reader-trampoline fptr staticp primitive-type)))

(defun %make-writer-trampoline (fptr staticp primitive-type)
  (let ((this (make-symbol (string '#:this))))
    (with-gensyms (value ex cleanup boxed)
      `(lambda (,@(unless staticp `(,this)) ,value)
         (with-foreign-objects ((,ex :pointer))
           (,@(if primitive-type
                '(progn)
                `(multiple-value-bind (,boxed ,cleanup)
                     (%box ,value)))
            (foreign-funcall-pointer
             ,fptr
             (:convention :stdcall)
             ,@(unless staticp `(:pointer (%dotnet-object-handle ,this)))
             ,@(if primitive-type
                 `(,primitive-type ,value)
                 `(:pointer ,boxed))
             :pointer ,ex
             :void)
            (let ((,ex (mem-ref ,ex :pointer)))
              ,@(unless primitive-type
                  `((when ,cleanup (%free-handle ,boxed))))
              (%transform-exception ,ex))))))))

(defun compile-writer-trampoline (fptr staticp primitive-type &optional name)
  (declare (type foreign-pointer fptr)
           (type boolean staticp)
           (type symbol primitive-type)
           (type (or null symbol) name))
  "Compiles a trampoline for a .Net writer method"
  (compile name (%make-writer-trampoline fptr staticp primitive-type)))

;;; vim: ft=lisp et
