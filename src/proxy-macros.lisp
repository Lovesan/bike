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

  (defun parse-callable-name-and-options (name-and-options &key (capitalize t))
    (let ((rest (ensure-list name-and-options))
          c
          (state :start)
          lisp-name dotnet-name)
      (flet ((next (new-state)
               (setf rest (cdr rest)
                     state new-state))
             (peek () (setf c (car rest))))
        (loop (case state
                (:start
                 (peek)
                 (cond
                   ((stringp c)
                    (setf dotnet-name c)
                    (next :cont))
                   ((and (symbolp c)
                         (not (null c))
                         (not (keywordp c)))
                    (setf lisp-name c)
                    (next :cont))
                   (t (error "No names found: ~s" name-and-options))))
                (:cont
                 (peek)
                 (cond
                   ((or (null c) (keywordp c)) (return))
                   ((stringp c)
                    (when dotnet-name
                      (error "Duplicate dotnet name: ~s" name-and-options))
                    (setf dotnet-name c)
                    (next :cont))
                   ((symbolp c)
                    (when lisp-name
                      (error "Duplicate lisp name: ~s" name-and-options))
                    (setf lisp-name c)
                    (next :cont))
                   (t (error "Ill-formed name-and-options: ~s" name-and-options))))))
        (unless lisp-name (setf lisp-name (intern (lisp-case-string dotnet-name))))
        (unless dotnet-name
          (setf dotnet-name (camel-case-string lisp-name :capitalize capitalize)))
        (list* lisp-name dotnet-name rest))))

  (defun check-callable-parameters (parameters)
    (loop :for p :in parameters
          :unless (or (consp p) (eql p '&rest))
            :do (error "Invalid parameter definition: ~s" p))
    parameters)

  (defun callable-parameters->lambda-list (parameters)
    (let ((lambda-list '())
          (aux '())
          (return-values '()))
      (dolist (p parameters (values (nreverse lambda-list)
                                    (nreverse aux)
                                    (nreverse return-values)))
        (if (consp p)
          (destructuring-bind (name type &key (direction :in)
                                         &allow-other-keys)
              p
            (declare (ignore type))
            (if (eq direction :out)
              (push name aux)
              (push name lambda-list))
            (unless (eq direction :in)
              (push name return-values)))
          ;; &rest
          (push p lambda-list)))))

  (defun parse-callable-method-definition (def class-name defmethodp)
    (destructuring-bind
        (name-options-and-generic-parameters return-type (&rest parameters) &body body)
        def
      (destructuring-bind (name-and-options &rest generic-parameters)
          (ensure-list name-options-and-generic-parameters)
        (destructuring-bind (name dotnet-name &rest slot-args
                                              &key (function-name name)
                                                   (defmethodp defmethodp)
                                              &allow-other-keys)
            (parse-callable-name-and-options name-and-options)
          (let* ((slot-args (copy-list slot-args))
                 (parameters (check-callable-parameters parameters))
                 (this-var (intern (string '#:this)))
                 (rv-var (gensym (string '#:rv))))
            (multiple-value-bind (lambda-list aux return-values)
                (callable-parameters->lambda-list parameters)
              (remf slot-args :function-name)
              (remf slot-args :defmethodp)
              (multiple-value-bind (body decls doc) (parse-body body :documentation t)
                (values `(,name :kind :method
                                :dotnet-name ,dotnet-name
                                :return-type ,return-type
                                :parameters ,parameters
                                :generic-parameters ,generic-parameters
                                :function-name ,function-name
                                :documentation ,doc
                                ,@slot-args)
                        (if defmethodp
                          `(progn
                             (eval-when (:compile-toplevel :load-toplevel :execute)
                               (ensure-generic-function
                                ',function-name
                                :lambda-list '(,this-var ,@lambda-list)))
                             (defmethod ,function-name ((,this-var ,class-name)
                                                        ,@lambda-list
                                                        ,@(when aux `(&aux ,@aux)))
                               ,@(when doc `(,doc))
                               (declare (ignorable ,this-var))
                               ,@decls
                               (let ((,rv-var (progn ,@body)))
                                 (values ,rv-var ,@return-values))))
                          `(defun ,function-name (,this-var ,@lambda-list
                                                  ,@(when aux `(&aux ,@aux)))
                             ,@(when doc `(,doc))
                             (declare (type ,class-name ,this-var)
                                      (ignorable ,this-var))
                             ,@decls
                             (let ((,rv-var (progn ,@body)))
                               (values ,rv-var ,@return-values))))))))))))

  (defun parse-callable-event-definition (def)
    (destructuring-bind (name-and-options type &rest slot-args &key &allow-other-keys)
        def
      (destructuring-bind (name dotnet-name &key (raise-method-dotnet-name
                                                  nil
                                                  raise-method-dotnet-name-p))
          (parse-callable-name-and-options name-and-options)
        `(,name :kind :event
                :dotnet-name ,dotnet-name
                :handler-type ,type
                ,@(when raise-method-dotnet-name-p
                    `(:raise-method-dotnet-name ,raise-method-dotnet-name))
                ,@slot-args))))

  (defun property-accessor-form-p (form)
    (and (consp form)
         (member (first form) '(:get :getter :set :setter))))

  (defun parse-callable-property-accessors
      (whole property-kind class-name defmethodp
       lambda-list aux return-values
       accessors)
    (when (endp accessors)
      (error "~:(~a~) must have at least one accessor. Form was: ~s" property-kind whole))
    (when (> (length accessors) 2)
      (error "Invalid ~(~a~) form: ~s" property-kind whole))
    (let (getter setter)
      (dolist (form accessors)
        (unless (consp form)
          (error "Invalid accessor form: ~s" form))
        (ecase (car form)
          ((:get :getter)
           (when getter
             (error "Duplicate getter form: ~s" whole))
           (setf getter form))
          ((:set :setter)
           (when setter
             (error "Duplicate setter form: ~s" whole))
           (setf setter form))))
      (labels ((parse (form)
                 (destructuring-bind (accessor-type function-name &body body)
                     form
                   (check-type function-name (or symbol (cons (eql setf)
                                                              (cons symbol null))))
                   (values
                    function-name
                    (when body
                      (multiple-value-bind (body decls doc)
                          (parse-body body :documentation t)
                        (let ((this-var (intern (string '#:this)))
                              (new-value-form (when (member accessor-type '(:set :setter))
                                                `(,(intern (string '#:value)))))
                              (rv-var (gensym (string '#:rv))))
                          (if defmethodp
                            `(progn
                               (eval-when (:compile-toplevel :load-toplevel :execute)
                                 (ensure-generic-function
                                  ',function-name
                                  :lambda-list '(,@new-value-form ,this-var ,@lambda-list)))
                               (defmethod ,function-name (,@new-value-form
                                                          (,this-var ,class-name)
                                                          ,@lambda-list
                                                          ,@(when aux `(&aux ,@aux)))
                                 ,@(when doc `(,doc))
                                 (declare (ignorable ,this-var))
                                 ,@decls
                                 (let ((,rv-var (progn ,@body)))
                                   (values ,rv-var ,@return-values))))
                            `(defun ,function-name (,@new-value-form
                                                    ,this-var
                                                    ,@lambda-list
                                                    ,@(when aux `(&aux ,@aux)))
                               ,@(when doc `(,doc))
                               (declare (type ,class-name ,this-var)
                                        (ignorable ,this-var))
                               ,@decls
                               (let ((,rv-var (progn ,@body)))
                                 (values ,rv-var ,@return-values)))))))))))
        (multiple-value-bind (getter-name getter-form)
            (when getter (parse getter))
          (multiple-value-bind (setter-name setter-form)
              (when setter (parse setter))
            (values getter-name
                    getter-form
                    setter-name
                    setter-form))))))

  (defun parse-callable-indexer-definition (whole class-name)
    (let ((def (rest whole)))
      (multiple-value-bind (name dotnet-name slot-args defmethodp type parameters accessors)
          (cond ((property-accessor-form-p (third def))
                 (destructuring-bind (type parameters &body accessors)
                     def
                   (let ((name (intern (string '#:item)))
                         (dotnet-name "Item"))
                     (values name dotnet-name '() nil type parameters accessors))))
                ((property-accessor-form-p (fourth def))
                 (destructuring-bind (name-and-options type parameters &body accessors)
                     def
                   (destructuring-bind (name dotnet-name &rest slot-args
                                                         &key defmethodp
                                                         &allow-other-keys)
                       (parse-callable-name-and-options name-and-options)
                     (remf slot-args :defmethodp)
                     (values name dotnet-name slot-args defmethodp type parameters accessors))))
                (t (error "Malformed indexer definition: ~s" whole)))
        (let* ((parameters (check-callable-parameters parameters)))
          (multiple-value-bind (lambda-list aux return-values)
              (callable-parameters->lambda-list parameters)
            (multiple-value-bind (getter-name getter-form setter-name setter-form)
                (parse-callable-property-accessors
                 whole :indexer class-name defmethodp
                 lambda-list aux return-values accessors)
              (values `(,name :kind :indexer
                              :dotnet-name ,dotnet-name
                              :property-type ,type
                              :getter ,getter-name
                              :setter ,setter-name
                              :parameters ,parameters
                              ,@slot-args)
                      `(progn ,@(when getter-form `(,getter-form))
                              ,@(when setter-form `(,setter-form))))))))))

  (defun parse-callable-property-long-form (whole class-name)
    (let ((def (rest whole)))
      (destructuring-bind (name-and-options type &body accessors)
          def
        (destructuring-bind (name dotnet-name &rest slot-args
                                              &key defmethodp
                                              &allow-other-keys)
            (parse-callable-name-and-options name-and-options)
          (remf slot-args :defmethodp)
          (multiple-value-bind (getter-name getter-form setter-name setter-form)
              (parse-callable-property-accessors whole :property class-name defmethodp
                                                 '() '() '() accessors)
            (values `(,name :kind :property
                            :dotnet-name ,dotnet-name
                            :property-type ,type
                            :getter ,getter-name
                            :setter ,setter-name
                            ,@slot-args)
                    `(progn ,@(when getter-form `(,getter-form))
                            ,@(when setter-form `(,setter-form)))))))))

  (defun parse-callable-property-definition (whole class-name)
    (let ((def (rest whole)))
      (if (property-accessor-form-p (third def))
        (parse-callable-property-long-form whole class-name)
        (destructuring-bind (name-and-options type &rest slot-args &key &allow-other-keys)
            def
          (destructuring-bind (name dotnet-name &key (getter t)
                                                     (setter t))
              (parse-callable-name-and-options name-and-options)
            `(,name :kind :property
                    :dotnet-name ,dotnet-name
                    :property-type ,type
                    :getter ,getter
                    :setter ,setter
                    ,@slot-args))))))

  (defun make-callable-slot-parser (slots class-name)
    (lambda ()
      (when slots
        (multiple-value-prog1
            (let ((slot (first slots)))
              (if (consp slot)
                (case (first slot)
                  (:property (parse-callable-property-definition slot class-name))
                  (:event (parse-callable-event-definition (rest slot)))
                  (:method (parse-callable-method-definition (rest slot) class-name nil))
                  (:defmethod (parse-callable-method-definition (rest slot) class-name t))
                  (:indexer (parse-callable-indexer-definition slot class-name))
                  (t slot))
                slot))
          (setf slots (cdr slots)))))))

(defmacro define-dotnet-callable-class
    (&whole whole name-and-options (&rest superclasses)
     &body slots)
  "Defines a class which instances could be passed to .NET code."
  (multiple-value-bind (slots decls doc)
      (parse-body slots :documentation t :whole whole)
    (when (and (null doc) (stringp slots))
      (psetf slots doc doc slots))
    (when decls
      (error "Declarations are not allowed here: ~s" whole))
    (destructuring-bind (name &rest class-options)
        (ensure-list name-and-options)
      (check-type name (and symbol (not null)))
      (let (slots
            other-defs
            (parser (make-callable-slot-parser slots name))
            (metaclass-opt (assoc :metaclass class-options)))
        (setf class-options (remove :metaclass class-options :key #'car))
        (unless metaclass-opt (setf metaclass-opt '(:metaclass dotnet-callable-class)))
        (loop (multiple-value-bind (slotd other)
                  (funcall parser)
                (unless slotd (return))
                (push slotd slots)
                (push other other-defs)))
        `(progn
           (defclass ,name ,superclasses
             ,(nreverse slots)
             ,metaclass-opt
             ,@class-options
             ,@(when doc `((:documentation ,doc))))
           ,@(nreverse other-defs)
           (ensure-finalized (find-class ',name))
           ',name)))))

;;; vim: ft=lisp et
