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

;;; Generic type definitions and simple types are
;;;  at the top-level (i.e.) could be acquired by its name
;;;  from type table `data' slot

(declaim (inline %type-entry))
(defun %type-entry (name)
  (declare (type dotnet-name name))
  (with-type-table (data)
    (values (the (or null type-entry) (gethash name data)))))

(declaim (inline (setf %type-entry)))
(defun (setf %type-entry) (new-entry name)
  (declare (type type-entry new-entry)
           (type dotnet-name name))
  (with-type-table (data)
    (setf (gethash name data) new-entry))
  new-entry)

(declaim (inline %get-aliased-type))
(defun %get-aliased-type (name)
  (declare (type dotnet-name name))
  (with-type-table (aliases)
    (values (gethash name aliases))))

(defun %maybe-get-qualified-name (type)
  (declare (type dotnet-type type))
  "Retrieves qualified name of a type unless it is dynamic"
  (unless (transient-type-p type)
    (type-assembly-qualified-name type)))

(defun %ensure-generic-type-definition-entry (type)
  (declare (type dotnet-type type))
  "Ensures generic type definition entry"
  (let ((name (%mknetsym (type-full-name type))))
    (or (%type-entry name)
        (with-type-table-lock (:write)
          (or (%type-entry name)
              (let ((args (type-generic-arguments type)))
                (setf (%type-entry name)
                      (%make-type-entry
                       type
                       :qualified-name (%maybe-get-qualified-name type)
                       :type-args args))))))))

(defun %ensure-generic-type-entry (type)
  (declare (type dotnet-type type))
  "Ensures instantiated generic type entry"
  (let* ((definition (%ensure-generic-type-definition-entry
                      (generic-type-definition-of type))))
    (with-type-entry (instances) definition
      (flet ((lookup (&aux (arg-type type))
               (dolist (instance instances)
                 (with-type-entry (type) instance
                   (when (bike-equals arg-type type)
                     (return instance))))))
        (or (lookup)
            (with-type-table-lock (:write)
              (or (lookup)
                  (let ((args (type-generic-arguments type)))
                    (car (push (%make-type-entry
                                type
                                :qualified-name (%maybe-get-qualified-name type)
                                :type-args args)
                               instances))))))))))

(defun %ensure-generic-type-entry-with-args (definition args)
  (declare (type (or dotnet-type type-entry) definition)
           (type (cons type-entry list) args))
  "Ensures instantiated generic type entry"
  (let ((definition (%ensure-type-entry definition)))
    (with-type-entry (instances (definition-type type)) definition
      (flet ((lookup ()
               (dolist (instance instances)
                 (with-type-entry ((instance-args type-args)) instance
                   (when (loop :for def :in args
                               :for def-arg = (%type-entry-type def)
                               :for instance-arg :in instance-args
                               :unless (bike-equals instance-arg def-arg)
                                 :do (return nil)
                               :finally (return t))
                     (return instance))))))
        (or (lookup)
            (with-type-table-lock (:write)
              (or (lookup)
                  (let* ((args (mapcar #'%type-entry-type args))
                         (type (apply #'make-generic-type definition-type args)))
                    (car (push (%make-type-entry
                                type
                                :qualified-name (%maybe-get-qualified-name type)
                                :type-args args)
                               instances))))))))))

(defun %ensure-pointer-type-entry-by-element-type (type)
  (declare (type (or type-entry dotnet-type) type))
  "Ensures pointer type entry for element type"
  (let ((entry (%ensure-type-entry type)))
    (with-type-entry (pointer-entry type) entry
      (or pointer-entry
          (with-type-table-lock (:write)
            (or pointer-entry
                (let ((ptr-type (make-pointer-type type)))
                  (setf pointer-entry
                        (%make-type-entry ptr-type
                                          :element-entry entry)))))))))

(defun %ensure-pointer-type-entry (type)
  (declare (type dotnet-type type))
  "Ensures pointer type entry"
  (let ((base-type (element-type-of type)))
    (%ensure-pointer-type-entry-by-element-type base-type)))

(defun %ensure-ref-type-entry-by-element-type (type)
  (declare (type (or type-entry dotnet-type) type))
  "Ensures ref type entry for element type"
  (let ((entry (%ensure-type-entry type)))
    (with-type-entry (ref-entry type) entry
      (or ref-entry
          (with-type-table-lock (:write)
            (or ref-entry
                (let ((ref-type (make-ref-type type)))
                  (setf ref-entry
                        (%make-type-entry ref-type
                                          :element-entry entry)))))))))

(defun %ensure-ref-type-entry (type)
  (declare (type dotnet-type type))
  "Ensures ref type entry"
  (let ((base-type (element-type-of type)))
    (%ensure-ref-type-entry-by-element-type base-type)))

(defun %ensure-array-type-entry-by-element-type (type rank)
  (declare (type (or dotnet-type type-entry) type)
           (type (integer 1 32) rank))
  "Ensures array type entry for element type"
  (let ((entry (%ensure-type-entry type)))
    (with-type-entry (array-entries type) entry
      (flet ((lookup ()
               (and array-entries (svref array-entries (1- rank)))))
        (or (lookup)
            (with-type-table-lock (:write)
              (or (lookup)
                  (let ((entries array-entries)
                        (array-type (if (= rank 1)
                                      (make-array-type type)
                                      (make-array-type* type rank))))
                    (unless entries
                      (setf entries (make-array +max-array-rank+ :initial-element nil)
                            array-entries entries))
                    (setf (svref entries (1- rank))
                          (%make-type-entry array-type
                                            :element-entry entry
                                            :rank rank))))))))))

(defun %ensure-array-type-entry (type)
  (declare (type dotnet-type type))
  "Ensures array type entry"
  (let ((base-type (element-type-of type))
        (rank (array-type-rank type)))
    (%ensure-array-type-entry-by-element-type base-type rank)))

(defun %ensure-simple-type-entry (type)
  (declare (type dotnet-type type))
  "Ensures simple, non-generic type entry"
  (let ((name (%mknetsym (%get-type-full-name type))))
    (or (%type-entry name)
        (with-type-table-lock (:write)
          (or (%type-entry name)
              (setf (%type-entry name)
                    (%make-type-entry
                     type
                     :qualified-name (%maybe-get-qualified-name type))))))))

(defun %ensure-type-entry (type)
  (declare (type (or type-entry dotnet-type) type))
  "Ensures an entry in global type table for a TYPE"
  (cond ((type-entry-p type) type)
        ((array-type-p type)
         (%ensure-array-type-entry type))
        ((pointer-type-p type)
         (%ensure-pointer-type-entry type))
        ((ref-type-p type)
         (%ensure-ref-type-entry type))
        ((generic-type-definition-p type)
         (%ensure-generic-type-definition-entry type))
        ((generic-type-p type)
         (%ensure-generic-type-entry type))
        (t (%ensure-simple-type-entry type))))

(defun %ensure-assembly (assembly)
  (cond ((null assembly) nil)
        ((dotnet-object-p assembly)
         assembly)
        ((typep assembly 'string-designator)
         (load-assembly assembly))
        (t (error 'invalid-assembly-designator :datum assembly))))

(defvar *intern-typespec-toplevel* nil)

(defun %intern-string-ast (ast level assembly)
  (declare (type dotnet-name ast))
  (with-type-table (data aliases namespaces)
    (let* ((dotnet-name (%mknetsym ast))
           (aliased (gethash dotnet-name aliases)))
      (if aliased
        (%intern-type-ast (%parse-typespec aliased) level nil)
        (flet ((lookup (name) (gethash name data)))
          (or (lookup dotnet-name)
              (loop :for prefix :in namespaces
                    :for resolved = (lookup (concatenate 'string prefix dotnet-name))
                    :when resolved :do (return resolved)
                      :finally (return (%ensure-type-entry
                                        (%get-type-by-name
                                         dotnet-name
                                         t
                                         (%ensure-assembly assembly)))))))))))

(defun %intern-type-ast (ast level assembly)
  "Interns parsed type designator AST"
  (cond ((stringp ast)
         (%intern-string-ast ast level assembly))
        ((consp ast)
         (case (car ast)
           (* (%ensure-pointer-type-entry-by-element-type
               (%intern-type-ast (second ast) (1+ level) assembly)))
           (:ref (unless (zerop level)
                   (error 'inner-ref-type-error :datum *intern-typespec-toplevel*))
            (%ensure-ref-type-entry-by-element-type
             (%intern-type-ast (second ast) (1+ level) assembly)))
           (:array (let ((element-type
                           (%intern-type-ast (second ast) (1+ level) assembly))
                         (rank (third ast)))
                     (%ensure-array-type-entry-by-element-type element-type rank)))
           (:qualified (%intern-type-ast (second ast) level (third ast)))
           ;; generic type
           (t (let ((definition (%intern-type-ast (car ast) (1+ level) assembly))
                    (args (loop :for arg :in (rest ast)
                                :collect (%intern-type-ast arg (1+ level) nil))))
                (%ensure-generic-type-entry-with-args definition args)))))
        (t (error 'invalid-type-ast :datum ast))))

(defun %intern-toplevel-type-ast (ast &optional assembly)
  (let ((*intern-typespec-toplevel* ast))
    (%intern-type-ast ast 0 assembly)))

(defun %parse-typespec (type)
  "Parses external API type designator"
  (cond ((dotnet-type-p type)
         (parse-type-name (or (type-assembly-qualified-name type)
                              (type-full-name type))))
        ((typep type 'string-designator)
         (parse-type-name (dotnet-name type)))
        ((consp type)
         (destructuring-bind (head &rest rest) type
           (declare (type string-designator head))
           (let* ((head-string (dotnet-name head))
                  (name (string-upcase head-string)))
             (cond ((string= name "ARRAY")
                    (destructuring-bind (element-type &optional (rank 1)) rest
                      (declare (type (integer 1 32) rank))
                      (list :array (%parse-typespec element-type) rank)))
                   ((string= name "REF")
                    (destructuring-bind (inner-type) rest
                      (list :ref (%parse-typespec inner-type))))
                   ((string= name "*")
                    (destructuring-bind (inner-type) rest
                      (list '* (%parse-typespec inner-type))))
                   (t (destructuring-bind (arg &rest args) rest
                        (let* ((args (cons arg args))
                               (def (format nil "~a`~d" head-string (length args))))
                          (cons (%parse-typespec def)
                                (mapcar #'%parse-typespec args)))))))))
        (t (error 'invalid-type-designator type))))

(defun %resolve-type-entry (type &optional assembly)
  (declare (type dotnet-type-designator type))
  "Resolves type entry for a type designator"
  (%intern-toplevel-type-ast (%parse-typespec type) assembly))

(defun resolve-type (type &key (errorp t) (error-value nil) assembly)
  (declare (type dotnet-type-designator type))
  "Resolves a .Net type designated by TYPE specifier from an
 internal type cache."
  (let ((ast (%parse-typespec type)))
    (handler-bind ((error (lambda (e)
                            (declare (ignore e))
                            (unless errorp
                              (return-from resolve-type error-value)))))
      (with-type-table-lock (:read)
        (%type-entry-type (%intern-toplevel-type-ast ast assembly))))))

;;; vim: ft=lisp et
