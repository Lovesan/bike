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
  (declare (type simple-character-string name))
  (with-type-table (data)
    (values (the (or null type-entry) (gethash name data)))))

(declaim (inline (setf %type-entry)))
(defun (setf %type-entry) (new-entry name)
  (declare (type type-entry new-entry)
           (type simple-character-string name))
  (with-type-table (data)
    (setf (gethash name data) new-entry))
  new-entry)

(declaim (inline %get-aliased-type))
(defun %get-aliased-type (name)
  (declare (type simple-character-string name))
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
  (let ((name (simple-character-string-upcase (type-full-name type))))
    (or (%type-entry name)
        (let* ((args (type-generic-arguments type))
               (qualified-name (%maybe-get-qualified-name type))
               (entry (%make-type-entry type
                                        :qualified-name qualified-name
                                        :type-args args)))
          (with-type-table-lock (:write)
            (or (%type-entry name)
                (setf (%type-entry name) entry)))))))

(defun %ensure-generic-type-entry (type)
  (declare (type dotnet-type type))
  "Ensures instantiated generic type entry"
  (let* ((definition (%ensure-generic-type-definition-entry
                      (generic-type-definition-of type))))
    (with-type-entry (instances) definition
      (flet ((lookup (&aux (instance-type type))
               (dolist (instance instances)
                 (with-type-entry (type) instance
                   (when (bike-equals instance-type type)
                     (return instance))))))
        (or (lookup)
            (let* ((args (type-generic-arguments type))
                   (qualified-name (%maybe-get-qualified-name type))
                   (entry (%make-type-entry type
                                            :qualified-name qualified-name
                                            :type-args args)))
              (with-type-table-lock (:write)
                (or (lookup) (first (push entry instances))))))))))

(defun %ensure-generic-type-entry-with-args (definition args)
  (declare (type (or dotnet-type type-entry) definition)
           (type (cons type-entry list) args))
  "Ensures instantiated generic type entry"
  (let ((definition (%ensure-type-entry definition)))
    (with-type-entry (instances (definition-type type)) definition
      (flet ((lookup ()
               (dolist (instance instances)
                 (with-type-entry ((instance-args type-args)) instance
                   (when (loop :for type-arg-entry :in args
                               :for type-arg = (%type-entry-type type-arg-entry)
                               :for instance-arg :in instance-args
                               :unless (bike-equals instance-arg type-arg)
                                 :do (return nil)
                               :finally (return t))
                     (return instance))))))
        (or (lookup)
            (let* ((args (mapcar #'%type-entry-type args))
                   (type (apply #'make-generic-type definition-type args))
                   (qualified-name (%maybe-get-qualified-name type))
                   (entry (%make-type-entry type
                                            :qualified-name qualified-name
                                            :type-args args)))
              (with-type-table-lock (:write)
                (or (lookup) (first (push entry instances))))))))))

(defun %ensure-pointer-type-entry-by-element-type (type)
  (declare (type (or type-entry dotnet-type) type))
  "Ensures pointer type entry for element type"
  (let ((entry (%ensure-type-entry type)))
    (with-type-entry (pointer-entry type) entry
      (or pointer-entry
          (let* ((ptr-type (make-pointer-type type))
                 (entry (%make-type-entry ptr-type :element-entry entry)))
            (with-type-table-lock (:write)
              (or pointer-entry (setf pointer-entry entry))))))))

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
          (let* ((ref-type (make-ref-type type))
                 (entry (%make-type-entry ref-type :element-entry entry)))
            (with-type-table-lock (:write)
              (or ref-entry (setf ref-entry entry))))))))

(defun %ensure-ref-type-entry (type)
  (declare (type dotnet-type type))
  "Ensures ref type entry"
  (let ((base-type (element-type-of type)))
    (%ensure-ref-type-entry-by-element-type base-type)))

(defun %ensure-array-type-entry-by-element-type (type rank mz-array-p)
  (declare (type (or dotnet-type type-entry) type)
           (type (integer 1 32) rank))
  "Ensures array type entry for element type"
  (let ((entry (%ensure-type-entry type)))
    (with-type-entry (array-entries mz-vector-entry type) entry
      (flet ((lookup ()
               (if (and mz-array-p (= rank 1))
                 mz-vector-entry
                 (and array-entries (svref array-entries (1- rank))))))
        (or (lookup)
            (let* ((entries (or array-entries
                                (make-array +max-array-rank+ :initial-element nil)))
                   (array-type (if (and (not mz-array-p) (= rank 1))
                                 (make-array-type type)
                                 (make-array-type* type rank)))
                   (array-entry (%make-type-entry array-type
                                                  :element-entry entry
                                                  :rank rank)))
              (with-type-table-lock (:write)
                (or (lookup)
                    (progn
                      (unless array-entries
                        (setf array-entries entries))
                      (if (and mz-array-p (= rank 1))
                        (setf mz-vector-entry array-entry)
                        (setf (svref entries (1- rank)) array-entry)))))))))))

(defun %ensure-array-type-entry (type)
  (declare (type dotnet-type type))
  "Ensures array type entry"
  (let ((base-type (element-type-of type))
        (rank (array-type-rank type))
        (sz-p (array-type-sz-p type)))
    (%ensure-array-type-entry-by-element-type base-type rank (not sz-p))))

(defun %ensure-simple-type-entry (type)
  (declare (type dotnet-type type))
  "Ensures simple, non-generic type entry"
  (let ((name (simple-character-string-upcase (type-full-name type))))
    (or (%type-entry name)
        (let* ((qualified-name (%maybe-get-qualified-name type))
               (entry (%make-type-entry type
                                        :qualified-name qualified-name)))
          (with-type-table-lock (:write)
            (or (%type-entry name)
                (setf (%type-entry name) entry)))))))

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
        ((typep assembly 'dotnet-object*)
         assembly)
        ((typep assembly 'string-designator)
         (load-assembly assembly))
        (t (error 'invalid-assembly-designator :datum assembly))))

(defun %intern-string-ast (ast assembly)
  (declare (type simple-character-string ast))
  (with-type-table (data aliases namespaces)
    (let* ((dotnet-name (simple-character-string-upcase ast))
           (aliased (gethash dotnet-name aliases)))
      (if aliased
        (%intern-type-ast (parse-typespec aliased) nil)
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

(defun %intern-type-ast (ast assembly)
  "Interns parsed type designator AST"
  (cond ((dotnet-type-p ast)
         (%ensure-type-entry ast))
        ((stringp ast)
         (%intern-string-ast ast assembly))
        ((consp ast)
         (case (car ast)
           (* (%ensure-pointer-type-entry-by-element-type
               (%intern-type-ast (second ast) assembly)))
           (:ref
            (%ensure-ref-type-entry-by-element-type
             (%intern-type-ast (second ast) assembly)))
           (:array (let* ((element-type
                            (%intern-type-ast (second ast) assembly))
                          (rank (third ast))
                          (mz-vector-p (eq rank '*)))
                     (%ensure-array-type-entry-by-element-type
                      element-type
                      (if mz-vector-p 1 rank)
                      mz-vector-p)))
           (:qualified (%intern-type-ast (second ast) (third ast)))
           ;; generic type
           (t (let ((definition (%intern-type-ast (car ast) assembly))
                    (args (loop :for arg :in (rest ast)
                                :collect (%intern-type-ast arg nil))))
                (%ensure-generic-type-entry-with-args definition args)))))
        (t (error 'invalid-type-ast :datum ast))))

(defvar *parse-typespec-toplevel* nil)

(defun type-name-generic-parameter-count (name)
  (declare (type simple-character-string name))
  (let* ((len (length name))
         (pos (loop :for i :of-type fixnum :downfrom (- len 2)
                    :downto 1
                    :when (char= (schar name i) #\`)
                      :return (1+ i))))
    (and pos
         (< pos len)
         (loop :with res = 0
               :for i :from pos :below len
               :for d = (digit-char-p (schar name i))
               :if d :do (setf res (+ d (* res 10)))
               :else :return nil
               :finally (return res)))))

(defun %parse-typespec (type level generic-parameters &optional parsedp qualifiedp)
  (declare (type list generic-parameters))
  (cond ((dotnet-type-p type) type)
        ((typep type 'string-designator)
         (let ((type (parse-type-name (simple-character-string type))))
           (if (consp type)
             (%parse-typespec type level generic-parameters t qualifiedp)
             (or (and generic-parameters
                      (find type generic-parameters
                            :test #'string-equal
                            :key #'simple-character-string))
                 type))))
        ((consp type)
         (destructuring-bind (head &rest rest) type
           (declare (type string-designator head))
           (if parsedp
             ;; Recursive search for generic parameters
             (case head
               (:array (list :array
                             (%parse-typespec (first rest) (1+ level) generic-parameters t)
                             (second rest)))
               (:ref
                (unless (zerop level)
                  (error 'inner-ref-type-error :datum *parse-typespec-toplevel*))
                (list :ref (%parse-typespec (first rest) (1+ level) generic-parameters t)))
               (* (list '* (%parse-typespec (first rest) (1+ level) generic-parameters t)))
               (:qualified
                (when qualifiedp
                  (error 'inner-qualified-type-error :datum *parse-typespec-toplevel*))
                (list :qualified
                      (%parse-typespec (first rest) level generic-parameters t)
                      (second rest)))
               ;; generic type
               (t (cons head
                        ;; ^
                        ;; apparently, a generic type parameter can not be
                        ;; a generic type designator itself
                        (mapcar (lambda (arg)
                                  (%parse-typespec arg (1+ level) generic-parameters t))
                                rest))))
             (let* ((name (simple-character-string-upcase head)))
               (cond ((string= name "ARRAY")
                      (destructuring-bind (element-type &optional (rank 1)) rest
                        (check-type rank (or (integer 1 32) (eql *)))
                        (list :array
                              (%parse-typespec element-type (1+ level) generic-parameters)
                              rank)))
                     ((string= name "REF")
                      (unless (zerop level)
                        (error 'inner-ref-type-error :datum *parse-typespec-toplevel*))
                      (destructuring-bind (inner-type) rest
                        (list :ref (%parse-typespec inner-type (1+ level) generic-parameters))))
                     ((string= name "*")
                      (destructuring-bind (inner-type) rest
                        (list '* (%parse-typespec inner-type (1+ level) generic-parameters))))
                     ((string= name "QUALIFIED")
                      (when qualifiedp
                        (error 'inner-qualified-type-error :datum *parse-typespec-toplevel*))
                      (destructuring-bind (inner-type assembly-designator) rest
                        (list :qualified
                              (%parse-typespec inner-type level generic-parameters nil t)
                              (simple-character-string assembly-designator))))
                     ;; Generic type
                     (t (destructuring-bind (arg &rest args) rest
                          (let* ((args (cons arg args))
                                 (arg-count (length args))
                                 (declared-count (type-name-generic-parameter-count name))
                                 (def (if declared-count
                                        (progn
                                          (unless (= declared-count arg-count)
                                            (error 'generic-argument-count-mismatch
                                                   :token nil
                                                   :value type
                                                   :position (position #\` name)
                                                   :datum name))
                                          name)
                                        (format nil "~a`~d" name arg-count))))
                            (cons (%parse-typespec def (1+ level) generic-parameters)
                                  (mapcar (lambda (arg)
                                            (%parse-typespec arg (1+ level) generic-parameters))
                                          args))))))))))
        (t (error 'invalid-type-designator :datum type))))

(defun parse-typespec (type &optional generic-parameters)
  "Parses external API type designator"
  (declare (type list generic-parameters))
  (let ((*parse-typespec-toplevel* type))
    (%parse-typespec type 0 generic-parameters)))

(defun normalize-typespec (typespec &optional generic-parameters)
  (declare (type list generic-parameters))
  "Returns normalized representation of a type specifier"
  (let ((ast (parse-typespec typespec generic-parameters)))
    (labels ((walk-type-entry (entry)
               (let* ((type (%type-entry-type entry))
                      (name (or (type-assembly-qualified-name type)
                                (type-full-name type)))
                      (ast (parse-type-name name)))
                 (walk ast nil t)))
             (walk (ast assembly &optional parsedp)
               (cond
                 ((dotnet-type-p ast)
                  (walk-type-entry (%ensure-type-entry ast)))
                 ((symbolp ast) ast) ;; generic parameter
                 ((stringp ast)
                  (if parsedp
                    ;;
                    (or (find ast generic-parameters
                              :test #'string-equal
                              :key #'simple-character-string)
                        (if assembly
                          (list :qualified ast assembly)
                          ast))
                    (walk-type-entry (%intern-string-ast ast assembly))))
                 ((consp ast)
                  (destructuring-bind (head &rest rest)
                      ast
                    (case head
                      (* (list '* (walk (first rest) assembly parsedp)))
                      (:ref (list :ref (walk (first rest) assembly parsedp)))
                      (:array (list :array
                                    (walk (first rest) assembly parsedp)
                                    (second rest)))
                      (:qualified
                       (let* ((assembly (second rest))
                              (inner-type (walk (first rest) assembly parsedp)))
                         inner-type))
                      ;; Generic type
                      (t (let ((def (walk head assembly parsedp))
                               (args (mapcar (lambda (arg) (walk arg nil parsedp)) rest)))
                           (if (and (consp def)
                                    (eq (car def) :qualified))
                             (list :qualified
                                   (cons (second def) args)
                                   (third def))
                             (cons def args)))))))
                 (t (error 'invalid-type-ast :datum ast)))))
      (with-type-table-lock (:read)
        (walk ast nil)))))

(defun %resolve-type-entry (type &optional assembly)
  (declare (type dotnet-type-designator type))
  "Resolves type entry for a type designator"
  (%intern-type-ast (parse-typespec type) assembly))

(defun resolve-type (type &key (errorp t) (error-value nil) assembly)
  (declare (type dotnet-type-designator type))
  "Resolves a .Net type designated by TYPE specifier from an
 internal type cache."
  (let ((ast (parse-typespec type)))
    (handler-bind ((error (lambda (e)
                            (declare (ignore e))
                            (unless errorp
                              (return-from resolve-type error-value)))))
      (with-type-table-lock (:read)
        (%type-entry-type (%intern-type-ast ast assembly))))))

;;; vim: ft=lisp et
