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

(in-readtable bike-syntax)

(defun %do-types (func search assembly namespace exported-only)
  (declare (type function-designator func)
           (type (or null string-designator) search)
           (type (or null string-designator) namespace)
           (type (or null string-designator dotnet-object) assembly))
  (let ((search (and search (simple-character-string search)))
        (namespace (and namespace (simple-character-string namespace))))
    (flet ((process-assembly (asm)
             (do-enumerable (type (if exported-only
                                    (%assembly-exported-types asm)
                                    (%assembly-defined-types asm)))
               (let ((satisfies-search
                       (or (and (not search) t)
                           (let ((name (type-name type)))
                             (search search name :test #'char-equal))))
                     (satisfies-namespace
                       (or (and (not namespace) t)
                           (let ((ns (type-namespace type)))
                             (and ns (string-equal ns namespace))))))
                 (when (and satisfies-search
                            satisfies-namespace)
                   (funcall func type))))))
      (if assembly
        (process-assembly (if (bike-type-p assembly 'System.Reflection.Assembly)
                            assembly
                            (import-assembly assembly)))
        (dolist (asm (get-loaded-assemblies))
          (process-assembly asm)))))
  (values))

(defmacro do-types ((type-var &key search assembly namespace (exported-only t)) &body body)
  "Evaluates BODY forms in a dynamic environment where TYPE-VAR is subsequently bound
  to each of the types found during the search based on the following filter arguments:

:SEARCH - a string-designator or NIL.
          Designates a substring to search for in the type name(excluding namespace).

:ASSEMBLY - Designates a specific assembly to search in. Can be a name,
            a System.Reflection.Assembly object, or NIL, in which case
            all loaded assemblies are searched.

:NAMESPACE - Designates a namespace to search for types. In case of it being null,
             all loaded namespaces are processed.

:EXPORTED-ONLY - Defaults to T. In case of it being non-null, only exported(i.e. public)
                 types of assemblies are processed.
                 Otherwise, all of the defined types are processed.

TYPE-VAR may be a symbol or a list of form (TYPE-VAR RESULT-FORM) where RESULT-FORM
  designates a form to be evaluated and returned at the end of the iteration."
  (with-gensyms (type-iterator type-arg)
    (destructuring-bind (type-var &optional result-form)
        (ensure-list type-var)
      `(let (,type-var)
         (declare (ignorable ,type-var))
         (flet ((,type-iterator (,type-arg)
                  (setf ,type-var ,type-arg)
                  (locally ,@body)))
           (%do-types #',type-iterator ,search ,assembly ,namespace ,exported-only)
           ,result-form)))))

(defun type-apropos (string-designator &key assembly namespace (exported-only t))
  (declare (type (or null string-designator) string-designator)
           (type (or null string-designator dotnet-object) assembly)
           (type (or null string-designator) namespace))
  "Briefly describes all types found according to the search
  given the following filter arguments:

STRING-DESIGNATOR - A substring to search for in the type name(excluding namespace).
                    Can be NIL, in which case all type names are processed.

:ASSEMBLY - Designates a specific assembly to search in. Can be a name,
            a System.Reflection.Assembly object, or NIL, in which case
            all loaded assemblies are searched.

:NAMESPACE - Designates a namespace to search for types. In case of it being null,
             all loaded namespaces are processed.

:EXPORTED-ONLY - Defaults to T. In case of it being non-null, only exported(i.e. public)
                 types of assemblies are processed.
                 Otherwise, all of the defined types are processed."
  (do-types ((type (values)) :search string-designator
                             :assembly assembly
                             :namespace namespace
                             :exported-only exported-only)
    (write-type-name type)
    (terpri)))

(defun type-apropos-list (string-designator &key assembly namespace (exported-only t))
  (declare (type (or null string-designator) string-designator)
           (type (or null string-designator dotnet-object) assembly)
           (type (or null string-designator) namespace))
  "Like TYPE-APROPOS, but returns a list of types instead of describing them."
  (let* ((head (cons nil nil))
         (tail head))
    (do-types (type :search string-designator
                    :assembly assembly
                    :namespace namespace
                    :exported-only exported-only)
      (let ((item (cons type nil)))
        (psetf (cdr tail) item
               tail item)))
    (cdr head)))

(defun namespace-apropos-list (string-designator &key assembly (exported-only t))
  (declare (type (or null string-designator) string-designator)
           (type (or null string-designator dotnet-object) assembly))
  "Like NAMESPACE-APROPOS, but returns a list of namespace names instead of describing them."
  (let ((ht (make-hash-table :test #'equal))
        (search (and string-designator (simple-character-string string-designator))))
    (do-types (type :assembly assembly
                    :exported-only exported-only)
      (when-let ((ns (type-namespace type)))
        (setf (gethash ns ht) t)))
    (if search
      (loop :for k :being :the :hash-keys :in ht
            :when (search search k :test #'char-equal)
              :collect k :into namespaces
            :finally (return (sort namespaces #'string<)))
      (sort (hash-table-keys ht) #'string<))))

(defun namespace-apropos (string-designator &key assembly (exported-only t))
  (declare (type (or null string-designator) string-designator)
           (type (or null string-designator dotnet-object) assembly))
  "Briefly describes all namespaces found according to the search
  given the following filter arguments:

STRING-DESIGNATOR - A substring to search for in the namespace name.
                    Can be NIL, in which case all names are processed.

:ASSEMBLY - Designates a specific assembly to search in. Can be a name,
            a System.Reflection.Assembly object, or NIL, in which case
            all loaded assemblies are searched.

:EXPORTED-ONLY - Defaults to T. In case of it being non-null, only exported(i.e. public)
                 types of assemblies are processed.
                 Otherwise, all of the defined types are processed."
  (dolist (ns (namespace-apropos-list string-designator :assembly assembly
                                                        :exported-only exported-only)
              (values))
    (write-line ns)))

(declaim (inline %apropos-binding-flags))
(defun %apropos-binding-flags (instance static public non-public declared-only)
  (enum 'System.Reflection.BindingFlags
        (logior +binding-flags-ignore-case+
                (if instance +binding-flags-instance+ 0)
                (if static +binding-flags-static+ 0)
                (if public +binding-flags-public+ 0)
                (if non-public +binding-flags-non-public+ 0)
                (if declared-only +binding-flags-declared-only+ 0))))

(defun %do-members (type func search instance static public non-public declared-only
                    constructors events fields methods properties custom nested-types)
  (declare (type dotnet-type-designator type)
           (type function-designator func)
           (type (or null string-designator) search))
  (let ((type (resolve-type type))
        (search (and search (simple-character-string search)))
        (flags (%apropos-binding-flags instance static public non-public declared-only)))
    (do-bike-vector (m (%get-type-members type flags))
      (let ((kind (unbox (member-info-member-type m))))
        (flet ((is-kind (condition flag)
                 (and condition (= flag (logand kind flag)))))
          (let* ((satisfies-search
                   (or (and (not search) t)
                       (let ((name (member-info-name m)))
                         (search search name :test #'char-equal))))
                 (satisfies-kind
                   (or (is-kind constructors +member-type-constructor+)
                       (is-kind events +member-type-event+)
                       (is-kind fields +member-type-field+)
                       (is-kind methods +member-type-method+)
                       (is-kind properties +member-type-property+)
                       (is-kind custom +member-type-custom+)
                       (is-kind nested-types +member-type-nested-type+))))
            (when (and satisfies-search
                       satisfies-kind)
              (funcall func m))))))))

(defmacro do-members ((var type &key search
                                     (instance t)
                                     (static t)
                                     (public t)
                                     non-public
                                     declared-only
                                     (constructors t)
                                     (events t)
                                     (fields t)
                                     (methods t)
                                     (properties t)
                                     (custom t)
                                     (nested-types t))
                      &body body)
  "Evaluates BODY forms in a dynamic environment where VAR is subsequently bound
  to each of the members of the TYPE found during the search based on the following
  filter arguments:

:SEARCH - a string-designator or NIL.
          Designates a substring to search for in the member name.

:INSTANCE - whether to search for instance members.

:STATIC - whether to search for static members.

:PUBLIC - whether to search for public members.

:NON-PUBLIC - whether to search for non-public members(i.e. private and protected).

:DECLARED-ONLY - whether to only include members declared in the type directly and
                 omit inherited ones.

:CONSTRUCTORS - whether to search for constructors.

:EVENTS - whether to search for events.

:FIELDS - whether to search for fields.

:METHODS - whether to search for methods.

:PROPERTIES - whether to search for properties.

:CUSTOM - whether to search for custom type members.

:NESTED-TYPES - whether to search for nested types.

VAR can either be a symbol or a list of form (VAR RESULT-FORM) where RESULT-FORM
  designates a form to be evaluated and returned at the end of the iteration."
  (with-gensyms (member-iterator tmp-var)
    (destructuring-bind (var &optional result-form)
        (ensure-list var)
      `(let (,var)
         (flet ((,member-iterator (,tmp-var)
                  (setf ,var ,tmp-var)
                  (locally ,@body)))
           (%do-members ,type #',member-iterator ,search
                        ,instance ,static ,public ,non-public ,declared-only
                        ,constructors ,events ,fields ,methods ,properties
                        ,custom ,nested-types)
           ,result-form)))))

(defun member-apropos (type string-designator &key (instance t)
                                                   (static t)
                                                   (public t)
                                                   non-public
                                                   declared-only
                                                   (constructors t)
                                                   (events t)
                                                   (fields t)
                                                   (methods t)
                                                   (properties t)
                                                   (custom t)
                                                   (nested-types t))
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Briefly describes members of the TYPE found during the search based on the following
  filter arguments:

STRING-DESIGNATOR - A substring to search for in the member name.
                    Can be NIL, in which case all names are processed.

:INSTANCE - whether to search for instance members.

:STATIC - whether to search for static members.

:PUBLIC - whether to search for public members.

:NON-PUBLIC - whether to search for non-public members(i.e. private and protected).

:DECLARED-ONLY - whether to only include members declared in the type directly and
                 omit inherited ones.

:CONSTRUCTORS - whether to search for constructors.

:EVENTS - whether to search for events.

:FIELDS - whether to search for fields.

:METHODS - whether to search for methods.

:PROPERTIES - whether to search for properties.

:CUSTOM - whether to search for custom type members.

:NESTED-TYPES - whether to search for nested types."
  (do-members ((m (values)) type :search string-designator
                                 :instance instance
                                 :static static
                                 :public public
                                 :non-public non-public
                                 :declared-only declared-only
                                 :constructors constructors
                                 :events events
                                 :fields fields
                                 :methods methods
                                 :properties properties
                                 :custom custom
                                 :nested-types nested-types)
    (write m :readably nil :escape nil)
    (terpri)))

(defun constructor-apropos (type &key (instance t)
                                      (static t)
                                      (public t)
                                      non-public
                                      declared-only)
  (declare (type dotnet-type-designator type))
  "Like MEMBER-APROPOS but only describes constructors and does not allow for name search."
  (member-apropos type nil :instance instance
                           :static static
                           :public public
                           :non-public non-public
                           :declared-only declared-only
                           :constructors t
                           :events nil
                           :fields nil
                           :methods nil
                           :properties nil
                           :custom nil
                           :nested-types nil))

(defun event-apropos (type string-designator &key (instance t)
                                                  (static t)
                                                  (public t)
                                                  non-public
                                                  declared-only)
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Like MEMBER-APROPOS but only describes events."
  (member-apropos type string-designator :instance instance
                                         :static static
                                         :public public
                                         :non-public non-public
                                         :declared-only declared-only
                                         :constructors nil
                                         :events t
                                         :fields nil
                                         :methods nil
                                         :properties nil
                                         :custom nil
                                         :nested-types nil))

(defun field-apropos (type string-designator &key (instance t)
                                                  (static t)
                                                  (public t)
                                                  non-public
                                                  declared-only)
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Like MEMBER-APROPOS but only describes fields."
  (member-apropos type string-designator :instance instance
                                         :static static
                                         :public public
                                         :non-public non-public
                                         :declared-only declared-only
                                         :constructors nil
                                         :events nil
                                         :fields t
                                         :methods nil
                                         :properties nil
                                         :custom nil
                                         :nested-types nil))

(defun method-apropos (type string-designator &key (instance t)
                                                  (static t)
                                                  (public t)
                                                  non-public
                                                  declared-only)
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Like MEMBER-APROPOS but only describes methods."
  (member-apropos type string-designator :instance instance
                                         :static static
                                         :public public
                                         :non-public non-public
                                         :declared-only declared-only
                                         :constructors nil
                                         :events nil
                                         :fields nil
                                         :methods t
                                         :properties nil
                                         :custom nil
                                         :nested-types nil))

(defun property-apropos (type string-designator &key (instance t)
                                                     (static t)
                                                     (public t)
                                                     non-public
                                                     declared-only)
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Like MEMBER-APROPOS but only describes properties."
  (member-apropos type string-designator :instance instance
                                         :static static
                                         :public public
                                         :non-public non-public
                                         :declared-only declared-only
                                         :constructors nil
                                         :events nil
                                         :fields nil
                                         :methods nil
                                         :properties t
                                         :custom nil
                                         :nested-types nil))

(defun custom-member-apropos (type string-designator &key (instance t)
                                                          (static t)
                                                          (public t)
                                                          non-public
                                                          declared-only)
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Like MEMBER-APROPOS but only describes custom members."
  (member-apropos type string-designator :instance instance
                                         :static static
                                         :public public
                                         :non-public non-public
                                         :declared-only declared-only
                                         :constructors nil
                                         :events nil
                                         :fields nil
                                         :methods nil
                                         :properties nil
                                         :custom t
                                         :nested-types nil))

(defun nested-type-apropos (type string-designator &key (instance t)
                                                        (static t)
                                                        (public t)
                                                        non-public
                                                        declared-only)
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Like MEMBER-APROPOS but only describes nested types."
  (member-apropos type string-designator :instance instance
                                         :static static
                                         :public public
                                         :non-public non-public
                                         :declared-only declared-only
                                         :constructors nil
                                         :events nil
                                         :fields nil
                                         :methods nil
                                         :properties nil
                                         :custom nil
                                         :nested-types t))

(defun member-apropos-list (type string-designator &key (instance t)
                                                        (static t)
                                                        (public t)
                                                        non-public
                                                        declared-only
                                                        (constructors t)
                                                        (events t)
                                                        (fields t)
                                                        (methods t)
                                                        (properties t)
                                                        (custom t)
                                                        (nested-types t))
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Like MEMBER-APROPOS but returns a list of members instead of describing them."
  (let* ((head (cons nil nil))
         (tail head))
    (do-members ((m (cdr head)) type :search string-designator
                                     :instance instance
                                     :static static
                                     :public public
                                     :non-public non-public
                                     :declared-only declared-only
                                     :constructors constructors
                                     :events events
                                     :fields fields
                                     :methods methods
                                     :properties properties
                                     :custom custom
                                     :nested-types nested-types)
      (let ((item (cons m nil)))
        (psetf (cdr tail) item
               tail item)))))

(defun constructor-apropos-list (type &key (instance t)
                                           (static t)
                                           (public t)
                                           non-public
                                           declared-only)
  (declare (type dotnet-type-designator type))
  "Like CONSTRUCTOR-APROPOS but returns a list of constructors instead of describing them."
  (member-apropos-list type nil :instance instance
                                :static static
                                :public public
                                :non-public non-public
                                :declared-only declared-only
                                :constructors t
                                :events nil
                                :fields nil
                                :methods nil
                                :properties nil
                                :custom nil
                                :nested-types nil))

(defun event-apropos-list (type string-designator &key (instance t)
                                                       (static t)
                                                       (public t)
                                                       non-public
                                                       declared-only)
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Like EVENT-APROPOS but returns a list of events instead of describing them."
  (member-apropos-list type string-designator :instance instance
                                              :static static
                                              :public public
                                              :non-public non-public
                                              :declared-only declared-only
                                              :constructors nil
                                              :events t
                                              :fields nil
                                              :methods nil
                                              :properties nil
                                              :custom nil
                                              :nested-types nil))

(defun field-apropos-list (type string-designator &key (instance t)
                                                       (static t)
                                                       (public t)
                                                       non-public
                                                       declared-only)
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Like FIELD-APROPOS but returns a list of fields instead of describing them."
  (member-apropos-list type string-designator :instance instance
                                              :static static
                                              :public public
                                              :non-public non-public
                                              :declared-only declared-only
                                              :constructors nil
                                              :events nil
                                              :fields t
                                              :methods nil
                                              :properties nil
                                              :custom nil
                                              :nested-types nil))

(defun method-apropos-list (type string-designator &key (instance t)
                                                        (static t)
                                                        (public t)
                                                        non-public
                                                        declared-only)
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Like METHOD-APROPOS but returns a list of methods instead of describing them."
  (member-apropos-list type string-designator :instance instance
                                              :static static
                                              :public public
                                              :non-public non-public
                                              :declared-only declared-only
                                              :constructors nil
                                              :events nil
                                              :fields nil
                                              :methods t
                                              :properties nil
                                              :custom nil
                                              :nested-types nil))

(defun property-apropos-list (type string-designator &key (instance t)
                                                          (static t)
                                                          (public t)
                                                          non-public
                                                          declared-only)
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Like PROPERTY-APROPOS but returns a list of properties instead of describing them."
  (member-apropos-list type string-designator :instance instance
                                              :static static
                                              :public public
                                              :non-public non-public
                                              :declared-only declared-only
                                              :constructors nil
                                              :events nil
                                              :fields nil
                                              :methods nil
                                              :properties t
                                              :custom nil
                                              :nested-types nil))

(defun custom-member-apropos-list (type string-designator &key (instance t)
                                                               (static t)
                                                               (public t)
                                                               non-public
                                                               declared-only)
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Like CUSTOM-MEMBER-APROPOS but returns a list of custom members instead of describing them."
  (member-apropos-list type string-designator :instance instance
                                              :static static
                                              :public public
                                              :non-public non-public
                                              :declared-only declared-only
                                              :constructors nil
                                              :events nil
                                              :fields nil
                                              :methods nil
                                              :properties nil
                                              :custom t
                                              :nested-types nil))

(defun nested-type-apropos-list (type string-designator &key (instance t)
                                                             (static t)
                                                             (public t)
                                                             non-public
                                                             declared-only)
  (declare (type dotnet-type-designator type)
           (type (or null string-designator) string-designator))
  "Like NESTED-TYPE-APROPOS but returns a list of nested types instead of describing them."
  (member-apropos-list type string-designator :instance instance
                                              :static static
                                              :public public
                                              :non-public non-public
                                              :declared-only declared-only
                                              :constructors nil
                                              :events nil
                                              :fields nil
                                              :methods nil
                                              :properties nil
                                              :custom nil
                                              :nested-types t))
;;; vim: ft=lisp et
