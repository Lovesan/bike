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

(defvar *print-dotnet-object* t
  "Non-NIL value forces printing of full contents of dotnet objects")

(defvar *print-enumerable* t
  "Non-NIL value forces printing of IEnumerable contents")

;; We cannot rely on *print-circle* because our .net objects
;;   may not share equal identity on the lisp side
;;   despite being the same objects.
;; This is especially true in the case of value types.
;; For ex. DateTime type has a DateTime value inside it.
;; So, instead, we disallow printing more than one level of a given type.
(defvar *printed-types* '())

(defvar *printer-dispatch* (make-hash-table :test #'equal)
  "Holds mappings of .Net type names to printing functions")

(defvar *print-dotnet-type-namespaces* :normalize
  "Value of NIL forces omitting printing namespaces while printing type names.
Value of :NORMALIZE omits printing namespaces whenever
  they are currently used by means of USE-NAMESPACE function.
Other values force the printing of type namespaces.")

(defvar *print-dotnet-type-parameters* t
  "Non-NIL value forces printing of generic type parameters.")

(defvar *print-dotnet-type-qualified* nil
  "Non-NIL value forces printing of assembly qualified type names whenever possible.")

(defvar *print-dotnet-type-pointer* t
  "Non-NIL value forces printing of pointer type name suffix (i.e. '*').")

(defvar *print-dotnet-type-ref* t
  "Non-NIL value forces printing of 'by-ref' type name suffix (i.e. '&').")

(defun write-type-name (type &key (namespaces *print-dotnet-type-namespaces*)
                                  (parameters *print-dotnet-type-parameters*)
                                  (qualified *print-dotnet-type-qualified*)
                                  (pointer *print-dotnet-type-pointer*)
                                  (ref *print-dotnet-type-ref*)
                                  (stream *standard-output*))
  (declare (type dotnet-type type)
           (type stream stream))
  "Outputs name of a TYPE to the specified STREAM, defaulting to *standard-output*

:NAMESPACES - whether to print type namespaces.
  NIL - do not print namespaces
  :NORMALIZE - omit used namespaces
  other value - print all namespaces

:PARAMETERS - whether to print generic type parameters

:QUALIFIED - whether to print assembly-qualified type names

:POINTER - whether to print '*' suffix for pointer types

:REF - whether to print '&' suffix for ByRef types

:STREAM - stream to output to"
  (let ((ast (parse-type-name (or (and qualified
                                       (type-assembly-qualified-name type))
                                  (type-full-name type)))))
    (labels ((rec (ast &optional is-type-arg)
               (cond ((stringp ast)
                      (case namespaces
                        ((nil) (let* ((dot-pos (position #\. ast :from-end t)))
                                 (when dot-pos
                                   (setf ast (subseq ast (1+ dot-pos))))
                                 (write-string ast stream)))
                        (:normalize
                         (with-type-table (namespaces lock)
                           (with-type-table-lock (:read)
                             (loop :with dot-pos = (or (position #\. ast :from-end t)
                                                       (progn
                                                         (write-string ast stream)
                                                         (return)))
                                   :with ns-prefix
                                     = (simple-character-string-upcase
                                        (subseq ast 0 (1+ dot-pos)))
                                   :for ns :in namespaces
                                   :when (string= ns ns-prefix)
                                     :do (write-string (subseq ast (1+ dot-pos)) stream)
                                         (return)
                                   :finally (write-string ast stream)))))
                        (t (write-string ast stream))))
                     ((consp ast)
                      (case (car ast)
                        (* (rec (second ast))
                         (when pointer
                           (write-char #\* stream)))
                        (:ref (rec (second ast))
                         (when ref
                           (write-char #\& stream)))
                        (:qualified
                         (when qualified
                           (when is-type-arg (write-char #\[ stream)))
                         (rec (second ast))
                         (when qualified
                           (format stream ", ~a" (third ast))
                           (when is-type-arg (write-char #\] stream))))
                        (:array (let ((element-type (second ast))
                                      (rank (third ast)))
                                  (declare (type (or (integer 1 32) (eql *)) rank))
                                  (rec element-type)
                                  (write-char #\[ stream)
                                  (if (eq rank '*)
                                    (write-char #\* stream)
                                    (loop :repeat (1- rank) :do
                                      (write-char #\, stream)))
                                  (write-char #\] stream)))
                        ;; generics
                        (t (let ((definition (first ast))
                                 (args (rest ast)))
                             (rec definition)
                             (when parameters
                               (write-char #\[ stream)
                               (rec (car args) t)
                               (dolist (arg (rest args))
                                 (write-char #\, stream)
                                 (rec arg t))
                               (write-char #\] stream))))))
                     (t (error 'invalid-type-ast :datum ast)))))
      (rec ast)
      type)))

(defun unqualified-type-definition-name (type)
  (declare (type dotnet-type type))
  "Returns an unqualified name of a TYPE"
  (when (ref-type-p type)
    (setf type (element-type-of type)))
  (loop :while (pointer-type-p type)
        :do (setf type (element-type-of type)))
  (let ((type (or (and (generic-type-p type)
                       (not (generic-type-definition-p type))
                       (generic-type-definition-of type))
                  type)))
    (with-output-to-string (out)
      (write-type-name type :namespaces t
                            :parameters nil
                            :qualified nil
                            :pointer nil
                            :ref nil
                            :stream out))))

(defun normalized-type-name (type)
  (declare (type dotnet-type type))
  "Returns a convenient string representation of a type name stripped of used namespaces."
  (with-output-to-string (out)
    (write-type-name type :namespaces :normalize
                          :parameters t
                          :qualified nil
                          :pointer t
                          :ref t
                          :stream out)))

(defun qualified-type-name (type)
  (declare (type dotnet-type type))
  "Returns fully qualified name of a TYPE"
  (simple-character-string
   (with-output-to-string (out)
     (write-type-name type :namespaces t
                           :parameters (not (generic-type-definition-p type))
                           :qualified t
                           :pointer t
                           :ref t
                           :stream out))))

(defun print-normalized-newlines (string stream)
  (declare (type string string))
  (loop :with seen-cr = nil
        :for c :across string
        :do (case c
              (#\return
               (when seen-cr (terpri stream))
               (setf seen-cr t))
              (#\newline
               (setf seen-cr nil)
               (pprint-newline :mandatory stream ))
              (T
               (when seen-cr (terpri stream))
               (setf seen-cr nil)
               (write-char c stream)))
        :finally (when seen-cr (pprint-newline :mandatory stream))))

(defun symbol-full-name (symbol)
  (declare (type symbol symbol))
  (let* ((package (symbol-package symbol))
         (name (symbol-name symbol))
         (status (and package (nth-value 1 (find-symbol name package)))))
    (format nil "~:[#~;~:*~a~]~:[::~;:~]~a"
            (and package (package-name package))
            (or (null package) (eq status :external))
            name)))

(defmacro pprint-dotnet-object ((object stream &key type
                                                    identity
                                                    (prefix (when type " "))
                                                    (suffix (when identity " ")))
                                &body body)
  "PRINT-UNREADABLE-OBJECT analogue for .Net objects

Output OBJECT to STREAM with \"#<\" prefix, \">\" suffix, optionally
      with object-type prefix and object-identity suffix,
      and post-type suffix and pre-identity prefix and executing the
      code in BODY to provide possible further output."
  (declare (type symbol object stream))
  (with-gensyms (object-var type-var identity-var prefix-var suffix-var)
    `(let ((,object-var ,object)
           ,@(when type `((,type-var ,type)))
           ,@(when identity `((,identity-var ,identity)))
           ,@(when prefix `((,prefix-var ,prefix)))
           ,@(when suffix `((,suffix-var ,suffix))))
       (when *print-readably*
         (error 'print-not-readable :object ,object-var))
       (pprint-logical-block (,stream nil :prefix "#<" :suffix ">")
         ,@(when type
             `((when ,type-var
                 (write-type-name (bike-type-of ,object-var) :stream ,stream)
                 (pprint-newline :fill ,stream))))
         ,@(when prefix
             `((when ,prefix-var
                 (write-string ,prefix-var ,stream))))
         (prog1 ,@body
           ,@(when suffix
               `((when ,suffix-var (write-string ,suffix-var ,stream))))
           ,@(when identity
               `((when ,identity-var
                   (format ,stream "{~8,'0X}" (dotnet-object-handle-id ,object-var))))))))))

(defun print-exception-verbose (ex stream)
  (declare (type dotnet-exception ex)
           (type stream stream))
  (let ((type-name (normalized-type-name (bike-type-of ex)))
        (message (exception-message ex))
        (trace (exception-stack-trace ex)))
    (pprint-logical-block (stream nil)
      (format stream ".Net exception ~a" type-name)
      (when message
        (write-string ": " stream)
        (pprint-newline :linear stream)
        (print-normalized-newlines message stream))
      (when trace
        (pprint-newline :mandatory stream)
        (print-normalized-newlines trace stream))))
  ex)

(defun print-dotnet-object-simple (object stream &optional identity)
  (declare (type dotnet-object object)
           (type stream stream))
  (let* ((type (bike-type-of object))
         (type-str (%to-string type))
         (str (%to-string object))
         (only-print-type (or (zerop (length str))
                              (string-equal str type-str))))
    (if *print-escape*
      (pprint-dotnet-object (object stream :type t :identity identity
                                           :prefix (unless only-print-type " ")
                                           :suffix (when identity " "))
        (unless only-print-type
          (pprint-logical-block (stream nil)
            (print-normalized-newlines str stream))))
      (pprint-logical-block (stream nil)
        (if only-print-type
          (write-type-name type :stream stream)
          (print-normalized-newlines str stream))))))

(defun %print-enumerable (object stream)
  (declare (type dotnet-object object)
           (type stream stream))
  (let* ((e (%get-enumerator object)))
    (unwind-protect
         (flet ((body (stream)
                  (pprint-logical-block (stream nil)
                    (when (%enumerator-move-next e)
                      (loop (pprint-pop)
                            (write (%enumerator-current e) :stream stream)
                            (unless (%enumerator-move-next e) (return))
                            (write-char #\Space stream)
                            (pprint-newline :fill stream))))))
           (if *print-escape*
             (pprint-dotnet-object (object stream :type t :prefix " (" :suffix ")")
               (body stream))
             (progn
               (write-char #\( stream)
               (body stream)
               (write-char #\) stream))))
      (when (bike-type-p e 'System.IDisposable)
        (%dispose e)))))

(defun print-enumerable (object stream)
  (if *print-enumerable*
    (%print-enumerable object stream)
    (print-dotnet-object-simple object stream t)))

(defun print-member-value (object stream member pop-callback)
  (declare (type dotnet-object object member)
           (type stream stream)
           (type (function () (values)) pop-callback))
  (funcall pop-callback)
  (let ((name [member %Name]))
    (format stream "~a: " name)
    (exception-case
        (format stream "~w" [member GetValue object])
      (System.Exception (e)
        (format stream "#<!Caught ~a>"
                (normalized-type-name (bike-type-of e))))))
  member)

(defun print-member-vector (object stream members pop-callback)
  (declare (type dotnet-object object members)
           (type stream stream)
           (type (function () (values)) pop-callback))
  (let ((len (%array-length members))
        (prop-member-type #e(System.Reflection.MemberTypes Property)))
    (flet ((maybe-print (member first &aux (member-type (member-info-member-type member)))
             (unless (and (bike-equals member-type prop-member-type)
                          (property-indexer-p member))
               (unless first (format stream " ~_"))
               (print-member-value object stream member pop-callback))))
      (when (plusp len)
        (maybe-print (dnvref members 0) t)
        (loop :for i :from 1 :below len
              :for member = (dnvref members i) :do
                (maybe-print member nil))
        t))))

(defun print-members-by-name (object stream member-names pop-callback)
  (declare (type dotnet-object object)
           (type stream stream)
           (type list member-names)
           (type (function () (values)) pop-callback))
  (let* ((type (bike-type-of object))
         (bflags #e(System.Reflection.BindingFlags Instance Public IgnoreCase))
         (all-member-types #e(System.Reflection.MemberTypes Property Field))
         has-any)
    (loop :for name = (car member-names) :do
      (when-let* ((members (type-get-members
                            type
                            (simple-character-string-upcase name)
                            all-member-types
                            bflags)))
        (setf has-any t)
        (print-member-vector object stream members pop-callback))
      (setf member-names (rest member-names))
      (when (endp member-names) (return has-any))
      (format stream " ~_"))))

(defun print-dotnet-object-full (object stream)
  (declare (type dotnet-object object)
           (type stream stream))
  (if *print-dotnet-object*
    (let* ((type (bike-type-of object))
           (qualified-name (or (type-assembly-qualified-name type)
                               (type-full-name type))))
      (if (member qualified-name *printed-types* :test #'string=)
        (print-dotnet-object-simple object stream)
        (let* ((*printed-types* (cons qualified-name *printed-types*))
               (bflags #e(System.Reflection.BindingFlags Public Instance))
               (fields (%get-type-fields type bflags))
               (props (%get-type-properties type bflags))
               (field-count (%array-length fields))
               (prop-count (%array-length props))
               (has-members (or (plusp field-count)
                                (plusp prop-count))))
          (pprint-dotnet-object (object stream :type t :identity (not has-members)
                                               :suffix (and (not has-members) " "))
            (when has-members
              (pprint-logical-block (stream nil)
                (flet ((pop-callback () (pprint-pop)))
                  (when (and (print-member-vector object stream fields #'pop-callback)
                             (plusp prop-count))
                    (format stream " ~_"))
                  (print-member-vector object stream props #'pop-callback))))))))
    (print-dotnet-object-simple object stream t)))

(defun print-enum (object stream)
  (declare (type dotnet-object object)
           (type stream stream))
  (if (eq (get-dispatch-macro-character #\# #\e) 'read-sharp-e)
    (let* ((value (unbox object))
           (type (bike-type-of object))
           (flags (member-custom-attrubute type (resolve-type 'System.FlagsAttribute))))
      (pprint-logical-block (stream nil :prefix "#e(" :suffix ")")
        (write-string (normalized-type-name (bike-type-of object)) stream)
        (if flags
          (loop :with names = (invoke type 'GetEnumNames)
                :with values = (invoke type 'GetEnumValues)
                :with len = (%array-length names)
                :with remaining = value
                :with default-name = nil
                :with has-any = nil
                :for i :below len
                :for k = (dnvref names i)
                :for v = (unbox (dnvref values i)) :do
                  (if (zerop v)
                    (setf default-name k)
                    (when (= v (logand v value))
                      (pprint-pop)
                      (setf remaining (logandc2 remaining v)
                            has-any t)
                      (format stream "~_ ")
                      (write-string k stream)))
                :finally
                   (if (zerop remaining)
                     (unless has-any
                       (pprint-pop)
                       (format stream "~_ ~:[~w~;~:*~a~]" default-name 0))
                     (progn (pprint-pop)
                            (format stream " ~w" remaining))))
          (let ((name (invoke type 'GetEnumName value)))
            (write-char #\Space stream)
            (if name
              (write-string name stream)
              (write value :stream stream))))))
    (print-dotnet-object-simple object stream)))

(defun print-array (object stream)
  (if *print-array*
    (%print-enumerable object stream)
    (print-dotnet-object-simple object stream t)))

(defun print-string (object stream)
  (declare (type dotnet-object object)
           (type stream stream))
  (let ((str (unbox object)))
    (if *print-escape*
      (pprint-dotnet-object (object stream :type t)
        (write str :stream stream))
      (write-string str stream))))

(defun print-intptr (object stream)
  (declare (type dotnet-object object)
           (type stream stream))
  (flet ((body (stream)
           (format stream "#x~8,'0X" [object ToInt64])))
    (if *print-escape*
      (pprint-dotnet-object (object stream :type t)
        (body stream))
      (body stream))))

(defun print-runtime-method-handle (object stream)
  (declare (type dotnet-object object)
           (type stream stream))
  (flet ((body (stream)
           (format stream "#x~8,'0X" (pointer-address [object %Value]))))
    (if *print-escape*
      (pprint-dotnet-object (object stream :type t)
        (body stream))
      (pprint-logical-block (stream nil)
        (body stream)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-printer-before/after-macrolet (name var-name)
    (with-gensyms (body next-member)
      `(,name (&body ,body)
              `(setf ,',var-name
                     (lambda (,',next-member)
                       (flet ((next-member () (funcall ,',next-member)))
                         ,@,body)))))))

(defmacro define-dotnet-printer-for-names (name (object stream)
                                           (&rest member-names)
                                           &body body)
  (with-gensyms (pop-callback before-callback after-callback body-printer)
    `(defun ,name (,object ,stream)
       (declare (type dotnet-object ,object)
                (type stream ,stream))
       (let (,before-callback ,after-callback)
         (declare (type (or null (function () (values)))
                        ,before-callback ,after-callback))
         (macrolet (,(make-printer-before/after-macrolet 'before-members before-callback)
                    ,(make-printer-before/after-macrolet 'after-members after-callback))
           (flet ((,body-printer (,stream) ,@body))
             (if (not *print-escape*)
               (pprint-logical-block (,stream nil)
                 (,body-printer ,stream))
               (pprint-dotnet-object (,object ,stream :type t)
                 (,body-printer ,stream)
                 (pprint-logical-block (,stream nil)
                   (flet ((,pop-callback () (pprint-pop)))
                     (when ,before-callback (funcall ,before-callback #',pop-callback))
                     (print-members-by-name ,object ,stream ',member-names #',pop-callback)
                     (when ,after-callback (funcall ,after-callback #',pop-callback)))))))
           ,object)))))

(define-dotnet-printer-for-names print-assembly (asm stream)
    (Location IsDynamic IsCollectible)
  (format stream "~a~_ " [asm %FullName]))

(define-dotnet-printer-for-names print-module (m stream)
    (FullyQualifiedName)
  (format stream "~a~_ " [m %Name]))

(defun write-custom-attributes (member-info stream)
  (declare (type dotnet-object member-info)
           (type stream stream))
  (exception-case
      (let ((attrs [member-info GetCustomAttributes t]))
        (when (plusp (%array-length attrs))
          (format stream " ~_CustomAttributes: ~w" attrs)))
    (System.Exception (e)
      (format stream " ~_CustomAttributes: #<!Caught ~a>"
              (normalized-type-name (bike-type-of e)))))
  member-info)

(defun write-member-attributes (attrs stream pop-callback)
  (declare (type stream stream)
           (type (function () (values)) pop-callback))
  (unless (zerop (unbox attrs))
    (format stream " ~_Attributes: ~w" attrs)
    (funcall pop-callback)))

(define-dotnet-printer-for-names print-field-info (info stream)
    (DeclaringType ReflectedType)
  (let ((constp (field-const-p info)))
    (write-string
     (cond ((field-public-p info) "public ")
           ((field-private-p info) "private ")
           ((field-internal-p info) "internal ")
           ((field-protected-p info) "protected ")
           ((field-protected-or-internal-p info) "protected internal ")
           (t ""))
     stream)
    (write-string
     (cond (constp "const ")
           ((field-static-p info) "static ")
           (t ""))
     stream)
    (when (field-readonly-p info)
      (write-string "readonly " stream))
    (write-type-name (field-type info) :stream stream)
    (format stream " ~a~:[~; = ~w~];"
            (member-info-name info)
            constp
            (and constp (field-raw-constant-value info)))
    (when *print-escape*
      (format stream "~_ ")))
  (after-members
   (write-member-attributes
    (enum 'System.Reflection.FieldAttributes
          (logandc2 (unbox (field-attributes info)) #x7))
    stream #'next-member)
   (write-custom-attributes info stream)))

(defun %print-parameter-info (info stream)
  (declare (type dotnet-object info)
           (type stream stream))
  (let* ((in (parameter-in-p info))
         (out (parameter-out-p info))
         (name (parameter-name info))
         (optional (parameter-optional-p info))
         (has-default-value (parameter-default-value-p info))
         (type (parameter-type info))
         (default-value (and has-default-value
                             (parameter-default-value info)))
         (genericp (type-generic-parameter-p type)))
    (when (and name
               (or (zerop (length name))
                   (every (lambda (c)
                            (case c
                              ((#\Space #\Tab #\Return #\Newline #\Page) t)))
                          name)))
      (setf name nil))
    (write-string
     (cond (in "in")
           (out "out ")
           ((ref-type-p type) "ref ")
           (t ""))
     stream)
    (write-type-name type :stream stream
                          :ref nil
                          :namespaces (if genericp nil *print-dotnet-type-namespaces*)
                          :qualified (if genericp nil *print-dotnet-type-qualified*))
    (format stream "~:[~;~:* ~a~]~:[~; =~_ ~w~]"
            name
            (and name has-default-value optional)
            default-value)))

(defun write-parameter-list (params stream)
  (declare (type list params)
           (type stream stream))
  (when params
    (pprint-logical-block (stream params)
      (%print-parameter-info (pprint-pop) stream)
      (loop :for p = (pprint-pop) :while p :do
        (format stream ", ~_")
        (%print-parameter-info p stream))))
  params)

(defun write-method-access-qualifiers (info stream)
  (declare (type dotnet-object info)
           (type stream stream))
  (write-string
   (cond ((method-public-p info) "public ")
         ((method-private-p info) "private ")
         ((method-protected-p info) "protected ")
         ((method-internal-p info) "internal ")
         ((method-protected-or-internal-p info) "protected internal ")
         (t ""))
   stream))

(defun write-method-qualifiers (info stream)
  (declare (type dotnet-object info)
           (type stream stream))
  (write-method-access-qualifiers info stream)
  (write-string
   (cond ((method-static-p info) "static ")
         ((method-abstract-p info) "abstract ")
         ((method-sealed-p info)
          (if (method-overriden-p info)
            "sealed override "
            "sealed "))
         ((method-virtual-p info)
          (if (method-overriden-p info)
            "override "
            "virtual "))
         (t ""))
   stream)
  info)

(define-dotnet-printer-for-names print-property-info (info stream)
    (DeclaringType ReflectedType)
  (let* ((params (property-index-parameters info))
         (can-read (property-readable-p info))
         (get-method (and can-read (property-get-method info)))
         (can-write (property-writable-p info))
         (set-method (and can-write (property-set-method info)))
         (accessors (remove nil (list get-method set-method)))
         (abstractp (some #'method-abstract-p accessors))
         (virtualp (some #'method-virtual-p accessors))
         (sealedp (some #'method-sealed-p accessors))
         (overridep (and virtualp (some #'method-overriden-p accessors))))
    (flet ((write-name ()
             (write-string
              (cond ((property-static-p info) "static ")
                    (abstractp "abstract ")
                    (sealedp (if overridep "sealed override " "sealed "))
                    (virtualp (if overridep "override " "virtual "))
                    (t ""))
              stream)
             (write-type-name (property-type info) :stream stream)
             (format stream " ~a" (member-info-name info))
             (when params
               (write-char #\[ stream)
               (write-parameter-list params stream)
               (write-char #\] stream))
             (write-char #\Space stream)))
      (cond ((and get-method set-method)
             (let ((get-access (logand #x7 (unbox (method-attributes get-method))))
                   (set-access (logand #x7 (unbox (method-attributes set-method)))))
               (cond ((= get-access set-access)
                      (write-method-access-qualifiers get-method stream)
                      (write-name)
                      (write-string "{ get; set; }" stream))
                     ((> get-access set-access)
                      (write-method-access-qualifiers get-method stream)
                      (write-name)
                      (write-string "{ get; " stream)
                      (write-method-access-qualifiers set-method stream)
                      (write-string "set; }" stream))
                     (t (write-method-access-qualifiers set-method stream)
                        (write-name)
                        (write-string "{ " stream)
                        (write-method-access-qualifiers get-method stream)
                        (write-string "get; set; }")))))
            (get-method
             (write-method-access-qualifiers get-method stream)
             (write-name)
             (write-string "{ get; }" stream))
            (set-method
             (write-method-access-qualifiers set-method stream)
             (write-name)
             (write-string "{ set; }" stream))
            (t (write-string "{ }" stream)))
      (format stream ";")
      (when *print-escape*
        (format stream "~_ "))))
  (after-members
   (write-member-attributes (property-attributes info) stream #'next-member)
   (write-custom-attributes info stream)))

(define-dotnet-printer-for-names print-parameter-info (info stream)
    (Position)
  (%print-parameter-info info stream)
  (format stream ";")
  (when *print-escape*
    (format stream "~_ "))
  (after-members
   (write-member-attributes [info %Attributes] stream #'next-member)
   (write-custom-attributes info stream)))

(define-dotnet-printer-for-names print-method-info (info stream)
    (DeclaringType ReflectedType CallingConvention)
  (let ((params (method-parameters info)))
    (write-method-qualifiers info stream)
    (%print-parameter-info (method-return-parameter info) stream)
    (format stream "~:_")
    (pprint-logical-block (stream nil)
      (format stream " ~a" (member-info-name info))
      (let* ((generic-args (method-generic-arguments info))
             (generic-arg-len (%array-length generic-args)))
        (unless (zerop generic-arg-len)
          (write-char #\[ stream)
          (pprint-logical-block (stream nil)
            (pprint-pop)
            (write-type-name (dnvref generic-args 0) :stream stream)
            (loop :for i :from 1 :below generic-arg-len
                  :do (pprint-pop)
                      (format stream ",~_")
                      (write-type-name (dnvref generic-args i) :stream stream)))
          (write-char #\] stream)))
      (format stream "(")
      (write-parameter-list params stream)
      (format stream ");"))
    (when *print-escape*
      (format stream "~_ "))
    (after-members
     (write-member-attributes
      (enum 'System.Reflection.MethodAttributes
            (logandc2 (unbox (method-attributes info)) #x7))
      stream #'next-member)
     (write-custom-attributes info stream))))

(define-dotnet-printer-for-names print-constructor-info (info stream)
    (CallingConvention)
  (let ((params (method-parameters info))
        (type (member-info-declaring-type info)))
    (write-method-qualifiers info stream)
    (write-type-name type :stream stream)
    (format stream "(")
    (write-parameter-list params stream)
    (format stream ");")
    (when *print-escape*
      (format stream "~_ "))
    (after-members
     (write-member-attributes
      (enum 'System.Reflection.MethodAttributes
            (logandc2 (unbox (method-attributes info)) #x7))
      stream #'next-member)
     (write-custom-attributes info stream))))

(define-dotnet-printer-for-names print-event-info (info stream)
    (DeclaringType ReflectedType)
  (let* ((add-method (event-get-add-method info t))
         (handler-type (event-handler-type info)))
    (write-method-qualifiers add-method stream)
    (format stream "event ")
    (write-type-name handler-type :stream stream)
    (format stream " ~a;" (member-info-name info))
    (when *print-escape*
      (format stream "~_ ")))
  (after-members
   (write-member-attributes (event-attributes info) stream #'next-member)
   (write-custom-attributes info stream)))

(defun dotnet-object-printer (type)
  (declare (type dotnet-type-designator type))
  "Retrieves a print function for a .Net TYPE"
  (let* ((table *printer-dispatch*)
         (type (resolve-type type))
         (def-name (unqualified-type-definition-name type)))
    (values (gethash def-name table))))

(defun set-dotnet-object-printer (type printer)
  (declare (type dotnet-type-designator type)
           (type (or symbol function) printer))
  "Installs a print function designated by PRINTER for a .Net TYPE.

PRINTER function must accept two arguments - an OBJECT to print, and a STREAM to print to."
  (let* ((table *printer-dispatch*)
         (type (resolve-type type))
         (def-name (unqualified-type-definition-name type)))
    (if printer
      (setf (gethash def-name table) printer)
      (remhash def-name table))
    (values printer type)))

(defmacro define-dotnet-object-printer (type (object stream) &body body)
  "A macro version of SET-DOTNET-OBJECT-PRINTER.

Installs a print function designated for a .Net TYPE.

Function accepts two arguments - an OBJECT to print, and a STREAM to print to."
  (declare (type dotnet-type-designator type)
           (type symbol object stream))
  (let ((name (gensym (string '#:dotnet-object-printer-))))
    `(labels ((,name (,object ,stream) ,@body))
       (set-dotnet-object-printer ',type (function ,name))
       ',type)))

(defmethod print-object ((dotnet-error dotnet-error) stream)
  (let ((ex (dotnet-error-object dotnet-error)))
    (if (and (not *print-escape*) (not *print-readably*))
      (print-exception-verbose ex stream)
      (print-unreadable-object (dotnet-error stream :type t :identity t)
        (write-type-name (bike-type-of ex) :stream stream))))
  dotnet-error)

(defmethod print-object ((delegate dotnet-delegate) stream)
  (print-dotnet-object-simple delegate stream t))

(defmethod print-object ((dotnet-type dotnet-type) stream)
  (flet ((body (stream)
           (handler-case
               (write-type-name dotnet-type :stream stream)
             (type-name-unexpected-token-error ()
               ;; We currently do not handle backquotes inside
               ;;  type name identifiers that do not designate
               ;;  generic types and simply treat such names
               ;;  as names of generic types.
               ;;  Although rare, such type names are still possible.
               (write-string (type-full-name dotnet-type) stream)))))
    (if *print-escape*
      (pprint-dotnet-object (dotnet-type stream :type t)
        (body stream))
      (pprint-logical-block (stream nil)
        (body stream))))
  dotnet-type)

(defmethod print-object ((object dotnet-object) stream)
  (flet ((maybe-invoke-printer (type)
           (declare (type dotnet-type type))
           (let ((printer (dotnet-object-printer type)))
             (when printer
               (funcall printer object stream)
               t))))
    (loop :with initial-type = (bike-type-of object)
          :for type = initial-type :then (type-base-type type)
          :while type :do
            (when (maybe-invoke-printer type)
              (return))
          :finally (loop :with interfaces = (type-interfaces initial-type)
                         ;; Seems like type interfaces are already sorted
                         ;;  by precedence
                         :for i :below (%array-length interfaces)
                         :for interface = (dnvref interfaces i) :do
                           (when (maybe-invoke-printer interface)
                             (return))
                         :finally
                            (cond
                              ((type-primitive-p initial-type)
                               (print-dotnet-object-simple object stream))
                              (t (print-dotnet-object-full object stream))))))
  object)

(flet ((ensure-printer (type printer)
         (unless (dotnet-object-printer type)
           (set-dotnet-object-printer type printer))))
  (macrolet ((frob (&body defs)
               (loop :for (type fn) :on defs by 'cddr
                     :collect `(ensure-printer ',type ',fn) :into calls
                     :finally (return `(progn ,@calls)))))
    (frob System.String print-string
          System.IntPtr print-intptr
          System.UintPtr print-intptr
          System.Numerics.BigInteger print-dotnet-object-simple
          System.Decimal print-dotnet-object-simple
          System.Guid print-dotnet-object-simple
          System.Array print-array
          System.Enum print-enum
          System.Version print-dotnet-object-simple
          System.DateTime print-dotnet-object-simple
          System.DateTimeOffset print-dotnet-object-simple
          System.TimeSpan print-dotnet-object-simple
          System.RuntimeMethodHandle print-runtime-method-handle
          System.Collections.IEnumerable print-enumerable
          System.Reflection.Assembly print-assembly
          System.Reflection.Module print-module
          System.Reflection.FieldInfo print-field-info
          System.Reflection.PropertyInfo print-property-info
          System.Reflection.ParameterInfo print-parameter-info
          System.Reflection.MethodInfo print-method-info
          System.Reflection.EventInfo print-event-info
          System.Reflection.ConstructorInfo print-constructor-info
          System.Reflection.Emit.ConstructorBuilder print-dotnet-object-simple
          System.Reflection.Emit.MethodBuilder print-dotnet-object-simple
          System.Reflection.Emit.FieldBuilder print-dotnet-object-simple
          System.Reflection.Emit.PropertyBuilder print-dotnet-object-simple
          System.Reflection.Emit.EventBuilder print-dotnet-object-simple)))

;;; vim: ft=lisp et
