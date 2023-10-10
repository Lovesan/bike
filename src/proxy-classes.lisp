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

(defclass dotnet-proxy-class (dotnet-proxy-object standard-class)
  ((%value :accessor dpc-object-type
           :reader dotnet-proxy-class-object-type)))

(defclass dotnet-callable-class (dotnet-proxy-class)
  ((%value :accessor dcc-proxy-type
           :reader dotnet-callable-class-proxy-type)
   (direct-base-type :initarg :base-type
                     :accessor dcc-direct-base-type)
   (direct-interfaces :initarg :interfaces
                      :accessor dcc-direct-interfaces)
   (%name-cache :accessor dcc-name-cache
                :type hash-table)
   (%function-name-cache :accessor dcc-function-name-cache
                         :type hash-table)
   (%name-cache-lock :accessor dcc-name-cache-lock
                     :type rwlock))
  (:documentation "Base metaclass for dotnet callable classes")
  (:default-initargs :interfaces '()
                     :base-type nil))

(defclass dotnet-slot-definition (slot-definition)
  ((dotnet-name :initarg :dotnet-name
                :type simple-character-string
                :accessor slotd-dotnet-name
                :reader slot-definition-dotnet-name)
   (kind :initarg :kind
         :type (member :event :property :method)
         :accessor slotd-kind
         :reader slot-definition-kind)))

(defclass property-slot-definition (dotnet-slot-definition)
  ((property-type :initarg :property-type
                  :type dotnet-type-designator
                  :accessor slotd-property-type
                  :reader slot-definition-property-type)
   (getter :initarg :getter
           :accessor slotd-getter
           :reader slot-definition-getter)
   (setter :initarg :setter
           :accessor slotd-setter
           :reader slot-definition-setter)))

(defclass event-slot-definition (dotnet-slot-definition)
  ((handler-type :initarg :handler-type
                 :type dotnet-type-designator
                 :accessor slotd-handler-type
                 :reader slot-definition-handler-type)))

(defclass callable-event-slot-definition (event-slot-definition)
  ((raise-method-dotnet-name
    :initarg :raise-method-dotnet-name
    :initform nil
    :accessor slotd-raise-method-dotnet-name
    :type (or null simple-character-string)
    :reader slot-definition-raise-method-dotnet-name)))

(defclass method-slot-definition (dotnet-slot-definition)
  ((return-type :initarg :return-type
                :accessor slotd-return-type
                :reader slot-definition-return-type)
   (parameters :initarg :parameters
               :accessor slotd-parameters
               :reader slot-definition-parameters)
   (params-array-parameter :accessor slotd-params-array-parameter
                           :initform nil
                           :reader slot-definition-rest-parameter)
   (generic-parameters :initarg :generic-parameters
                       :accessor slotd-generic-parameters
                       :reader slot-definition-generic-parameters)
   (function-name :initarg :function-name
                  :accessor slotd-function-name
                  :type (or (and symbol (not null))
                            (cons (eql setf) (cons symbol null)))
                  :reader slot-definition-function-name)))

(defclass direct-dotnet-slot-definition (dotnet-slot-definition
                                         standard-direct-slot-definition)
  ())

(defclass direct-property-slot-definition (property-slot-definition
                                           direct-dotnet-slot-definition)
  ()
  (:default-initargs
   :property-type (required-slot :name :property-type
                                 :message "Please supply property type.")
   :getter t
   :setter t))

(defclass direct-event-slot-definition (event-slot-definition
                                        direct-dotnet-slot-definition)
  ()
  (:default-initargs
   :handler-type (required-slot :name :handler-type
                                :message "Please supply event handler type.")))

(defclass direct-callable-event-slot-definition (callable-event-slot-definition
                                                 direct-event-slot-definition)
  ())

(defclass direct-method-slot-definition (method-slot-definition
                                         direct-dotnet-slot-definition)
  ()
  (:default-initargs
   :return-type (required-slot :name :return-type
                               :message "Please supply method return type.")
   :parameters '()
   :generic-parameters '()))

(defclass effective-dotnet-slot-definition (dotnet-slot-definition
                                            standard-effective-slot-definition)
  ())

(defclass effective-property-slot-definition (property-slot-definition
                                              effective-dotnet-slot-definition)
  ())

(defclass effective-event-slot-definition (event-slot-definition
                                           effective-dotnet-slot-definition)
  ())

(defclass effective-callable-event-slot-definition (callable-event-slot-definition
                                                    effective-event-slot-definition)
  ())

(defclass effective-method-slot-definition (method-slot-definition
                                            effective-dotnet-slot-definition)
  ())

(defmethod validate-superclass ((class dotnet-callable-class) (superclass standard-class))
  t)

(defmethod initialize-instance :after
    ((slotd direct-dotnet-slot-definition)
     &rest initargs &key &allow-other-keys)
  (destructuring-bind (&key (dotnet-name nil dotnet-name-p) kind &allow-other-keys)
      (fix-slot-initargs initargs)
    (setf (slotd-dotnet-name slotd)
          (if dotnet-name-p
            (simple-character-string dotnet-name)
            (camel-case-string (slot-definition-name slotd)
                               :capitalize (member kind '(:event :property :method)))))))

(defmethod initialize-instance :after
    ((slotd direct-property-slot-definition)
     &rest initargs &key &allow-other-keys)
  (destructuring-bind (&key property-type &allow-other-keys)
      (fix-slot-initargs initargs)
    (let ((type (resolve-type property-type)))
      (when (ref-type-p type)
        (error 'invalid-ref-type :datum property-type))
      (setf (slotd-property-type slotd) (normalize-typespec type)))))

(defmethod initialize-instance :after
    ((slotd direct-event-slot-definition)
     &rest initargs &key &allow-other-keys)
  (destructuring-bind (&key handler-type &allow-other-keys)
      (fix-slot-initargs initargs)
    (let ((type (resolve-type handler-type)))
      (when (ref-type-p type)
        (error 'invalid-ref-type :datum handler-type))
      (unless (delegate-type-p type)
        (error 'delegate-type-expected :datum type))
      (setf (slotd-handler-type slotd) (normalize-typespec type)))))

(defmethod initialize-instance :after
    ((slotd direct-callable-event-slot-definition)
     &rest initargs &key &allow-other-keys)
  (destructuring-bind
      (&key (raise-method-dotnet-name nil raise-method-dotnet-name-p)
       &allow-other-keys)
      (fix-slot-initargs initargs)
    (setf (slotd-raise-method-dotnet-name slotd)
          (if raise-method-dotnet-name-p
            (and raise-method-dotnet-name
                 (simple-character-string raise-method-dotnet-name))
            (simple-character-string
             (strcat "raise_" (slotd-dotnet-name slotd)))))))

(defun parse-method-parameter-definition (def restp generic-parameters)
  (declare (type list generic-parameters))
  (destructuring-bind (name type &key (dotnet-name nil dotnet-name-p)
                                      (direction :in dirp)
                                      (ref nil refp))
      def
    (check-type name (and symbol (not null)))
    (setf dotnet-name (if dotnet-name-p
                        (simple-character-string dotnet-name)
                        (camel-case-string name)))
    (setf type (normalize-typespec type generic-parameters))
    (if restp
      (unless (and (consp type)
                   (eq :array (first type))
                   (integerp (third type))
                   (eq direction :in)
                   (not ref))
        (error 'invalid-params-array-definition :datum def))
      (progn
        (check-type direction (member :in :out :io))
        (cond (refp
               (if ref
                 (unless dirp (setf direction :io))
                 (when dirp
                   (assert (eq direction :in) (ref direction)
                           'parameter-direction-mismatch :datum def))))
              (dirp (setf ref (case direction ((:out :io) t))))
              (t (when (and (consp type) (eq (car type) :ref))
                   (setf direction :io))))
        (when (and (consp type) (eq (car type) :ref))
          (setf ref t
                type (second type)))))
    `(,name ,type :dotnet-name ,dotnet-name
                  :direction ,direction
                  :ref ,ref)))

(defun check-duplicate-method-parameter-names (parameters parsed)
  (declare (type list parameters parsed))
  (let (names dotnet-names)
    (dolist (definition parsed)
      (destructuring-bind (name type &key dotnet-name &allow-other-keys)
          definition
        (declare (ignore type))
        (when (find name names)
          (duplicate-parameter-name name parameters))
        (when (find dotnet-name dotnet-names :test #'string=)
          (duplicate-parameter-name dotnet-name parameters))
        (push name names)
        (push dotnet-name dotnet-names)))))

(defun parse-method-lambda-list (parameters generic-parameters)
  (declare (type list parameters generic-parameters))
  (loop :with rest = nil
        :for c :on parameters
        :for p = (car c)
        :if (eq p '&rest)
          :collect (let ((p (second c)))
                     (when (or (null p)
                               (endp (cdr c))
                               (cddr c))
                       (error 'invalid-params-array-definition
                              :datum parameters))
                     (setf c (cddr c))
                     (setf rest (parse-method-parameter-definition
                                 p t generic-parameters))
                     rest)
            :into parsed
        :else
          :collect (parse-method-parameter-definition
                    p nil generic-parameters)
            :into parsed
        :finally
           (check-duplicate-method-parameter-names parameters parsed)
           (return (values parsed rest))))

(defun validate-generic-parameters (generic-parameters)
  (declare (type list generic-parameters))
  (let ((names (loop :for p :in generic-parameters
                     :collect
                     (destructuring-bind (name &rest constraints)
                         (ensure-list p)
                       (declare (ignore constraints))
                       (check-type name symbol)
                       (when (find name names)
                         (duplicate-parameter-name name generic-parameters))
                       name)
                       :into names
                     :finally (return names))))
    (loop :for param :in generic-parameters
          :collect
          (destructuring-bind (name &rest constraints)
              (ensure-list param)
            (flet ((error-one-of (a b)
                     (invalid-generic-constraint
                      constraints "Only one of: ~s or ~s should be specified" a b))
                   (error-duplicate (x)
                     (invalid-generic-constraint
                      constraints "Duplicate ~s constraint" x))
                   (error-unknown (x)
                     (invalid-generic-constraint
                      constraints "Unknown constraint: ~s" x))
                   (error-no-arg (x)
                     (invalid-generic-constraint
                      constraints "Constraint ~s requires an argument" x))
                   (error-single-arg (x)
                     (invalid-generic-constraint
                      constraints "Constraint ~s requires only one argument" x)))
              (loop :with in = nil
                    :with out = nil
                    :with class = nil
                    :with struct = nil
                    :with base-type = nil
                    :with interfaces = nil
                    :with new = nil
                    :for c :in constraints
                    :collect
                    (typecase c
                      ((eql :in)
                       (when in (error-duplicate :in))
                       (when out (error-one-of :in :out))
                       (setf in t)
                       :in)
                      ((eql :out)
                       (when out (error-duplicate :out))
                       (when in (error-one-of :in :out))
                       (setf out t)
                       :out)
                      ((eql :new)
                       (when new (error-duplicate :new))
                       (setf new t)
                       :new)
                      ((eql :struct)
                       (when struct (error-duplicate :struct))
                       (when class (error-one-of :struct :class))
                       (setf struct t)
                       :struct)
                      ((eql :class)
                       (when class (error-duplicate :class))
                       (when struct (error-one-of :struct :class))
                       (setf class t)
                       :class)
                      ((eql :base-type)
                       (error-no-arg :base-type))
                      ((eql :interfaces)
                       (error-no-arg :interfaces))
                      (cons (destructuring-bind (constraint-name value &rest values) c
                              (case constraint-name
                                (:base-type
                                 (unless (null values)
                                   (error-single-arg :base-type))
                                 (when base-type
                                   (error-duplicate :base-type))
                                 (prog1 (list :base-type (normalize-typespec value names))
                                   (setf base-type t)))
                                (:interfaces
                                 (prog1 (list* :interfaces
                                               (normalize-typespec value names)
                                               (mapcar (lambda (v)
                                                         (normalize-typespec v names))
                                                       values))
                                   (setf interfaces t)))
                                (t (error-unknown c)))))
                      (t (error-unknown c)))
                      :into validated
                    :finally (setf constraints validated))
              (cons name constraints))))))

(defmethod initialize-instance :after
    ((slotd direct-method-slot-definition)
     &rest initargs &key &allow-other-keys)
  (destructuring-bind (&key name
                            return-type
                            parameters
                            generic-parameters
                            (function-name name)
                       &allow-other-keys)
      (fix-slot-initargs initargs)
    (check-type function-name (and symbol (not null)))
    (let* ((generic-parameters (validate-generic-parameters generic-parameters))
           (gp-names (mapcar #'car generic-parameters))
           (return-type (normalize-typespec return-type gp-names)))
      (multiple-value-bind (parameters params-array-parameter)
          (parse-method-lambda-list parameters gp-names)
        (setf (slotd-return-type slotd) return-type
              (slotd-generic-parameters slotd) generic-parameters
              (slotd-parameters slotd) parameters
              (slotd-params-array-parameter slotd) params-array-parameter
              (slotd-function-name slotd) function-name)))))

(defmethod initialize-instance :around
    ((slotd direct-event-slot-definition)
     &rest initargs &key &allow-other-keys)
  (destructuring-bind (&rest initargs &key (allocation :instance) &allow-other-keys)
      (fix-slot-initargs initargs)
    (declare (ignore allocation))
    (remf initargs :allocation)
    (apply #'call-next-method slotd :allocation :dotnet initargs)))

(defmethod initialize-instance :around
    ((slotd direct-method-slot-definition)
     &rest initargs &key &allow-other-keys)
  (destructuring-bind (&rest initargs &key (allocation :instance) &allow-other-keys)
      (fix-slot-initargs initargs)
    (declare (ignore allocation))
    (remf initargs :allocation)
    (apply #'call-next-method slotd :allocation :dotnet initargs)))

(defmethod direct-slot-definition-class ((class dotnet-callable-class)
                                         &rest initargs &key &allow-other-keys)
  (destructuring-bind (&key kind (allocation :instance) &allow-other-keys)
      (fix-slot-initargs initargs)
    (assert (eq allocation :instance) (allocation)
            "~s only supports :instance slot :allocation(was: ~s)" class allocation)
    (case kind
      (:property (find-class 'direct-property-slot-definition))
      (:event (find-class 'direct-callable-event-slot-definition))
      (:method (find-class 'direct-method-slot-definition))
      (t (call-next-method)))))

(defgeneric compute-direct-slot-definitions (class slot-name)
  (:documentation "Computes ordered list of direct slot definitons for SLOT-NAME")
  (:method ((class standard-class) slot-name)
    (loop :for class :in (class-precedence-list class)
          :for dslotd = (find slot-name (class-direct-slots class)
                              :key #'slot-definition-name)
          :when dslotd :collect dslotd)))

(defmethod effective-slot-definition-class ((class dotnet-callable-class)
                                            &rest initargs &key &allow-other-keys)
  (destructuring-bind (&key name &allow-other-keys)
      (fix-slot-initargs initargs)
    (let ((direct-slotd (first (compute-direct-slot-definitions class name))))
      (typecase direct-slotd
        (direct-property-slot-definition
         (find-class 'effective-property-slot-definition))
        (direct-callable-event-slot-definition
         (find-class 'effective-callable-event-slot-definition))
        (direct-method-slot-definition
         (find-class 'effective-method-slot-definition))
        (t (call-next-method))))))

(defgeneric initialize-effective-slot-definition (effective-slotd direct-slotd)
  (:documentation "Initializes effective slot definition from direct slot definition.")
  (:method (effective-slotd direct-slotd)))

(defmethod initialize-effective-slot-definition :after
    ((slotd effective-dotnet-slot-definition)
     (direct-slotd direct-dotnet-slot-definition))
  (setf (slotd-dotnet-name slotd) (slotd-dotnet-name direct-slotd)
        (slotd-kind slotd) (slotd-kind direct-slotd)))

(defmethod initialize-effective-slot-definition :after
    ((slotd effective-property-slot-definition)
     (direct-slotd direct-property-slot-definition))
  (setf (slotd-property-type slotd)
        (resolve-type (slotd-property-type direct-slotd))
        (slotd-getter slotd) (slotd-getter direct-slotd)
        (slotd-setter slotd) (slotd-setter direct-slotd)))

(defmethod initialize-effective-slot-definition :after
    ((slotd effective-event-slot-definition)
     (direct-slotd direct-event-slot-definition))
  (let ((handler-type (resolve-type (slotd-handler-type direct-slotd))))
    (setf (slotd-handler-type slotd) handler-type)))

(defmethod initialize-effective-slot-definition :after
    ((slotd effective-callable-event-slot-definition)
     (direct-slotd direct-callable-event-slot-definition))
  (setf (slotd-raise-method-dotnet-name slotd)
        (slotd-raise-method-dotnet-name direct-slotd)))

(defmethod initialize-effective-slot-definition :after
    ((slotd effective-method-slot-definition)
     (direct-slotd direct-method-slot-definition))
  (setf (slotd-return-type slotd) (slotd-return-type direct-slotd)
        (slotd-function-name slotd) (slotd-function-name direct-slotd))
  (loop :for (name . constraints) :in (slotd-generic-parameters direct-slotd)
        :collect (make-gpi :name name :constraints constraints)
          :into generic-params
        :finally (setf (slotd-generic-parameters slotd) generic-params))
  (loop :with params-array = (slotd-params-array-parameter direct-slotd)
        :for param :in (slotd-parameters direct-slotd)
        :for i :from 0
        :collect (destructuring-bind (name type &key dotnet-name
                                                     direction
                                                     ref)
                     param
                   (let ((parsed (make-mpi :name name
                                           :dotnet-name dotnet-name
                                           :type type
                                           :position i
                                           :direction direction
                                           :ref-p ref)))
                     (when (eq param params-array)
                       (setf params-array parsed))
                     parsed))
          :into params
        :finally (setf (slotd-params-array-parameter slotd) params-array
                       (slotd-parameters slotd) params)))

(defmethod compute-effective-slot-definition ((class dotnet-callable-class) name dslotds)
  (let ((slotd (call-next-method)))
    (initialize-effective-slot-definition slotd (first dslotds))
    slotd))

(defun check-duplicate-dotnet-slot-names (class eslotds)
  (declare (type dotnet-proxy-class class))
  (let ((eslotds (remove-if (lambda (c)
                              (not (typep c 'effective-dotnet-slot-definition)))
                            eslotds)))
    (dolist (slotd eslotds)
      (loop :for s :in eslotds
            :when (and (not (eq s slotd))
                       (string= (slotd-dotnet-name s)
                                (slotd-dotnet-name slotd)))
              :do (error 'duplicate-dotnet-name
                         :class class
                         :value (slotd-dotnet-name slotd))))))

(defun find-callable-class-base-type (class)
  (declare (type dotnet-callable-class class))
  (let ((direct-base-type (dcc-direct-base-type class)))
    (if direct-base-type
      (values (resolve-type direct-base-type) nil)
      (let ((base-class (find-if (lambda (c)
                                   (and (not (eq c class))
                                        (typep c 'dotnet-callable-class)))
                                 (class-precedence-list class))))
        (if base-class
          (progn (ensure-finalized base-class)
                 (values (dcc-proxy-type base-class) t))
          (values (resolve-type :object) nil))))))

(defun find-base-type-constructors (base-type)
  (declare (type dotnet-type base-type))
  (let ((candidates (type-constructors* base-type #e(System.Reflection.BindingFlags
                                                     Public
                                                     NonPublic
                                                     Instance)))
        (result '()))
    (do-bike-vector (ci candidates)
      (unless (method-private-p ci)
        (push ci result)))
    result))

(defgeneric initialize-class-proxy (class)
  (:documentation "Initializes class proxy"))

(defmethod initialize-class-proxy ((class dotnet-callable-class))
  (multiple-value-bind (base-type callable-base-type-p)
      (find-callable-class-base-type class)
    (when (type-sealed-p base-type)
      (error 'sealed-inheritance :type base-type))
    (let ((base-constructors (find-base-type-constructors base-type)))
      (unless base-constructors
        (error 'constructor-resolution-error :type base-type
                                             :args '(*)))
      (let* ((eslotds (class-slots class))
             (direct-interfaces (mapcar #'resolve-type (dcc-direct-interfaces class)))
             (interfaces (delete-duplicates
                          (cons
                           (dtc-proxy-interface-type -dynamic-type-cache-)
                           (append direct-interfaces
                                   (bike-vector-to-list
                                    (type-interfaces base-type))))
                          :test #'bike-equals))
             (class-name (class-name class))
             (name (simple-character-string
                    (or (and class-name (symbol-full-name class-name))
                        "DynamicType")))
             (state (make-type-builder-state name base-type interfaces)))
        (dolist (c base-constructors)
          (tbs-add-constructor state c callable-base-type-p))
        (dolist (slotd eslotds)
          (typecase slotd
            (effective-event-slot-definition
             (tbs-add-event state
                            (slotd-dotnet-name slotd)
                            (resolve-type (slotd-handler-type slotd))
                            (slotd-raise-method-dotnet-name slotd)))
            (effective-property-slot-definition
             (tbs-add-property state
                               (slotd-dotnet-name slotd)
                               (slotd-property-type slotd)
                               (slotd-getter slotd)
                               (slotd-setter slotd)))
            (effective-method-slot-definition
             (setf (slotd-return-type slotd)
                   (tbs-add-method state
                                   (slotd-dotnet-name slotd)
                                   (slotd-generic-parameters slotd)
                                   (slotd-return-type slotd)
                                   (slotd-parameters slotd)
                                   (slotd-params-array-parameter slotd))))))
        (setf (dcc-name-cache class) (make-hash-table :test #'equal)
              (dcc-function-name-cache class) (make-hash-table :test #'equal)
              (dcc-name-cache-lock class) (make-rwlock
                                           :name (format nil "~s name cache lock"
                                                         (class-name class)))
              (dcc-proxy-type class) (tbs-create-type state))))))

(defgeneric fix-class-initargs (object &rest initargs &key &allow-other-keys)
  (:method ((class class) &rest initargs &key &allow-other-keys)
    initargs)
  (:method ((class symbol) &rest initargs &key &allow-other-keys)
    (apply #'fix-class-initargs (find-class class) initargs)))

(defmethod fix-class-initargs ((class dotnet-callable-class)
                               &rest initargs
                               &key ((:direct-superclasses dscs) '())
                                    (base-type nil)
                                    (interfaces '())
                               &allow-other-keys)
  (when base-type
    (remf initargs :base-type)
    (let ((type (resolve-type base-type)))
      (setf initargs (list* :base-type (normalize-typespec type) initargs))))
  (when interfaces
    (remf initargs :interfaces)
    (let ((interfaces (mapcar (lambda (interface)
                                (let ((type (resolve-type interface)))
                                  (unless (interface-type-p type)
                                    (error 'interface-type-expected
                                           :datum type))
                                  (normalize-typespec type)))
                              interfaces)))
      (setf initargs (list* :interfaces interfaces initargs))))
  (remf initargs :direct-superclasses)
  (let ((dotnet-callable-object (find-class 'dotnet-callable-object)))
    (unless (some (lambda (c) (subclassp c dotnet-callable-object))
                  dscs)
      (setf dscs (append dscs (list dotnet-callable-object)))))
  (list* :direct-superclasses dscs initargs))

(defmethod initialize-instance :around ((class dotnet-callable-class)
                                        &rest initargs &key &allow-other-keys)
  (apply #'call-next-method class (apply #'fix-class-initargs class initargs)))

(defmethod reinitialize-instance :around ((class dotnet-callable-class)
                                          &rest initargs &key &allow-other-keys)
  (multiple-value-prog1
      (apply #'call-next-method class (apply #'fix-class-initargs class initargs))
    (make-instances-obsolete class)))

(defmethod compute-slots :around ((class dotnet-callable-class))
  (let ((eslotds (call-next-method)))
    (check-duplicate-dotnet-slot-names class eslotds)
    eslotds))

(defmethod finalize-inheritance :after ((class dotnet-callable-class))
  (initialize-class-proxy class)
  (with-accessors ((lock dtc-lock)
                   (classes dtc-classes))
      -dynamic-type-cache-
    (with-read-lock (lock)
      (or (find class classes :key #'tg:weak-pointer-value)
          (with-write-lock (lock)
            (or (find class classes :key #'tg:weak-pointer-value)
                (push (tg:make-weak-pointer class) classes)))))))

(defun get-slot-name-by-dotnet-name (class dotnet-name function-name-cache-p)
  (declare (type dotnet-callable-class class)
           (type simple-character-string dotnet-name))
  (check-type class dotnet-callable-class)
  (let ((cache (if function-name-cache-p
                 (dcc-function-name-cache class)
                 (dcc-name-cache class)))
        (lock (dcc-name-cache-lock class)))
    (with-read-lock (lock)
      (let ((slot-name (gethash dotnet-name cache)))
        (or slot-name
            (let ((slot-name
                    (dolist (slotd (class-slots class))
                      (when (typep slotd 'effective-dotnet-slot-definition)
                        (let ((name (slotd-dotnet-name slotd)))
                          (when (string= name dotnet-name)
                            (return
                              (if function-name-cache-p
                                (let* ((function-name (slotd-function-name slotd))
                                       (args (slotd-parameters slotd))
                                       (params-array
                                         (slotd-params-array-parameter slotd))
                                       (argc (if params-array
                                               (1- (length args))
                                               (length args))))
                                  (list* function-name
                                         (bike-equals
                                          [:void]
                                          (slotd-return-type slotd))
                                         argc
                                         args))
                                (slot-definition-name slotd)))))))))
              (when slot-name
                (with-write-lock (lock)
                  (setf (gethash dotnet-name cache) slot-name)))
              slot-name))))))

(defun get-dotnet-callable-object-slot-value (proxy object name)
  (declare (type (or null dotnet-callable-object) object)
           (type simple-character-string name))
  ;; Proxy has outlived its parent. This is an erroneous condition.
  (unless object
    (restart-case
        (error 'dotnet-callable-object-orphan-proxy
               :value proxy
               :operation 'slot-value
               :arguments (list name))
      (continue ()
        (return-from get-dotnet-callable-object-slot-value))))
  (let* ((class (class-of object))
         (slot-name (get-slot-name-by-dotnet-name class name nil)))
    (if slot-name
      (slot-value object slot-name)
      (slot-missing class object name 'slot-value))))

(defun set-dotnet-callable-object-slot-value (proxy object name new-value)
  (declare (type (or null dotnet-callable-object) object)
           (type simple-character-string name))
  ;; Proxy has outlived its parent. This is an erroneous condition.
  (unless object
    (restart-case
        (error 'dotnet-callable-object-orphan-proxy
               :value proxy
               :operation '(setf slot-value)
               :arguments (list name new-value))
      (continue () (return-from set-dotnet-callable-object-slot-value))))
  (let* ((class (class-of object))
         (slot-name (get-slot-name-by-dotnet-name class name nil)))
    (if slot-name
      (setf (slot-value object slot-name) new-value)
      (slot-missing class object name 'setf new-value))))

(defun invoke-dotnet-callable-object-method (proxy object name args)
  (declare (type (or null dotnet-callable-object) object)
           (type simple-character-string name)
           (type dotnet-object args))
  ;; Proxy has outlived its parent. This is an erroneous condition.
  (unless object
    (restart-case
        (error 'dotnet-callable-object-orphan-proxy
               :value proxy
               :operation 'invoke
               :arguments (list name args))
      (continue ()
        (return-from invoke-dotnet-callable-object-method))))
  (let ((class (class-of object)))
    (destructuring-bind (function-name voidp argc &rest parameters)
        (get-slot-name-by-dotnet-name class name t)
      (declare (type (or (and symbol (not null))
                         (cons (eql setf) (cons symbol null)))
                     function-name)
               (type (signed-byte 32) argc))
      (let* ((arglist (loop :for i :below (%array-length args)
                            :for plist = parameters :then (rest plist)
                            :for p = (car plist)
                            :if (and p (member (mpi-direction p) '(:in :io)))
                              :collect (dnvref args i)
                            :else
                              :if (null plist)
                                :collect (dnvref args i)))
             (result (multiple-value-list
                      (apply (fdefinition function-name) object arglist))))
        (loop :with return-values = (if voidp result (rest result))
              :for i :below argc
              :for p :in parameters :do
                (when (or (mpi-ref-p p)
                          (member (mpi-direction p) '(:out :io)))
                  (setf (dnvref args i) (car return-values)
                        return-values (cdr return-values))))
        (if voidp (values) (first result))))))

(defun initialize-dotnet-callable-object-proxy (object)
  (declare (type dotnet-callable-object object))
  (when-let* ((class (class-of object))
              (callable-class-p (typep class 'dotnet-callable-class))
              (type-proxy (dcc-proxy-type class))
              ;; TODO: Handle constructors with parameters
              (proxy (with-accessors ((getter dtc-getter-delegate)
                                      (setter dtc-setter-delegate)
                                      (invoke dtc-invoke-delegate))
                         -dynamic-type-cache-
                       (reflection-new
                        type-proxy
                        (box object)
                        getter
                        setter
                        invoke)))
              (proxy-handle (%dotnet-object-handle proxy)))
    (setf (dco-proxy object) proxy)
    (tg:cancel-finalization proxy)
    (tg:finalize object (lambda ()
                          (%free-handle proxy-handle)))
    proxy))

(defmethod shared-initialize ((object dotnet-callable-object)
                              slot-names
                              &rest initargs &key &allow-other-keys)
  (declare (ignore initargs slot-names))
  (call-next-method)
  (initialize-dotnet-callable-object-proxy object)
  object)

(defmethod slot-value-using-class ((class dotnet-proxy-class)
                                   (object dotnet-proxy-object)
                                   (slotd effective-callable-event-slot-definition))
  (let* ((proxy (dpo-value object))
         (proxy-type (dpc-object-type class))
         (event-name (slotd-dotnet-name slotd))
         (field-info (invoke proxy-type 'GetField
                             event-name
                             (enum 'System.Reflection.BindingFlags
                                   'NonPublic
                                   'Instance))))
    (when-let ((handler (invoke field-info 'GetValue proxy)))
      (labels ((raise-event (&rest handler-args)
                 (apply #'invoke handler 'Invoke handler-args)))
        #'raise-event))))

(defmethod (setf slot-value-using-class)
    (new-handler
     (class dotnet-proxy-class)
     (object dotnet-proxy-object)
     (slotd effective-callable-event-slot-definition))
  (check-type new-handler (or symbol function))
  (let* ((handler-type (slotd-handler-type slotd))
         (handler (and new-handler (new handler-type new-handler)))
         (event-name (slotd-dotnet-name slotd))
         (proxy (dpo-value object))
         (proxy-type (dpc-object-type class))
         (field-info (invoke proxy-type 'GetField
                             event-name
                             (enum 'System.Reflection.BindingFlags
                                   'NonPublic
                                   'Instance))))
    (invoke field-info 'SetValue proxy handler)
    new-handler))

(defmethod slot-boundp-using-class ((class dotnet-proxy-class)
                                    (object dotnet-proxy-object)
                                    (slotd effective-callable-event-slot-definition))
  (and (slot-boundp object '%value)
       (slot-value-using-class class object slotd)
       t))

(defmethod slot-makunbound-using-class ((class dotnet-proxy-class)
                                        (object dotnet-proxy-object)
                                        (slotd effective-callable-event-slot-definition))
  (when (slot-boundp object '%value)
    (setf (slot-value-using-class class object slotd) nil))
  object)

(defmethod slot-boundp-using-class ((class dotnet-proxy-class)
                                    (object dotnet-proxy-object)
                                    (slotd effective-method-slot-definition))
  (slot-boundp object '%value))


(defmethod slot-makunbound-using-class ((class dotnet-proxy-class)
                                        (object dotnet-proxy-object)
                                        (slotd effective-method-slot-definition))
  (error 'method-slot-makunbound-attempt
         :object object
         :slot-name (slot-definition-name slotd)))

(defmethod slot-value-using-class ((class dotnet-callable-class)
                                   (object dotnet-proxy-object)
                                   (slotd effective-method-slot-definition))
  (let ((function-name (slotd-function-name slotd)))
    (labels ((delegate (&rest method-arguments)
               (apply function-name object method-arguments)))
      #'delegate)))

(defmethod (setf slot-value-using-class) (new-value
                                          (class dotnet-proxy-class)
                                          (object dotnet-proxy-object)
                                          (slotd effective-method-slot-definition))
  (error 'method-slot-write-attempt
         :object object
         :slot-name (slot-definition-name slotd)
         :value new-value))

(defun initialize-dynamic-type-cache ()
  (let ((getter (new '(System.Func :object :object :string :object)
                     #'get-dotnet-callable-object-slot-value))
        (setter (new '(System.Action :object :object :string :object)
                     #'set-dotnet-callable-object-slot-value))
        (invoke (new '(System.Func :object :object :string (array :object) :object)
                     #'invoke-dotnet-callable-object-method)))
    (if (null -dynamic-type-cache-)
      (setf -dynamic-type-cache- (make-dynamic-type-cache getter setter invoke))
      (let ((classes (loop :for wp :in (dtc-classes -dynamic-type-cache-)
                           :for class = (tg:weak-pointer-value wp)
                           :when (and class (not (find class classes)))
                             :collect class :into classes
                           :finally (return classes))))
        (setf -dynamic-type-cache- (make-dynamic-type-cache getter setter invoke))
        (dolist (class (reverse classes))
          (let ((slots (class-slots class)))
            (dolist (eslotd (remove-if
                             (lambda (slotd)
                               (not (typep slotd 'effective-dotnet-slot-definition)))
                             slots))
              (initialize-effective-slot-definition
               eslotd
               (first (compute-direct-slot-definitions
                       class
                       (slot-definition-name eslotd))))))
          (initialize-class-proxy class)
          (make-instances-obsolete class))
        (setf (dtc-classes -dynamic-type-cache-) (mapcar #'tg:make-weak-pointer classes))
        -dynamic-type-cache-))))

(defun clear-dynamic-type-cache ()
  (let* ((dtc -dynamic-type-cache-))
    (with-accessors ((lock dtc-lock)
                     (assembly dtc-assembly)
                     (module dtc-module)
                     (classes dtc-classes)
                     (type-count dtc-type-count)
                     (getter-delegate dtc-getter-delegate)
                     (setter-delegate dtc-setter-delegate)
                     (invoke-delegate dtc-invoke-delegate))
        dtc
      (let* ((new-assembly (make-dynamic-type-cache-assembly))
             (new-module (make-dynamic-type-cache-module new-assembly))
             (new-getter (new '(System.Func :object :object :string :object)
                              #'get-dotnet-callable-object-slot-value))
             (new-setter (new '(System.Action :object :object :string :object)
                              #'set-dotnet-callable-object-slot-value))
             (new-invoke (new '(System.Func :object :object :string (array :object) :object)
                              #'invoke-dotnet-callable-object-method)))
        (with-write-lock (lock)
          (setf assembly new-assembly
                module new-module
                getter-delegate new-getter
                setter-delegate new-setter
                invoke-delegate new-invoke
                classes '()
                type-count 0)
          (values))))))

(defun dotnet-callable-proxy-object (proxy)
  (declare (type dotnet-object proxy))
  "Retrieves an object associated with the proxy"
  (let* ((type (bike-type-of proxy))
         (field (type-get-field type "_context" #e(System.Reflection.BindingFlags
                                                   NonPublic
                                                   Instance))))
    (when field
      [field GetValue proxy])))

(defun dotnet-callable-proxy-type-p (type)
  "Returns non-NIL in case if a TYPE is a proxy type for dotnet-callable-class"
  (declare (type dotnet-type type))
  (let ((proxy-if-type
          (with-read-lock ((dtc-lock -dynamic-type-cache-))
            (dtc-proxy-interface-type -dynamic-type-cache-))))
    (assignable-from-p proxy-if-type type)))

(defun dotnet-callable-proxy-p (object)
  "Return non-NIL in case of an OBJECT being a callable proxy of a dotnet-callable-object."
  (and (dotnet-object-p object)
       (dotnet-callable-proxy-type-p (bike-type-of object))))

(defun unwrap-dotnet-callable-proxy (object)
  "Returns an underlying object of a dotnet callable proxy in case an OBJECT is a proxy."
  (if (dotnet-callable-proxy-p object)
    (dotnet-callable-proxy-object object)
    object))

(register-image-restore-hook 'initialize-dynamic-type-cache (not -dynamic-type-cache-))

;;; vim: ft=lisp et
