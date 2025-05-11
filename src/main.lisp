(uiop:define-package logadat
  (:use #:cl))
(in-package #:logadat)

;; <program>   ::= <rule> <program> | ""
;; <rule>      ::= <atom> ":-" <atom-list> "."
;; <atom-list> ::= <atom> | <atom> "," <atom-list> | ""
;; <atom>      ::= <relation> "(" <term-list> ")"
;; <term-list> ::= <term> | <term> "," <term-list> | ""
;; <term>      ::= <constant> | <variable>

(defvar *db-rules* nil)

(defmacro symbolp-r (x &rest body)
  `(and (symbolp ,x)
        ,@body))

;; variable processing
(defun variable-p (x)
  "Is X a named variable (a symbol beginning with '?')?"
  (symbolp-r x (equal (char (symbol-name x) 0) #\?)))

;; (defun anonvar-p (x)
;;   "Is X an anonymous variable (an underscore symbol)?"
;;   (symbolp-r x (equal (char (symbol-name x) 0) #\_)))

(variable-p '?xa)
;; (anonvar-p '__)

;; constant processing
(defun unbound-is-string (x)
  (handler-case (eval x)
    (unbound-variable () (symbol-name x))))

(defun typep-rs (x types)
  (if (member t (mapcar #'(lambda (ty) (typep x ty)) types))
      t))

(defun typep-r (x types)
  "Is X a type of an element of the specified list?"
  (let ((v (unbound-is-string x)))
    (typep-rs v types)))

(defun constant-p (x)
  "Is X a constant (a number, string, or boolean)?"
  (and (not (variable-p x))
       ;; (not (anonvar-p x))
       (typep-r x '(number string boolean))))

(constant-p 'nil)

;; (and (symbolp '1)
;;      (not (variable-p '1))
;;      (not (anonvar-p '1))
;;      (typep-r '1 '(number string boolean)))

;; predicate processing
(defun predicate-p (x)
  "Is X a predicate?"
  (symbolp-r x (alpha-char-p (char (symbol-name x) 0))))

(predicate-p 'a)

;; atom processing

;; database
(defvar *sample-db* nil)

(defclass table ()
  ((rows :accessor rows :initarg :rows :initform nil)
   (schema :accessor schema :initarg :schema)))

(defclass column ()
  ((name
    :reader name
    :initarg :name)
   (type-props
    :reader type-props
    :initarg :type-props)
   (default-value
    :reader default-value
    :initarg :default-value)))

(defclass type-props ()
  ((name
    :reader name
    :initarg :name)
   (comparator
    :reader comparator
    :initarg :comparator)
   (eq-predicate
    :reader eq-predicate
    :initarg :eq-predicate)
   (default-value
    :reader default-value
    :initarg :default-value)))

;; make make-column a defun
(defgeneric make-type-props (type))

(defmethod make-type-props ((type (eql 'string)))
  (make-instance
   'type-props
   :name          'string
   :comparator    #'string<
   :eq-predicate  #'string=
   :default-value ""))

(defmethod make-type-props ((type (eql 'number)))
  (make-instance
   'type-props
   :name          'number
   :comparator    #'<
   :eq-predicate  #'=
   :default-value 0))

(defmethod make-type-props ((type (eql 'boolean)))
  (make-instance
   'type-props
   :name          'boolean
   :comparator    #'(lambda (b1 b2) (car (cons b1 b2)))
   :eq-predicate  #'eq
   :default-value nil))

(defun make-column (name type &optional default-value)
  (let ((t-props (make-type-props type)))
    (make-instance
     'column
     :name name
     :type-props t-props
     :default-value (or default-value (default-value t-props)))))

;; (defgeneric make-column (name type &optional default-value))

;; (defmacro make-colmethod (type
;;                           comparator
;;                           eq-predicate
;;                           &optional default-value)
;;   `(defmethod make-column (name ,type &optional default-value)
;;      (make-instance
;;       'column
;;       :name name
;;       ;; :col-type ,type
;;       :comparator ,comparator
;;       :eq-predicate ,eq-predicate
;;       :default-value ,default-value)))

;; (make-colmethod (type (eql 'string))
;;                 #'string<
;;                 #'string=
;;                 "")

;; (make-colmethod (type (eql 'number))
;;                 #'<
;;                 #'=
;;                 0)


;; (make-colmethod (type (eql 'boolean))
;;                 #'t-first
;;                 #'eq)

(defun mk-schema (spec)
  (mapcar #'(lambda (column-spec) (apply #'make-column column-spec)) spec))

(defmacro make-schema (&rest specs)
  `(mk-schema '(,@specs)))

(defun schema-slot (schema slot)
  (mapcar #'(lambda (column) (slot-value column slot)) schema))

(defun schema-names (schema)
  (schema-slot schema 'name))

(defvar *sample-schema*
  (make-schema
   (:column1 string "default")
   (:column2 number 123)
   (:column3 boolean)))

(schema-names *sample-schema*)

(defvar *sample-table*
  (make-instance 'table :schema *sample-schema*))

(rows *sample-table*)

(defun validate-row (schema values)
  (loop for column in schema
        for name = (name column)
        for type = (name (type-props column))
        for value = (or (getf values name) (default-value column))
        collect name
        collect (if (typep value type)
                    value
                    (error "~S is not of type ~S for column ~S" value type name))))

(defun insert-row (table values)
  (push (validate-row (schema table) values) (rows table)))

(defun insert-rows (table &rest rows)
  (dolist (r rows table)
    (insert-row table r)))

(insert-row *sample-table* '())
(insert-rows *sample-table* '() '(:column1 "aaa"))

(rows *sample-table*)

;; (append '(1 2 3) '(4 5 6))
;; (mapcar #'(lambda (x y) (cons x y)) '(1 2 3) '(4 5 6))

;; (cdr (list :test 1 :test2 2))
;; (cdr '(1 . 2))

;; (defmethod make-column (name (type (eql 'string)) &optional default-value)
;;   (make-instance
;;    'column
;;    :name name
;;    :comparator #'string<
;;    :eq-predicate #'string=
;;    :default-value default-value))



;; backtracking

;; unification

;; rule processing
;; add-rule  : Rule -> List Rule -> List Rule
;; eval-rule : Rule -> List Rule -> Value

;; term
;; term-list
;; atom
;; atom-list
;; rule
;; program

