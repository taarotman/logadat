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


(defun class-find (id-reader id class-list)
  (find-if #'(lambda (class) (equal (slot-value class id-reader) id)) class-list))

(defun find-name (name class-list)
  (class-find 'name name class-list))


;; database
(defclass table ()
  ((name :accessor name :initarg :name)
   (rows :accessor rows :initarg :rows :initform nil)
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
  (make-instance 'table :name :sample-table :schema *sample-schema*))

(rows *sample-table*)

;; (defun validate-with-schema (schema values validate)
;;   (loop for column in schema
;;         for name = (name column)
;;         for type = (name (type-props column))
;;         for value = (or (getf values name) (default-value column))
;;         collect name
;;         collect (if (collect)
;;                     value
;;                     (error "~S is not of type ~S for column ~S"
;;                            value type name))))

(defun validate-row (schema values)
  (loop for column in schema
        for name = (name column)
        for type = (name (type-props column))
        for value = (or (getf values name) (default-value column))
        collect name
        collect (if (typep value type)
                    value
                    (error "~S is not of type ~S for column ~S"
                           value type name))))

(defun insert-row (table values)
  (push (validate-row (schema table) values) (rows table)))

(defun insert-rows (table &rest rows)
  (dolist (r rows table)
    (insert-row table r)))

(insert-row *sample-table* '())
(insert-rows *sample-table* '() '(:column1 "aaa"))

(rows *sample-table*)

;; (rows *sample-table*)


;; rule processing
;; (defrule predicate-name (atom) (atoms))
;; (body-name &rest props)

;; terms are plist
;; term    ::= variable | constant ;

(variable-p (getf '(:column1 ?a) :column1))

(defun validate-terms-from-schema (schema terms)
  (loop for column in schema
        for name = (name column)
        for type = (name (type-props column))
        for value = (or (getf terms name) (default-value column))
        collect name
        collect (if (or (typep value type) (variable-p value))
                    value
                    (error "~S is not a variable nor of type ~S for column ~S"
                           value type name))))

(validate-terms-from-schema *sample-schema* '(:column1 ?a))

(defclass rule ()
  ((name
    :reader name
    :initarg :name)
   (head ;; terms
    :reader head
    :initarg :head)
   (body ;; list of rules
    :reader body
    :initarg :body)))

;; a rule without a body is an fact - atom (datom)

(defun mk-rule (name head body)
  (make-instance
   'rule
   :name name
   :head head
   :body body))

(defun make-fact (name head)
  (mk-rule name head nil))

(make-fact :abc '(:col1 ?a :col2 ?b))


(defun make-fact-from-table (table terms)
  (make-fact (name table) (validate-terms-from-schema (schema table) terms))
  ;; (make-instance
  ;;  'rule
  ;;  :name (name table)
  ;;  :head (validate-terms (schema table) terms)
  ;;  :body nil)
  )

(defvar *sample-rule* (make-fact-from-table *sample-table* '(:column1 ?a
                                                             :column2 ?b)))
(head *sample-rule*)

(defun keys (plist)
  (loop for (key value) on plist by #'cddr
        collect key))

(defun validate-terms-from-rule (rule terms)
  (if (equal (keys (head rule)) (keys terms))
      terms
      (error "~S keys do not match for ~S which are ~S"
             terms
             (name rule)
             (keys (head rule)))))

(defun make-fact-from-rule (rule terms)
  (make-fact (name rule) (validate-terms-from-rule rule terms)))

(defun fact-processing (parent-rule body-part &optional rules tables)
  (let ((name (car body-part))
        (terms (getf body-part (car body-part))))
    (cond ((equal name (name parent-rule))
           (make-fact-from-rule parent-rule terms))
          ((find-name name rules)
           (make-fact-from-rule (find-name name rules) terms))
          ((find-name name tables)
           (make-fact-from-table (find-name name tables) terms))
          (t (make-fact name terms)))))

(defun make-rule (name head body &optional db)
  (let ((rule (make-fact name head)))
    (mk-rule (name rule)
             (head rule)
             (mapcar #'(lambda (b)
                         (if db
                             (fact-processing rule b (rules db) (tables db))
                             (fact-processing rule b)))
                     body))))

(defvar *sample-rule2*
  (make-rule :sample-rule
             '(:col1 ?x :col2 ?y)
             '(
               (:a-fact (:c1 ?z))
               (:sample-rule (:col1 ?y :col2 ?z))
               )))

(head *sample-rule2*)

(mapcar #'head (body *sample-rule2*)) 

(caddr '(1 2 3 4))


;; rule (:name head body
(defclass database ()
  ((name
    :reader name
    :initarg :name)
   (tables
    :accessor tables
    :initarg :tables
    :initform nil)
   (rules
    :accessor rules
    :initarg :rules
    :initform nil)))

(defvar *sample-db*
  (make-instance
   'database
   :name 'sample-db))


(defun ins-rule (db rule)
  (push rule (rules db)))

(defun insert-rule (db rule)
  (ins-rule db (make-rule (car rule) (cadr rule) (caddr rule) db)))

(ins-rule *sample-db* *sample-rule2*)

(insert-rule *sample-db* '(:n (:c1 ?x :c2 ?y) nil))

(insert-rule *sample-db* '(:n2 (:c1 ?x :c2 ?y) ((:n ( :c1 ?y :c2 ?x)))
                           ))

(mapcar #'(lambda (r) (list (name r) (head r)
                            (mapcar #'(lambda (br) (cons (name br) (head br)))
                                    (body r))))
        (rules *sample-db*))


(defun ins-table (db table)
  (push table (tables db)))


;; select columns of variables in terms
;; filter based on columns of constants
(defun collect-keyvalue (fun plist)
  (loop for (key value) on plist by #'cddr
        collect (funcall fun key value)))

(defun constants-p-list (row constants)
  (collect-keyvalue (lambda (k v) (equal v (getf row k))) constants))
;; (defun constants-p-list (row constants)
;;   (loop for (key value) on constants by #'cddr
;;         collect (equal value (getf row key))))


(constants-p-list '(:a 1 :b 2) '(:a 1 :b 3))

(defun constants-p (row constants)
  (every #'identity (constants-p-list row constants)))

(constants-p '(:a 1 :b 2) '(:a 1 :b 3))

(defun filter-rows (rows constants)
  (remove-if-not #'(lambda (row)
                     (constants-p row constants))
                 rows))

(defvar *sample-rows* (rows *sample-table*)) 
(filter-rows *sample-rows* '(:column1 "default"))

(defmacro values-as-keys (row k v)
  `(list (intern (string ,v) "KEYWORD") (getf ,row ,k)))

(values-as-keys '(:col1 123) :col1 'x)


(defun assoc-variables (row variables)
  (apply #'append (collect-keyvalue (lambda (k v)
                                        (values-as-keys row k v))
                                    variables)))

(assoc-variables '(:column1 "abc" :column2 "def") '(:column1 ?a))

(defun select-column (rows variables)
  (mapcar #'(lambda (row) (assoc-variables row variables)) rows))

(select-column *sample-rows* '(:column1 ?X))


(defun terms-variables (terms)
  (apply #'append (collect-keyvalue (lambda (k v)
                                      (if (variable-p v) (list k v)))
                                    terms)))

(defun terms-constants (terms)
  (apply #'append (collect-keyvalue (lambda (k v)
                                      (if (not (variable-p v)) (list k v)))
                                    terms)))

(terms-variables '(:c1 ?a :c2 abc :c3 ?x))
(terms-constants '(:c1 ?a :c2 abc :c3 ?x))

(defun query-rows (rows terms)
  (let ((variables (terms-variables terms))
        (constants (terms-constants terms)))
    ;; (cons variables constants)
    (select-column (filter-rows rows constants) variables)
    ))

(query-rows *sample-rows* '(:column1 "default" :column2 ?x :column3 ?x))


(defun query-table (table terms)
  (query-rows (rows table) terms))

(query-table *sample-table* '(:column1 "aaa" :column2 ?x :column3 ?x))


;; query evaluation
;; (fact ?X const) means select all X in fact with cons

;; unification
;; substitution


