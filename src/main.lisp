(uiop:define-package logadat
  (:use #:cl))
(in-package #:logadat)

;; <program>   ::= <rule> <program> | ""
;; <rule>      ::= <atom> ":-" <atom-list> "."
;; <atom-list> ::= <atom> | <atom> "," <atom-list> | ""
;; <atom>      ::= <relation> "(" <term-list> ")"
;; <term-list> ::= <term> | <term> "," <term-list> | ""
;; <term>      ::= <constant> | <variable>


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

(defun values-as-keys (row k v)
  (cons v (getf row k)))

;; (defmacro values-as-keys (row k v)
;;   `(cons (intern (string ,v) "KEYWORD") (getf ,row ,k)))

(defun assoc-variables (row variables)
  (collect-keyvalue (lambda (k v) (values-as-keys row k v))
                    variables))

(defun select-column (rows variables)
  "Returns a substitution"
  (apply #'append (mapcar #'(lambda (row) (assoc-variables row variables)) rows)))

(defun terms-variables (terms)
  (apply #'append (collect-keyvalue (lambda (k v)
                                      (if (variable-p v) (list k v)))
                                    terms)))

(defun terms-constants (terms)
  (apply #'append (collect-keyvalue (lambda (k v)
                                      (if (not (variable-p v)) (list k v)))
                                    terms)))

(defun query-rows (rows terms)
  (let ((variables (terms-variables terms))
        (constants (terms-constants terms)))
    ;; (cons variables constants)
    (select-column (filter-rows rows constants) variables)
    ))


(defun query-table (table terms)
  (query-rows (rows table) terms))


;; query evaluation
(defun query-rule (rule terms)
  (let ((variables (terms-variables terms))
        (constants (terms-constants terms))
        (head (head rule))
        (body (body rule)))
    )
  )

(defun query (name terms rules tables)
  (cond (((find-name name tables)
          (query-table (find-name name tables) terms))
         (t nil))))
;; (fact ?X const) means select all X in fact with cons

;; unification
(defun get-binding (x bindings)
  (assoc x bindings))

(get-binding 'x '((x . 1) (y . 2) '(x . 3)))

(defun add-binding (bindings left right)
  (cons (cons left right) bindings))

(defun unify-term (left right &optional bindings)
  (cond
    ((eql right left) bindings)
    ((variable-p left)  (unify-var left right bindings))
    ((variable-p right) (unify-var right left bindings))
    (t nil)))

(defun unify-var (var x bindings)
  (let ((var-in-bindings (get-binding var bindings))
        (x-in-bindings (get-binding x bindings)))
    (cond
      (var-in-bindings
       (unify-term (cdr var-in-bindings) x bindings))
      ((and (variable-p x) x-in-bindings)
       (unify-term var (cdr x-in-bindings) bindings))
      (t (add-binding bindings var x)))))

(defun zip (left right)
  (mapcar #'list left right))

(defun unify-zipped (zipped &optional bindings)
  (let ((left (first (car zipped)))
        (right (second (car zipped))))
    (if (null zipped)
        bindings
        (unify-zipped (cdr zipped)
               (unify-term left right bindings)))))

(defun unify (left right &optional bindings)
  (unify-zipped (zip left right) bindings))


(setq unify1 (unify-term '?x '?y)) 
(setq unify2 (unify-term "abc" '?y unify1)) 
(setq unify3 (unify-term '?y '?x unify1)) 

(setq zipped (zip '(1 2 3) '(4 5 6)))
(setq unify-list (unify '(?x ?y a) '(?y ?x ?x)))
;; (defun unify-var (x y &optional bindings)
;;   (let ((x-in-bindings (get-binding x bindings))
;;         (y-in-bindings (get-binding y bindings)))
;;     (cond ((eql x y) bindings)
;;           ()
;;       (t (add-binding bindings x y))))
;;   )

;; (unify-var 'x 'x)

;; (defun unify-var (x y &optional substitution)
;;   (cons x y))

;; substitution


;; printing
(defun print-list (list)
  (dolist (e list)
    (print e)))

(defun print-rows (table) (print-list (rows table)))
(defun print-query (query) (print-list query))


;; testing
(defvar *sample-schema*
  (make-schema
   (:id number)
   (:c1 number)
   (:c2 string "default")
   (:c3 boolean)))

(defvar *sample-table*
  (make-instance
   'table
   :name :sample-table
   :schema *sample-schema*))

(setf (rows *sample-table*) nil)

(insert-rows *sample-table*
             '(:id 1 :c1 123 :c2 "abc" :c3 t)
             '(:id 2)
             '(:id 3 :c3 t :c2 "abc")
             '(:id 4 :c3 t)
             )

;; (defvar *sample-rows*
;;   (rows *sample-table*))

(print-rows *sample-table*)

(defvar *sample-table-query*)

(setq *sample-table-query*
      (query-table *sample-table*
                   '(
                     :id ?identifier
                     :c3 ?bool
                     :c1 ?c1
                     :c2 "abc"
                     )))

(print-query *sample-table-query*)
