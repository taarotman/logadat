;; (require "DATABASE" "logadat-db")
(load "logadat-db.lisp")

(defpackage :logadat
  (:use :cl
        :logadat-db))
(in-package :logadat)
(in-package :logadat-db)

;; (use-package cl-logadat-db)

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


;; ;; database
;; (defclass table ()
;;   ((name :accessor name :initarg :name)
;;    (rows :accessor rows :initarg :rows :initform nil)
;;    (schema :accessor schema :initarg :schema :initform nil)))

;; (defclass column ()
;;   ((name
;;     :reader name
;;     :initarg :name)
;;    (type-props
;;     :reader type-props
;;     :initarg :type-props)
;;    (default-value
;;     :reader default-value
;;     :initarg :default-value)))

;; (defclass type-props ()
;;   ((name
;;     :reader name
;;     :initarg :name)
;;    (comparator
;;     :reader comparator
;;     :initarg :comparator)
;;    (eq-predicate
;;     :reader eq-predicate
;;     :initarg :eq-predicate)
;;    (default-value
;;     :reader default-value
;;     :initarg :default-value)))

;; ;; make make-column a defun
;; (defgeneric make-type-props (type))

;; (defmethod make-type-props ((type (eql 'string)))
;;   (make-instance
;;    'type-props
;;    :name          'string
;;    :comparator    #'string<
;;    :eq-predicate  #'string=
;;    :default-value ""))

;; (defmethod make-type-props ((type (eql 'number)))
;;   (make-instance
;;    'type-props
;;    :name          'number
;;    :comparator    #'<
;;    :eq-predicate  #'=
;;    :default-value 0))

;; (defmethod make-type-props ((type (eql 'boolean)))
;;   (make-instance
;;    'type-props
;;    :name          'boolean
;;    :comparator    #'(lambda (b1 b2) (car (cons b1 b2)))
;;    :eq-predicate  #'eq
;;    :default-value nil))

;; (defun make-column (name type &optional default-value)
;;   (let ((t-props (make-type-props type)))
;;     (make-instance
;;      'column
;;      :name name
;;      :type-props t-props
;;      :default-value (or default-value (default-value t-props)))))


;; (defun mk-schema (spec)
;;   (mapcar #'(lambda (column-spec) (apply #'make-column column-spec)) spec))

;; (defmacro make-schema (&rest specs)
;;   `(mk-schema '(,@specs)))

;; (defun schema-slot (schema slot)
;;   (mapcar #'(lambda (column) (slot-value column slot)) schema))

;; (defun schema-names (schema)
;;   (schema-slot schema 'name))


;; ;; (defun validate-with-schema (schema values validate)
;; ;;   (loop for column in schema
;; ;;         for name = (name column)
;; ;;         for type = (name (type-props column))
;; ;;         for value = (or (getf values name) (default-value column))
;; ;;         collect name
;; ;;         collect (if (collect)
;; ;;                     value
;; ;;                     (error "~S is not of type ~S for column ~S"
;; ;;                            value type name))))

;; (defun validate-row (schema values)
;;   (loop for column in schema
;;         for name = (name column)
;;         for type = (name (type-props column))
;;         for value = (or (getf values name) (default-value column))
;;         collect name
;;         collect (if (typep value type)
;;                     value
;;                     (error "~S is not of type ~S for column ~S"
;;                            value type name))))

;; (defun insert-row (table values)
;;   (push (validate-row (schema table) values) (rows table)))

;; (defun insert-rows (table &rest rows)
;;   (dolist (r rows table)
;;     (insert-row table r)))


;; ;; (rows *sample-table*)


;; ;; rule processing
;; ;; (defrule predicate-name (atom) (atoms))
;; ;; (body-name &rest props)

;; ;; terms are plist
;; ;; term    ::= variable | constant ;

;; (variable-p (getf '(:column1 ?a) :column1))

;; (defun validate-terms-from-schema (schema terms)
;;   (loop for column in schema
;;         for name = (name column)
;;         for type = (name (type-props column))
;;         for value = (or (getf terms name) (default-value column))
;;         collect name
;;         collect (if (or (typep value type) (variable-p value))
;;                     value
;;                     (error "~S is not a variable nor of type ~S for column ~S"
;;                            value type name))))


;; (defclass rule ()
;;   ((name
;;     :reader name
;;     :initarg :name)
;;    (head ;; terms
;;     :reader head
;;     :initarg :head)
;;    (body ;; list of rules
;;     :reader body
;;     :initarg :body)))

;; ;; a rule without a body is an fact - atom (datom)

;; (defun mk-rule (name head body)
;;   (make-instance
;;    'rule
;;    :name name
;;    :head head
;;    :body body))

;; (defun make-fact (name head)
;;   (mk-rule name head nil))

;; (make-fact :abc '(:col1 ?a :col2 ?b))


;; (defun make-fact-from-table (table terms)
;;   (make-fact (name table) (validate-terms-from-schema (schema table) terms))
;;   ;; (make-instance
;;   ;;  'rule
;;   ;;  :name (name table)
;;   ;;  :head (validate-terms (schema table) terms)
;;   ;;  :body nil)
;;   )


;; (defun keys (plist)
;;   (loop for (key value) on plist by #'cddr
;;         collect key))

;; (defun vals (plist)
;;   (loop for (key value) on plist by #'cddr
;;         collect value))

;; (defun validate-terms-from-rule (rule terms)
;;   (if (equal (keys (head rule)) (keys terms))
;;       terms
;;       (error "~S keys do not match for ~S which are ~S"
;;              terms
;;              (name rule)
;;              (keys (head rule)))))

;; (defun make-fact-from-rule (rule terms)
;;   (make-fact (name rule) (validate-terms-from-rule rule terms)))

;; (defun fact-processing (parent-rule body-part &optional rules tables)
;;   (let ((name (car body-part))
;;         (terms (getf body-part (car body-part))))
;;     (cond ((equal name (name parent-rule))
;;            (make-fact-from-rule parent-rule terms))
;;           ((find-name name rules)
;;            (make-fact-from-rule (find-name name rules) terms))
;;           ((find-name name tables)
;;            (make-fact-from-table (find-name name tables) terms))
;;           (t (make-fact name terms)))))

;; (defun make-rule (name head body &optional db)
;;   (let ((rule (make-fact name head)))
;;     (mk-rule (name rule)
;;              (head rule)
;;              (mapcar #'(lambda (b)
;;                          (if db
;;                              (fact-processing rule b (rules db) (tables db))
;;                              (fact-processing rule b)))
;;                      body))))



;; ;; rule (:name head body
;; (defclass database ()
;;   ((name
;;     :reader name
;;     :initarg :name)
;;    (tables
;;     :accessor tables
;;     :initarg :tables
;;     :initform nil)
;;    (rules
;;     :accessor rules
;;     :initarg :rules
;;     :initform nil)))


;; (defun ins-table (db table)
;;   (push table (tables db)))

;; (defun ins-rule (db rule)
;;   (push rule (rules db)))


;; select columns of variables in terms
;; filter based on columns of constants

(defun collect-keyvalue (fun plist)
  (loop for (key value) on plist by #'cddr
        collect (funcall fun key value)))

(defun mapplist (function plist)
  (loop for (key value) on plist by #'cddr
        collect key
        collect (funcall function value)))

(defun mapplist-kv (function plist)
  (loop for (key value) on plist by #'cddr
        collect key
        collect (funcall function key value)))

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
  "Example: (:name . ?X) become (?X . v)"
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

;; (fact ?X const) means select all X in fact with cons

;; unification
(defun get-binding (x bindings)
  (assoc x bindings))

(get-binding 'x '((x . 1) (y . 2) '(x . 3)))

(defun add-binding (bindings left right)
  (cons (cons left right)
        (if (eq t (caar bindings))
            nil
            bindings)))

(defun unify-term (left right &optional bindings)
  (cond
    ((equal right left) (or bindings (cons (cons t right) nil)))
    ((variable-p left)  (unify-var left right bindings))
    ((variable-p right) (unify-var right left bindings))
    (t (error "~S and ~S cannot be unified" left right))))

(defun unify-var (var x bindings)
  (let ((var-in-bindings (get-binding var bindings))
        (x-in-bindings (get-binding x bindings)))
    (cond
      (var-in-bindings
       (unify-term (cdr var-in-bindings) x bindings))
      ((and (variable-p x) x-in-bindings)
       (unify-term var (cdr x-in-bindings) bindings))
      (t (add-binding bindings var x)))))

(defmacro zip (&rest lists)
  `(mapcar #'list ,@lists))

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
(setq unify3 (unify-term '?x '?x)) 

(setq unify3 (unify '(?x ?z) '(?z ?y))) 

;; (setq zipped (zip '(1 2 3) '(11 22 33) '(111 222 333)))
;; (setq unify-list (unify '(?x ?y a) '(?y ?x ?x)))
;; (setq unify-list (unify '(?x ?x ?x) '(?y ?y ?y)))
;; (setq unify-list (unify '(?y ?x) '(?x ?y)))
;; (setq unify-list (unify '(?y ?x ?x) '(?x ?y a)))
;; (setq unify-list (unify '(?x ?x ?x) '(a ?y ?y)))
;; (setq unify-list (unify '(a ?y ?x) '(?x ?y ?y)))
;; (setq unify-list (unify '(?y ?x ?x) '(?x ?y a)))
;; (setq unify-list (unify '(?x ?x ?y) '(?x ?y a) '((?y . ?x))))

;; (setq unify-list (unify '(?x) '(?y) '((?x . a))))
;; (setq unify-list (unify '((f ?x)) '((f ?y))))
;; (setq unify-list (unify '(?x ?z) '(?z ?y)))
;; (setq unify-list (unify '(abc ?x ?y) '(bac a b)))

;; (setq unify-list (unify '(?x b) '(?x s)))

;; (defun unify-facts (facts)
;;   (cond ((null facts) nil)
;;         ((null (cdr facts)) facts)
;;         (t (unify (second facts) (first facts) (unify-facts (cdr facts))))))

;; (setq uf1 (unify-facts '((?x ?x ?y)
;;                          (?x ?y a)
;;                          (?y ?x ?x))))
;; substitution

;; proving
;; (defun query-rule (rule terms)
;;   (let ((variables (terms-variables terms))
;;         (constants (terms-constants terms))
;;         (head (head rule))
;;         (body (body rule)))

;;     )
;;   )

;; (defun query (name terms rules tables)
;;   (cond (
;;          ((find-name name tables)
;;           (query-table (find-name name tables) terms))
;;          (t nil))))

;; unify-row

;; (defun unify-table (terms table &optional bindings-list))

;; (defun query (name terms db &optional bindings-list)
  
;;   )

;; (defun unify-row (terms row)
;;   (let ((unified
;;           (unify (vals terms)
;;                  (mapcar #'(lambda (k) (getf row k)) (keys terms)))))
;;     (zip (keys terms) unified)))

;; (defmacro ignore-errors-nw (&rest forms)
;;   `(handler-bind
;;        (progn ,@forms)
;;      (error (condition) (values nil condition))))

;; will cause a warning... use handler-bind instead later perhaps
(defun unify-row (terms row)
  (ignore-errors
   (mapplist-kv
    #'(lambda (k v)
        (cdar (unify-term v (getf row k))))
    terms)))

(setq unifyt (unify-term 'a 'a))
;; (cdar unifyt)
(setq unifyr (unify-row '(:a ?x :b 2) '(:a "x" :b 2)))
(setq unifyr (unify-row '(:a ?x :b 2) '(:a ?y :b 2)))
(setq unifyr (unify-row '(:a ?x :b 2) '(:a "x" :b 3)))
(setq unifyr
      (unify-row '(:name ?x     :role "severed")
                 '(:name "Mark" :role "severed")))

(defun mapnonnil (function list)
  (loop for i in list
        for result = (funcall function i)
        when result
          collect result))

(defun eval-table (terms table)
  (mapnonnil #'(lambda (row) (unify-row terms row)) (rows table))
  ;; (loop for row in (rows table)
  ;;       for result = (unify-row terms row)
  ;;       when result
  ;;         collect result)
  ;;(remove nil (mapcar #'(lambda (row) (unify-row terms row)) (rows table)))
  )

(defun eval-rule (db rule)
  (let ((tables (tables db))
        (rules  (rules db)))
    ))



(defun fix (function &optional value)
  "Repeatedly apply 'function' until value equals value applied to function"
  (let ((next (funcall function value)))
    (if (equal value next)
        value
        (fix function next))))

;; (defun fac (f)
;;   (lambda (n)
;;     (case n
;;       (0 1)
;;       (t (* n (funcall f (- n 1)))))))

(setq fix-test (fix (lambda (x) (+ 5)) 1))

;; (defun fact (n) (fix (lambda (x) (fac x)) n))

;; (setq fix-test (fact 5))
(defun find-fact (fact list)
  "note: facts are defined as rules without a body"
  (find-if #'(lambda (table) (equal (name table) (name fact))) list))

(defun eval-fact (db fact)
  "Evaluate a fact (which can be from a query or from the body of a rule)"
  (let ((head (head fact))
        (table? (find-fact fact (tables db)))
        ;;(rules (rules db))
        )
    (cond (table? (eval-table head table?))
          (t (error "fact not in ~S" (name db))))))


;; (defun collect-p (rule ilist elist)
;;   "Collect a list of rules with the same name (and head structure)"
;;   )

;; (defun tables-to-subst-rows (tables))

;; (defun subst-rows-to-tables (subst-rows))

;; before evaluating a query, evaluate the entire program first
(defun naive-eval (rules &optional tables subst-rows)
  )


;; printing
(defun print-list (list)
  (dolist (e list)
    (print e)))

(defun print-rows (table) (print-list (rows table)))
(defun print-query (query) (print-list query))


;; testing
(defvar *sample-schema*
  (make-schema
   (:name string)
   (:role string)
   ))

(defvar *sample-table*
  (make-instance
   'table
   :name :sample-table
   :schema *sample-schema*))

(setf (rows *sample-table*) nil)

(insert-rows *sample-table*
             '(:name "Milchick" :role "manager")
             '(:name "Mark"     :role "severed")
             '(:name "Helly"    :role "severed")
             '(:name "Irving"   :role "severed")
             '(:name "Dylan"    :role "severed")
             )

;; (defvar *sample-rows*
;;   (rows *sample-table*))

(print-rows *sample-table*)

(setq *sample-db*
      (make-instance
       'database
       :name :sample-db))

(ins-table *sample-db* *sample-table*)

(tables *sample-db*)

(setq *sample-rule*
      (make-rule :sample-rule '(:boss ?x :employee ?y)
                              '((:sample-table (:name ?x :role "manager"))
                                (:sample-table (:name ?y :role "severed")))
                 *sample-db*))

(ins-rule *sample-db* *sample-rule*)

(mapcar #'name (tables *sample-db*)) 

(setq *sample-rules* (rules *sample-db*))

(mapcar #'head (body (car *sample-rules*))) 

(setq sample-query
      '(:name ?name
        :role "severed"
        ))

(defvar *sample-table-query*)

(setq *sample-table-query*
      (query-table *sample-table* sample-query))

(print-query *sample-table-query*)

(setq evt (eval-table sample-query *sample-table*))
(print-query evt)


(setq query-to-rule (mk-rule :sample-table sample-query nil))
(head query-to-rule)

(setq evf (eval-fact *sample-db* query-to-rule))

(find-fact query-to-rule (tables *sample-db*))


