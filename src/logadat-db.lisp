(defpackage :logadat-db
  (:use :cl))
(in-package :logadat-db)

;; <program>   ::= <rule> <program> | ""
;; <rule>      ::= <atom> ":-" <atom-list> "."
;; <atom-list> ::= <atom> | <atom> "," <atom-list> | ""
;; <atom>      ::= <relation> "(" <term-list> ")"
;; <term-list> ::= <term> | <term> "," <term-list> | ""
;; <term>      ::= <constant> | <variable>

;; (defclass table ()
;;   ((name :accessor name :initarg :name)
;;    (rows :accessor rows :initarg :rows :initform nil)
;;    (schema :accessor schema :initarg :schema :initform nil)))

(defclass table ()
  ((name :accessor name :initarg :name)
   (schema :accessor schema :initarg :schema :initform nil)
   (rows :accessor rows :initarg :rows :initform nil)))

(defun make-table (name schema &optional rows)
  (make-instance
   'table
   :name name
   :schema schema
   :rows rows))

(setq table1 (make-table :table-name '(:col1 :col2)))
(schema table1)
(rows table1)

(setq table2 (make-table :table-name '(:col1 :col2)
               '((a 1)
                (b 2))))
(rows table2)


(defmacro mk-table (name schema &body rows)
  `(make-table ,name ',schema ',rows))

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

(defun insert-row (table values)
  (push (validate-row (schema table) values) (rows table)))

(defun insert-rows (table &rest rows)
  (dolist (r rows table)
    (insert-row table r)))


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


(defclass datom ()
  ((relation
    :reader relation
    :initarg :relation)
   (term-list
    :reader term-list
    :initarg :term-list)))

(defclass rule ()
  ((head ;; head atom
    :reader head
    :initarg :head)
   (body ;; body list of atoms
    :reader body
    :initarg :body)))

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

;; ;; a rule without a body is a fact - atom (datom)

;; (defun make-rule (name head &optional body)
;;   (make-instance
;;    'rule
;;    :name name
;;    :head head
;;    :body body))

;; (setq rule1
;;       (make-rule :rule-name '(?x ?y)
;;         '((:rule-name2 (?x ?c ?z)) (:rule-name (?z ?y)))))
;; (head rule1)
;; (body rule1)

;; (setq rule2
;;       (make-rule :rule-name '(?x ?y)))
;; (head rule2)
;; (body rule2)


(defmacro mk-rule (name head &body body)
  `(make-rule ,name ',head ',body))


;; (defun make-fact (name head)
;;   (make-rule name head nil))

;; (make-rule :abc (?a ?b))

;; (make-fact :abc '(?a ?b))


;; (defun make-fact-from-table (table terms)
;;   (make-fact (name table) (validate-terms-from-schema (schema table) terms))
;;   ;; (make-instance
;;   ;;  'rule
;;   ;;  :name (name table)
;;   ;;  :head (validate-terms (schema table) terms)
;;   ;;  :body nil)
;;   )


(defun keys (plist)
  (loop for (key value) on plist by #'cddr
        collect key))

(defun vals (plist)
  (loop for (key value) on plist by #'cddr
        collect value))

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

(defun ins-rule (db rule)
  (push rule (rules db)))
