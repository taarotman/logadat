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

;; source: https://stackoverflow.com/questions/2680864/how-to-remove-nested-parentheses-in-lisp
(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

;; source: https://stackoverflow.com/questions/24252539/defining-aliases-to-standard-common-lisp-functions
(defmacro alias (fn to)
  `(setf (fdefinition ',to) #',fn))

(alias mapcan mappend)

(defun bind (list function)
  "list a -> (a -> list b) -> list b"
  (mappend function list))

(defun pure (x)
  "a -> list a"
  (cons x nil))

(defun id-list (x)
  (if (not (listp x))
      (pure x)
      x))

(bind '(1 2 3 4 5)
      (lambda (x) (pure (* x x))))
(bind '(1 2 3 4 5)
      (lambda (x) (if (= (mod x 2) 0) (pure x))))
(bind '(1 2 3 4)
      (lambda (x) (bind '(a b)
                        (lambda (y) (pure (cons x y))))))
(bind '(1 2 3 4)
      (lambda (x) (if (= (mod x 2) 0) (bind '(a b)
                                            (lambda (y) (pure (cons x y)))))))

(mappend (lambda (x y) (pure (cons x y))) '(1 2 3) '(A B)) 


(defmacro symbolp-r (x &body body)
  `(and (symbolp ,x)
        ,@body))

(defun symbol= (x y)
  (symbolp-r x
    (string= (symbol-name x) (symbol-name y))))

(defun lambdavar-p (x)
  "Can X be a lambda variable?"
  (symbolp-r x
    (not (fboundp x))
    (both-case-p (char (symbol-name x) 0))))

;; (defmacro id-var (symbol list)
;;   `(identity (if (equal ',symbol (car ',list))
;;                  ,symbol
;;                  ,list)))



;; ;; (defun filter-symbols (exp)
;; ;;   (let ((cached nil))
;; ;;    (loop for s in exp
;; ;;         for symbol? = (lambdavar-p s)
;; ;;         for not-cached? = (not (find s cached))
;; ;;         when (and symbol? not-cached?)
;; ;;           collect s)))))


;; (defun filter-symbols (exp)
;;   (let ((cached nil))
;;    (remove-if-not
;;     #'(lambda (s)
;;         (when (and (lambdavar-p s) (not (find s cached)))
;;           (push s cached)
;;           s))
;;     exp)))

;; (defun symbols (exp)
;;   (let ((syms (filter-symbols (flatten exp))))
;;     (if (= (length syms) 1)
;;         (car syms)
;;         syms)))

;; (defun ziplist (list)
;;   (apply #'mapcar #'list list))

(defmacro compr (exp &body quals)
  "List comprehension: [exp | qual1,qual2,..,qualn]"
  (let ((qual (car quals))
        (rest-quals (cdr quals)))
    (cond ((null quals)
           `(pure ,exp))
          ((symbol= (car qual) 'in) ;; regular/parallel list comprehension
           `(mappend (lambda ,(id-list (second qual))
                       (compr ,exp ,@rest-quals))
                     ,@(cddr qual)))
          ((symbol= (car qual) 'inzip) ;; zipped list comprehension
           (let ((lambda-id (gensym)))
             `(bind ,(third qual)
                    (lambda (,lambda-id)
                      (apply (lambda ,(id-list (second qual))
                               (compr ,exp ,@rest-quals))
                             ,lambda-id)))))
          (qual
           `(if ,qual (compr ,exp ,@rest-quals))))))

(defmacro forc (&rest exp)
  "List comprehension but the output expression is put last"
  `(compr ,(car (last exp)) ,@(butlast exp)))

(compr (cons x y)
  (in x '(1 2 3 4))
  (= (mod x 2) 0)
  (in y '(a b)))

(forc 
  (in x '(1 2 3 4))
  (= (mod x 2) 0)
  (in y '(a b))
  (cons x y))

(compr (list x y z)
  (in (x y) '(1 2 3 4) '(a b c))
  (in z '(d e)))

(compr x
  (in x '(1 2 3)))

(setq link '((a b) (b c) (c c) (c d)))

(setq p1x
      (compr (list x z)
        (inzip (x y) link)
        (inzip (y1 z) link)
        (equal y y1)))

;; fix union
(union link p1x)


;; (defun dumb-function (x y z) (+ x y z))

;; (apply #'dumb-function '(1 2 3))

;; (setq dumb-list '((1 2 3) (11 22 33)))

;; (compr var
;;   (in var dumb-list))

;; [(a -> b)] -> [a] -> [b]
;; https://stackoverflow.com/questions/58837372/haskell-function-that-applies-a-list-of-functions-to-a-list-of-inputs
;; (mapcar #'funcall '(+ + +) '(1 2 3) '(1 1 1))

;; (funcall (lambda (x) (+ 1 x)) 1)

;; ;; doesnt work with quote
;; (setq dumb-list2 (list (lambda (x) (+ 1 x))
;;                        (lambda (y) (+ 2 y))
;;                        (lambda (z) (+ 3 z))))

;; (mapcar #'funcall dumb-list2 '(1 1 1))

;; (compr (mapcar #'funcall dumb-list2 var)
;;   (in var dumb-list))

;; (compr (apply #'dumb-function var)
;;   (in var dumb-list))

;; (compr (+ x y)
;;   (inzip (x y) '((1 2) (3 4))))


;; [(a,b,c), (d,e,f)] -> [(a,d), (b,e) (c,f)]
;; (ziplist dumb-list)



;; variable processing
(defun variable-p (x)
  "Is X a named variable (a symbol beginning with '?')?"
  (symbolp-r x
    (equal (char (symbol-name x) 0) #\?)))

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
    ((equal right left) (or bindings (cons (cons t right) nil))
     ;;(add-binding bindings right left)
     )
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
  (ignore-errors
   (reverse (unify-zipped (zip left right) bindings))))


(setq unify1 (unify-term '?x '?y)) 
(setq unify2 (unify-term "abc" '?y unify1)) 
(setq unify3 (unify-term '?y '?x unify1)) 
(setq unify3 (unify-term '?x '?x)) 

(setq unify3 (unify '(?x ?z) '(?z ?y))) 

;; (setq zipped (zip '(1 2 3) '(11 22 33) '(111 222 333)))
(setq unify-list (unify '(?x ?y a) '(?y ?x ?x)))
(setq unify-list (unify '(?x ?c ?y) '(a b c)))
(mapcar #'cdr unify-list)

(setq unify-list (unify '(?x "a") '(?y "a")))
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
(setq unifyr
      (unify-row '(:name ?x     :role ?z)
                 '(:name ?z     :role ?y)))

(defun mapnonnil (function list)
  "mapcar but only collect non-nil results" 
  (loop for i in list
        for result = (funcall function i)
        when result
          collect result))


(defun binary-composition (function list-rx list-ry list-sy list-sz)
  "(x . z) <- list-rx X list-sz when (y <- list-ry) = (y <- list-sy)"
  (compr (cons x z)
    (in (x y1) list-rx list-ry)
    (in (y2 z) list-sy list-sz)
    (equal y1 y2)))

(defun bincomp (list-rx list-ry list-sy list-sz)
  "(x . z) <- list-rx X list-sz when (y <- list-ry) = (y <- list-sy)"
  (binary-composition #'cons list-rx list-ry list-sy list-sz))

(bincomp '(a b c d e)
         '(1 2 3 4 5)
         '(3 7 2 6 4)
         '(v w x y z))


(defun union-plist (plist1 plist2)
  ;; (compr (k (or (getf plist1 k) (getf plist2 k)))
  ;;   (in k (union (keys plist1) (keys plist2))))
  (let ((keys (union (keys plist1) (keys plist2))))
    (loop for k in keys
          collect k
          collect (or (getf plist1 k) (getf plist2 k)))))

(union-plist '(:v1 1 :c 1 :v2 1) '(:v2 1 :v3 2))


;; (defun join-pred (function row1 row2)
;;   "Rows are plists"
;;   (loop for (k1 v1) on row1 by #'cddr
;;         when (equal v1 (getf row2 k1))
;;           return (funcall function row1 row2)))

(defun join-pred (row1 row2)
  "Rows are plists"
  (loop for (k1 v1) on row1 by #'cddr
        when (equal v1 (getf row2 k1))
          return t))

;; (defun join-pred (row1 row2)
;;   (compr t
;;     (in (k1 v1))))

;; (join-pred '(:v1 1 :v2 2) '(:v2 2 :v3 3))


(defun natural-join (relation1 relation2)
  "Combine relations if there is a common attribute"
  (compr (union-plist r s)
    (in r relation1)
    (in s relation2)
    (join-pred r s)))

(alias natural-join join)

(setq join1
      (natural-join '((:name "Harry" :empid 3415 :deptname "Finance")
                      (:name "Sally" :empid 2241 :deptname "Sales")
                      (:name "George" :empid 3401 :deptname "Finance")
                      (:name "Harriet" :empid 2202 :deptname "Sales")
                      (:name "Mary" :empid 1257 :deptname "Human Resources"))

                    '((:deptname "Finance" :manager "George")
                      (:deptname "Sales" :manager "Harriet")
                      (:deptname "Production" :manager "Charles"))))


(defun eval-table (terms table)
  (mapnonnil #'(lambda (row)
                 (unify-row terms row))
             (rows table)))

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
(defun find-fact (fact tables)
  "note: facts are defined as rules without a body"
  (find-if #'(lambda (table) (equal (name table) (name fact))) tables))

(alias find-fact match-rows)

;; (defun eval-fact (db fact)
;;   "Evaluate a fact (which can be from a query or from the body of a rule)"
;;   (let ((head (head fact))
;;         (table? (find-fact fact (tables db)))
;;         ;;(rules (rules db))
;;         )
;;     (cond (table? (eval-table head table?))
;;           (t (error "fact not in ~S" (name db))))))

;; (defun eval-fact (fact tables &optional bindings)
;;   "Evaluate a fact (which can be from a query or from the body of a rule)"
;;   )


;; (defun collect-p (rule ilist elist)
;;   "Collect a list of rules with the same name (and head structure)"
;;   )

;; (defun tables-to-subst-rows (tables))

;; (defun subst-rows-to-tables (subst-rows))

;; (defun walk-body (body bindings-tables))

(defun tables-to-grounds (tables)
  "tables - list (name X schema X list row) -> facts - list (name X row)"
  (compr (make-rule (name table) (pure p))
    (in table tables)
    (in p (rows table))))

(setq table1
      (mk-table :table1 (:v1 :colour :v2)
        (1 red 2)
        (2 blue 1)
        (2 green 3)
        (1 blue 4)
        (3 red 4)
        (4 yellow 5)))
(rows table1)

(compr (cons (name i) (head i))
  (in i (tables-to-grounds (pure table1))))


(defun eval-grounds0 (terms table)
  "terms -> table -> name X list substitution"
  (compr (cons (name table) f)
    (in row (rows table))
    (in f (pure (unify terms row)))
    (identity f)))

(setq subst1 (eval-grounds0 '(?x ?c ?y) table1))


;; if variable

;; if name equals subst-row name then if that doesnt exist yet in the substitiution then
;; push that

;; if not then 

(defun esrow (name head subst-row)
  (compr
      (if )
    (in term head)
    (in st (cdr subst-row))
    ))

(defun eval-grounds1 (name head substitutions)
  ;; (bind
  ;;  substitutions
  ;;  (lambda (subst)
  ;;    (if (string= name (car subst))
  ;;        (pure subst))))
  (compr srow
    (in subst substitutions)
    (in srow (cdr subst))
    ))

(setq subst2 (eval-grounds1 :r1 '(?x ?y) subst1))


(setq rule1
      (mk-rule :rule1 (?x ?y)
        (:table1 (?x ?c ?y))))
(body rule1)


;; (defun delta-init (idb edb)
;;   (compr (eval-table (head p) tb)
;;     (in p idb)
;;     (= (length (body p)) 1)
;;     (in tb (find-fact p edb))
;;     ;; (and (= (length (body p)) 1)
;;     ;;      (find-fact p edb))
;;     ))

;; (db-eval-init
;;  '((1 2 3) (4 5 6)))

;; before evaluating a query, evaluate the entire program first
(defun eval-db (idb &optional edb bindings-db)
  "semi-naive evaluation"
  (let ()
    ))


;; printing
;; (defun print-list (list)
;;   (dolist (e list)
;;     (print e)))

;; (defun print-rows (table) (print-list (rows table)))
;; (defun print-query (query) (print-list query))


;; ;; testing
;; (defvar *sample-schema*
;;   (make-schema
;;    (:name string)
;;    (:role string)
;;    ))

;; (defvar *sample-table*
;;   (make-instance
;;    'table
;;    :name :sample-table
;;    :schema *sample-schema*))

;; (setf (rows *sample-table*) nil)

;; (insert-rows *sample-table*
;;              '(:name "Milchick" :role "manager")
;;              '(:name "Mark"     :role "severed")
;;              '(:name "Helly"    :role "severed")
;;              '(:name "Irving"   :role "severed")
;;              '(:name "Dylan"    :role "severed")
;;              )

;; ;; (defvar *sample-rows*
;; ;;   (rows *sample-table*))

;; (print-rows *sample-table*)

;; (setq *sample-db*
;;       (make-instance
;;        'database
;;        :name :sample-db))

;; (ins-table *sample-db* *sample-table*)

;; (tables *sample-db*)

;; (setq *sample-rule*
;;       (make-rule :sample-rule '(:boss ?x :employee ?y)
;;                               '((:sample-table (:name ?x :role "manager"))
;;                                 (:sample-table (:name ?y :role "severed")))
;;                  *sample-db*))

;; (ins-rule *sample-db* *sample-rule*)

;; (mapcar #'name (tables *sample-db*)) 

;; (setq *sample-rules* (rules *sample-db*))

;; (mapcar #'head (body (car *sample-rules*))) 

;; (setq sample-query
;;       '(:name ?name
;;         :role "severed"
;;         ))

;; (defvar *sample-table-query*)

;; (setq *sample-table-query*
;;       (query-table *sample-table* sample-query))

;; (print-query *sample-table-query*)

;; (setq evt (eval-table sample-query *sample-table*))
;; (print-query evt)


;; (setq query-to-rule (mk-rule :sample-table sample-query nil))
;; (head query-to-rule)

;; (setq evf (eval-fact *sample-db* query-to-rule))

;; (find-fact query-to-rule (tables *sample-db*))


