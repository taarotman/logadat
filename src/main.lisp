;; (require "DATABASE" "logadat-db")
;; (load "logadat-db.lisp")

;; (defpackage :logadat
;;   (:use :cl
;;         :logadat-db))
;; (in-package :logadat)
;; (in-package :logadat-db)

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

(defun map-hashtbl (function hashtbl)
  (let ((new-hashtbl (make-hash-table)))
    (maphash #'(lambda (k v)
                 (setf (gethash k new-hashtbl) (funcall function v)))
             hashtbl)
    new-hashtbl))

(defun equalnilerr (x y)
  (if (equal x y)
      t
      (error "~S is not equal to ~S" x y)))

(defun set-exclusive-nilerr (x y)
  (let ((setex (set-exclusive-or x y :test #'equal)))
    (if (null setex)
        t
        (error "~S is not equal to ~S" x y))))


;; (equalnilerr 1 2)


(alias mapcan mappend)

(defun bind (list function)
  "list a -> (a -> list b) -> list b"
  (mappend function list))

(defun pure (x)
  "a -> list a"
  (cons x nil))

(defun id-list (x)
  "a | list a -> list a"
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
    (not (boundp x))
    (or (both-case-p (char (symbol-name x) 0))
        (symbol= '_ x))))

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

(defun bincomp (list1 list2 &optional (exp #'list))
  "(x z) for every (x y) in list1 for every (y z) in list2"
  (compr (funcall exp x z)
    (inzip (x y1) list1)
    (inzip (y2 z) list2)
    (equal y1 y2)))

(setq p1x
      (compr (list x z)
        (inzip (x y) link)
        (inzip (y1 z) link)
        (equal y y1)))
;; ((A C) (B C) (B D) (C C) (C D))

(setq p1x
      (bincomp link link))
;; ((A C) (B C) (B D) (C C) (C D))

;; fix union
(union link p1x)
(union link p1x :test #'equal)

(defun lunion (list1 list2)
  "union but with equal instead"
  (union list1 list2 :test #'equal))

(defun lintersection (list1 list2)
  "intersection but with equal instead"
  (intersection list1 list2 :test #'equal))

(defun ldifference (list1 list2)
  "set-difference but with equal instead"
  (set-difference list1 list2 :test #'equal))

(setq p1 (lunion link p1x))


(defun lunions (&rest rlist)
  (reduce #'lunion rlist))

(defun lintersections (&rest rlist)
  (reduce #'lintersection rlist))

(defun ldifferences (&rest rlist)
  (reduce #'ldifference rlist))


;; (defun bc-quals (quals)
;;   (let ((existing-vars nil)
;;         (new-quals nil))
;;     (loop for qual in quals
;;           for qvars = (id-list (second qual))
          
;;     )))

;; if var not in existing-vars:
;;   (list qual (cons var existing-vars)
;; else:
;;   (list (

;; (defun bc-qual (qual &optional existing-vars new-quals)
;;   (let ((vars (id-list (second qual))))

;;     )
;;   )

;; (defun bc-quals (quals &optional existing-vars new-quals same-qual)
;;   (let* ((qual (car quals))
;;          (vars (id-list (second qual)))
;;          )
;;     (if (null quals)
;;         new-quals
;;         ()
;;         )))

(defun bc-quals (quals &optional new-quals existing-vars)
  "formatting quals in compr for binary composition"
  ;; what a shitty code
  (loop for qual in quals
        for aquals = nil 
        for variables = (id-list (second qual))
        for vars = variables
        for new-qual = qual
        when (or (symbol= (car qual) 'in)
                 (symbol= (car qual) 'inzip))
          do (progn
               (loop for i upto (- (length variables) 1)
                     for var = (nth i variables)
                     do (if (not (member var existing-vars))
                            (push var existing-vars)
                            (let ((new-var (gensym)))
                              (setf (nth i vars) new-var)
                              (push `(equal ,var ,new-var) aquals))))
               (setf (second new-qual) vars))
        do (setf new-quals (append new-quals
                                   (cons new-qual (reverse aquals))))
        finally (return new-quals)))

(bc-quals '((in (x y) '(1 2 3))
            (inzip (y z) '((1 2) (3 4)))
            )
          )

(compr (cons x y)
  (in x '(1 2 3 4 5))
  (in y '(4 5 6 7 8))
  (equal x y)
  (equal x 5))

(setq aaa '(1 2 3))
(setf aaa (append aaa '(4 5 6)))

(defmacro compr-bc (exp &body quals)
  "compr with built-in binary composition matching"
  `(compr ,exp ,@(bc-quals quals)))

(compr x
  (in x '(1 2 3))
  (in x '(2 3 4)))

(compr-bc x
  (in x '(1 2 3))
  (in x '(2 3 4)))

(setq p1x
      (compr-bc (list x z)
        (inzip (x y) link)
        (inzip (y z) link)))
;; ((A C) (B C) (B D) (C C) (C D))

(setq p1 (lunion link p1x))


(defun patmatch-atom (atom pattern)
  (equal atom pattern))

(defun patmatch-cons (cons pattern))

(defun patmatch (x pattern)
  )


(defun pm-quals (quals &optional new-quals)
  "formatting quals in compr for pattern matching"
  (loop for qual in quals
        for aquals = nil
        for variables = (id-list (second qual))
        for vars = variables
        for new-qual = qual
        when (or (symbol= (car qual) 'in)
                 (symbol= (car qual) 'inzip))
          do (progn
               (loop for i upto (- (length variables) 1)
                     for var = (nth i variables)
                     do (if (not (lambdavar-p var))
                            (let ((new-var (gensym)))
                              (setf (nth i vars) new-var)
                              (push `(equal ,new-var ,var) aquals))))
               (setf (second new-qual) vars))
        do (setf new-quals (append new-quals (cons new-qual (reverse aquals))))
        finally (return new-quals)))

(pm-quals '(
            (in x '(1 2 3))
            (in (1 x) '(1 2 3))
            ))

(defmacro compr-pm (exp &body quals)
  "compr with built-in primitive (non-recursive) pattern matching"
  `(compr ,exp ,@(pm-quals (bc-quals quals))))

(compr x
  (inzip (x y) '((1 1) (2 2) (3 1)))
  (equal y 1))
;; (1 3)

(compr-pm x
  (inzip (x 1) '((1 1) (2 2) (3 1))))
;; (1 3)

(setq aaa "aaa")

(lambdavar-p 'aaa)

(compr-pm x
  (in (x aaa) '(1 2 3) '("aaa" "aaa" "ccc")))

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
(subsetp '((1 2) (3 4)) '((1 2) (3 4) (5 6)) :test #'equal)

(defun lsubsetp (list1 list2)
  "subsetp but with equal instead"
  (subsetp list1 list2 :test #'equal))

(lsubsetp '((1 2) (3 4)) '((1 2) (3 4) (5 6)))


(defun naive-ex1 (edge)
  (let ((path (compr-pm (list x y) (inzip (x y) edge))))
    (loop while t
          for newpath = (compr-pm (list x z)
                          (inzip (x y) edge)
                          (inzip (y z) path))
          do (print path)
          if (lsubsetp newpath path)
            return path
          else
            do (setf path (lunion path newpath)))))

(defun seminaive-ex1 (edge)
  (let* ((path (compr-pm (list x y) (inzip (x y) edge)))
         (deltapath path))
    (loop while t
          for newpath = (compr-pm (list x z)
                          (inzip (x y) edge)
                          (inzip (y z) deltapath))
          do (print deltapath)
          if (lsubsetp newpath path)
            return path
          else
            do (progn
                 (setf deltapath (ldifference newpath path))
                 (setf path (lunion path newpath))))))

(setq edge '((1 2) (2 3) (3 4)))
(setq nex1 (naive-ex1 edge))
(setq snex1 (seminaive-ex1 edge))
(lintersection nex1 snex1)
(ldifference nex1 snex1)




(defclass predicate ()
  ((pred-name
    :reader pred-name
    :initarg :pred-name)
   (term-length
    :reader term-length
    :initarg :term-length)
   (rule-list
    :accessor rule-list
    :initarg :rule-list)
   (rules-rewrite
    :accessor rules-rewrite
    :initarg :rules-rewrite
    :initform nil)
   (current-value
    :accessor current-value
    :initarg :current-value
    :initform nil)))

(defmacro rules (&body rules)
  `(collect-preds ',rules (make-hash-table)))

;; (defmacro facts (&body facts)
;;   (let ((fact-table (make-hash-table)))
;;     (forc
;;      (in afact facts)
;;      (in fact (cdr afact))
;;      )))

(defun collect-preds (idb &optional preds)
  (labels ((pos (r) (gethash (first r) preds))
           (pred (r) (nth-value 0 (pos r)))
           (head-and-body (r) (cons (second r) (cddr r))))
    (dolist (rule idb)
      (if (nth-value 1 (pos rule))
          (if (= (length (second rule)) (term-length (pred rule)))
              (setf (rule-list (pred rule)) (cons (head-and-body rule)
                                                  (rule-list (pred rule))))
              (error "Rules of predicate ~S must have the same head length"
                     (first rule)))
          (if (symbolp (first rule))
              (setf (gethash (first rule) preds)
                    (make-instance 'predicate
                                   :pred-name (first rule)
                                   :term-length (length (second rule))
                                   :rule-list (pure (head-and-body rule))))
              (error "Predicate name ~S is not a symbol" (first rule)))))
    preds))

;; (r '((1 2) (3 4)))

(defmacro facts (&body facts)
  `(collect-facts ',facts (make-hash-table)))

(defun collect-facts (edb &optional facts)
  (forc
   (in fact edb)
   (setf (gethash (first fact) facts)
         (validate-fact fact)))
  facts)

(defun validate-fact (fact &optional fact-length)
  (forc
   (in body (cdr fact))
   (if (null fact-length)
       (progn
         (setf fact-length (length body))
         body)
       (if (= (length body) fact-length)
           body
           (error "Fact ~S contains a body length (~S) mismatch from ~S"
                  (cdr fact) fact-length body)))))


(defun rewrite-preds-rules (predicates edb)
  (flet ((rewrite-rules (rules)
           (compr (cons (car r) (rewrite-atoms (cdr r) predicates edb))
             (in r rules))))
    (map-hashtbl #'(lambda (p) (make-instance
                                'predicate
                                :pred-name (pred-name p)
                                :term-length (term-length p)
                                :rule-list (rule-list p)
                                :rules-rewrite (rewrite-rules (rule-list p))
                                :current-value (current-value p)))
                 predicates)))

(defun rewrite-atoms (rule-body predicates edb)
  (forc
   (in datom rule-body)
   (cond
     ((not (symbol= 'in (first datom))) datom)
     ((nth-value 1 (gethash (third datom) predicates))
      `(in ,(second datom)
           ',(current-value (nth-value 0 (gethash (third datom) predicates)))))
     ((nth-value 1 (gethash (third datom) edb))
      `(in ,(second datom)
           ',(nth-value 0 (gethash (third datom) edb))))
     (t (error "~S not found in database" (third datom))))))



;; (rewrite-atoms '((in (x y) r)) colpreds (make-hash-table))

;; (mapcar #'(lambda (x) (+ 1 (car x))) '((1 a) (2 b)))

(defun eval-preds (predicates)
  (map-hashtbl #'(lambda (p)
                   (make-instance
                    'predicate
                    :pred-name (pred-name p)
                    :term-length (term-length p)
                    :rule-list (rule-list p)
                    :rules-rewrite (rules-rewrite p)
                    :current-value (eval-rules (rules-rewrite p))))
               predicates))


;; (defmacro rule-to-compr (head &body body)
;;   `(compr-pm (list ,@head)
;;      ,@(to-inzip body)))

(defun rule-to-compr (rule)
  ;; hacky solution because eval mutates the previous declared quotes for some reason
  (let ((*read-eval* nil)
        (compr-string
          (write-to-string (rule-compr-gen (car rule) (cdr rule)))))
    (eval (read-from-string compr-string))))

(defun rule-compr-gen (head body)
  `(compr-pm (list ,@head)
     ,@(to-inzip body)))

(defun to-inzip (predicates)
  (forc
   (in pred predicates)
   (if (not (symbol= (first pred) 'in))
       pred
       `(inzip ,(second pred) ,(third pred)))))


(defun eval-rules (rules)
  (remove-duplicates
   (reduce #'lunion (mapcar #'rule-to-compr rules))
   :test #'equal))


(defun predicate=nilerr (pred1 pred2)
  (map-hashtbl
   #'(lambda (p)
       (set-exclusive-nilerr
        (current-value p)
        (current-value (nth-value 0 (gethash (pred-name p) pred2)))))
   pred1))

(defun predicate= (pred1 pred2)
  (handler-case (predicate=nilerr pred1 pred2)
    (error ()
      nil)))

;; FOR DEBUG
;; (defun predicate= (pred1 pred2)
;;   (restart-case (predicate=nilerr pred1 pred2)
;;     (predicate-not-nill ()
;;       nil)))

(defun naive-evaluation (idb edb)
  (let ((new-preds (eval-preds (rewrite-preds-rules idb edb))))
    (if (predicate= idb new-preds)
        new-preds
        (naive-evaluation new-preds edb))))


(defmacro query-eval (pred-name pred-terms evaluated-preds &optional edb)
  `(compr-pm (list ,@pred-terms)
     (inzip ,pred-terms (get-rule-or-fact ',pred-name
                                          ,evaluated-preds
                                          ,edb))))

(defun get-rule-or-fact (pred-name idb edb)
  (cond ((nth-value 1 (gethash pred-name idb))
         (current-value (nth-value 0 (gethash pred-name idb))))
        ((nth-value 1 (gethash pred-name edb))
         (nth-value 0 (gethash pred-name edb)))
        (t (error "~S not found in database for query" pred-name))))


;; to trace
;; naive-evaluation
;; collect-rules
;; eval-rules
;; rule-to-compr
;; rule-compr-gen
;; rewrite-preds-rules
;; rules-rewrite
;; rewrite-atoms
;; predicate=
;; equalnilerr

;; (funcall #'(lambda (_) (+ _ 1)) 2)  

;; (setq colpreds3
;;       (naive-evaluation
;;        (rules
;;          (t (x y)
;;             (in (x y _) n))
;;          )
;;        (facts
;;          (n (a b c) (d e f))))) 

;; (current-value (nth-value 0 (gethash 't colpreds3))) 

(setq colpreds3
      (naive-evaluation
       (rules
         (t (x y)
            (in (x z) t)
            (in (z y) t))
         (t (x y)
            (in (x y _) n)))
       (facts
         (n (a b c) (b c e))))) 

(rule-list (nth-value 0 (gethash 't colpreds3))) 
(rules-rewrite (nth-value 0 (gethash 't colpreds3))) 
(current-value (nth-value 0 (gethash 't colpreds3))) 

(query-eval t (x y) colpreds3 (facts (n (a b c) (b c e))))

;; (setq colpreds3
;;       (naive-evaluation
;;        (rules
;;          (t (x)
;;             (in (x) t))
;;          (t (x)
;;             (in (x) n)))
;;        (facts
;;          (n (a) (b))))) 

;; (current-value (nth-value 0 (gethash 't colpreds3))) 

;; (setq test
;;       (rule-list (nth-value 0 (gethash 't colpreds3))))

;; (equal '(((X Y) (IN (X Y) R)) ((X Y) (IN (X Z) T) (IN (Z Y) R))) test) 

;; (compr-pm (list x y)
;;   (inzip (x y) 'nil))
;; (setq quotetest (rule-compr-gen '(x y)
;;                                 '((in (x y) '((1 2) (3 4))))))

;; (setq stringtest (write-to-string quotetest))

;; (setq stringtest2
;;       (eval (read-from-string stringtest)))

;; (compr-pm (list x y)
;;   (inzip (x 'b) '((b c) (a b) (a c)))
;;   (inzip (z y)  '((b c) (a b) (a c))))

;; (remove-duplicates '((a c) (a b) (a c)) :test #'equal)

;; (setq comprtest
;;       (compr-pm (list x y)
;;         (inzip (x c) '((b c) (a b) (a c)))
;;         (inzip (z y) '((b c) (a b) (a c)))))

;; (remove-duplicates comprtest :test #'equal)

;; (setq setor
;;       (set-exclusive-or comprtest '((a c) (b c) (b b)) :test #'equal))

