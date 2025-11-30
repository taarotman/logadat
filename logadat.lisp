;; source: https://stackoverflow.com/questions/24252539/defining-aliases-to-standard-common-lisp-functions
(defmacro alias (fn to)
  `(setf (fdefinition ',to) #',fn))

(defun map-hashtbl (function hashtbl)
  (let ((new-hashtbl (make-hash-table)))
    (maphash #'(lambda (k v)
                 (setf (gethash k new-hashtbl) (funcall function v)))
             hashtbl)
    new-hashtbl))

;; (defun equalnilerr (x y)
;;   (if (equal x y)
;;       t
;;       (error "~S is not equal to ~S" x y)))

(defun set-exclusive-nilerr (x y)
  (let ((setex (set-exclusive-or x y :test #'equal)))
    (if (null setex)
        t
        (error "~S is not equal to ~S" x y))))



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


(defun lunion (list1 list2)
  "union but with equal instead"
  (union list1 list2 :test #'equal))

(defun lintersection (list1 list2)
  "intersection but with equal instead"
  (intersection list1 list2 :test #'equal))

(defun ldifference (list1 list2)
  "set-difference but with equal instead"
  (set-difference list1 list2 :test #'equal))

(defun lunions (&rest rlist)
  (reduce #'lunion rlist))

(defun lintersections (&rest rlist)
  (reduce #'lintersection rlist))

(defun ldifferences (&rest rlist)
  (reduce #'ldifference rlist))


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

(defmacro compr-bc (exp &body quals)
  "compr with built-in binary composition matching"
  `(compr ,exp ,@(bc-quals quals)))

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

(defmacro compr-pm (exp &body quals)
  "compr with built-in primitive (non-recursive) pattern matching"
  `(compr ,exp ,@(pm-quals (bc-quals quals))))


(defun lsubsetp (list1 list2)
  "subsetp but with equal instead"
  (subsetp list1 list2 :test #'equal))



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

;; (defun seminaive-evaluation (idb edb)
;;   (let ((new-preds (eval-preds (rewrite-preds-rules idb edb))))
;;     (if (predicate= idb new-preds)
;;         new-preds
;;         (naive-evaluation new-preds edb))))


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


(defmacro queries-eval (evaluated-preds edb query-list &optional evald-queries)
  (let ((query (car query-list)))
    (if (null query)
        `,evald-queries
        `(queries-eval
          ,evaluated-preds ,edb ,(cdr query-list)
          (cons (cons ',(first query)
                      (query-eval ,(first query)
                                  ,(second query)
                                  ,evaluated-preds
                                  ,edb))
                ,evald-queries)))))

(defmacro queries (evaluated-preds edb &body query-list)
  `(queries-eval ,evaluated-preds ,edb ,(reverse query-list)))

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

(setq factstest (facts (n (a b c) (b c e))))

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

;; (query-eval t (x y) colpreds3 (facts (n (a b c) (b c e))))
;; (query-eval n (x y z) colpreds3 (facts (n (a b c) (b c e))))

(queries colpreds3 factstest
  (n (x y z))
  (n (x 'c z))
  (t (x y))
  (t ('a y)))


(lunion '((a b c) (d e f) (g h i)) '((1 2 3) (4 5 6) (7 8 9)))

;; unit testing
