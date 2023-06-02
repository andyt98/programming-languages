;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; Racket List -> MUPL List
;; takes a Racket list and produces an analogous mupl list with the same elements in the same order.

(define (racketlist->mupllist list)
  (cond [(null? list) (aunit)]
        [else (apair (car list) (racketlist->mupllist (cdr list)))]
        ))

;; MUPL List -> Racket List
;; takes a mupl list and produces an analogous Racket list (of mupl values) with the same elements in the same order.

(define (mupllist->racketlist list)
  (cond [(aunit? list) null]
        [else (cons (apair-e1 list) (mupllist->racketlist (apair-e2 list)))]
        ))
                    

;; Problem 2
;; Write a mupl interpreter, i.e., a Racket function eval-exp
;; that takes a mupl expression e and either returns the mupl value that e evaluates to under the empty
;; environment or calls Racket's error if evaluation encounters a run-time mupl type error or unbound
;; mupl variable.

;; lookup a variable in an environment
;; use a Racket list of Racket pairs to represent this environment (which is initially empty)
;; in a pair - the first elem is the name of the var and the second is the value
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond
    [(int? e) e]
    [(aunit? e) e]
    [(closure? e) e]
    [(fun? e) (closure env e)]
    [(var? e) 
     (envlookup env (var-string e))]
    [(add? e) 
     (let ([v1 (eval-under-env (add-e1 e) env)]
           [v2 (eval-under-env (add-e2 e) env)])
       (if (and (int? v1)
                (int? v2))
           (int (+ (int-num v1) 
                   (int-num v2)))
           (error "MUPL addition applied to non-number")))]
    [(ifgreater? e)
     (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
           [v2 (eval-under-env (ifgreater-e2 e) env)])
       (if (and (int? v1) (int? v2))
           (if (> (int-num v1) (int-num v2))
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env))
           (error "MUPL ifgreater applied to non-number")))]
    [(apair? e)
     (apair (eval-under-env (apair-e1 e) env)
            (eval-under-env (apair-e2 e) env))]
    [(fst? e)
     (let ([v (eval-under-env (fst-e e) env)])
       (if (apair? v)
           (apair-e1 v)
           (error "MUPL fst applied to non-pair")))]
    [(snd? e)
     (let ([v (eval-under-env (snd-e e) env)])
       (if (apair? v)
           (apair-e2 v)
           (error "MUPL snd applied to non-pair")))]
    [(isaunit? e)
     (let ([v (eval-under-env (isaunit-e e) env)])
       (if (aunit? v) (int 1) (int 0)))]
    [(mlet? e)
     (let* ([v (eval-under-env (mlet-e e) env)]        ; evaluates the expression e to get its value and assigns it to v
            [newenv (cons (cons (mlet-var e) v) env)]) ; creates newenv by extending env by adding a new binding (cons (mlet-var e) v)
       (eval-under-env (mlet-body e) newenv))]
    [(call? e)
     (let ([cl (eval-under-env (call-funexp e) env)]
           [arg (eval-under-env (call-actual e) env)])
       (if (closure? cl)
           (let ([myfun (closure-fun cl)])
             (eval-under-env
              (fun-body myfun)
              (let ([newenv
                     (cons (cons (fun-formal myfun) arg)
                           (closure-env cl))])
                (if (fun-nameopt myfun)
                    (cons (cons (fun-nameopt myfun) cl) newenv)
                    newenv))))
           (error "MUPL call's funexp is not a closure")))]
     
    [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;(eval-exp (int 7))
        
;; Problem 3

(define (ifaunit e1 e2 e3) "CHANGE")

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
