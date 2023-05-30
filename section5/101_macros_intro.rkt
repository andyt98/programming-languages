; Programming Languages, Dan Grossman
; Section 5: Macros: The Key Points

#lang racket

(provide (all-defined-out))

;; a cosmetic macro -- adds then, else
(define-syntax my-if 
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3) ; if you see something like this 
     (if e1 e2 e3)]))           ; expand it like this

(define seven (my-if #t then 7 else 14))
seven
; SYNTAX ERROR: (define does-not-work (my-if #t then 7 then 9))
; SYNTAX ERROR: (define does-not-work (my-if #t then 7 else else))

;; a macro to replace an expression with another one
(define-syntax comment-out
  (syntax-rules ()
    [(comment-out ignore instead) instead]))

(define no-problem (comment-out (car null) #f)) ; (car null) would normally throw an error
no-problem ; #f

;; makes it so users do *not* write the thunk when using my-delay
(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e)
     (mcons #f (lambda () e))]))

(define (my-delay-old th) 
  (mcons #f th)) ;; a one-of "type" we will update in place

(define (my-force th)
  (if (mcar th)
      (mcdr th)
      (begin (set-mcar! th #t)
             (set-mcdr! th ((mcdr th)))
             (mcdr th))))

(define x (begin (pretty-print "hello") (* 3 4))) ; this prints "hello" right away
x ; calling x doesn't print hello, only returns 12

(define p (my-delay (begin (pretty-print "hi") (* 3 4)))) ; this prints "hi" when (my-force p) is called
(define p-old (my-delay-old (lambda () (begin (pretty-print "hi") (* 3 4))))) ; using my-delay-old

(my-force p) ; this prins "hi" and returns 12 when p is first time called 
(my-force p) ; this only returns 12

;(my-force p-old)