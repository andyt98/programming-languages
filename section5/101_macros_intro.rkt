; Programming Languages, Dan Grossman
; Section 5: Macros: The Key Points

#lang racket

(provide (all-defined-out))

; You do *not* need to understand these macro definitions 
; (that is a later optional section), but we want to show
; uses of them

;; a cosmetic macro -- adds then, else
(define-syntax my-if 
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if e1 e2 e3)]))

;; a macro to replace an expression with another one
(define-syntax comment-out
  (syntax-rules ()
    [(comment-out ignore instead) instead]))

; makes it so users don't write the thunk when using my-delay
(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e)
     (mcons #f (lambda () e))]))

; some uses

(define seven (my-if #t then 7 else 14))
seven
; SYNTAX ERROR: (define does-not-work (my-if #t then 7 then 9))
; SYNTAX ERROR: (define does-not-work (my-if #t then 7 else else))

(define no-problem (comment-out (car null) #f))
no-problem

(define x (begin (pretty-print "hello") (* 3 4))) ; this prints "hello" right away

(define p (my-delay (begin (pretty-print "hi") (* 3 4)))) ; this prints "hi" when (my-force p) is called

(define (my-force th)
  (if (mcar th)
      (mcdr th)
      (begin (set-mcar! th #t)
             (set-mcdr! th ((mcdr th)))
             (mcdr th))))

(my-force p)
; (my-force p)