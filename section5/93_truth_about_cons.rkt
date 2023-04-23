; Programming Languages, Dan Grossman
; Section 5: The Truth About Cons

#lang racket

(provide (all-defined-out))

(define pr (cons 1 (cons #t "hi"))) ; (1, (true, "hi")) in ML
(car pr)       ; first elem of the pair  -> 1
(cdr pr)       ; second elem of the pait -> '(#t . "hi")
(cdr (cdr pr)) ; second elem of the second elem -> "hi"

(define lst (cons 1 (cons #t (cons "hi" null))))
(car lst)       ; first elem -> 1
(cdr lst)       ; second elem -> '(#t "hi")
(cdr (cdr lst)) ; second elem of the second elem -> '("hi")
(car (cdr (cdr lst))) ; first elem of the second elem of the second elem -> "hi"
(caddr lst)           ; built-in function equivalent to (car (cdr (cdr lst))) -> "hi"
(cdr (cdr (cdr lst))) ; second elem of the second elem of the second elem -> '()


(define hi (cdr (cdr pr)))
(define hi-again (car (cdr (cdr lst))))
(define hi-again-shorter (caddr lst))
(define no (list? pr))
(define yes (pair? pr))
(define of-course (and (list? lst) (pair? lst)))
; (define do-not-do-this (length pr))

