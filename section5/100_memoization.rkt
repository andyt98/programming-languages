; Programming Languages, Dan Grossman
; Section 5: Memoizaton

#lang racket

(provide (all-defined-out))

; 0 1 1 2 3 5 8 13 21 ...

(define (fibonacci1 x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fibonacci1 (- x 1))
         (fibonacci1 (- x 2)))))

(define (fibonacci2 x)
  (letrec ([f (lambda (acc1 acc2 y)
                (if (= y x)
                    (+ acc1 acc2)
                    (f (+ acc1 acc2) acc1 (+ y 1))))])
    (if (or (= x 1) (= x 2))
        1
        (f 1 1 3))))

(define fibonacci3
  (letrec([memo null] ; list of pairs (arg . result) 
          [f (lambda (x)
               (let ([ans (assoc x memo)]) ; check if it's already in the memo table
                 (if ans 
                     (cdr ans) ; if it's already in the table, return the result 
                     (let ([new-ans (if (or (= x 1) (= x 2)) ; if it's not already in the table, compute it
                                        1
                                        (+ (f (- x 1))
                                           (f (- x 2))))])
                       (begin 
                         (set! memo (cons (cons x new-ans) memo)) ; put the computed result in the memo
                         new-ans)))))])  ; return the result
    f))


(define xs (list (cons 1 2) (cons 3 4) (cons 5 6)))
(assoc 1 xs)  ; '(1 . 2)
(assoc 3 xs)  ; '(3 . 4)
(assoc 6 xs) ; #f
