; Programming Languages, Dan Grossman
; Section 5: Memoizaton

#lang racket

(provide (all-defined-out))

;; 0 1 1 2 3 5 8 13 21 ...

;; with 2 recursive calls (without memoization)
;; very slow implementation
(define (fibonacci1 x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fibonacci1 (- x 1))
         (fibonacci1 (- x 2)))))

;; altnernative implemenation using accumulators (without memoization)
(define (fibonacci2 x)
  ; y -> the current position
  ; acc1 -> the current number in the sequence
  ; acc2 -> the previous number in the sequence
  (letrec ([f (lambda (acc1 acc2 y) 
                (if (= y x)
                    (+ acc1 acc2)
                    (f (+ acc1 acc2) acc1 (+ y 1))))])
    (if (or (= x 1) (= x 2))
        1
        (f 1 1 3))))


;; implementation using memoization
(define fibonacci3
  (letrec([memo null] ; the memo table is initially null, we will use mutation to update it
                      ; is is a list of pair (arg . result)
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

;; assoc check the car fields of the pairs for its first argument and returns the first pair
(define xs (list (cons 1 2) (cons 3 4) (cons 5 6)))
(assoc 1 xs)  ; '(1 . 2)
(assoc 3 xs)  ; '(3 . 4)
(assoc 6 xs) ; #f
