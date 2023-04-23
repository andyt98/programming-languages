; Programming Languages, Dan Grossman
; Section 5: Dynamic Typing

#lang racket

(provide (all-defined-out))

; [second big difference from ML (and Java)] Dynamic Typing!!

; dynamic typing: can use values of any type anywhere
;  e.g., a list that holds numbers or other lists

(define xs (list 4 5 6))
(define ys (list (list 4 (list 5 0)) 6 7 (list 8) 9 2 3 (list 0 1)))
(define zs (list (list "hi" (list 4)) 5 (list 7 2)))

; this function sums lists of (numbers or lists of (numbers or ...)),
; but it does assume it only encounters lists or numbers (else run-time error)
(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum1 (cdr xs)))
          (+ (sum1 (car xs)) (sum1 (cdr xs))))))


(sum1 xs)
(sum1 ys)
;(sum1 zs)

; this version does not fail on non-lists -- it treats them as 0
(define (sum2 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum2 (cdr xs)))
          (if (list? (car xs))
              (+ (sum2 (car xs)) (sum2 (cdr xs)))
              (sum2 (cdr xs))))))

(sum2 zs)