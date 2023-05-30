; Programming Languages, Dan Grossman
; Section 5: Using and Defining Streams

; [same code for segments on using streams and defining streams]

#lang racket

(provide (all-defined-out))

;; define some streams

;(define ones-really-bad (cons 1 ones-really-bad))
;(define ones-bad (lambda () (cons 1 (ones-bad)))) ; infinite loop

(define ones (lambda () (cons 1 ones)))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))


;; code that uses streams

powers-of-two ; returns a procedure -> #<procedure:powers-of-two>
(powers-of-two) ; return a pair with 2 as the car and a procedure as the cdr -> '(2 . #<procedure:...8_using_streams.rkt:22:34>)
(car (powers-of-two)) ; -> 2
((cdr(powers-of-two))) ; -> '(4 . #<procedure:...8_using_streams.rkt:22:34>)
(car ((cdr(powers-of-two)))) ; -> 4
(car ((cdr ((cdr(powers-of-two)))))) ; -> 8

;; counts how many elements you need to process until tester returns true
(define (number-until stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)]) ; call the stream and put the result in pr
                  (if (tester (car pr))
                      ans                         ; if tester returns true, return ans             
                      (f (cdr pr) (+ ans 1)))))]) ; otherwise call f on the next stream and increment ans
    (f stream 1)))

(define four (number-until powers-of-two (lambda (x) (= x 16))))
(define forty-four (number-until powers-of-two (lambda (x) (> x 10000000000000))))


(define (stream-maker fn arg)
  (letrec ([f (lambda (x) 
                (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))
(define nats2  (stream-maker + 1))
(define powers2 (stream-maker * 2))


