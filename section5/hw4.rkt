
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Number Number Number -> ListOfNumber
;; Produces a list of numbers from low to high (inclusive) separated by stride and in sorted order.
;; ASSUME: stride > 0

(define (sequence low high stride)
  (letrec ([sequence-helper (lambda (current rsf)
                              (if (> current high)
                                  (reverse rsf)
                                  (sequence-helper (+ current stride) (cons current rsf))))])
    (sequence-helper low null)))

;; ListOfString String -> ListOfString
;; Each element of the output is the corresponding element of the input appended
;; with suffix (with no extra space between the element and suffix)

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; List Number -> ListElement
;; return the i-th element of the list where we count from zero and i is the remainder
;; produced when dividing n by the list's length.

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [else (let ([i (remainder n (length xs))])
                (car (list-tail xs i)))]
        ))


;; Stream Number -> List
;; returns a list holding the first n values produced by s in order
;; ASSUME: n > 0

(define (stream-for-n-steps s n)
  (letrec ([helper (lambda (stream ans)
                     (let ([pr (stream)])  ;call the stream and put the result in pr
                       (if (< (length ans) n)
                           (helper (cdr pr) (cons (car pr) ans))
                           (reverse ans))))])
    (helper s null)))


;; Some streams

(define ones (lambda () (cons 1 ones))) ; stream of ones

(define nats ; stream of natural numbers, starting from one 
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (let ([num (if (= (remainder x 5) 0) (* x -1) x)])
                  (cons num (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (lambda () (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog)))))

;; Stream Any -> Stream
;; If s would produce v for its i-th element,
;; then (stream-add-x s x) would produce the pair (x . v) for its i-th element.

(define (stream-add-x s x)
  (lambda () (letrec [(next (s))]
               (cons (cons x (car next))
                     (stream-add-x (cdr next) x)))))

(define (stream-add-zero s)
  (stream-add-x s 0))


;; List List -> Stream
;; Produce a stream where the elements are pairs where the first part is from xs and the second part is from ys.
;; ASUME: Both lists are non empty

(define (cycle-lists xs ys)
  (letrec ([helper (lambda (n)
                     (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                           (lambda () (helper (+ n 1)))))]) ; helper function callsitself with (+ n 1) inside a thunk.
    (lambda () (helper 0))))

;; Value Vector -> pair or #f
;; Behave like Racket's assoc library function except
;; (1) it processes a vector instead of a list,
;; (2) it allows vector elements not to be pairs in which case it skips them
;; (3) it always takes exactly two arguments. Process the vector elements in order starting from 0.

(define vector-assoc
  

(define xs (list (cons 1 2) (cons 3 4) (cons 5 6)))
(assoc 1 xs)  ; '(1 . 2)
(assoc 3 xs)  ; '(3 . 4)
(assoc 42 xs) ; #f
