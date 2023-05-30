
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Number Number Number -> ListOfNumber
;; Produces a list of numbers from low to high (inclusive) separated by stride and in sorted order.
;; ASSUME: stride > 0

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

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

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([next (s)]) ;call the stream and put the result in next
        (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))


;; Some streams

(define ones (lambda () (cons 1 ones))) ; stream of ones

(define nats ; stream of natural numbers, starting from one 
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (let ([num (if (= (remainder x 5) 0) (- x) x)])
                  (cons num (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (lambda () (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog)))))

;; Stream Any -> Stream
;; If s would produce v for its i-th element,
;; then (stream-add-x s x) would produce the pair (x . v) for its i-th element.

(define (stream-add-x s x)
  (lambda () (let [(next (s))] ;call the stream and put the result in next
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
                           (lambda () (helper (+ n 1)))))])
    ; helper function calls itself with (+ n 1) inside a thunk.
    (lambda () (helper 0))))

;; Value Vector -> pair or #f
;; Behave like Racket's assoc library function except
;; (1) it processes a vector (Racket's array) instead of a list,
;; (2) it allows vector elements not to be pairs in which case it skips them
;; (3) it always takes exactly two arguments. Process the vector elements in order starting from 0.

(define (vector-assoc v vec)
  (letrec ([search-vec (lambda (i)
                         (if (>= i (vector-length vec))
                             #f
                             (let ([current (vector-ref vec i)])
                               (if (and (pair? current) (equal? (car current) v))
                                   current
                                   (search-vec (+ i 1))))))])
    (search-vec 0)))


;; List Integer -> Function
;; produces a function of one argument that acts like Racket's assoc, which has an n-element vector-cache.
;; Assumes: n is positive

;; With print expression
;(define (cached-assoc xs n)
;  (let ([cache (make-vector n #f)] ; cache  -> vector of n size, starts empty (all elements #f).
;        [i 0])
;    (lambda(x)
;      (let ([ans (vector-assoc x cache)])    ; check if it's already in the memo
;        (if ans
;            (begin (display "Cache hit. ")
;                   (display "Current cache: ")
;                   (display cache)         
;                   (newline)
;                   ans)                      ; if it's already in the memo, return the result
;            (let ([new-ans (assoc x xs)])    ; if it's not already in the memo, compute it
;              (begin
;                (display "Cache miss: ")
;                (display "Current cache: ")
;                (display cache)         
;                (newline)
;                (vector-set! cache i new-ans) ; put the new computed result in the cache
;                (if (= i (- n 1))             ; if we reached the last index
;                    (set! i 0)                ; then make the index 0
;                    (set! i (+ i 1)))         ; otherwise increment the index
;                new-ans)))))                  ; return the result
;    ))

;(define c (cached-assoc '((1 . "one") (2 . "two") (3 . "three") (4 . "four")) 2))
;(c 1) 
;(c 2)
;(c 3)
;(c 4)
;(c 4)


(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)] ; cache  -> vector of n size, starts empty (all elements #f).
        [i 0])
    (lambda (x)
      (let ([ans (vector-assoc x cache)])     ; check if it's already in the memo
        (if ans
            ans                               ; if it's already in the memo, return the result
            (let ([new-ans (assoc x xs)])     ; if it's not already in the memo, compute it
              (begin
                (vector-set! cache i new-ans) ; put the new computed result in the cache
                (if (= i (- n 1))             ; if we reached the last index
                    (set! i 0)                ; then make the index 0
                    (set! i (+ i 1)))         ; otherwise increment the index
                new-ans)))))                  ; return the result
    ))

;; challenge Problem

(define-syntax while-less
  (syntax-rules (do)
    ((while-less x do y)
     (let ([z x])
       (letrec ([loop (lambda ()
                        (let ([w y])
                          (if (or (not (number? w)) (>= w z))
                              #t
                              (loop))))])
         (loop))))))

;(define a 2)
;(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
;(println a)
;(while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
;(println a)
