; Programming Languages, Dan Grossman
; Section 6: Datatypes With Struct

#lang racket

(provide (all-defined-out))

(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)

(define (eval-exp e)
  (cond [(const? e) e] ; note returning an exp, not a number
        [(negate? e) (const (- (const-int (eval-exp (negate-e e)))))]
        [(add? e) (let ([v1 (const-int (eval-exp (add-e1 e)))]
                        [v2 (const-int (eval-exp (add-e2 e)))])
                    (const (+ v1 v2)))]
        [(multiply? e) (let ([v1 (const-int (eval-exp (multiply-e1 e)))]
                             [v2 (const-int (eval-exp (multiply-e2 e)))])
                         (const (* v1 v2)))]
        [#t (error "eval-exp expected an exp")]))

(println add)    ; #<procedure:add>
(println add?)   ; <procedure:add?>
(println add-e1) ; <procedure:add-e1>
(println add-e2) ; <procedure:add-e2>xx

(define add-example (add (const 3) (const 4)))
(println add-example)            ;(add (const 3) (const 4))
(println (add? add-example))     ; #t
(println (eval-exp add-example)) ; (const 7)

(println (eval-exp (multiply (negate (add (const 2) (const 2))) (const 7))))
(println (pair? (cons 1 2)))
(println (number? 34))