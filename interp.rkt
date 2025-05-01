#lang racket

(provide interp)

;; Interpreter semantics:
;; 0 and #f are treated as falsy
;; Everything else is treated as truthy

;; Integer type guard 
(define (only-int v)
  (if (integer? v) v (raise-user-error 'interp "Expected integer, got ~v" v)))

;; Boolean type guard  
(define (only-bool v)
  (if (boolean? v) v (raise-user-error 'interp "Expected boolean, got ~v" v)))

;; Expr -> Value
;; Interpret given expression
(define (interp e)
  (match e
    [(? integer?)                 e]
    [(? boolean?)                 e]
    
    [`(add1 ,e)                   (+ (only-int (interp e)) 1)]
    [`(sub1 ,e)                   (- (only-int (interp e)) 1)]
    
    [`(+ ,e1 ,e2)                 (+ (only-int (interp e1)) (only-int (interp e2)))]
    [`(- ,e1 ,e2)                 (- (only-int (interp e1)) (only-int (interp e2)))]
    [`(* ,e1 ,e2)                 (* (only-int (interp e1)) (only-int (interp e2)))]
    [`(/ ,e1 ,e2)                 (interp-div (only-int (interp e1)) (only-int (interp e2)))]

    [`(if ,e1 ,e2 ,e3)            (interp-if e1 e2 e3)]
    [`(and ,e1 ,e2)               (interp-and e1 e2)]

    [`(truthy? ,e1)               (interp-truthy? e1)]
    [`(falsy? ,e1)                (interp-falsy? e1)]

    [`(bool->num ,e1)             (interp-to-num (only-bool e1))]
    [`(value->bool ,e1)           (interp-truthy? (only-int e1))]
    [_                            (raise-user-error 'interp "Invalid syntax: ~v" e)]))

;; Value -> Bool
;; Determine if value is truthy
(define (interp-truthy? v)
  (match v
    [#f   #f]
    [0    #f]
    [_    #t]))

;; Value -> Bool
;; Determine if value is falsy
(define (interp-falsy? v)
  (match v
    [#f   #t]
    [0    #t]
    [_    #f]))

;; Bool -> Value
;; Convert boolean to equivalent value
(define (interp-to-num v)
  (match v
    [#f   0]
    [#t   1]))

;; Value -> Value
;; Evaluate the quotient of two values
(define (interp-div v1 v2)
  (match v2
    [0            (raise-user-error 'interp "Divison by 0 not allowed")]
    [_            (quotient v1 v2)]))

;; If conditional
(define (interp-if e1 e2 e3)
  (if (interp-truthy? (interp e1))
      (interp e2)
      (interp e3)))

;; And conditional
(define (interp-and e1 e2)
  (if (interp-falsy? (interp e1))
      #f
      (interp e2)))

;; Or conditional
(define (interp-or e1 e2)
  (match (interp e1)
    [#f           #f]
    [_            (interp e2)]))
