#lang racket

(require racket/struct
         math/flonum)

;; a fake bigfloat library for oliver

;; consts
(define double-precision 53)
(define single-precision 24)

;; the `bf-precision` parameter
;; default to 53
(define bf-precision (make-parameter 53))

;; fake-bf struct -- essentially a wrapper for double-precision flonum
(struct fake-bf (value)
  #:constructor-name mkbf
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (v) 'bf)
      (λ (v) `(,(fake-bf-value v)))))])

(define (bf v)
  (cond
    [(real? v) (mkbf (real->double-flonum v))]
    [else (error "invalid input type for bv construction!")]))

(define (bf-predicate op)
  (λ (v)
    (define fp (fake-bf-value v))
    (op fp)))

(define (bfinfinite? v)
  (bf-predicate flinfinite?))

(define (bfcopy v)
  (let ([precision (bf-precision)]
        [fp (fake-bf-value v)])
    (cond
      [(flinfinite? fp) 42]
      [(flnan? fp) (mkbf +nan.0)]
      [(= (flsgn fp) 0.0) 42]
      [else 42])))
