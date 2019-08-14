#lang racket

(require racket/struct
         math/flonum)

(provide bf-precision
         bigfloat-precision
         bf
         bfinfinite?
         bfnan?
         bfpositive?
         bfnegative?
         bfzero?
         bf+
         bf-
         bf*
         bf/
         bfabs
         bfsqrt
         bf>
         bf<
         bf>=
         bf<=
         bf=
         bfcopy
         bigfloat-signbit
         )

;; a fake bigfloat library for oliver

;; consts
(define single-precision 24)
(define double-precision 53)

;; the `bf-precision` parameter
;; default to 53
(define bf-precision (make-parameter 53))
(define (single-precision?) (= (bf-precision) single-precision))
(define (double-precision?) (= (bf-precision) double-precision))

;; fake-bf struct -- essentially a wrapper for flonum
(struct fake-bf (value precision)
  #:constructor-name mkbf
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (v) 'bf)
      (λ (v) `(,(fake-bf-value v) ,(fake-bf-precision v)))))])

(define (bigfloat-precision bf)
  (fake-bf-precision bf))

(define (bf v)
  (cond
    [(real? v)
     (cond
       [(single-precision?) (mkbf
                            (real->single-flonum v)
                            (bf-precision))]
       [(double-precision?) (mkbf
                            (real->double-flonum v)
                            (bf-precision))]
       [else (error "unsupported format!")])]
    [else (error "invalid input type for bv construction!")]))

;; unary predicates
(define (bf-uni-pred op)
  (λ (v)
    (op (fake-bf-value v))))

(define (bfinfinite? v)
  ((bf-uni-pred infinite?) v))
(define (bfnan? v)
  ((bf-uni-pred nan?) v))
(define (bfpositive? v)
  ((bf-uni-pred positive?) v))
(define (bfnegative? v)
  ((bf-uni-pred negative?) v))
(define (bfzero? v)
  ((bf-uni-pred zero?) v))

;; binary predicates
(define (bf-bin-pred op)
  (λ (v1 v2)
    (if (and
         (= (bf-precision)
            (bigfloat-precision v1))
         (= (bf-precision)
            (bigfloat-precision v2)))
        (op v1 v2)
        (error "precision mismatches!"))))

(define (bf> v1 v2)
  ((bf-bin-pred >) v1 v2))
(define (bf< v1 v2)
  ((bf-bin-pred <) v1 v2))
(define (bf>= v1 v2)
  ((bf-bin-pred >=) v1 v2))
(define (bf<= v1 v2)
  ((bf-bin-pred <=) v1 v2))
(define (bf= v1 v2)
  ((bf-bin-pred =) v1 v2))

;; unary arithmetic ops
(define (bfabs v)
  (mkbf
   (abs (fake-bf-value v))
   (fake-bf-precision v)))

(define (bf-uni-arith op)
  (λ (v)
    (if 
     (= (bf-precision)
        (bigfloat-precision v))
     (op
      (fake-bf-value v))
     (error "precision mismatches!"))))

(define (bfsqrt v)
  ((bf-uni-arith sqrt) v))

;; binary arithmetic ops
(define (bf-bin-arith op)
  (λ (v1 v2)
    (if (and
         (= (bf-precision)
            (bigfloat-precision v1))
         (= (bf-precision)
            (bigfloat-precision v2)))
        (op
         (fake-bf-value v1)
         (fake-bf-value v2))
        (error "precision mismatches!"))))

(define (bf+ v1 v2)
  ((bf-bin-arith +) v1 v2))
(define (bf- v1 v2)
  ((bf-bin-arith +) v1 v2))
(define (bf* v1 v2)
  ((bf-bin-arith +) v1 v2))
(define (bf/ v1 v2)
  ((bf-bin-arith +) v1 v2))


(define (bfcopy v)
  (define fp (fake-bf-value v))
  (cond
    [(single-precision?) (mkbf
                          (real->single-flonum fp)
                          (bf-precision))]
    [(double-precision?) (mkbf
                          (fl fp)
                          (bf-precision))]
    [else (error "unsupported precision!")]))

(define (bigfloat-signbit v)
  (define fp-val (fake-bf-value v))
  (define precision (bigfloat-precision v))
  (define (get-signbit bw)
    (bitwise-and
     (arithmetic-shift (bigfloat->nat v) (- 0 (- bw 1)))
     1))
  (cond
    [(nan? fp-val) (error "what's the sign bit of a NaN?")]
    [(= precision single-precision) (get-signbit 32)]
    [(= precision double-precision) (get-signbit 64)]
    [else (error "unsupported precision!")]))

(define (bigfloat->nat v)
  (define fp-val (fake-bf-value v))
  (define precision (bigfloat-precision v))
  (define (get-nat bw)
    (integer-bytes->integer
     (real->floating-point-bytes fp-val bw)
     #f))
  (cond
    [(nan? fp-val) (error "what's the bit-representation of a NaN?")]
    [(= precision single-precision) (get-nat 4)]
    [(= precision double-precision) (get-nat 8)]
    [else (error "unsupported precision!")]))

(define (nat->bigfloat v precision)
  42)
