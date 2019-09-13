#lang typed/racket

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
         bigfloat->nat
         nat->bigfloat
         bigfloat->flonum
         )

;; a fake bigfloat library for oliver

;; consts
(define single-precision 24)
(define double-precision 53)

;; the `bf-precision` parameter
;; default to 53
(: bf-precision (Parameterof Natural))
(define bf-precision (make-parameter 53))
(define (single-precision?) (= (bf-precision) single-precision))
(define (double-precision?) (= (bf-precision) double-precision))

;; fake-bf struct -- essentially a wrapper for flonum
(struct fake-bf ([value : Inexact-Real]
                 [precision : Natural])
  #:constructor-name mkbf
  #:transparent
  ;#:methods gen:custom-write
  #;[(define write-proc
     (make-constructor-style-printer
      (λ (v) 'bf)
      (λ (v) `(,(fake-bf-value v) ,(fake-bf-precision v)))))])

(define (bigfloat-precision bf)
  (fake-bf-precision bf))

(: bf (-> Real fake-bf))
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
       [else (error "unsupported format!~a" (bf-precision) v)])]
    [else (error "invalid input type for bv construction!")]))

;; unary predicates
(: bf-uni-pred (-> (-> Inexact-Real Boolean) (-> fake-bf Boolean)))
(define (bf-uni-pred op)
  (λ (v)
    (op (fake-bf-value v))))

(: bfinfinite? (-> fake-bf Boolean))
(define (bfinfinite? v)
  ((bf-uni-pred infinite?) v))
(: bfnan? (-> fake-bf Boolean))
(define (bfnan? v)
  ((bf-uni-pred nan?) v))
(: bfpositive? (-> fake-bf Boolean))
(define (bfpositive? v)
  ((bf-uni-pred positive?) v))
(: bfnegative? (-> fake-bf Boolean))
(define (bfnegative? v)
  ((bf-uni-pred negative?) v))
(: bfzero? (-> fake-bf Boolean))
(define (bfzero? v)
  ((bf-uni-pred zero?) v))

;; binary predicates
(: bf-bin-pred (-> (-> Inexact-Real Inexact-Real Boolean)
                   (-> fake-bf fake-bf Boolean)))
(define (bf-bin-pred op)
  (λ (v1 v2)
    (if (=
         (bigfloat-precision v1)
         (bigfloat-precision v2))
        (op
         (fake-bf-value v1)
         (fake-bf-value v2))
        (error "precision mismatches!~a"
               (bigfloat-precision v1)
               (bigfloat-precision v2)))))

(: bf> (-> fake-bf fake-bf Boolean))
(define (bf> v1 v2)
  ((bf-bin-pred >) v1 v2))
(: bf< (-> fake-bf fake-bf Boolean))
(define (bf< v1 v2)
  ((bf-bin-pred <) v1 v2))
(: bf>= (-> fake-bf fake-bf Boolean))
(define (bf>= v1 v2)
  ((bf-bin-pred >=) v1 v2))
(: bf<= (-> fake-bf fake-bf Boolean))
(define (bf<= v1 v2)
  ((bf-bin-pred <=) v1 v2))
(: bf= (-> fake-bf fake-bf Boolean))
(define (bf= v1 v2)
  ((bf-bin-pred =) v1 v2))

;; unary arithmetic ops
(: bfabs (-> fake-bf fake-bf))
(define (bfabs v)
  (mkbf
   (abs (fake-bf-value v))
   (fake-bf-precision v)))

#;(: bf-uni-arith (-> (case->
                     (-> Single-Flonum Single-Flonum)
                    (-> fake-bf fake-bf))))
#;(define (bf-uni-arith op)
  (λ (v)
    (if 
     (= (bf-precision)
        (bigfloat-precision v))
     (mkbf
      (op (fake-bf-value v))
      (bf-precision))
     (error "precision mismatches!~a"
            (bf-precision)
            (bigfloat-precision v)))))

(: bfsqrt (-> fake-bf fake-bf))
#;(define (bfsqrt v)
  ((bf-uni-arith sqrt) v))

#;(: sqrt-wrapper (case->
                 (-> Nonnegative-Flonum Nonnegative-Flonum)
                 (-> Nonnegative-Single-Flonum Nonnegative-Single-Flonum)
                 (-> Negative-Flonum Flonum-Nan)
                 (-> Negative-Single-Flonum Single-Flonum-Nan)
                 (-> Flonum-Nan Flonum-Nan)
                 (-> Single-Flonum-Nan Single-Flonum-Nan)))
(: sqrt-wrapper (-> Inexact-Real Inexact-Real))
(define (sqrt-wrapper x)
  (cond
    [(nan? x) x]
    [(negative? x) (if (single-flonum? x)
                       +nan.f
                       +nan.0)]
    [else (sqrt x)]))

(define (bfsqrt v)
  (define fp (fake-bf-value v))
  (define precision (bigfloat-precision v))
  (cond
    [(< fp 0.0) (parameterize ([bf-precision precision])
                  (bf +nan.0))]
    [else (mkbf (sqrt-wrapper fp) precision)]))

;; binary arithmetic ops
(: bf-bin-arith (-> (-> Inexact-Real Inexact-Real Inexact-Real)
                    (-> fake-bf fake-bf fake-bf)))
(define (bf-bin-arith op)
  (λ (v1 v2)
    (if (and
         (= (bf-precision)
            (bigfloat-precision v1))
         (= (bf-precision)
            (bigfloat-precision v2)))
        (mkbf
         (op
          (fake-bf-value v1)
          (fake-bf-value v2))
         (bf-precision))
        (error "precision mismatches!~a"
               (bf-precision)
               (bigfloat-precision v1)
               (bigfloat-precision v2)))))

(: bf+ (-> fake-bf fake-bf fake-bf))
(define (bf+ v1 v2)
  ((bf-bin-arith +) v1 v2))
(: bf- (-> fake-bf fake-bf fake-bf))
(define (bf- v1 v2)
  ((bf-bin-arith -) v1 v2))
(: bf* (-> fake-bf fake-bf fake-bf))
(define (bf* v1 v2)
  ((bf-bin-arith *) v1 v2))
(: bf/ (-> fake-bf fake-bf fake-bf))
(define (bf/ v1 v2)
  ((bf-bin-arith /) v1 v2))

(: bfcopy (-> fake-bf fake-bf))
(define (bfcopy v)
  (define fp (fake-bf-value v))
  (cond
    [(single-precision?) (mkbf
                          (real->single-flonum fp)
                          (bf-precision))]
    [(double-precision?) (mkbf
                          (fl fp)
                          (bf-precision))]
    [else (error "unsupported precision!~a" (bf-precision))]))

(: bigfloat-signbit (-> fake-bf Natural))
(define (bigfloat-signbit v)
  (define fp-val (fake-bf-value v))
  (define precision (bigfloat-precision v))
  (: get-signbit (-> (U 32 64) Natural))
  (define (get-signbit bw)
    (bitwise-and
     (arithmetic-shift (bigfloat->nat v) (- 0 (- bw 1)))
     1))
  (cond
    [(nan? fp-val) (error "what's the sign bit of a NaN?")]
    [(= precision single-precision) (get-signbit 32)]
    [(= precision double-precision) (get-signbit 64)]
    [else (error "unsupported precision!~a" precision)]))

(: bigfloat->nat (-> fake-bf Natural))
(define (bigfloat->nat v)
  (define fp-val (fake-bf-value v))
  (define precision (bigfloat-precision v))
  (: get-nat (-> (U 4 8) Natural))
  (define (get-nat bw)
    (integer-bytes->integer
     (real->floating-point-bytes fp-val bw)
     #f))
  (cond
    [(nan? fp-val) (error "what's the bit-representation of a NaN?")]
    [(= precision single-precision) (get-nat 4)]
    [(= precision double-precision) (get-nat 8)]
    [else (error "unsupported precision!~a" precision)]))

(: nat->bigfloat (-> Natural Natural fake-bf))
(define (nat->bigfloat v precision)
  (: get-float (-> (U 4 8) Real))
  (define (get-float bw)
    (floating-point-bytes->real (integer->integer-bytes v bw #f)))
  (cond
    [(= precision single-precision) (mkbf (real->single-flonum (get-float 4)) precision)]
    [(= precision double-precision) (mkbf (real->double-flonum (get-float 8)) precision)]
    [else (error "unsupported precision!~a" precision)]))

(define (bigfloat->flonum val)
  (fake-bf-value val))
