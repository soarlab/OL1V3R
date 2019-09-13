#lang racket

(require ;math/bigfloat
         "bit-vec.rkt"
         "fake-bigfloat.rkt")

(provide BitVec->bf bf->BitVec bf-round-to-subnormal)

(define (bf-round-to-subnormal v exp-width sig-width)
  v)
#;(define (bf-round-to-subnormal v exp-width sig-width)
  (define subnormal-min
    (BitVec->bf
     (mkBV
      (+ exp-width sig-width)
      1)
     exp-width
     sig-width))
  (define pv (bfabs v))
  (let-values
      ([(s1 p1) (bigfloat->sig+exp pv)]
       [(s2 p2) (bigfloat->sig+exp subnormal-min)])
    (let ([r (round
              (/
               (* s1 (expt 2 p1))
               (* s2 (expt 2 p2))))])
      (cond
        [(< r 1) (if (bfpositive? v)
                     (bf 0.0)
                     (bf -0.0))]
        [else (let ([rv (BitVec->bf
                         (mkBV (+ exp-width sig-width) r)
                         exp-width
                         sig-width)])
                (if (bfpositive? v)
                    rv
                    (parameterize ([bf-precision sig-width])
                      (bf* (bf -1.0) rv))))]))))

(define (BitVec->bf bv exp-width sig-width)
  (nat->bigfloat
   (BitVec-value bv)
   sig-width))

#;(define (BitVec->bf bv exp-width sig-width)
  (if (= (BitVec-width bv) (+ exp-width sig-width))
      (parameterize ([bf-precision sig-width])
        (let* ([bv-value (BitVec-value bv)]
               [sig-width-wo (- sig-width 1)]
               [sig-bits (modulo bv-value (expt 2 sig-width-wo))]
               [exp-bits (modulo (arithmetic-shift bv-value (- 0 sig-width-wo))
                                 (expt 2 exp-width))]
               [exp-bias (- (expt 2 (- exp-width 1)) 1)]
               [sign-bit (bitwise-bit-set? bv-value (+ exp-width sig-width-wo))])
          (cond
            [(= (- (expt 2 exp-width) 1) exp-bits) ; exp all 1s
             (if (= sig-bits 0)
                 (bfcopy (if sign-bit -inf.bf +inf.bf))
                 (bfcopy +nan.bf))]
            [(= exp-bits 0) ; exp all 0s
             (if (= sig-bits 0)
                 (bfcopy (if sign-bit -0.bf 0.bf))
                 (bf (if sign-bit (- 0 sig-bits) sig-bits)
                     (- (+ (- 0 exp-bias) 1) sig-width-wo)))]
            [else (bf (let ([sig (+ (expt 2 sig-width-wo) sig-bits)])
                        (if sign-bit (- 0 sig) sig))
                      (- (- exp-bits exp-bias)
                         sig-width-wo))])))
      (error "Bit width doesn't match!")))

(define (bf->BitVec fp-val exp-width sig-width)
  (mkBV (+ exp-width sig-width) (bigfloat->nat fp-val)))

#;(define (bf->BitVec fp-val exp-width sig-width)
  (define sig-width-wo (- sig-width 1))
  (define exp-bias (- (expt 2 (- exp-width 1)) 1))
  (define sign-wrap (λ (v sign-bit)
                      (+
                       v
                       (* sign-bit (expt 2 (+ exp-width sig-width-wo))))))
  (mkBV (+ exp-width sig-width)
        (cond
          [(bfnan? fp-val) (error "no unique bv representation for nans!")]
          [(bfinfinite? fp-val) (sign-wrap
                                 (arithmetic-shift (- (expt 2 exp-width) 1) sig-width-wo)
                                 (bigfloat-signbit fp-val))]
          [(bfzero? fp-val) (sign-wrap 0 (bigfloat-signbit fp-val))]
          [else
           (let-values ([(sig exp) (bigfloat->sig+exp fp-val)])
             (sign-wrap
              (let ([sig (abs sig)])
                (if (and (>= sig (expt 2 sig-width-wo))
                         (< sig (expt 2 sig-width)))
                    (if (>= exp (- (- 1 exp-bias) sig-width-wo)) ; normals
                        (+
                         (arithmetic-shift (+ (+ exp exp-bias) sig-width-wo) sig-width-wo)
                         (modulo sig (expt 2 (- sig-width 1))))
                        (/ sig (expt 2 (- (- 1 (+ exp-bias sig-width-wo)) exp))))
                    (error "unrecognized format for sig+exp!~a" fp-val)))
              (bigfloat-signbit fp-val)))])))
  