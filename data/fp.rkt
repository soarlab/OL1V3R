#lang racket

(require math/bigfloat
         racket/struct
         "bit-vec.rkt"
         "fp-utils.rkt")

(provide (all-defined-out))

;; sig-width includes the hidden bit
(struct FloatingPoint (exp-width sig-width value)
  #:constructor-name mkFP
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (fp) 'FloatingPoint)
      (λ (fp) `(,(FloatingPoint-exp-width fp)
                ,(FloatingPoint-sig-width fp)
                ,(FloatingPoint-value fp)))))])

(define FloatingPoint->FPConst
  (λ (fp)
    (define exp-width (FloatingPoint-exp-width fp))
    (define sig-width (FloatingPoint-sig-width fp))
    (cond
      [(fp/nan? fp) `(_ NaN ,exp-width ,sig-width)]
      [(fp/infinity? fp) `(_
                           ,(if (fp/positive? fp)
                                '+oo
                                '-oo)
                           ,exp-width
                           ,sig-width)]
      [(fp/zero? fp) `(_
                       ,(if (fp/positive? fp)
                            '+zero
                            '-zero)
                       ,exp-width
                       ,sig-width)]
      [else (let ([abs-value
                   (bitwise-and
                    (BitVec-value (FloatingPoint->BitVec fp))
                    (- (expt 2 (- (+ exp-width sig-width) 1)) 1))]
                  [sig-width-wo (- sig-width 1)])
              `(fp
                ,(BitVec->BVConst
                  (if (fp/positive? fp)
                      (mkBV 1 0)
                      (mkBV 1 1)))
                ,(BitVec->BVConst
                  (mkBV
                   exp-width
                   (arithmetic-shift abs-value (- 0 sig-width-wo))))
                ,(BitVec->BVConst
                  (mkBV
                   sig-width-wo
                   (bitwise-and
                    (- (expt 2 sig-width-wo) 1)
                    abs-value)))))])))

(define initialize/fp
  (λ (exp-width sig-width)
    (mkFP
     exp-width
     sig-width
     (parameterize ([bf-precision sig-width])
       (bf 0.0)))))

(define random/fp
  (λ (exp-width sig-width)
    (if (coin-flip 0.8)
        (car (shuffle
              `(, (real->FloatingPoint +inf.f exp-width sig-width)
                , (real->FloatingPoint -inf.f exp-width sig-width)
                , (real->FloatingPoint +nan.f exp-width sig-width))))
        (BitVec->FloatingPoint
         (random/bv (+ exp-width sig-width))
         exp-width
         sig-width))))

(define exp/±
  (λ (fp)
    (define two (real->FloatingPoint 2.0
                 (FloatingPoint-exp-width fp)
                 (FloatingPoint-sig-width fp)))
    `(,(eval/fpmul fp two)
      ,(eval/fpdiv fp two))))

(define (sig/± fp)
  (define exp-width (FloatingPoint-exp-width fp))
  (define sig-width (FloatingPoint-sig-width fp))
  (define br (FloatingPoint->BitVec fp))
  (define one (mkBV (BitVec-width br) 1))
  `(,(BitVec->FloatingPoint (eval/bvadd br one) exp-width sig-width)
    ,(BitVec->FloatingPoint (eval/bvsub br one) exp-width sig-width)))

(define get/fp-extended-neighbors
  (λ (fp)
    (define exp-width (FloatingPoint-exp-width fp))
    (define sig-width (FloatingPoint-sig-width fp))
    (define fp-val (FloatingPoint-value fp))
    (cond
      [(fp/nan? fp) `(,(random/fp exp-width sig-width))]
      [else
       (append
        (let* ([ns (map
                    (λ (bv) (BitVec->FloatingPoint bv exp-width sig-width))
                    (get/bv-extended-neighbors (FloatingPoint->BitVec fp)))]
               [fns (filter (λ (fp) (and (not (fp/nan? fp))
                                         (not (fp/infinity? fp)))) ns)])
          (if (= (length ns) (length fns))
              fns
              (cons (real->FloatingPoint +nan.f exp-width sig-width)
                    (cons (real->FloatingPoint +inf.f exp-width sig-width)
                          (cons (real->FloatingPoint -inf.f exp-width sig-width) fns)))))
        (exp/± fp))])))

(define (get/fp-neighbor-flip-sign fp)
  `(,(eval/fpneg fp)))

(define (get/fp-neighbors val ni)
  (cond
    [(= ni 1) (if (fp/nan? val) '() (get/fp-neighbor-flip-sign val))]
    [(= ni 2) (if (fp/nan? val) '() (get/fp-neighbor-exp val))]
    [(= ni 3) (if (fp/nan? val)
                  (get/fp-extended-neighbors val)
                  (get/fp-neighbor-sig val))]))

(define (get/fp-neighbor-exp fp)
  (define exp-width (FloatingPoint-exp-width fp))
  (define sig-width (FloatingPoint-sig-width fp))
  (define flip-exp
    (map
     (λ (bv) (BitVec->FloatingPoint bv exp-width sig-width))
     (let ([sig-width-wo (- sig-width 1)])
       (get/1-exchange-range
        (FloatingPoint->BitVec fp)
        sig-width-wo
        (+ sig-width-wo exp-width)))))
  (append flip-exp (exp/± fp)))

(define (get/fp-neighbor-sig fp)
  (define exp-width (FloatingPoint-exp-width fp))
  (define sig-width (FloatingPoint-sig-width fp))
  (define flip-sig
    (map
     (λ (bv) (BitVec->FloatingPoint bv exp-width sig-width))
     (let ([sig-width-wo (- sig-width 1)])
       (get/1-exchange-range
        (FloatingPoint->BitVec fp)
        0
        sig-width-wo))))
  (append flip-sig (sig/± fp)))
   
;; floating-point arithmetic
#;(define fp/round-to-subnormal
  (λ (v exp-width sig-width)
    (define subnormal-min
      (FloatingPoint-value
       (BitVec->FloatingPoint
        (mkBV
         (+ exp-width sig-width)
         1)
        exp-width
        sig-width)))
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
                       (real->FloatingPoint 0.0 exp-width sig-width)
                       (real->FloatingPoint -0.0 exp-width sig-width))]
          [else (let ([rv (BitVec->FloatingPoint
                           (mkBV (+ exp-width sig-width) r)
                           exp-width
                           sig-width)])
                  (if (bfpositive? v)
                      rv
                      (eval/fpneg rv)))])))))

(define (fp/round-to-subnormal v exp-width sig-width)
  (mkFP exp-width sig-width (bf-round-to-subnormal v exp-width sig-width)))

(define fp/result-infinity?
  (λ (v exp-width sig-width)
    (define pv (bfabs v))
    (bf>
     pv
     (FloatingPoint-value
      (get/maximum-normal exp-width sig-width)))))

(define get/+inf
  (λ (exp-width sig-width)
    (mkFP
     exp-width
     sig-width
     (parameterize ([bf-precision sig-width])
      (bfcopy +inf.bf)))))

(define get/-inf
  (λ (exp-width sig-width)
    (mkFP
     exp-width
     sig-width
     (parameterize ([bf-precision sig-width])
      (bfcopy -inf.bf)))))

(define get/maximum-normal
  (λ (exp-width sig-width)
    (define +inf-bv (FloatingPoint->BitVec (get/+inf exp-width sig-width)))
    (BitVec->FloatingPoint
     (mkBV
      (+ exp-width sig-width)
      (- (BitVec-value +inf-bv) 1))
      exp-width
      sig-width)))

(define get/maximum-subnormal
  (λ (exp-width sig-width)
    (BitVec->FloatingPoint
     (mkBV
      (+ exp-width sig-width)
      (- (expt 2 (- sig-width 1)) 1))
      exp-width
      sig-width)))

(define ((eval/fparith/binop op) fp1 fp2)
  (define v1 (FloatingPoint-value fp1))
  (define v2 (FloatingPoint-value fp2))
  (define exp-w-1 (FloatingPoint-exp-width fp1))
  (define exp-w-2 (FloatingPoint-exp-width fp2))
  (define sig-w-1 (FloatingPoint-sig-width fp1))
  (define sig-w-2 (FloatingPoint-sig-width fp2))
  (if (and
       (=
        (bigfloat-precision v1)
        (bigfloat-precision v2))
       (and
        (= exp-w-1 exp-w-2)
        (= sig-w-1 sig-w-2)))
      ;; apply arithmetic
      (let ([result (parameterize ([bf-precision sig-w-1])
                      (op v1 v2))])
        (cond
          [(fp/result-infinity? result exp-w-1 sig-w-1)
           (if (bfpositive? result)
               (get/+inf exp-w-1 sig-w-1)
               (get/-inf exp-w-1 sig-w-1))]
          [(and
            (not
             (bfzero? result))
            (bf<=
             (bfabs result)
             (FloatingPoint-value (get/maximum-subnormal exp-w-1 sig-w-1))))
           (fp/round-to-subnormal result exp-w-1 sig-w-1)]
          [else (mkFP exp-w-1 sig-w-1 result)]))
      (error "invalid arithmetic operation~a" op fp1 fp2)))

(define eval/fpadd (eval/fparith/binop bf+))
(define eval/fpsub (eval/fparith/binop bf-))
(define eval/fpmul (eval/fparith/binop bf*))
(define eval/fpdiv (eval/fparith/binop bf/))

(define eval/fpsqrt
  (λ (fp)
    ((eval/fparith/binop (λ (f s) (bfsqrt f))) fp fp)))

(define eval/fpabs
  (λ (fp)
    (define exp-width (FloatingPoint-exp-width fp))
    (define sig-width (FloatingPoint-sig-width fp))
    (mkFP exp-width sig-width (bfabs (FloatingPoint-value fp)))))

(define eval/fpneg
  (λ (fp)
    (define exp-width (FloatingPoint-exp-width fp))
    (define sig-width (FloatingPoint-sig-width fp))
    (mkFP
     exp-width
     sig-width
     (parameterize ([bf-precision sig-width])
       (bf* (bf -1.0) (FloatingPoint-value fp))))))

(define fp/prune
  (λ (fp)
    ((eval/fparith/binop (λ (f s) f)) fp fp)))

(define eval/fpconv
  (λ (fp dest-exp-width dest-sig-width)
    (define make-new-fp
      (λ (fp)
        (parameterize ([bf-precision dest-sig-width])
          (mkFP dest-exp-width dest-sig-width (bfcopy (FloatingPoint-value fp))))))
    (cond
      [(or
        (fp/nan? fp)
        (fp/infinity? fp)) (make-new-fp fp)]
      [else (let ([new-fp (make-new-fp fp)])
              (fp/prune new-fp))])))

;; floating-point predicates
(define ((fp/pred/uni pred) fp)
  (pred (FloatingPoint-value fp)))

(define ((fp/pred/bin pred) fp1 fp2)
  (pred (FloatingPoint-value fp1) (FloatingPoint-value fp2)))

(define fp/infinity? (fp/pred/uni bfinfinite?))
(define fp/nan? (fp/pred/uni bfnan?))
(define fp/zero? (fp/pred/uni bfzero?))
;(define fp/positive? (fp/pred/uni bfpositive?))
;(define fp/negative? (fp/pred/uni bfnegative?))

;;[[fp.isPositive]](x) = true iff x is [[+zero]] or [[fp.lt]]([[+zero]], x) holds.
(define fp/positive?
  (λ (fp)
    (cond
      [(fp/zero? fp)
       (=
        (bigfloat-signbit (FloatingPoint-value fp))
        0)]
      [else ((fp/pred/uni bfpositive?) fp)])))

(define fp/negative?
  (λ (fp)
    (cond
      [(fp/zero? fp)
       (=
        (bigfloat-signbit (FloatingPoint-value fp))
        1)]
      [else ((fp/pred/uni bfnegative?) fp)])))

(define fp/normal?
  (λ (fp)
    (cond
      [(fp/nan? fp) #f]
      [(fp/infinity? fp) #f]
      [(fp/zero? fp) #f]
      [else (let ([fpp (eval/fpabs fp)])
              (fp>
               fpp
               (get/maximum-subnormal
                (FloatingPoint-exp-width fp)
                (FloatingPoint-sig-width fp))))])))

(define fp/subnormal?
  (λ (fp)
    (cond
      [(fp/nan? fp) #f]
      [(fp/infinity? fp) #f]
      [(fp/zero? fp) #f]
      [else (let ([fpp (eval/fpabs fp)])
              (fp≤
               fpp
               (get/maximum-subnormal
                (FloatingPoint-exp-width fp)
                (FloatingPoint-sig-width fp))))])))

(define fp= (fp/pred/bin bf=))
(define fp< (fp/pred/bin bf<))
(define fp> (fp/pred/bin bf>))
(define fp≤ (fp/pred/bin bf<=))
(define fp≥ (fp/pred/bin bf>=))

;; floating-point related conversions
;; real->fp
(define real->FloatingPoint
  (λ (rv exp-width sig-width)
    (fp/prune (parameterize ([bf-precision sig-width])
      (mkFP exp-width sig-width (bf rv))))))

;; bv->fp
#;(define BitVec->FloatingPoint
  (λ (bv exp-width sig-width)
    (mkFP exp-width sig-width
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
              (error "Bit width doesn't match!")))))

(define (BitVec->FloatingPoint bv exp-width sig-width)
  (mkFP exp-width sig-width (BitVec->bf bv exp-width sig-width)))

(define (FloatingPoint->BitVec fp)
  (define exp-width (FloatingPoint-exp-width fp))
  (define sig-width (FloatingPoint-sig-width fp))
  (define fp-val (FloatingPoint-value fp))
  (mkBV (+ exp-width sig-width) (bf->BitVec fp-val exp-width sig-width)))

; fp->bv
#;(define FloatingPoint->BitVec
  (λ (fp)
    (let* ([exp-width (FloatingPoint-exp-width fp)]
           [sig-width (FloatingPoint-sig-width fp)]
           [fp-val (FloatingPoint-value fp)]
           [sig-width-wo (- sig-width 1)]
           [exp-bias (- (expt 2 (- exp-width 1)) 1)]
           [sign-wrap (λ (v sign-bit)
                        (+
                         v
                         (* sign-bit (expt 2 (+ exp-width sig-width-wo)))))])
      (mkBV
       (+ exp-width sig-width)
       (cond
         [(fp/nan? fp) (error "no unique bv representation for nans!")]
         [(fp/infinity? fp) (sign-wrap
                             (arithmetic-shift (- (expt 2 exp-width) 1) sig-width-wo)
                             (bigfloat-signbit (FloatingPoint-value fp)))]
         [(fp/zero? fp) (sign-wrap 0 (bigfloat-signbit (FloatingPoint-value fp)))]
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
                   (error "unrecognized format for sig+exp!~a" fp)))
             (bigfloat-signbit (FloatingPoint-value fp))))])))))
