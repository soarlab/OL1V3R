#lang racket

(require math/bigfloat)
(require racket/struct)
(require "bit-vec.rkt")

(provide (all-defined-out))

;; sig-width includes the hidden bit
(struct FloatingPoint (exp-width sig-width value)
  #:constructor-name mkFP
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (fp) 'FloatingPoint)
      (λ (fp)
        `(,(FloatingPoint-exp-width fp) ,(FloatingPoint-sig-width fp)
                                        ,(FloatingPoint-value fp)))))])

(define FloatingPoint->FPConst
  (λ (fp)
    (define exp-width (FloatingPoint-exp-width fp))
    (define sig-width (FloatingPoint-sig-width fp))
    (cond
      [(fp/nan? fp) `(_ NaN ,exp-width ,sig-width)]
      [(fp/infinity? fp)
       `(_ ,(if (fp/positive? fp) '+oo '-oo) ,exp-width ,sig-width)]
      [(fp/zero? fp)
       `(_ ,(if (fp/positive? fp) '+zero '-zero) ,exp-width ,sig-width)]
      [else
       (let ([abs-value (bitwise-and
                         (BitVec-value (FloatingPoint->BitVec fp))
                         (- (expt 2 (- (+ exp-width sig-width) 1)) 1))]
             [sig-width-wo (- sig-width 1)])
         `(fp
           ,(BitVec->BVConst (if (fp/positive? fp) (mkBV 1 0) (mkBV 1 1)))
           ,(BitVec->BVConst
             (mkBV exp-width (arithmetic-shift abs-value (- 0 sig-width-wo))))
           ,(BitVec->BVConst (mkBV sig-width-wo
                                   (bitwise-and (- (expt 2 sig-width-wo) 1)
                                                abs-value)))))])))

(define initialize/fp
  (λ (exp-width sig-width)
    (mkFP exp-width
          sig-width
          (parameterize ([bf-precision sig-width]) (bf 0.0)))))

(define random/fp
  (λ (exp-width sig-width)
    (if (coin-flip 0.8)
        (car (shuffle `(,(real->FloatingPoint +inf.f exp-width sig-width)
                        ,(real->FloatingPoint -inf.f exp-width sig-width)
                        ,(real->FloatingPoint +nan.f exp-width sig-width))))
        (BitVec->FloatingPoint (random/bv (+ exp-width sig-width))
                               exp-width
                               sig-width))))

(define exp/±
  (λ (fp)
    (define two
      (real->FloatingPoint 2.0
                           (FloatingPoint-exp-width fp)
                           (FloatingPoint-sig-width fp)))
    ;; neighbor doubling/halving: rounding mode is irrelevant here, use nearest
    `(,(eval/fpmul 'nearest fp two) ,(eval/fpdiv 'nearest fp two))))

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
        (let* ([ns (map (λ (bv) (BitVec->FloatingPoint bv exp-width sig-width))
                        (get/bv-extended-neighbors (FloatingPoint->BitVec fp)))]
               [fns (filter
                     (λ (fp) (and (not (fp/nan? fp)) (not (fp/infinity? fp))))
                     ns)])
          (if (= (length ns) (length fns))
              fns
              (cons (real->FloatingPoint +nan.f exp-width sig-width)
                    (cons (real->FloatingPoint +inf.f exp-width sig-width)
                          (cons (real->FloatingPoint -inf.f exp-width sig-width)
                                fns)))))
        (exp/± fp))])))

(define (get/fp-neighbor-flip-sign fp)
  `(,(eval/fpneg fp)))

(define (get/fp-neighbors val ni)
  (cond
    [(= ni 1) (if (fp/nan? val) '() (get/fp-neighbor-flip-sign val))]
    [(= ni 2) (if (fp/nan? val) '() (get/fp-neighbor-exp val))]
    [(= ni 3)
     (if (fp/nan? val)
         (get/fp-extended-neighbors val)
         (get/fp-neighbor-sig val))]))

(define (get/fp-neighbor-exp fp)
  (define exp-width (FloatingPoint-exp-width fp))
  (define sig-width (FloatingPoint-sig-width fp))
  (define flip-exp
    (map (λ (bv) (BitVec->FloatingPoint bv exp-width sig-width))
         (let ([sig-width-wo (- sig-width 1)])
           (get/1-exchange-range (FloatingPoint->BitVec fp)
                                 sig-width-wo
                                 (+ sig-width-wo exp-width)))))
  (append flip-exp (exp/± fp)))

(define (get/fp-neighbor-sig fp)
  (define exp-width (FloatingPoint-exp-width fp))
  (define sig-width (FloatingPoint-sig-width fp))
  (define flip-sig
    (map (λ (bv) (BitVec->FloatingPoint bv exp-width sig-width))
         (let ([sig-width-wo (- sig-width 1)])
           (get/1-exchange-range (FloatingPoint->BitVec fp) 0 sig-width-wo))))
  (append flip-sig (sig/± fp)))

;; floating-point arithmetic

;; Round the positive integer ratio num/den to an integer, directed by the
;; bigfloat rounding mode and the value's sign `neg`. Mirrors the Rust
;; round_rational for the four modes bigfloat supports (RNA is refused upstream).
(define (round-ratio-directed num den mode neg)
  (define-values (q r) (quotient/remainder num den))
  (cond
    [(zero? r) q]
    [else
     (define up?
       (case mode
         [(zero) #f]                ; toward zero: truncate the magnitude
         [(up)   (not neg)]         ; toward +inf: round magnitude up iff positive
         [(down) neg]               ; toward -inf: round magnitude up iff negative
         [(nearest)
          (let ([2r (* 2 r)])
            (cond [(< 2r den) #f] [(> 2r den) #t] [else (odd? q)]))] ; ties to even
         [else #f]))
     (if up? (+ q 1) q)]))

;; Round a bigfloat once onto the subnormal grid, honoring `mode`. `v` must carry
;; enough precision that this single grid rounding is correctly rounded (callers
;; recompute at 2*sig+2 bits), so there is no double-rounding.
(define fp/round-to-subnormal
  (λ (v exp-width sig-width mode)
    (define subnormal-min
      (FloatingPoint-value (BitVec->FloatingPoint
                            (mkBV (+ exp-width sig-width) 1)
                            exp-width
                            sig-width)))
    (define neg (bfnegative? v))
    (define pv (bfabs v))
    (let-values ([(s1 p1) (bigfloat->sig+exp pv)]
                 [(s2 p2) (bigfloat->sig+exp subnormal-min)])
      ;; exact magnitude |v| / subnormal-min = num/den (integers)
      (define d (- p1 p2))
      (define-values (num den)
        (if (>= d 0)
            (values (* s1 (expt 2 d)) s2)
            (values s1 (* s2 (expt 2 (- d))))))
      (let ([r (round-ratio-directed num den mode neg)])
        (cond
          [(< r 1)
           (if neg
               (real->FloatingPoint -0.0 exp-width sig-width)
               (real->FloatingPoint 0.0 exp-width sig-width))]
          [else
           (let ([rv (BitVec->FloatingPoint (mkBV (+ exp-width sig-width) r)
                                            exp-width
                                            sig-width)])
             (if neg (eval/fpneg rv) rv))])))))

;; The format value an overflow rounds to, per mode and sign (`pos?`). Mirrors the
;; Rust overflow_value: only nearest saturates to +/-inf for both signs.
(define (overflow-value exp-width sig-width mode pos?)
  (define maxn (get/maximum-normal exp-width sig-width))
  (case mode
    [(zero) (if pos? maxn (eval/fpneg maxn))]
    [(up)   (if pos? (get/+inf exp-width sig-width) (eval/fpneg maxn))]
    [(down) (if pos? maxn (get/-inf exp-width sig-width))]
    [else   (if pos? (get/+inf exp-width sig-width) (get/-inf exp-width sig-width))]))

;; Smallest positive normal of the format (bit pattern 2^(sig-width-1)).
(define (get/minimum-normal exp-width sig-width)
  (FloatingPoint-value (BitVec->FloatingPoint
                        (mkBV (+ exp-width sig-width) (expt 2 (- sig-width 1)))
                        exp-width sig-width)))

;; Map a freshly-rounded `result` (at sig-width precision under `mode`) into the
;; format: overflow -> mode/sign-aware clamp; magnitude below the smallest normal
;; (this INCLUDES the (max-subnormal, smallest-normal) sliver, which is off the
;; subnormal grid) -> a single rounding of the high-precision recompute
;; (`hi-thunk`) onto the subnormal grid; else as-is (already a grid value).
(define (finish-fp-result result exp-width sig-width mode hi-thunk)
  (cond
    [(fp/result-infinity? result exp-width sig-width)
     (overflow-value exp-width sig-width mode (bfpositive? result))]
    [(and (not (bfzero? result))
          (bf< (bfabs result) (get/minimum-normal exp-width sig-width)))
     (fp/round-to-subnormal (hi-thunk) exp-width sig-width mode)]
    [else (mkFP exp-width sig-width result)]))

(define fp/result-infinity?
  (λ (v exp-width sig-width)
    (define pv (bfabs v))
    (bf> pv (FloatingPoint-value (get/maximum-normal exp-width sig-width)))))

(define get/+inf
  (λ (exp-width sig-width)
    (mkFP exp-width
          sig-width
          (parameterize ([bf-precision sig-width]) (bfcopy +inf.bf)))))

(define get/-inf
  (λ (exp-width sig-width)
    (mkFP exp-width
          sig-width
          (parameterize ([bf-precision sig-width]) (bfcopy -inf.bf)))))

(define get/maximum-normal
  (λ (exp-width sig-width)
    (define +inf-bv (FloatingPoint->BitVec (get/+inf exp-width sig-width)))
    (BitVec->FloatingPoint
     (mkBV (+ exp-width sig-width) (- (BitVec-value +inf-bv) 1))
     exp-width
     sig-width)))

(define get/maximum-subnormal
  (λ (exp-width sig-width)
    (BitVec->FloatingPoint
     (mkBV (+ exp-width sig-width) (- (expt 2 (- sig-width 1)) 1))
     exp-width
     sig-width)))

;; An SMT rounding-mode constant -> a Racket bigfloat rounding mode.
;;
;; LIMITATION: Racket's bigfloat has no ties-to-away mode, so we cannot honor
;; roundNearestTiesToAway. We deliberately do NOT approximate it as 'nearest --
;; that would round ties the wrong way and could report a spurious model (an
;; unsoundness). Instead we refuse it here and omit it from the rm-variable
;; enumeration (see ROUNDING-MODES in parse.rkt). OL1V3R is therefore incomplete
;; on RNA: instances that require ties-to-away are not solved. (None occur in our
;; evaluation set.)
(define (rm->bf-mode rm)
  (case rm
    [(roundNearestTiesToEven RNE) 'nearest]
    [(roundTowardZero RTZ) 'zero]
    [(roundTowardPositive RTP) 'up]
    [(roundTowardNegative RTN) 'down]
    [else (error "unsupported rounding mode (no bigfloat equivalent):" rm)]))

(define ((eval/fparith/binop op) mode fp1 fp2)
  (define v1 (FloatingPoint-value fp1))
  (define v2 (FloatingPoint-value fp2))
  (define exp-w-1 (FloatingPoint-exp-width fp1))
  (define exp-w-2 (FloatingPoint-exp-width fp2))
  (define sig-w-1 (FloatingPoint-sig-width fp1))
  (define sig-w-2 (FloatingPoint-sig-width fp2))
  (if (and (= (bigfloat-precision v1) (bigfloat-precision v2))
           (and (= exp-w-1 exp-w-2) (= sig-w-1 sig-w-2)))
      ;; apply arithmetic under the requested rounding mode
      (let ([result (parameterize ([bf-precision sig-w-1] [bf-rounding-mode mode])
                      (op v1 v2))])
        ;; renormalize: mode-aware overflow, and subnormals rounded ONCE from a
        ;; high-precision recompute (avoids the prior double-rounding + ties-even).
        (finish-fp-result result exp-w-1 sig-w-1 mode
                          (λ () (parameterize ([bf-precision (+ (* 2 sig-w-1) 2)]
                                               [bf-rounding-mode mode])
                                  (op v1 v2)))))
      (error "invalid arithmetic operation~a" op fp1 fp2)))

(define eval/fpadd (eval/fparith/binop bf+))
(define eval/fpsub (eval/fparith/binop bf-))
(define eval/fpmul (eval/fparith/binop bf*))
(define eval/fpdiv (eval/fparith/binop bf/))

(define eval/fpsqrt (λ (mode fp) ((eval/fparith/binop (λ (f s) (bfsqrt f))) mode fp fp)))

(define eval/fpabs
  (λ (fp)
    (define exp-width (FloatingPoint-exp-width fp))
    (define sig-width (FloatingPoint-sig-width fp))
    (mkFP exp-width
          sig-width
          (parameterize ([bf-precision sig-width])
            (bfabs (FloatingPoint-value fp))))))

(define eval/fpneg
  (λ (fp)
    (define exp-width (FloatingPoint-exp-width fp))
    (define sig-width (FloatingPoint-sig-width fp))
    (mkFP exp-width
          sig-width
          (parameterize ([bf-precision sig-width])
            (bf* (bf -1.0) (FloatingPoint-value fp))))))

(define fp/prune (λ (fp) ((eval/fparith/binop (λ (f s) f)) 'nearest fp fp)))

;; (_ to_fp E S) rm op  — round the source value ONCE into the destination format
;; under `mode` (no double-rounding; honors the rounding mode, unlike the old
;; nearest-only fp/prune path). NaN/inf copy across unchanged.
(define eval/fpconv
  (λ (fp dest-exp-width dest-sig-width mode)
    (define v (FloatingPoint-value fp))
    (cond
      [(or (fp/nan? fp) (fp/infinity? fp))
       (parameterize ([bf-precision dest-sig-width])
         (mkFP dest-exp-width dest-sig-width (bfcopy v)))]
      [else
       (let ([result (parameterize ([bf-precision dest-sig-width] [bf-rounding-mode mode])
                       (bfcopy v))])
         (finish-fp-result result dest-exp-width dest-sig-width mode
                           (λ () (parameterize ([bf-precision (+ (* 2 dest-sig-width) 2)]
                                                [bf-rounding-mode mode])
                                   (bfcopy v)))))])))

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
      [(fp/zero? fp) (= (bigfloat-signbit (FloatingPoint-value fp)) 0)]
      [else ((fp/pred/uni bfpositive?) fp)])))

(define fp/negative?
  (λ (fp)
    (cond
      [(fp/zero? fp) (= (bigfloat-signbit (FloatingPoint-value fp)) 1)]
      [else ((fp/pred/uni bfnegative?) fp)])))

(define fp/normal?
  (λ (fp)
    (cond
      [(fp/nan? fp) #f]
      [(fp/infinity? fp) #f]
      [(fp/zero? fp) #f]
      [else
       (let ([fpp (eval/fpabs fp)])
         (fp> fpp
              (get/maximum-subnormal (FloatingPoint-exp-width fp)
                                     (FloatingPoint-sig-width fp))))])))

(define fp/subnormal?
  (λ (fp)
    (cond
      [(fp/nan? fp) #f]
      [(fp/infinity? fp) #f]
      [(fp/zero? fp) #f]
      [else
       (let ([fpp (eval/fpabs fp)])
         (fp≤ fpp
              (get/maximum-subnormal (FloatingPoint-exp-width fp)
                                     (FloatingPoint-sig-width fp))))])))

(define fp= (fp/pred/bin bf=))
(define fp< (fp/pred/bin bf<))
(define fp> (fp/pred/bin bf>))
(define fp≤ (fp/pred/bin bf<=))
(define fp≥ (fp/pred/bin bf>=))

;; ---- atom truth predicates: single source of truth shared by score.rkt
;; (which returns score 1 exactly when these hold) and eval.rkt (which decides
;; an `ite` condition with them). NaN comparisons are false; fp.eq treats
;; +0 = -0; structural `=` is bit-identical (so +0 != -0).
(define (fp.lt-true?  a b) (and (not (fp/nan? a)) (not (fp/nan? b)) (fp< a b)))
(define (fp.leq-true? a b) (and (not (fp/nan? a)) (not (fp/nan? b)) (fp≤ a b)))
(define (fp.gt-true?  a b) (and (not (fp/nan? a)) (not (fp/nan? b)) (fp> a b)))
(define (fp.geq-true? a b) (and (not (fp/nan? a)) (not (fp/nan? b)) (fp≥ a b)))
(define (fp.eq-true?  a b)
  (and (not (fp/nan? a)) (not (fp/nan? b))
       (or (and (fp/zero? a) (fp/zero? b))
           (bv= (FloatingPoint->BitVec a) (FloatingPoint->BitVec b)))))
(define (fp=-true? a b)
  (or (and (fp/nan? a) (fp/nan? b))
      ;; only reach for the bit pattern when neither is NaN: NaN has no unique
      ;; bv representation, so FloatingPoint->BitVec would error.
      (and (not (fp/nan? a)) (not (fp/nan? b))
           (bv= (FloatingPoint->BitVec a) (FloatingPoint->BitVec b)))))

;; floating-point related conversions
;; real->fp
(define real->FloatingPoint
  (λ (rv exp-width sig-width)
    (fp/prune (parameterize ([bf-precision sig-width])
                (mkFP exp-width sig-width (bf rv))))))

;; bv->fp
(define BitVec->FloatingPoint
  (λ (bv exp-width sig-width)
    (mkFP exp-width
          sig-width
          (if (= (BitVec-width bv) (+ exp-width sig-width))
              (parameterize ([bf-precision sig-width])
                (let* ([bv-value (BitVec-value bv)]
                       [sig-width-wo (- sig-width 1)]
                       [sig-bits (modulo bv-value (expt 2 sig-width-wo))]
                       [exp-bits
                        (modulo (arithmetic-shift bv-value (- 0 sig-width-wo))
                                (expt 2 exp-width))]
                       [exp-bias (- (expt 2 (- exp-width 1)) 1)]
                       [sign-bit (bitwise-bit-set? bv-value
                                                   (+ exp-width sig-width-wo))])
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
                    [else
                     (bf (let ([sig (+ (expt 2 sig-width-wo) sig-bits)])
                           (if sign-bit (- 0 sig) sig))
                         (- (- exp-bits exp-bias) sig-width-wo))])))
              (error "Bit width doesn't match!")))))

; fp->bv
(define FloatingPoint->BitVec
  (λ (fp)
    (let* ([exp-width (FloatingPoint-exp-width fp)]
           [sig-width (FloatingPoint-sig-width fp)]
           [fp-val (FloatingPoint-value fp)]
           [sig-width-wo (- sig-width 1)]
           [exp-bias (- (expt 2 (- exp-width 1)) 1)]
           [sign-wrap
            (λ (v sign-bit)
              (+ v (* sign-bit (expt 2 (+ exp-width sig-width-wo)))))])
      (mkBV
       (+ exp-width sig-width)
       (cond
         [(fp/nan? fp) (error "no unique bv representation for nans!")]
         [(fp/infinity? fp)
          (sign-wrap (arithmetic-shift (- (expt 2 exp-width) 1) sig-width-wo)
                     (bigfloat-signbit (FloatingPoint-value fp)))]
         [(fp/zero? fp)
          (sign-wrap 0 (bigfloat-signbit (FloatingPoint-value fp)))]
         [else
          (let-values ([(sig exp) (bigfloat->sig+exp fp-val)])
            (sign-wrap
             (let ([sig (abs sig)])
               (if (and (>= sig (expt 2 sig-width-wo))
                        (< sig (expt 2 sig-width)))
                   (if (>= exp (- (- 1 exp-bias) sig-width-wo)) ; normals
                       (+ (arithmetic-shift (+ (+ exp exp-bias) sig-width-wo)
                                            sig-width-wo)
                          (modulo sig (expt 2 (- sig-width 1))))
                       (/ sig (expt 2 (- (- 1 (+ exp-bias sig-width-wo)) exp))))
                   (error "unrecognized format for sig+exp!~a" fp)))
             (bigfloat-signbit (FloatingPoint-value fp))))])))))
