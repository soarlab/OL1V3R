#lang racket

;; Rounding-correctness tests for the FP evaluator: overflow, subnormals, the
;; (max-subnormal, min-normal) sliver, FP->FP conversion, and the +0/-0 score.
;; Centerpiece is a DIFFERENTIAL check against an independent exact-rational IEEE
;; oracle (no shared rounding code with data/fp.rkt) across all four supported
;; rounding modes and every regime. Plus explicit per-bug regressions.

(require rackunit
         rackunit/text-ui
         "../../data/fp.rkt"
         "../../data/bit-vec.rkt"
         "../../src/score.rkt")

;; ===========================================================================
;; Independent exact-rational IEEE oracle.
;; ===========================================================================
(define (fmt-bias eb) (- (expt 2 (- eb 1)) 1))
(define (fmt-emin eb) (- 1 (fmt-bias eb)))
(define (fmt-emax eb) (fmt-bias eb))

;; decode (eb,sb) bit pattern -> 'nan | (cons 'inf neg) | (cons 'fin exact-rational)
(define (decode eb sb bits)
  (define w (+ eb sb))
  (define s (bitwise-bit-field bits (- w 1) w))
  (define ef (bitwise-bit-field bits (- sb 1) (- w 1)))
  (define fr (bitwise-bit-field bits 0 (- sb 1)))
  (define neg (= s 1))
  (cond
    [(= ef (- (expt 2 eb) 1)) (if (= fr 0) (cons 'inf neg) 'nan)]
    [(= ef 0) (cons 'fin (* (if neg -1 1) fr (expt 2 (- (fmt-emin eb) (- sb 1)))))]
    [else (cons 'fin (* (if neg -1 1)
                        (+ (expt 2 (- sb 1)) fr)
                        (expt 2 (- ef (fmt-bias eb) (- sb 1)))))]))

;; floor(log2(mag)) for a positive exact rational.
(define (floor-log2 mag)
  (define n (numerator mag)) (define d (denominator mag))
  (define (ge? e) (if (>= e 0) (>= n (* d (expt 2 e))) (>= (* n (expt 2 (- e))) d)))
  (let* ([e0 (- (integer-length n) (integer-length d) 1)]
         [e1 (let up ([e e0]) (if (ge? (+ e 1)) (up (+ e 1)) e))])
    (let dn ([e e1]) (if (ge? e) e (dn (- e 1))))))

;; round positive integer ratio num/den per mode and sign neg.
(define (rnd-dir num den mode neg)
  (define-values (q r) (quotient/remainder num den))
  (if (zero? r) q
      (let ([up? (case mode
                   [(zero) #f] [(up) (not neg)] [(down) neg]
                   [(nearest) (cond [(< (* 2 r) den) #f] [(> (* 2 r) den) #t] [else (odd? q)])])])
        (if up? (+ q 1) q))))

(define (mkbits eb sb neg ef fr)
  (+ (if neg (expt 2 (- (+ eb sb) 1)) 0) (* ef (expt 2 (- sb 1))) fr))

(define (oracle-ovf eb sb mode neg)
  (define maxn (mkbits eb sb neg (- (expt 2 eb) 2) (- (expt 2 (- sb 1)) 1)))
  (define inf  (mkbits eb sb neg (- (expt 2 eb) 1) 0))
  (case mode [(zero) maxn] [(up) (if neg maxn inf)] [(down) (if neg inf maxn)] [else inf]))

;; round an exact NONZERO rational q once to (eb,sb) bits under mode.
(define (oracle-round eb sb q mode)
  (define neg (< q 0)) (define mag (abs q))
  (define num (numerator mag)) (define den (denominator mag))
  (define e (floor-log2 mag))
  (define emin (fmt-emin eb)) (define emax (fmt-emax eb)) (define bias (fmt-bias eb))
  (cond
    [(< e emin)                                   ; subnormal (includes the sliver)
     (define scale (- (- sb 1) emin))
     (define-values (p qd) (if (>= scale 0) (values (* num (expt 2 scale)) den) (values num (* den (expt 2 (- scale))))))
     (define m (rnd-dir p qd mode neg))
     (cond [(= m 0) (mkbits eb sb neg 0 0)]
           [(< m (expt 2 (- sb 1))) (mkbits eb sb neg 0 m)]
           [else (mkbits eb sb neg 1 0)])]
    [else
     (define scale (- (- sb 1) e))
     (define-values (p qd) (if (>= scale 0) (values (* num (expt 2 scale)) den) (values num (* den (expt 2 (- scale))))))
     (define m0 (rnd-dir p qd mode neg))
     (define-values (e2 m) (if (= m0 (expt 2 sb)) (values (+ e 1) (expt 2 (- sb 1))) (values e m0)))
     (if (> e2 emax) (oracle-ovf eb sb mode neg)
         (mkbits eb sb neg (+ e2 bias) (- m (expt 2 (- sb 1)))))]))

;; ===========================================================================
;; Differential driver.
;; ===========================================================================
(define (fpb eb sb bits) (BitVec->FloatingPoint (mkBV (+ eb sb) bits) eb sb))
(define (bof fp) (BitVec-value (FloatingPoint->BitVec fp)))
(define ev-ops (list (list '+ + eval/fpadd) (list '- - eval/fpsub)
                     (list '* * eval/fpmul) (list '/ / eval/fpdiv)))

(define (run-diff eb sb operands)
  (define mism 0)
  (for* ([ab operands] [bb operands] [opx ev-ops] [mode '(nearest zero up down)])
    (define da (decode eb sb ab)) (define db (decode eb sb bb))
    (when (and (pair? da) (eq? (car da) 'fin) (pair? db) (eq? (car db) 'fin))
      (define av (cdr da)) (define bv (cdr db))
      (when (not (and (eq? (car opx) '/) (= bv 0)))
        (define exact ((cadr opx) av bv))
        (when (not (= exact 0))                    ; signed-zero rules are out of scope
          (define want (oracle-round eb sb exact mode))
          (define got (let ([r ((caddr opx) mode (fpb eb sb ab) (fpb eb sb bb))])
                        (if (fp/nan? r) 'nan (bof r))))
          (when (not (equal? got want))
            (set! mism (+ mism 1))
            (when (< mism 8)
              (printf "  MISMATCH ~a a=~x b=~x mode=~a exact=~a got=~a want=~a\n"
                      (car opx) ab bb mode exact got want)))))))
  mism)

;; interesting operand bit patterns for (eb,sb): subnormals, min/max normal,
;; values near 1, plus their negations -- dense across all regimes.
(define (interesting eb sb)
  (define sw (- sb 1)) (define bias (fmt-bias eb))
  (define maxsub (- (expt 2 sw) 1))
  (define base
    (list 1 2 (quotient maxsub 2) (- maxsub 1) maxsub          ; subnormals incl. max-sub
          (expt 2 sw) (+ (expt 2 sw) 1)                        ; min-normal, +1 ulp
          (* (- bias 1) (expt 2 sw))                           ; 0.5
          (* bias (expt 2 sw)) (+ (* bias (expt 2 sw)) 1)      ; 1.0, 1.0+ulp
          (+ (* bias (expt 2 sw)) (quotient (expt 2 sw) 3))    ; ~1.33 (inexact ratios)
          (* (+ bias 1) (expt 2 sw))                           ; 2.0
          (- (* (- (expt 2 eb) 1) (expt 2 sw)) 1)              ; max-normal
          (- (* (- (expt 2 eb) 1) (expt 2 sw)) 2)))            ; max-normal - ulp
  (append base (map (λ (b) (+ b (expt 2 (- (+ eb sb) 1)))) base)))

;; ===========================================================================
;; The suite.
;; ===========================================================================
(define rounding-tests
  (test-suite
   "FP rounding correctness"

   (test-suite "Differential vs independent exact-rational IEEE oracle"
     (test-case "f16 (5,11): all modes x +,-,*,/ x normal/overflow/subnormal/sliver"
       (check-equal? (run-diff 5 11 (interesting 5 11)) 0))
     (test-case "f32 (8,24): all modes x +,-,*,/ x normal/overflow/subnormal/sliver"
       (check-equal? (run-diff 8 24 (interesting 8 24)) 0)))

   (test-suite "R1: overflow honors the rounding mode (f16 max-normal * 2)"
     (let ([maxn (fpb 5 11 #x7BFF)] [two (real->FloatingPoint 2.0 5 11)])
       (check-equal? (bof (eval/fpmul 'zero maxn two)) #x7BFF)              ; -> +max-normal
       (check-equal? (bof (eval/fpmul 'down maxn two)) #x7BFF)             ; +overflow toward -inf
       (check-equal? (bof (eval/fpmul 'up   maxn two)) #x7C00)             ; -> +inf
       (check-equal? (bof (eval/fpmul 'up (eval/fpneg maxn) two)) #xFBFF)  ; -overflow up -> -max-normal
       (check-equal? (bof (eval/fpmul 'nearest maxn two)) #x7C00)))        ; -> +inf

   (test-suite "R3: directed rounding in the subnormal range (f16 min-sub * 1.5 = 1.5 ULP)"
     (let ([smin (fpb 5 11 1)] [onehalf (fpb 5 11 #x3E00)])
       (check-equal? (bof (eval/fpmul 'zero    smin onehalf)) 1)   ; truncate
       (check-equal? (bof (eval/fpmul 'down    smin onehalf)) 1)
       (check-equal? (bof (eval/fpmul 'up      smin onehalf)) 2)
       (check-equal? (bof (eval/fpmul 'nearest smin onehalf)) 2))) ; ties to even

   (test-suite "R3: the (max-subnormal, min-normal) sliver lands on the grid"
     ;; max-sub / ~0.9995 ~= 1023.5 ULP -- must round to a grid point, never off-grid.
     (let ([r (eval/fpdiv 'nearest (fpb 5 11 #x3FF) (fpb 5 11 #x3BFF))])
       (check-pred exact-integer? (bof r))     ; off-grid would be e.g. 2047/2
       (check-equal? (bof r) #x3FF))           ; nearest -> max-subnormal
     (check-equal? (bof (eval/fpdiv 'up (fpb 5 11 #x3FF) (fpb 5 11 #x3BFF))) #x400)) ; up -> min-normal

   (test-suite "R2: FP->FP conversion honors the rounding mode (f64 1/3 -> f32)"
     (let* ([third (real->FloatingPoint (/ 1.0 3.0) 11 53)]
            [up (bof (eval/fpconv third 8 24 'up))]
            [dn (bof (eval/fpconv third 8 24 'down))])
       (check-true (> up dn))                  ; directed modes must differ
       (check-equal? up #x3EAAAAAB)
       (check-equal? dn #x3EAAAAAA))
     ;; specials pass through a conversion unchanged.
     (check-pred fp/nan? (eval/fpconv (fpb 5 11 #x7E00) 11 53 'nearest))
     (let ([pinf (eval/fpconv (fpb 5 11 #x7C00) 11 53 'nearest)])
       (check-true (and (fp/infinity? pinf) (fp/positive? pinf))))
     (let ([nz (eval/fpconv (fpb 5 11 #x8000) 11 53 'nearest)])
       (check-true (and (fp/zero? nz) (fp/negative? nz)))))

   (test-suite "B1: (= +0 -0) scores < 1 even at c2 = 1 (no spurious sat)"
     (let ([pz (fpb 5 11 0)] [nz (fpb 5 11 #x8000)])
       (check-true (< (score/fp= 1 pz nz) 1))  ; was exactly 1 -> spurious sat
       (check-equal? (score/fp= 1 pz pz) 1)    ; genuine (bit-equal) equality
       (check-true (< (score/fp= 1/2 pz nz) 1))))

   (test-suite "Special values: signed zero, inf and NaN propagation"
     (let ([one (real->FloatingPoint 1.0 5 11)] [pz (fpb 5 11 0)]
           [pinf (fpb 5 11 #x7C00)] [nan (fpb 5 11 #x7E00)])
       ;; x - x = +0 (RNE) but -0 toward -inf
       (check-equal? (bof (eval/fpsub 'nearest one one)) 0)
       (check-equal? (bof (eval/fpsub 'down    one one)) #x8000)
       ;; 0 * x = +0 ; inf arithmetic ; NaN propagation
       (check-equal? (bof (eval/fpmul 'nearest pz one)) 0)
       (check-pred fp/infinity? (eval/fpadd 'nearest pinf one))
       (check-pred fp/nan? (eval/fpsub 'nearest pinf pinf))
       (check-pred fp/nan? (eval/fpadd 'nearest nan one))
       (check-pred fp/nan? (eval/fpmul 'nearest pinf pz))))))

;; exit non-zero on any failure/error so CI fails loudly
(exit (run-tests rounding-tests))
