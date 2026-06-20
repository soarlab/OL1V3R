#lang racket

;; Shared, dependency-free exact-rational IEEE oracle helpers used by the FP test
;; files (rounding / sqrt / classify / compare). No dependency on data/fp.rkt, so
;; the differential tests share no rounding code with the code under test.

(provide fmt-bias fmt-emin fmt-emax
         decode floor-log2 rnd-dir mkbits oracle-ovf oracle-round
         interesting)

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

;; interesting operand bit patterns for (eb,sb): subnormals, min/max normal,
;; values near 1, plus their negations -- dense across all regimes.
(define (interesting eb sb)
  (define sw (- sb 1)) (define bias (fmt-bias eb))
  (define maxsub (- (expt 2 sw) 1))
  (define base
    (list 1 2 (quotient maxsub 2) (- maxsub 1) maxsub
          (expt 2 sw) (+ (expt 2 sw) 1)
          (* (- bias 1) (expt 2 sw))
          (* bias (expt 2 sw)) (+ (* bias (expt 2 sw)) 1)
          (+ (* bias (expt 2 sw)) (quotient (expt 2 sw) 3))
          (* (+ bias 1) (expt 2 sw))
          (- (* (- (expt 2 eb) 1) (expt 2 sw)) 1)
          (- (* (- (expt 2 eb) 1) (expt 2 sw)) 2)))
  (append base (map (λ (b) (+ b (expt 2 (- (+ eb sb) 1)))) base)))
