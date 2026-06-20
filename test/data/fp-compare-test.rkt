#lang racket

;; FP comparisons (fp.lt/leq/gt/geq/eq and SMT =) vs an exact-rational ground
;; truth, over all pairs of a special-rich operand set, plus the score.rkt
;; invariant "score = 1 exactly when the relation holds".

(require rackunit
         rackunit/text-ui
         "fp-oracle.rkt"
         "../../src/score.rkt"
         "../../data/fp.rkt"
         "../../data/bit-vec.rkt")

(define EB 5) (define SB 11)
(define (fpb bits) (BitVec->FloatingPoint (mkBV 16 bits) EB SB))

;; numeric value for ordering: 'nan | +inf.0 | -inf.0 | exact-rational (+0/-0 -> 0)
(define (numval bits)
  (define d (decode EB SB bits))
  (cond [(eq? d 'nan) 'nan]
        [(eq? (car d) 'inf) (if (cdr d) -inf.0 +inf.0)]
        [else (cdr d)]))
(define (both-fin? a b) (and (not (eq? (numval a) 'nan)) (not (eq? (numval b) 'nan))))

(define (gt-lt  a b) (and (both-fin? a b) (<  (numval a) (numval b))))
(define (gt-leq a b) (and (both-fin? a b) (<= (numval a) (numval b))))
(define (gt-gt  a b) (and (both-fin? a b) (>  (numval a) (numval b))))
(define (gt-geq a b) (and (both-fin? a b) (>= (numval a) (numval b))))
(define (gt-eq  a b) (and (both-fin? a b) (=  (numval a) (numval b))))   ; IEEE fp.eq: +0 = -0
(define (gt-smteq a b)                                                   ; SMT = : bit equality
  (define da (decode EB SB a)) (define db (decode EB SB b))
  (or (and (eq? da 'nan) (eq? db 'nan))
      (and (not (eq? da 'nan)) (not (eq? db 'nan)) (= a b))))

(define operands
  (list 0 #x8000        ; +0 -0
        #x3C00 #xBC00   ; +1 -1
        #x4000 #xC000   ; +2 -2
        #x3800          ; 0.5
        1 #x8001        ; +/- min subnormal
        #x3FF #x83FF    ; +/- max subnormal
        #x400 #x8400    ; +/- min normal
        #x7BFF #xFBFF   ; +/- max normal
        #x7C00 #xFC00   ; +/- inf
        #x7E00 #xFE00   ; +/- NaN
        #x4900))        ; ~10.0

(define rels
  (list (list fp.lt-true?  gt-lt  score/fplt)
        (list fp.leq-true? gt-leq score/fpleq)
        (list fp.gt-true?  gt-gt  score/fpgt)
        (list fp.geq-true? gt-geq score/fpgeq)
        (list fp.eq-true?  gt-eq  score/fpeq)))

(define compare-tests
  (test-suite
   "FP comparisons and scoring invariants"

   (test-case "lt/leq/gt/geq/eq truth match exact-rational ground truth (all pairs)"
     (define bad 0)
     (for* ([a operands] [b operands] [rel rels])
       (unless (eq? (and ((car rel) (fpb a) (fpb b)) #t) (and ((cadr rel) a b) #t))
         (set! bad (add1 bad))))
     (check-equal? bad 0))

   (test-case "SMT = (fp=-true?) is bit equality (+0 != -0, NaN = NaN)"
     (define bad 0)
     (for* ([a operands] [b operands])
       (unless (eq? (and (fp=-true? (fpb a) (fpb b)) #t) (gt-smteq a b)) (set! bad (add1 bad))))
     (check-equal? bad 0))

   (test-case "score = 1 exactly when the relation holds, and stays in [0,1] (c = 1/2)"
     (define bad 0)
     (for* ([a operands] [b operands] [rel rels])
       (define truth (and ((car rel) (fpb a) (fpb b)) #t))
       (define s (((caddr rel) 1/2) (fpb a) (fpb b)))
       (unless (eq? (= s 1) truth) (set! bad (add1 bad)))
       (unless (and (>= s 0) (<= s 1)) (set! bad (add1 bad))))
     (check-equal? bad 0))

   (test-case "spot checks: signed zero, NaN, infinity ordering"
     (check-true  (fp.eq-true?  (fpb 0) (fpb #x8000)))      ; +0 = -0 (fp.eq)
     (check-false (fp=-true?    (fpb 0) (fpb #x8000)))      ; +0 != -0 (SMT =)
     (check-false (fp.lt-true?  (fpb 0) (fpb #x8000)))      ; not (+0 < -0)
     (check-true  (fp.leq-true? (fpb 0) (fpb #x8000)))      ; +0 <= -0
     (check-true  (fp=-true?    (fpb #x7E00) (fpb #xFE00))) ; NaN = NaN  (SMT =)
     (check-false (fp.eq-true?  (fpb #x7E00) (fpb #x3C00))) ; NaN fp.eq x is false
     (check-false (fp.lt-true?  (fpb #x7E00) (fpb #x3C00))) ; NaN < x is false
     (check-true  (fp.lt-true?  (fpb #xFC00) (fpb #x7C00))) ; -inf < +inf
     (check-true  (fp.lt-true?  (fpb #x7BFF) (fpb #x7C00)))))) ; max-normal < +inf

(exit (run-tests compare-tests))
