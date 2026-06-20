#lang racket

;; Exhaustive (all 65536 f16 bit patterns) tests of the classification predicates,
;; BitVec<->FP round-tripping, and fp.abs / fp.neg.

(require rackunit
         rackunit/text-ui
         "../../data/fp.rkt"
         "../../data/bit-vec.rkt")

(define EB 5) (define SB 11) (define W 16)
(define (fpb bits) (BitVec->FloatingPoint (mkBV W bits) EB SB))
(define (bof fp) (BitVec-value (FloatingPoint->BitVec fp)))

;; ground-truth class from the bit pattern
(define (gt-class bits)
  (define ef (bitwise-bit-field bits 10 15))
  (define fr (bitwise-bit-field bits 0 10))
  (cond [(= ef 31) (if (= fr 0) 'inf 'nan)]
        [(= ef 0)  (if (= fr 0) 'zero 'sub)]
        [else 'normal]))
(define (gt-sign bits) (bitwise-bit-field bits 15 16))

(define classify-tests
  (test-suite
   "Exhaustive f16 classification / round-trip / abs / neg"

   (test-case "predicates match bit-pattern ground truth (all 65536 patterns)"
     (define bad 0)
     (for ([bits (in-range 65536)])
       (define fp (fpb bits)) (define cls (gt-class bits)) (define sgn (gt-sign bits))
       (define (chk pred want) (unless (eq? (and (pred fp) #t) want) (set! bad (add1 bad))))
       (chk fp/nan?       (eq? cls 'nan))
       (chk fp/infinity?  (eq? cls 'inf))
       (chk fp/zero?      (eq? cls 'zero))
       (chk fp/subnormal? (eq? cls 'sub))
       (chk fp/normal?    (eq? cls 'normal))
       (chk fp/positive?  (and (not (eq? cls 'nan)) (= sgn 0)))
       (chk fp/negative?  (and (not (eq? cls 'nan)) (= sgn 1))))
     (check-equal? bad 0))

   (test-case "exactly one of {nan,inf,zero,sub,normal}; positive/negative disjoint"
     (define bad 0)
     (for ([bits (in-range 65536)])
       (define fp (fpb bits))
       (define n (for/sum ([p (list fp/nan? fp/infinity? fp/zero? fp/subnormal? fp/normal?)])
                   (if (p fp) 1 0)))
       (unless (= n 1) (set! bad (add1 bad)))
       (when (and (fp/positive? fp) (fp/negative? fp)) (set! bad (add1 bad))))
     (check-equal? bad 0))

   (test-case "BitVec<->FP round-trips for every non-NaN pattern"
     (define bad 0)
     (for ([bits (in-range 65536)])
       (unless (eq? (gt-class bits) 'nan)
         (unless (= (bof (fpb bits)) bits) (set! bad (add1 bad)))))
     (check-equal? bad 0))

   (test-case "fp.abs clears the sign, fp.neg flips it; NaN stays NaN; involutive"
     (define bad 0)
     (for ([bits (in-range 65536)])
       (define fp (fpb bits))
       (cond
         [(eq? (gt-class bits) 'nan)
          (unless (and (fp/nan? (eval/fpabs fp)) (fp/nan? (eval/fpneg fp))) (set! bad (add1 bad)))]
         [else
          (unless (= (bof (eval/fpabs fp)) (bitwise-and bits #x7FFF)) (set! bad (add1 bad)))
          (unless (= (bof (eval/fpneg fp)) (bitwise-xor bits #x8000)) (set! bad (add1 bad)))
          (unless (= (bof (eval/fpneg (eval/fpneg fp))) bits) (set! bad (add1 bad)))
          (unless (= (bof (eval/fpabs (eval/fpabs fp))) (bitwise-and bits #x7FFF)) (set! bad (add1 bad)))]))
     (check-equal? bad 0))))

(exit (run-tests classify-tests))
