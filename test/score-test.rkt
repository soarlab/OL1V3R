#lang racket

(require rackunit
         rackunit/text-ui
         "../src/score.rkt"
         "../data/bit-vec.rkt"
         "../data/fp.rkt")

(define score-tests
  (test-suite
   "Tests for objective scoring"
   (test-suite "BitVector Scoring"
               (let ([a (mkBV 4 10)] ; 1010
                     [b (mkBV 4 10)]
                     [c (mkBV 4 15)]) ; 1111
                 (check-equal? ((score/= 1/2) a b) 1)
                 ; Hamming distance between 1010 and 1111 is 2. Width is 4.
                 ; score is c * (1 - dist/width) = 1/2 * (1 - 2/4) = 1/4
                 (check-equal? ((score/= 1/2) a c) 1/4)
                 
                 ; bv-dist-score for <
                 ; bv1 = 10, bv2 = 15. a < c is true, score = 1
                 (check-equal? ((score/bv< 1/2) a c) 1)
                 ; c < a is false. dist = |15 - 10| + 1 = 6.
                 ; score = 1/2 * (1 - 6/16) = 1/2 * 10/16 = 5/16
                 (check-equal? ((score/bv< 1/2) c a) 5/16)
                 ))
   (test-suite "FloatingPoint Scoring"
               (let ([a (real->FloatingPoint 1.0 5 11)]
                     [b (real->FloatingPoint 2.0 5 11)]
                     [c (real->FloatingPoint 1.0 5 11)])
                 (check-equal? ((score/fpeq 1/2) a c) 1)
                 (check-equal? ((score/fplt 1/2) a b) 1)
                 
                 ; distance scoring when falsified
                 (let ([score-val ((score/fplt 1/2) b a)])
                   (check-pred (λ (x) (and (rational? x) (<= x 1/2) (>= x 0))) score-val))))
                 
   ))

;; exit non-zero on any failure/error so CI fails loudly
(exit (run-tests score-tests))
