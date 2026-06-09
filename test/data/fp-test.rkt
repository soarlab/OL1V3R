#lang racket

(require rackunit
         rackunit/text-ui
         "../../data/fp.rkt"
         "../../data/bit-vec.rkt")

(define fp-tests
  (test-suite
   "Tests for Floating-point Data Types"
   (test-suite "Creation of floating-point values"
               (check-pred fp/infinity? (real->FloatingPoint 65520.0 5 11))
               (check-pred fp/positive? (real->FloatingPoint 0.0 5 11))
               (check-pred fp/negative? (real->FloatingPoint -0.0 5 11)))
   (test-suite "Floating-point arithmetic"
               (check-pred (λ (x) (and (fp/positive? x) (fp/infinity? x)))
                           (eval/fpdiv 'nearest (real->FloatingPoint 1.0 5 11)
                                       (real->FloatingPoint 0.0 5 11)))
               (check-pred (λ (x) (and (fp/negative? x) (fp/infinity? x)))
                           (eval/fpdiv 'nearest (real->FloatingPoint 1.0 5 11)
                                       (real->FloatingPoint -0.0 5 11))))
   (test-suite "Rounding modes are honored"
               ;; 1/3 is inexact, so rounding toward +inf and -inf must differ
               ;; (this would collapse if the rounding mode were ignored).
               (let ([one   (real->FloatingPoint 1.0 5 11)]
                     [three (real->FloatingPoint 3.0 5 11)])
                 (check-false
                  (bv= (FloatingPoint->BitVec (eval/fpdiv 'up   one three))
                       (FloatingPoint->BitVec (eval/fpdiv 'down one three))))))))

;; exit non-zero on any failure/error so CI fails loudly
(exit (run-tests fp-tests))
