#lang racket

(require rackunit
         rackunit/text-ui
         "../../data/fp.rkt")

(define fp-tests
  (test-suite
   "Tests for Floating-point Data Types"
   (test-suite "Creation of floating-point values"
               (check-pred fp/infinity? (real->FloatingPoint 65520.0 5 11))
               (check-pred fp/positive? (real->FloatingPoint 0.0 5 11))
               (check-pred fp/negative? (real->FloatingPoint -0.0 5 11)))
   (test-suite "Floating-point to bit-vector conversions")
   (test-suite "Floating-point arithmetic"
               (check-pred (λ (x) (and (fp/positive? x) (fp/infinity? x)))
                           (eval/fpdiv (real->FloatingPoint 1.0 5 11)
                                       (real->FloatingPoint 0.0 5 11)))
               (check-pred (λ (x) (and (fp/negative? x) (fp/infinity? x)))
                           (eval/fpdiv (real->FloatingPoint 1.0 5 11)
                                       (real->FloatingPoint -0.0 5 11))))))

(run-tests fp-tests)
