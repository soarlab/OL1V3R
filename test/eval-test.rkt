#lang racket

(require rackunit
         rackunit/text-ui
         "../data/eval.rkt"
         "../data/bit-vec.rkt"
         "../data/fp.rkt")

(define eval-tests
  (test-suite
   "Tests for Expression Evaluator"
   (test-suite "Evaluate BitVector expressions"
               (let ([env `((a . ,(mkBV 4 10))
                            (b . ,(mkBV 4 3)))])
                 (check-equal? (BitVec-value (eval 'a (make-immutable-hash) env)) 10)
                 (check-equal? (BitVec-value (eval '(bvadd a b) (make-immutable-hash) env)) 13)
                 (check-equal? (BitVec-value (eval '(bvsub a b) (make-immutable-hash) env)) 7)
                 ; test bitvector constants
                 (check-equal? (BitVec-value (eval '(_ bv15 4) (make-immutable-hash) env)) 15)
                 ))
   (test-suite "Evaluate Let bindings"
               (let ([env '()])
                 (check-equal? (BitVec-value (eval '(let ([a (_ bv5 4)]) (bvadd a (_ bv3 4)))
                                                   (make-immutable-hash) env))
                               8)))
   (test-suite "Evaluate ite expressions"
               (let ([env '()])
                 (check-equal? (BitVec-value (eval '(ite ⊤ (_ bv10 4) (_ bv5 4)) (make-immutable-hash) env)) 10)
                 (check-equal? (BitVec-value (eval '(ite ⊥ (_ bv10 4) (_ bv5 4)) (make-immutable-hash) env)) 5)
                 (check-equal? (BitVec-value (eval '(ite (= (_ bv3 4) (_ bv3 4)) (_ bv1 4) (_ bv0 4)) (make-immutable-hash) env)) 1)
                 (check-equal? (BitVec-value (eval '(ite (= (_ bv3 4) (_ bv2 4)) (_ bv1 4) (_ bv0 4)) (make-immutable-hash) env)) 0)))
   (test-suite "Evaluate floating-point operations"
               (let ([env `((f1 . ,(real->FloatingPoint 1.0 5 11))
                            (f2 . ,(real->FloatingPoint 2.0 5 11)))])
                 (check-equal? (fp/positive? (eval '(fp.add roundNearestTiesToEven f1 f2) (make-immutable-hash) env)) #t)
                 ; fp.isPositive returns a width-1 BV
                 (check-equal? (BitVec-value (eval '(fp.isPositive f1) (make-immutable-hash) env)) 1)
                 (check-equal? (BitVec-value (eval '(fp.isNormal f1) (make-immutable-hash) env)) 1)))
   ))

;; exit non-zero on any failure/error so CI fails loudly
(exit (run-tests eval-tests))
