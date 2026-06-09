#lang racket

(require rackunit
         rackunit/text-ui
         "../../data/bit-vec.rkt")

(define bit-vec-tests
  (test-suite
   "Tests for BitVector Data Types"
   (test-suite "Creation of BitVector values"
               (check-equal? (BitVec-value (mkBV 8 255)) 255)
               (check-equal? (BitVec->bits (mkBV 4 10)) '(1 0 1 0)))
   (test-suite "BitVector arithmetic operations"
               (let ([a (mkBV 4 10)]
                     [b (mkBV 4 3)])
                 (check-equal? (BitVec-value (eval/bvadd a b)) 13)
                 (check-equal? (BitVec-value (eval/bvadd a (mkBV 4 7))) 1) ; overflow
                 (check-equal? (BitVec-value (eval/bvsub a b)) 7)
                 (check-equal? (BitVec-value (eval/bvsub b a)) 9) ; underflow 3 - 10 = -7 = 9 mod 16
                 (check-equal? (BitVec-value (eval/bvmul a b)) 14) ; 30 mod 16 = 14
                 ))
   (test-suite "BitVector bitwise operations"
               (let ([a (mkBV 4 10)] ; 1010
                     [b (mkBV 4 12)]) ; 1100
                 (check-equal? (BitVec-value (eval/bvand a b)) 8) ; 1000
                 (check-equal? (BitVec-value (eval/bvor a b)) 14) ; 1110
                 (check-equal? (BitVec-value (eval/bvnot a)) 5) ; 0101
                 (check-equal? (BitVec-value (eval/bvshl a (mkBV 4 1))) 4) ; 10100 mod 16 = 0100 = 4
                 (check-equal? (BitVec-value (eval/bvlshr a (mkBV 4 1))) 5) ; 0101 = 5
                 ))
   (test-suite "BitVector neighborhood generation"
               (let ([a (mkBV 3 2)]) ; 010
                 (let ([neighbors (get/1-exchange a)])
                   (check-equal? (length neighbors) 3)
                   (check-equal? (map BitVec-value neighbors) '(3 0 6))) ; 011, 000, 110
                 (let ([neighbors (get/±1 a)])
                   (check-equal? (length neighbors) 2)
                   (check-equal? (map BitVec-value neighbors) '(3 1))))
               (let ([b (mkBV 3 7)]) ; 111
                 (let ([neighbors (get/±1 b)])
                   (check-equal? (map BitVec-value neighbors) '(0 6))))))) ; 7+1=0 mod 8, 7-1=6

;; exit non-zero on any failure/error so CI fails loudly
(exit (run-tests bit-vec-tests))
