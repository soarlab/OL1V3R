#lang racket

;; fp.sqrt: differential vs a high-precision reference (rounded once by the
;; independent oracle), across all four supported modes, plus special cases.

(require rackunit
         rackunit/text-ui
         math/bigfloat
         "fp-oracle.rkt"
         "../../data/fp.rkt"
         "../../data/bit-vec.rkt")

(define EB 5) (define SB 11) (define W 16)
(define (fpb bits) (BitVec->FloatingPoint (mkBV W bits) EB SB))
(define (bof fp) (BitVec-value (FloatingPoint->BitVec fp)))

;; high-precision sqrt of a nonnegative exact rational -> exact rational approx
;; (256 bits is far more than enough to disambiguate any f16 rounding decision).
(define (ref-sqrt q)
  (bigfloat->rational (parameterize ([bf-precision 256]) (bfsqrt (bf q)))))

(define sqrt-tests
  (test-suite
   "fp.sqrt"

   (test-case "differential vs high-precision oracle (finite > 0, all modes)"
     (define bad 0)
     (for ([bits (in-range 1 #x7C00 7)])              ; finite, non-negative, strided
       (define d (decode EB SB bits))
       (when (and (pair? d) (eq? (car d) 'fin) (> (cdr d) 0))
         (define want-rat (ref-sqrt (cdr d)))
         (for ([mode '(nearest zero up down)])
           (define want (oracle-round EB SB want-rat mode))
           (define got (let ([r (eval/fpsqrt mode (fpb bits))]) (if (fp/nan? r) 'nan (bof r))))
           (unless (equal? got want)
             (set! bad (add1 bad))
             (when (< bad 8) (printf "  sqrt MISMATCH bits=~x mode=~a got=~a want=~a\n" bits mode got want))))))
     (check-equal? bad 0))

   (test-case "sqrt special cases"
     (check-equal? (bof (eval/fpsqrt 'nearest (fpb 0))) 0)              ; sqrt(+0) = +0
     (check-equal? (bof (eval/fpsqrt 'nearest (fpb #x8000))) #x8000)    ; sqrt(-0) = -0
     (check-pred fp/nan? (eval/fpsqrt 'nearest (fpb #x7E00)))           ; sqrt(NaN) = NaN
     (let ([r (eval/fpsqrt 'nearest (fpb #x7C00))])                     ; sqrt(+inf) = +inf
       (check-true (and (fp/infinity? r) (fp/positive? r))))
     (check-pred fp/nan? (eval/fpsqrt 'nearest (fpb #xFC00)))           ; sqrt(-inf) = NaN
     (check-pred fp/nan? (eval/fpsqrt 'nearest (fpb #xBC00)))           ; sqrt(-1.0) = NaN
     (check-equal? (bof (eval/fpsqrt 'nearest (fpb #x4400))) #x4000)    ; sqrt(4.0) = 2.0
     (check-equal? (bof (eval/fpsqrt 'nearest (fpb #x3C00))) #x3C00)    ; sqrt(1.0) = 1.0
     (check-equal? (bof (eval/fpsqrt 'nearest (fpb #x3400))) #x3800)))) ; sqrt(0.25) = 0.5

(exit (run-tests sqrt-tests))
