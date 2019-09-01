#lang racket

(require racket/struct)
(require math/base)

(provide (all-defined-out))
; `value` is a positive integer whose two's complement
; is the bv to represent.
; it simplifies arithmetic
(struct BitVec (width value)
  #:constructor-name mkBV
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (bv) 'BitVec)
      (λ (bv) `(,(BitVec-width bv)
                ,(BitVec-value bv)))))])

; utils
(define BitVec->bits
  (λ (bv)
    (let ([v (BitVec-value bv)])
      (reverse (for/list ([p (range (BitVec-width bv))])
        (if (bitwise-bit-set? v p) 1 0))))))

(define BitVec->BVConst
  (λ (bv)
    `(_
      ,(string->symbol (string-append
                        "bv"
                        (number->string (BitVec-value bv))))
      ,(BitVec-width bv))))

(define initialize/bv
  (λ (width)
    (mkBV width 0)))

(define mkBoolBV
  (λ (b)
    (mkBV 1 (if b 1 0))))
    
(define eval/id
  (curry BitVec-value))

; arithmetic operations
; binary
(define ((eval/bvarith/binop op) bv1 bv2)
  (let ([r (op (BitVec-value bv1)
               (BitVec-value bv2))]
        [d (expt 2 (BitVec-width bv1))])
    (mkBV (BitVec-width bv1)
          (modulo r d))))

(define ((bv-pred op) bv1 bv2)
  (if (= (BitVec-width bv1)
         (BitVec-width bv2))
      (op (BitVec-value bv1)
          (BitVec-value bv2))
      (error "not the same bit-width")))

(define bv= (bv-pred =))
(define bv< (bv-pred <))
(define bv> (bv-pred >))
(define bv≤ (bv-pred <=))
(define bv≥ (bv-pred >=))

(define eval/bvadd (eval/bvarith/binop +))

(define eval/bvsub (eval/bvarith/binop -))

(define eval/bvmul (eval/bvarith/binop *))

(define eval/bvudiv (eval/bvarith/binop /))

(define eval/bvurem
  (λ (bv1 bv2)
    (error "not implemented!")))

; arithmetic negation
(define eval/bvneg
  (λ (bv)
    (let ([width (BitVec-width bv)])
      (mkBV width
            (modulo
             (- (BitVec-value bv))
             (expt 2 width))))))

; bitwise operations
; binary
(define ((eval/bitwise/binop op) bv1 bv2)
  (mkBV
   (BitVec-width bv1)
   (op (BitVec-value bv1)
       (BitVec-value bv2))))

(define eval/bvand (eval/bitwise/binop bitwise-and))

(define eval/bvor (eval/bitwise/binop bitwise-ior))

(define eval/bvshl
  (λ (bv1 bv2)
    (let ([w (BitVec-width bv1)])
      (mkBV w (modulo
               (arithmetic-shift (BitVec-value bv1)
                                 (BitVec-value bv2))
               (expt 2 w))))))

(define eval/bvlshr
  (λ (bv1 bv2)
    (mkBV (BitVec-width bv1)
          (arithmetic-shift (BitVec-value bv1)
                            (- (BitVec-value bv2))))))

; bitwise not
(define eval/bvnot
  (λ (bv)
    (let ([v (BitVec-value bv)]
          [w (BitVec-width bv)])
      (mkBV w (- (- (expt 2 w) 1) v)))))

(define (get/1-exchange-range bv l h)
  (define v (BitVec-value bv))
  (define w (BitVec-width bv))
  (for/list ([i (in-range l h)])
    (let ([mask (arithmetic-shift 1 i)])
      (mkBV w (bitwise-xor mask v)))))

(define (get/1-exchange bv)
  (get/1-exchange-range bv 0 (BitVec-width bv)))

(define get/±1
  (λ (bv)
    (let ([w (BitVec-width bv)])
      (for/list ([f `(,eval/bvadd ,eval/bvsub)])
        (f bv (mkBV w 1))))))

(define get/bv-extended-neighbors
  (λ (bv)
    (append (get/1-exchange bv) (get/±1 bv))))

(define coin-flip
  (λ (p)
    (if (< (random) p)
        #t
        #f)))

(define random/bv
  (λ (w)
    (mkBV w (random-natural (expt 2 w)))))
