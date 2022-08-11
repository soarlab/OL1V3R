#lang racket

(require "data/bit-vec.rkt"
         "data/fp.rkt"
         "data/eval.rkt")

(provide (all-defined-out))

(define Hamming-distance
  (λ (bv1 bv2)
    (let ([bs1 (BitVec->bits bv1)] [bs2 (BitVec->bits bv2)])
      (foldl (λ (t r) (+ r (bitwise-xor (car t) (cdr t))))
             0
             (map cons bs1 bs2)))))

; smt equality/inequality
; bv's equality
(define (score/bv= c bv1 bv2)
  (if (bv= bv1 bv2)
      1
      (* c (- 1 (/ (Hamming-distance bv1 bv2) (BitVec-width bv1))))))
; fp's equality
(define (score/fp= c fp1 fp2)
  (cond
    [(and (fp/nan? fp1) (fp/nan? fp2)) 1]
    [(or (fp/nan? fp1) (fp/nan? fp2)) 0]
    [(bv= (FloatingPoint->BitVec fp1) (FloatingPoint->BitVec fp2)) 1]
    [else (fp-dist-score c fp1 fp2 #f)]))

(define ((score/= c) v1 v2)
  (match v1
    [(struct BitVec _) (score/bv= c v1 v2)]
    [(struct FloatingPoint _) (score/fp= c v1 v2)]
    [_ (error "unimplemented type")]))

; bv's inequality
(define score/bv≠ (λ (bv1 bv2) (if (bv= bv1 bv2) 0 1)))
; fp's inequality
(define score/fp≠
  (λ (fp1 fp2)
    (cond
      [(and (fp/nan? fp1) (fp/nan? fp2)) 0]
      [(or (fp/nan? fp1) (fp/nan? fp2)) 1]
      [else
       (score/bv≠ (FloatingPoint->BitVec fp1) (FloatingPoint->BitVec fp2))])))

(define (score/≠ v1 v2)
  (match v1
    [(struct BitVec _) (score/bv≠ v1 v2)]
    [(struct FloatingPoint _) (score/fp≠ v1 v2)]
    [_ (error "unimplemented type")]))

; fp's equality/inequality
(define ((score/fpeq c) fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) 0]
    [(and (fp/zero? fp1) (fp/zero? fp2)) 1]
    [else (score/fp= c fp1 fp2)]))

(define (score/fp!eq fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) 1]
    [(and (fp/zero? fp1) (fp/zero? fp2)) 0]
    [else (score/bv≠ (FloatingPoint->BitVec fp1) (FloatingPoint->BitVec fp2))]))

(define bv-dist-score
  (λ (c bv1 bv2 eq)
    (define dist
      (+ (abs (- (BitVec-value bv1) (BitVec-value bv2))) (if eq 1 0)))
    (* c (- 1 (/ dist (expt 2 (BitVec-width bv1)))))))

; bv's lt
; score = c*(1-(|bv1-bv2|+1)/2^n)
(define ((score/bv< c) bv1 bv2)
  (if (bv< bv1 bv2) 1 (bv-dist-score c bv1 bv2 #t)))

; bv's geq
; score = c*(1-|bv1-bv2|/2^n)
(define ((score/bv≥ c) bv1 bv2)
  (if (bv≥ bv1 bv2) 1 (bv-dist-score c bv1 bv2 #f)))

; bv's gt
; score = c*(1-(|bv1-bv2|+1)/2^n)
(define ((score/bv> c) bv1 bv2)
  (if (bv> bv1 bv2) 1 (bv-dist-score c bv1 bv2 #t)))

; bv's leq
; score = c*(1-|bv1-bv2|/2^n)
(define ((score/bv≤ c) bv1 bv2)
  (if (bv≤ bv1 bv2) 1 (bv-dist-score c bv1 bv2 #f)))

(define get/fp-pos
  (λ (fp)
    (define exp-width (FloatingPoint-exp-width fp))
    (define sig-width (FloatingPoint-sig-width fp))
    (if (fp/positive? fp)
        (BitVec-value (FloatingPoint->BitVec fp))
        (- 0
           (- (BitVec-value (FloatingPoint->BitVec fp))
              (expt 2 (- (+ exp-width sig-width) 1)))))))

(define fp-dist-score
  (λ (c fp1 fp2 eq)
    (define dist (+ (abs (- (get/fp-pos fp1) (get/fp-pos fp2))) (if eq 1 0)))
    (* c
       (- 1
          (/ dist
             (expt 2
                   (+ (FloatingPoint-exp-width fp1)
                      (FloatingPoint-sig-width fp2))))))))

; note that we're not pursuing a minimal set of operations here
; instead we give each operation its score as well as for its negation
; fp's lt
(define ((score/fplt c) fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) 0]
    [(fp< fp1 fp2) 1]
    ;; fp1 >= fp2 and fp1 != nan and fp2 != nan
    [else (fp-dist-score c fp1 fp2 #t)]))

(define ((score/fp!lt c) fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) 1]
    [(fp≥ fp1 fp2) 1]
    ;; fp1 < fp2 and fp1 != nan and fp2 != nan
    [else (fp-dist-score c fp1 fp2 #f)]))

(define ((score/fpleq c) fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) 0]
    [(fp≤ fp1 fp2) 1]
    ;; fp1 > fp2 and fp1 != nan and fp2 != nan
    [else (fp-dist-score c fp1 fp2 #f)]))

(define ((score/fp!leq c) fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) 1]
    [(fp> fp1 fp2) 1]
    ;; fp1 <= fp2 and fp1 != nan and fp2 != nan
    [else (fp-dist-score c fp1 fp2 #t)]))

(define ((score/fpgt c) fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) 0]
    [(fp> fp1 fp2) 1]
    ;; fp1 <= fp2 and fp1 != nan and fp2 != nan
    [else (fp-dist-score c fp1 fp2 #t)]))

(define ((score/fp!gt c) fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) 1]
    [(fp≤ fp1 fp2) 1]
    ;; fp1 > fp2 and fp1 != nan and fp2 != nan
    [else (fp-dist-score c fp1 fp2 #f)]))

(define ((score/fpgeq c) fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) 0]
    [(fp≥ fp1 fp2) 1]
    ;; fp1 < fp2 and fp1 != nan and fp2 != nan
    [else (fp-dist-score c fp1 fp2 #f)]))

(define ((score/fp!geq c) fp1 fp2)
  (cond
    [(or (fp/nan? fp1) (fp/nan? fp2)) 1]
    [(fp< fp1 fp2) 1]
    ;; fp1 >= fp2 and fp1 != nan and fp2 != nan
    [else (fp-dist-score c fp1 fp2 #t)]))

(define score2
  (λ (op1 op2 assignment env score-bf)
    (let ([bv1 (eval op1 assignment env)] [bv2 (eval op2 assignment env)])
      (score-bf bv1 bv2))))

; bool's score function
; note that bool is treated as bv1
(define score-bool (λ (v) (eval/id v)))

(define score-bool! (λ (v) (- 1 (score-bool v))))

(define score-seq (λ (es sf cf) (foldl (λ (e s) (cf s (sf e))) 0 es)))

(define ((score c assignment [env '()]) formula)
  (define extend-env (λ (sym val env) (cons (cons sym val) env)))
  (let ([env (if (empty? env) (hash->list assignment) env)])
    (match formula
      [`⊤ 1]
      [`⊥ 0]
      [`(let (,bindings ...) ,body)
       (define new-env
         (foldl (λ (binding env)
                  (extend-env (car binding)
                              (eval (car (cdr binding)) assignment env)
                              env))
                env
                bindings))
       ((score c assignment new-env) body)]
      [`(∨ ,es ...) (score-seq es (score c assignment env) max)]
      [`(∧ ,es ...) (/ (score-seq es (score c assignment env) +) (length es))]
      [`(¬ (= ,op1 ,op2)) (score2 op1 op2 assignment env score/≠)]
      [`(= ,op1 ,op2) (score2 op1 op2 assignment env (score/= c))]
      [`(¬ (bvult ,op1 ,op2)) (score2 op1 op2 assignment env (score/bv≥ c))]
      [`(bvult ,op1 ,op2) (score2 op1 op2 assignment env (score/bv< c))]
      [`(¬ (fp.lt ,op1 ,op2)) (score2 op1 op2 assignment env (score/fp!lt c))]
      [`(fp.lt ,op1 ,op2) (score2 op1 op2 assignment env (score/fplt c))]
      [`(¬ (fp.leq ,op1 ,op2)) (score2 op1 op2 assignment env (score/fp!leq c))]
      [`(fp.leq ,op1 ,op2) (score2 op1 op2 assignment env (score/fpleq c))]
      [`(¬ (fp.gt ,op1 ,op2)) (score2 op1 op2 assignment env (score/fp!gt c))]
      [`(fp.gt ,op1 ,op2) (score2 op1 op2 assignment (score/fpgt c))]
      [`(¬ (fp.geq ,op1 ,op2)) (score2 op1 op2 assignment env (score/fp!geq c))]
      [`(fp.geq ,op1 ,op2) (score2 op1 op2 assignment env (score/fpgeq c))]
      [`(¬ (fp.eq ,op1 ,op2)) (score2 op1 op2 assignment env score/fp!eq)]
      [`(fp.eq ,op1 ,op2) (score2 op1 op2 assignment env (score/fpeq c))]
      [`(¬ ,b) (score-bool! (eval b assignment env))]
      [else (score-bool (eval formula assignment env))])))

;(define assignment (hash-set (hash-set (make-immutable-hash) "a" (mkBV 3 1)) "b" (mkBV 3 2)))
