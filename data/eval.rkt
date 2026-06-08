#lang racket

(require "bit-vec.rkt"
         "fp.rkt")

(provide (all-defined-out))

(define get-value (λ (assignment sym) (hash-ref assignment sym)))

; the evaluator
(define eval
  (λ (be assignment env)
    (define extend-env (λ (sym val env) (cons (cons sym val) env)))
    (define lookup
      (λ (sym env)
        (cond
          [(null? env) (error "symbol not found during evaluation!~a" sym)]
          [else
           (let ([binding (car env)])
             (if (equal? (car binding) sym)
                 (cdr binding)
                 (lookup sym (cdr env))))])))
    (define eval^
      (λ (be env)
        (match be
          [`(let (,bindings ...) ,body)
           (define new-env
             (foldl
              (λ (binding env)
                (extend-env (car binding) (eval (car (cdr binding)) env) env))
              env
              bindings))
           (eval body new-env)]
          [`(bvneg ,op) (eval/bvneg (eval^ op env))]
          [`(bvadd ,op1 ,op2) (eval/bvadd (eval^ op1 env) (eval^ op2 env))]
          [`(bvsub ,op1 ,op2) (eval/bvsub (eval^ op1 env) (eval^ op2 env))]
          [`(bvmul ,op1 ,op2) (eval/bvmul (eval^ op1 env) (eval^ op2 env))]
          [`(bvudiv ,op1 ,op2) (eval/bvudiv (eval^ op1 env) (eval^ op2 env))]
          [`(bvurem ,op1 ,op2) (eval/bvurem (eval^ op1 env) (eval^ op2 env))]
          [`(bvnot ,op) (eval/bvnot (eval^ op env))]
          [`(bvand ,op1 ,op2) (eval/bvand (eval^ op1 env) (eval^ op2 env))]
          [`(bvor ,op1 ,op2) (eval/bvor (eval^ op1 env) (eval^ op2 env))]
          [`(_ ,op1 ,op2)
           (mkBV op2
                 (string->number (substring (symbol->string op1)
                                            (string-length "bv"))))]
          [`(fp.add ,rm ,op1 ,op2) (eval/fpadd (eval^ op1 env) (eval^ op2 env))]
          [`(fp.sub ,rm ,op1 ,op2) (eval/fpsub (eval^ op1 env) (eval^ op2 env))]
          [`(fp.mul ,rm ,op1 ,op2) (eval/fpmul (eval^ op1 env) (eval^ op2 env))]
          [`(fp.div ,rm ,op1 ,op2) (eval/fpdiv (eval^ op1 env) (eval^ op2 env))]
          [`(fp.neg ,op) (eval/fpneg (eval^ op env))]
          [`(fp.sqrt ,rm ,op) (eval/fpsqrt (eval^ op env))]
          [`(fp.isNormal ,op) (mkBoolBV (fp/normal? (eval^ op env)))]
          [`(fp.isSubnormal ,op) (mkBoolBV (fp/subnormal? (eval^ op env)))]
          [`(fp.isZero ,op) (mkBoolBV (fp/zero? (eval^ op env)))]
          [`(fp.isPositive ,op) (mkBoolBV (fp/positive? (eval^ op env)))]
          [`(fp.isNaN ,op) (mkBoolBV (fp/nan? (eval^ op env)))]
          [`(fp.isInfinite ,op) (mkBoolBV (fp/infinity? (eval^ op env)))]
          [`((_ to_fp ,new-exp-width ,new-sig-width) ,rm ,op)
           (eval/fpconv (eval^ op env) new-exp-width new-sig-width)]
          [`(ite ,c ,thn ,els)
           (if (eval-bool c env) (eval^ thn env) (eval^ els env))]
          [(struct FloatingPoint _) be]
          [(struct BitVec _) be]
          [`(,op ...) ((displayln op) (error "unsupported operations"))]
          ;[else (get-value assignment be)])))
          [else (lookup be env)])))
    ;; Hard-evaluate a boolean condition (for `ite`) to #t/#f, mirroring the
    ;; truth conditions of score.rkt (an atom scores 1 iff true). Mutually
    ;; recursive with eval^ for the term operands.
    (define eval-bool
      (λ (be env)
        (define (fpcmp op a b)
          (and (not (fp/nan? a)) (not (fp/nan? b)) (op a b)))
        (match be
          ['⊤ #t]
          ['⊥ #f]
          [`(¬ ,b) (not (eval-bool b env))]
          [`(not ,b) (not (eval-bool b env))]
          [`(∧ ,bs ...) (andmap (λ (b) (eval-bool b env)) bs)]
          [`(and ,bs ...) (andmap (λ (b) (eval-bool b env)) bs)]
          [`(∨ ,bs ...) (ormap (λ (b) (eval-bool b env)) bs)]
          [`(or ,bs ...) (ormap (λ (b) (eval-bool b env)) bs)]
          [`(= ,a ,b)
           (let ([x (eval^ a env)] [y (eval^ b env)])
             (match x
               [(struct FloatingPoint _)
                (or (and (fp/nan? x) (fp/nan? y))
                    (bv= (FloatingPoint->BitVec x) (FloatingPoint->BitVec y)))]
               [_ (bv= x y)]))]
          [`(fp.eq ,a ,b)
           (let ([x (eval^ a env)] [y (eval^ b env)])
             (cond [(or (fp/nan? x) (fp/nan? y)) #f]
                   [(and (fp/zero? x) (fp/zero? y)) #t]
                   [else (bv= (FloatingPoint->BitVec x) (FloatingPoint->BitVec y))]))]
          [`(fp.lt ,a ,b) (fpcmp fp< (eval^ a env) (eval^ b env))]
          [`(fp.leq ,a ,b) (fpcmp fp≤ (eval^ a env) (eval^ b env))]
          [`(fp.gt ,a ,b) (fpcmp fp> (eval^ a env) (eval^ b env))]
          [`(fp.geq ,a ,b) (fpcmp fp≥ (eval^ a env) (eval^ b env))]
          [`(bvult ,a ,b) (bv< (eval^ a env) (eval^ b env))]
          ;; bare boolean term / fp.isX: eval^ yields a width-1 BV, true iff 1
          [_ (not (zero? (BitVec-value (eval^ be env))))])))
    (eval^ be env)))
