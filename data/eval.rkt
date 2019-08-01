#lang racket

(require "bit-vec.rkt"
         "fp.rkt")

(provide (all-defined-out))

(define get-value
  (λ (assignment sym)
    (hash-ref assignment sym)))

; the evaluator
(define eval
  (λ (be assignment env)
    (define extend-env (λ (sym val env) (cons (cons sym val) env)))
    (define lookup
      (λ (sym env)
        (cond
          [(null? env) (error "symbol not found during evaluation!~a" sym)]
          [else (let ([binding (car env)])
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
                (extend-env
                 (car binding)
                 (eval (car (cdr binding)) env)
                 env))
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
          [`(_ ,op1, op2) (mkBV op2 (string->number
                                     (substring
                                      (symbol->string op1)
                                      (string-length "bv"))))]
          [`(fp.add ,rm ,op1 ,op2) (eval/fpadd (eval^ op1 env) (eval^ op2 env))]
          [`(fp.sub ,rm ,op1 ,op2) (eval/fpsub (eval^ op1 env) (eval^ op2 env))]
          [`(fp.mul ,rm ,op1 ,op2) (eval/fpmul (eval^ op1 env) (eval^ op2 env))]
          [`(fp.div ,rm ,op1 ,op2) (eval/fpdiv (eval^ op1 env) (eval^ op2 env))]
          [`(fp.neg, op) (eval/fpneg (eval^ op env))]
          [`(fp.sqrt ,rm ,op) (eval/fpsqrt (eval^ op env))]
          [`(fp.isNormal ,op) (mkBoolBV (fp/normal? (eval^ op env)))]
          [`(fp.isSubnormal ,op) (mkBoolBV (fp/subnormal? (eval^ op env)))]
          [`(fp.isZero ,op) (mkBoolBV (fp/zero? (eval^ op env)))]
          [`(fp.isPositive, op) (mkBoolBV (fp/positive? (eval^ op env)))]
          [`(fp.isNaN, op) (mkBoolBV (fp/nan? (eval^ op env)))]
          [`(fp.isInfinite, op) (mkBoolBV (fp/infinity? (eval^ op env)))]
          [`((_ to_fp ,new-exp-width ,new-sig-width) ,rm ,op) (eval/fpconv (eval^ op env) new-exp-width new-sig-width)]
          [(struct FloatingPoint _) be]
          [(struct BitVec _) be]
          [`(,op ...) ((displayln op) (error "unsupported operations"))]
          ;[else (get-value assignment be)])))
          [else (lookup be env)])))
    (eval^ be env)))