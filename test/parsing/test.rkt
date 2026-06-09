#lang racket

(require rackunit
         rackunit/text-ui
         "../../parsing/parse.rkt"
         "../../parsing/transform.rkt")

(define bv-script
  "(declare-const x (_ BitVec 8))
   (declare-const y (_ BitVec 8))
   (assert (bvult (bvadd x y) (_ bv4 8)))")

;; a let-bound formula (z3 emits these); must parse and transform without error
(define let-script
  "(assert (let ((?x ((_ to_fp 11 53) a!0))) (fp.leq ?x ?x)))")

(define parse-tests
  (test-suite
   "Tests for the SMT-LIB parser and transforms"
   (test-case "string->sexp yields a non-empty command list"
     (define cmds (string->sexp bv-script))
     (check-pred list? cmds)
     (check-true (> (length cmds) 0)))
   (test-case "get-var-info collects declared variables and their sorts"
     (define vi (get-var-info (string->sexp bv-script)))
     (check-true (hash-has-key? vi 'x))
     (check-true (hash-has-key? vi 'y))
     (check-equal? (hash-ref vi 'x) '(_ BitVec 8)))
   (test-case "get-formula + formula->nnf produce a formula"
     (define f (formula->nnf (get-formula (string->sexp bv-script))))
     (check-pred pair? f))
   (test-case "remove-let-bindings runs on a let-bound formula"
     (check-not-exn
      (λ () (remove-let-bindings (get-formula (string->sexp let-script))))))))

;; exit non-zero on any failure/error so CI fails loudly
(exit (run-tests parse-tests))
