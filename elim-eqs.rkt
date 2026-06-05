#lang racket

(require "parsing/parse.rkt")
(provide eliminate-eqs simplify-jfs)

;; The `jfs-opt --standard-passes` pipeline, reproduced with z3 tactics alone.
;; JFS's SimplificationPass is z3's `simplify` (with `bv_ite2id=true`) and its
;; ConstantPropagationPass is z3's `propagate-values`; the other standard passes
;; (and-hoist, true/duplicate-constraint elimination, contradiction->false) are
;; done for free by z3's goal/(apply ...) machinery. So this single tactic
;; reproduces the pipeline with no JFS (LLVM/clang) build. `propagate-values`,
;; not `solve-eqs`, so no declared variable is substituted away.
(define jfs-opt-tactic
  (string-append "(then (! simplify :bv_ite2id true) "
                 "propagate-values "
                 "(! simplify :bv_ite2id true) "
                 "propagate-values "
                 "(! simplify :bv_ite2id true))"))

;; Apply a z3 tactic to `file` and return the path to a rewritten SMT-LIB file
;; (original declarations + the resulting goal's literals + check-sat). Robust
;; to inputs without (check-sat) and to z3 emitting an unexpected result: in
;; those cases it falls back to the original file rather than dropping asserts.
(define (apply-tactic file tactic)
  (define temp-file (make-temporary-file "rkttmp~a" file))
  (define raw-content (file->string file #:mode 'text))
  (define decls
    (filter (λ (e)
              (match e
                [`(declare-const ,id ,type) #t]
                [`(declare-fun ,id () ,type) #t]
                [_ #f]))
            (string->sexp raw-content)))
  (call-with-output-file
   temp-file
   (λ (output-port)
     ;; Strip the trailing solver commands and append the tactic application.
     ;; (exit) must be removed too, not just (check-sat): many SMT-LIB files end
     ;; with `(check-sat) (exit)`, and a leftover (exit) makes z3 quit before
     ;; reaching the appended tactic (empty output -> silent fallback).
     (define stripped
       (foldl (λ (s acc) (string-replace acc s ""))
              raw-content
              '("(check-sat)" "(exit)" "(get-model)" "(get-unsat-core)")))
     (display (string-append stripped "\n(apply " tactic ")\n") output-port))
   #:mode 'text
   #:exists 'replace)
  (define z3-output
    (string->sexp (with-output-to-string
                   (thunk (system (~v "z3" (path->string temp-file)))))))
  ;; z3 prints `(goals (goal <lits> :precision … :depth …))`. Take the literals
  ;; up to the first `:keyword` (so a bare `false`/`true` goal is preserved);
  ;; fall back to the original file if the output is not a parseable goal.
  (define (keyword? x) (and (symbol? x) (string-prefix? (symbol->string x) ":")))
  (define real-goal
    (match z3-output
      [`((goals (goal ,gs ...)) ,_ ...) (takef gs (λ (x) (not (keyword? x))))]
      [_ #f]))
  (cond
    [real-goal
     (define asserts (map (λ (x) `(assert ,x)) real-goal))
     (define (print output-port path)
       (for ([decl decls])
         (displayln decl output-port))
       (for ([assert asserts])
         (displayln assert output-port))
       (displayln '(check-sat) output-port))
     (call-with-atomic-output-file temp-file print)
     temp-file]
    [else file]))

;; use Z3's solve-eqs tactic to remove equalities
(define (eliminate-eqs file) (apply-tactic file "solve-eqs"))

;; jfs-opt-equivalent simplification (z3 only)
(define (simplify-jfs file) (apply-tactic file jfs-opt-tactic))
