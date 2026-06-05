#lang racket

(require "parsing/parse.rkt")
(provide eliminate-eqs)

;; use Z3's solve-eqs tactic to remove equalities
(define (eliminate-eqs file)
  (define temp-file (make-temporary-file "rkttmp~a" file))
  (define raw-content (file->string file #:mode 'text))
  (define raw-expr (string->sexp raw-content))
  (define decls
    (filter (λ (e)
              (match e
                [`(declare-const ,id ,type) #t]
                [`(declare-fun ,id () ,type) #t]
                [_ #f]))
            raw-expr))
  (call-with-output-file
   temp-file
   (λ (output-port)
     ;; Strip any existing (check-sat) and append the tactic application, so
     ;; this works even for benchmarks that omit (check-sat) (some QF_FP files
     ;; do); the previous `string-replace "check-sat" ...` left them with no
     ;; command and z3 produced empty output.
     (display (string-append (string-replace raw-content "(check-sat)" "")
                             "\n(apply solve-eqs)\n")
              output-port))
   #:mode 'text
   #:exists 'replace)
  (define z3-output
    (string->sexp (with-output-to-string
                   (thunk (system (~v "z3" (path->string temp-file)))))))
  ;; z3 prints `(goals (goal <lits> :precision … :depth …))`. If it produced
  ;; anything else (empty output, an error), fall back to the original file.
  (define real-goal
    (match z3-output
      [`((goals (goal ,gs ...)) ,_ ...) (filter list? gs)]
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

;(eliminate-eqs (vector-ref (current-command-line-arguments) 0))
