#lang racket

(require "parsing/parse.rkt")
(provide eliminate-eqs)

;; use Z3's solve-eqs tactic to remove equalities
(define (eliminate-eqs file)
  (define temp-file (make-temporary-file "rkttmp~a" file))
  (define raw-content (file->string file #:mode 'text))
  (define raw-expr (string->sexp raw-content))
  (define decls (filter
                 (λ (e) (match e
                          [`(declare-const ,id ,type) #t]
                          [`(declare-fun ,id () ,type) #t]
                          [_ #f]))
                 raw-expr))
  (call-with-output-file
      temp-file
    (λ (output-port)
      (display
       (string-replace
        raw-content
        "check-sat"
        "apply solve-eqs")
       output-port))
    #:mode 'text
    #:exists 'replace)
  (define z3-output
    (string->sexp
     (with-output-to-string
       (thunk (system (~v "z3" (path->string temp-file)))))))
  (define real-goal (filter list? (cdr (second (car z3-output)))))
  (define asserts
        (map (λ (x) `(assert ,x)) real-goal))
  (define (print output-port path)
   (for ([decl decls])
    (displayln decl output-port))
   (for ([assert asserts])
    (displayln assert output-port))
    (displayln '(check-sat) output-port))
  (begin (call-with-atomic-output-file temp-file print)
  temp-file))

;(eliminate-eqs (vector-ref (current-command-line-arguments) 0))
