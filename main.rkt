#lang racket

(provide (all-defined-out))

(require "sls.rkt"
         "parsing/parse.rkt"
         "parsing/transform.rkt"
         "fp2real.rkt"
         "elim-eqs.rkt"
         racket/cmdline)

(define main
  (λ ()
    (let* ([seed (make-parameter 1)]
           [c2 (make-parameter 1/2)]
           [wp (make-parameter 0.001)]
           [step (make-parameter 200)]
           [start-with-zeros? (make-parameter #t)]
           [try-real-models? (make-parameter #f)]
           [print-models? (make-parameter #f)]
           [elim-eqs? (make-parameter #f)]
           [vns (make-parameter #f)]
           [enable-log (make-parameter #f)]
           [file-to-analyze
            (command-line
             #:program "sls"
             #:once-each
             ["--seed" seed-arg
                       "RNG seed"
                       (define n-seed (string->number seed-arg))
                       (if (and
                            n-seed
                            (exact-positive-integer? n-seed)
                            (>= n-seed 0)
                            (<= n-seed (sub1 (expt 2 31))))
                           (seed n-seed)
                           (error "not a valid seed"))]
             ["--c2" c2-arg
                     "Score scaling constant"
                     (define n-c2 (string->number c2-arg))
                     (if (and
                          n-c2
                          (rational? n-c2)
                          (>= n-c2 0)
                          (<= n-c2 1))
                         (c2 n-c2)
                         (error "not a valid score scaling constant"))]
             ["--wp" wp-arg
                     "Diversification probability"
                     (define n-wp (string->number wp-arg))
                     (if (and
                          n-wp
                          (>= n-wp 0)
                          (<= n-wp 1))
                         (wp n-wp)
                         (error "not a valid diversification probability"))]
             ["--step" step-arg
                       "Search steps"
                       (define n-step (string->number step-arg))
                       (if (and
                            n-step
                            (exact-nonnegative-integer? n-step))
                           (step n-step)
                           (error "not a valid search step"))]
             ["--initialize-with-random" ("Initialize search space with random values"
                                          "otherwise search space is initialized to all 0s")
                                         (start-with-zeros? #f)]
             ["--try-real-models" ("Try real models as initial models") (try-real-models? #t)]
             ["--vns" ("Enable variable neighborhood search") (vns #t)]
             ["--elim-eqs" ("Enable Z3 `solve-eqs` tactics") (elim-eqs? #t)]
             ["--print-models" ("Print models") (print-models? #t)]
             ["--debug" ("Enable logger") (enable-log #t)]
             #:args (filename) ; expect one command-line argument: <filename>
             ; return the argument as a filename to compile
             filename)]
           [input-file (if (elim-eqs?) (eliminate-eqs file-to-analyze) file-to-analyze)]
           [script (file->sexp input-file)]
           [formula (remove-fpconst
                     (simplify
                      (unnest
                       (formula->nnf
                        (remove-let-bindings
                         (get-formula script))))))]
           [var-info (get-var-info script)])
      (begin
        (random-seed (seed))
        (if (enable-log)
            (let* ([oliver-logger (make-logger 'oliver-says)]
                   [oliver-lr (make-log-receiver oliver-logger 'debug)])
              (begin (void 
                      (thread 
                       (λ()(let loop () 
                             (define v (sync oliver-lr))
                             (printf "[~a] ~a\n" (vector-ref v 0) (vector-ref v 1)) 
                             (loop)))))
                     (current-logger oliver-logger)))
            42)
        (define initial-models
          (let ([models (if (start-with-zeros?)
                            (initialize/Assignment var-info)
                            (randomize/Assignment var-info))])
            (if (try-real-models?)
                (let ([real-models (get-real-model input-file)])
                  (if real-models
                      (real-model->fp-model real-models var-info)
                      models))
                models)))
        (define result ((if (vns) sls-vns sls)
          var-info formula (c2) (step) (wp) initial-models))
        (if (equal? (car result) 'sat)
            (begin
              (displayln "sat")
              (if (print-models?)
                  (for ([as (cdr result)])
                    (displayln as))
                  (display "")))
            (displayln "unknown"))))))

(main)
