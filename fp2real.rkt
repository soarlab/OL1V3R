#lang racket

(require "parsing/parse.rkt"
         "parsing/transform.rkt")

;; convert QF_FP to Real in place
(define (real->fp/file file)
  (define (convert output-port path)
    (for ([elem (fp->real (remove-fpconst (file->sexp file)))])
      (writeln elem output-port)))
  (call-with-atomic-output-file file convert))

(define (get-real-model file)
  (define temp-file (make-temporary-file "rkttmp~a" file))
  (define z3-output (open-output-string))
  (call-with-output-file
      temp-file
    (λ (output-port)
      (begin
        (for ([elem (fp->real (remove-fpconst (file->sexp file)))])
          (writeln elem output-port))
        (writeln "(get-model)")))
    #:mode-flag 'text
    #:exists-flag 'replace)
  (process/ports z3-output #f z3-output (~v "z3" temp-file))
  (get-output-string z3-output))
  

(define (model->assignment sexp)
  (define (build-assignment exprs)
    (define (rationalize expr)
      (match expr
        [`(,expr ...) (map rationalize expr)]
        [_ (if (and (number? expr) (inexact? expr))
               (inexact->exact expr) expr)]))
    (foldl
     (λ (expr assignment)
       (match expr
         [`(define-fun ,id () Real ,val)
          (hash-set assignment id (eval (rationalize val)))]
         [_ (error "unsupported model")]))
     (make-immutable-hash)
     exprs))
  (match sexp
    [`(model ,exprs ...) (build-assignment exprs)]
    [_ (error "unsupported model")]))

;(real->fp/file (vector-ref (current-command-line-arguments) 0))
(get-real-model (vector-ref (current-command-line-arguments) 0))