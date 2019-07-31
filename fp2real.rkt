#lang racket

(require "parsing/parse.rkt"
         "parsing/transform.rkt"
         "data/fp.rkt")

(provide get-real-model
         real-model->fp-model)

;; convert QF_FP to Real in place
(define (real->fp/file file)
  (define (convert output-port path)
    (for ([elem (fp->real (remove-fpconst (file->sexp file)))])
      (writeln elem output-port)))
  (call-with-atomic-output-file file convert))

(define (get-real-model file)
  (define temp-file (make-temporary-file "rkttmp~a" file))
  (call-with-output-file
      temp-file
    (λ (output-port)
      (begin
        (for ([elem (fp->real (remove-fpconst (file->sexp file)))])
          (writeln elem output-port))
        (writeln '(get-model) output-port)))
    #:mode 'text
    #:exists 'replace)
  (define z3-output
    (string->sexp
     (with-output-to-string
       (thunk (system (~v "z3" (path->string temp-file)))))))
  (displayln z3-output)
  (match (car z3-output)
    ['sat (model->assignment (second z3-output))]
    [_ #f]))

(define (real-model->fp-model real-model var-info)
  (define (real->fp-by-type key value)
    (define type (hash-ref var-info key))
    (cond
      [(fp-type? type)
       (define widths (get/fp-type-widths type))
       (cons key (real->FloatingPoint value (car widths) (cdr widths)))]
      [else value]))
  (make-immutable-hash (hash-map real-model real->fp-by-type)))

(define (model->assignment sexp)
  (define (build-assignment exprs)
    (define (rationalize expr)
      (match expr
        [`(,expr ...) (map rationalize expr)]
        [_ (if (and (number? expr) (inexact? expr))
               (inexact->exact expr) expr)]))
    ;; TODO: 1. use macros 2. use eval
    (define (simple-eval expr)
      (match expr
        [`(+ ,o1 ,o2) (+ (simple-eval o1) (simple-eval o2))]
        [`(- ,o1 ,o2) (- (simple-eval o1) (simple-eval o2))]
        [`(- ,o) (- 0 (simple-eval o))]
        [`(* ,o1 ,o2) (* (simple-eval o1) (simple-eval o2))]
        [`(/ ,o1 ,o2) (/ (simple-eval o1) (simple-eval o2))]
        [_ expr]))
    (foldl
     (λ (expr assignment)
       (match expr
         [`(define-fun ,id () Real ,val)
          (define new-val (rationalize val))
          (hash-set assignment id (simple-eval new-val))]
         [_ (error "unsupported model")]))
     (make-immutable-hash)
     exprs))
  (match sexp
    [`(model ,exprs ...) (build-assignment exprs)]
    [_ (error "unsupported model")]))

;(real->fp/file (vector-ref (current-command-line-arguments) 0))
;(get-real-model (vector-ref (current-command-line-arguments) 0))
