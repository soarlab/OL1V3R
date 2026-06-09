#lang racket

(require "../parsing/parse.rkt"
         "../parsing/transform.rkt"
         "../data/fp.rkt"
         "sls.rkt")

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
    (string->sexp (with-output-to-string
                   (thunk (system (~v "z3" (path->string temp-file)))))))
  (match (car z3-output)
    ['sat (model->assignment (second z3-output))]
    [_ #f]))

;; Overlay the (numeric) real-model values onto an all-zeros assignment so that
;; every variable is defined even when z3's model omits one or gives it a
;; non-numeric value (e.g. an algebraic `root-obj` from nonlinear/division
;; constraints, which we cannot convert to a floating-point literal).
(define (real-model->fp-model real-model var-info)
  (foldl (λ (key model)
           (define value (hash-ref real-model key))
           (define type (hash-ref var-info key))
           (if (and (fp-type? type) (real? value))
               (let ([widths (get/fp-type-widths type)])
                 (hash-set model
                           key
                           (real->FloatingPoint value (car widths) (cdr widths))))
               model))
         (initialize/Assignment var-info)
         (hash-keys real-model)))

(define (model->assignment sexp)
  (define (build-assignment exprs)
    (define (rationalize expr)
      (match expr
        [`(,expr ...) (map rationalize expr)]
        [_
         (if (and (number? expr) (inexact? expr)) (inexact->exact expr) expr)]))
    ;; TODO: 1. use macros 2. use eval
    (define (simple-eval expr)
      (match expr
        [`(+ ,o1 ,o2) (+ (simple-eval o1) (simple-eval o2))]
        [`(- ,o1 ,o2) (- (simple-eval o1) (simple-eval o2))]
        [`(- ,o) (- 0 (simple-eval o))]
        [`(* ,o1 ,o2) (* (simple-eval o1) (simple-eval o2))]
        [`(/ ,o1 ,o2) (/ (simple-eval o1) (simple-eval o2))]
        [_ expr]))
    (foldl (λ (expr assignment)
             (match expr
               [`(define-fun ,id () Real ,val)
                (define v (simple-eval (rationalize val)))
                ;; skip values we cannot turn into a rational (e.g. root-obj)
                (if (real? v) (hash-set assignment id v) assignment)]
               ;; skip auxiliary definitions such as z3's `/0` division function
               [_ assignment]))
           (make-immutable-hash)
           exprs))
  (match sexp
    ;; z3 up to ~4.8 wraps the model as `(model (define-fun ...) ...)`.
    [`(model ,exprs ...) (build-assignment exprs)]
    ;; newer z3 (incl. 4.16) prints a bare list `( (define-fun ...) ... )`.
    [(list `(define-fun ,_ ...) ...) (build-assignment sexp)]
    [_ (error "unsupported model")]))

;(real->fp/file (vector-ref (current-command-line-arguments) 0))
;(get-real-model (vector-ref (current-command-line-arguments) 0))
