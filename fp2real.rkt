#lang racket

(require "parsing/parse.rkt"
         "parsing/transform.rkt")

;; convert QF_FP to Real in place
(define (real->fp/file file)
  (define (convert output-port path)
    (for ([elem(fp->real (remove-fpconst (file->sexp file)))])
      (writeln elem output-port)))
  (call-with-atomic-output-file file convert))

(real->fp/file (vector-ref (current-command-line-arguments) 0))