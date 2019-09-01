#lang racket

(require racket/set)
(require racket/file)
(require "../data/bit-vec.rkt")
(require "../data/fp.rkt")
(require "transform.rkt")

(provide (all-defined-out))

(define string->sexp
  (λ (str)
    (let ([fd (open-input-string str)])
      (define parse-string^
        (λ (fd)
          (let ([c (read fd)])
            (cond
              [(eq? c eof) '()]
              [else (cons c (parse-string^ fd))]))))
      (parameterize ([current-readtable smt-read-table])
        (parse-string^ fd)))))

(define file->sexp
  (λ (fn)
    (string->sexp (file->string fn))))

(define get-formula
  (λ (cmds)
    (let ([get-expr
              (λ (cmd result)
                (match cmd
                  [`(assert, expr) (list '∧ (transform-expr expr) result)]
                  [else result]))])
      (foldl get-expr '⊤ cmds))))

(define get-var-info
  (λ (cmds)
    (let ([get-var
           (λ (cmd result)
             (match cmd
               [`(declare-const ,id ,type) (hash-set result id type)]
               ;e.g., (declare-fun |c::main::main::1::x@1!0&0#1| () (_ FloatingPoint 8 24))
               [`(declare-fun ,id () ,type) (hash-set result id type)]
               [else result]))])
      (foldl get-var (make-immutable-hash) cmds))))

(define bv-type?
  (λ (t)
    (match t
      [`(_ BitVec ,bw) #t]
      [_ #f])))

(define get/bv-type-width
  (λ (t)
    (match t
      [`(_ BitVec ,bw) bw]
      [_ (error "not valid bv type!")])))

(define bool-type?
  (λ (t)
    (match t
      ['Bool #t]
      [_ #f])))

(define fp-type?
  (λ (t)
    (match t
      [`(_ FloatingPoint ,a ,b) #t]
      ; short-cuts
      ['Float16 #t]
      ['Float32 #t]
      ['Float64 #t]
      ['Float128 #t]
      [_ #f])))

(define get/fp-type-widths
  (λ (t)
    (match t
      [`(_ FloatingPoint ,exp-width ,sig-width)
       (cons exp-width sig-width)]
      ['Float16 (cons 5 11)]
      ['Float32 (cons 8 24)]
      ['Float64 (cons 11 53)]
      ['Float128 (cons 15 113)]
      [_ (error "not valid fp type!")])))

(define get/assertions
  (λ (F)
    (match F
      [`(∧ ,as ...) as]
      [_ `(,F)])))

(define (set->ordered-list s)
  (sort (set->list s) #:key symbol->string string<? ))

(define get/vars
  (λ (F assignment)
    (define get/vars/do
      (λ (F)
        (match F
          [`(,fs ...)
           (apply append (map (λ (f) (get/vars/do f)) fs))]
          [(struct FloatingPoint _) '()]
          [(struct BitVec _) '()]
          [_
           (if (number? F)
               '()
               (if (hash-has-key? assignment F)
                   `(,F)
                   '()))])))
    (set->ordered-list (list->set (get/vars/do F)))))

(define get-reachable-vars
  (λ (F assignment)
    (define get/vars/do
      (λ (F)
        (match F
          [`(let ((,id ,binding) ...) ,expr)
           `(,@(apply append (map get/vars/do binding)) ,@(get/vars/do expr))]
          [`(,op ,args ...)
           (apply append (map get/vars/do args))]
          [(struct FloatingPoint _) '()]
          [(struct BitVec _) '()]
          [_
           (if (and (symbol? F)
                    (hash-has-key? assignment F))
               `(,F)
               '())])))
    (get/vars/do F)))

(define build/bvconst
  (λ (v w)
    `(_
      ,(string->symbol
        (string-append "bv" (number->string v)))
      ,w)))

(define read-hex
  (λ (v p o1 o2 o3 o4)
    (define read-hex^
      (λ (p vs)
        (define c (peek-char p))
        (if (or
             (and (char<=? #\0 c)
                  (char<=? c #\9))
             (or
              (and (char<=? #\a c)
                   (char<=? c #\f))
              (and (char<=? #\A c)
                   (char<=? c #F))))
            (begin
              (read-char p)
              (read-hex^ p (string-append vs (string c))))
            vs)))
    (let ([str (read-hex^ p "")])
      (build/bvconst
       (string->number (string-append "#x" str))
       (* 4 (string-length str))))))

(define read-bin
  (λ (v p o1 o2 o3 o4)
    (define read-bin^
      (λ (p vs)
        (define c (peek-char p))
        (if (and (char<=? #\0 c)
                 (char<=? c #\1))
            (begin
              (read-char p)
              (read-bin^ p (string-append vs (string c))))
            vs)))
    (let ([str (read-bin^ p "")])
      (build/bvconst
       (string->number (string-append "#b" str))
       (string-length str)))))

(define smt-read-table
  (make-readtable
   (make-readtable
    #f
    #\x
    'dispatch-macro
    read-hex)
   #\b
   'dispatch-macro
   read-bin))
