#lang racket

(require "parsing/parse.rkt"
         "score.rkt"
         "data/bit-vec.rkt"
         "data/fp.rkt"
         "data/eval.rkt")

(provide (all-defined-out))

(define get/extended-neighbors
  (λ (v)
    (match v
      [(struct BitVec _) (get/bv-extended-neighbors v)]
      [(struct FloatingPoint _) (get/fp-extended-neighbors v)]
      [_ (error "unimplemented type!")])))

(define get/models
  (λ (assignment asserts)
    (define reachable-vars
      (list->set
       (apply
        append
        (map (λ (a) (get-reachable-vars a assignment)) asserts))))
    (for/list ([pr (hash->list assignment)]
               #:when (set-member? reachable-vars (car pr)))
      (define name (car pr))
      (define value (cdr pr))
      `(assert
        (=
         ,(match value
            [(struct BitVec _) (BitVec->BVConst value)]
            [(struct FloatingPoint _) (FloatingPoint->FPConst value)]
            [_ (error "unimplemented type!")])
         ,name)))))

(define update/Assignment
  (λ (assignment sym val)
    (hash-set assignment sym val)))

(define initialize/Assignment
  (λ (var-info)
    (define initialize-var
      (λ (var-name h)
        (hash-set
         h
         var-name
         (let ([type (hash-ref var-info var-name)])
           (cond
             [(bv-type? type)
              (initialize/bv (get/bv-type-width type))]
             [(fp-type? type)
              (let ([widths (get/fp-type-widths type)])
                (initialize/fp
                 (car widths)
                 (cdr widths)))]
             [(bool-type? type)
              (initialize/bv 1)])
           ))))
    (foldl
     initialize-var
     (make-immutable-hash)
     (hash-keys var-info))))

(define randomize/Assignment
  (λ (var-info)
    (define initialize-var
      (λ (var-name h)
        (hash-set
         h
         var-name
         (let ([type (hash-ref var-info var-name)])
           (cond
             [(bv-type? type)
              (random/bv (get/bv-type-width type))]
             [(fp-type? type)
              (let ([widths (get/fp-type-widths type)])
                (random/fp
                 (car widths)
                 (cdr widths)))]
             [(bool-type? type)
              (random/bv 1)])
           ))))
      (foldl
       initialize-var
       (make-immutable-hash)
       (hash-keys var-info))))

(define (sls-vns var-info F c2 maxSteps wp initial-model)
  (define (sls/do i assignment ni nc)
    (define (select/Candidates assert-scores)
      (let* ([currScore (/ (apply + assert-scores) (length assert-scores))]
             [candAssertion (select/Assertion assert-scores)]
             [get/neighbors
              (λ (candVars)
                (apply append
                       (map
                        (λ (candVar)
                          (define val (get-value assignment candVar))
                          (map ((curry update/Assignment) assignment candVar)
                               (get/fp-neighbors val ni)))
                        candVars)))]
             [select/Move
              (λ (candVars)
                (define neighbors (get/neighbors candVars))
                ; choose the neighbor with the highest score
                (if (empty? neighbors)
                    #f
                    (argmax (λ (a) ((score c2 a) F)) neighbors)))])
        (let ([local-opt (select/Move (get/vars candAssertion assignment))])
          (if (and local-opt
                   (> ((score c2 local-opt) F) currScore))
              ; improving
              (cons #t local-opt)
              ; not improving
              (cons #f #f)))))
    (define select/Assertion
      (λ (assert-scores)
        ;; assume when the score of an assertion is 1, it's satisfiable
        ;; no diversification TODO: UCB
        (define asserts (get/assertions F))
        ;; choose the assertion that has the highest score but is not satisfied
        (cdr (argmax (λ (t) (if (< (car t) 1) (car t) -1))
                     (for/list ([as assert-scores]
                                [a asserts])
                       (cons as a))))))
    (cond
      [(>= i maxSteps) (cons 'unknown '())]
      [else (let* ([asserts (get/assertions F)]
                   [assert-scores (map (score c2 assignment) asserts)])
              (begin
                (log-debug "=========================================")
                (log-debug "~a\n" (map exact->inexact assert-scores))
                (log-debug "~a\n" assignment)
                (if (andmap (λ (s) (= s 1)) assert-scores)
                    ;; if sat, print models and return 'sat
                    (cons 'sat (get/models assignment asserts))
                    ;; if not, select the best-improving candidate
                    ;; note that the candidate can be a random walk
                    (let ([newAssign (select/Candidates assert-scores)])
                      (if (car newAssign)
                          ;; best improving
                          ;; change neighbor id to 1
                          (sls/do (+ i 1) (cdr newAssign) 1 nc)
                          ;; no improving candidate, randomize
                          (begin
                            (log-debug "no improving candidate!~a\n" ni)
                            (let ([new-ni (+ ni 1)])
                              (if (> new-ni nc)
                                  (begin
                                    (log-debug "exhaust neighborhood relations!")
                                    (sls/do (+ i 1) (randomize/Assignment var-info) 1 nc))
                                  (begin
                                    (log-debug "using relation~a\n" new-ni)
                                    (sls/do (+ i 1) assignment new-ni nc))))))))))]))
  (sls/do
   0
   initial-model 1 3))

(define sls
  (λ (var-info F c2 maxSteps wp initial-model)
    (define sls/do
      (λ (i assignment)
        (define select/Candidates
          (λ (assert-scores)
            (let* ([currScore (/ (apply + assert-scores) (length assert-scores))]
                   [candAssertion (select/Assertion assert-scores)]
                   [get/neighbors ; get the neighors of all variables in the candidate assertion
                    (λ (candVars)
                      (apply append
                             (map
                              (λ (candVar)
                                (define val (get-value assignment candVar))
                                (map ((curry update/Assignment) assignment candVar)
                                     (get/extended-neighbors val)))
                              candVars)))]
                   [select/Move
                    (λ (candVars)
                      (define neighbors (get/neighbors candVars))
                      (if (coin-flip wp)
                          ; random walk
                          (begin (log-debug "random walking!")
                                 (cons #t (list-ref neighbors (random (length neighbors)))))
                          ; choose the neighbor with the highest score
                          (cons #f (argmax
                                    (λ (a) ((score c2 a) F))
                                    neighbors))))])
              (let ([local-opt (select/Move (get/vars candAssertion assignment))])
                (if (car local-opt)
                    (cons #t (cdr local-opt))
                    (if (> ((score c2 (cdr local-opt)) F) currScore)
                        ; improving
                        (cons #t (cdr local-opt))
                        ; not improving
                        (cons #f (cdr local-opt))))))))
        (define select/Assertion
          (λ (assert-scores)
            ;; assume when the score of an assertion is 1, it's satisfiable
            ;; no diversification TODO: UCB
            (define asserts (get/assertions F))
            ;; choose the assertion that has the highest score but is not satisfied
            (cdr (argmax (λ (t) (if (< (car t) 1) (car t) -1))
                         (for/list ([as assert-scores]
                                    [a asserts])
                           (cons as a))))))
        (cond
          [(>= i maxSteps) (cons 'unknown '())]
          [else (let* ([asserts (get/assertions F)]
                       [assert-scores (map (score c2 assignment) asserts)])
                  (begin
                    (log-debug "=========================================")
                    (log-debug "~a\n" (map exact->inexact assert-scores))
                    (log-debug "~a\n" assignment)
                    (if (andmap (λ (s) (= s 1)) assert-scores)
                        ;; if sat, print models and return 'sat
                        (cons 'sat (get/models assignment asserts))
                        ;; if not, select the best-improving candidate
                        ;; note that the candidate can be a random walk
                        (let ([newAssign (select/Candidates assert-scores)])
                          (if (car newAssign)
                              ;; best improving or random walk
                              (sls/do (+ i 1) (cdr newAssign))
                              ;; no improving candidate, randomize
                              (begin
                                (log-debug "no improving candidate!")
                                (sls/do (+ i 1) (randomize/Assignment var-info)))
                              )))))])))
    (sls/do
     0
     initial-model)))
