#lang racket

(require "../../parsing/parse.rkt")
(require "../../parsing/transform.rkt")
(require "../../src/score.rkt")
(require "../../src/sls.rkt")
(require "../../data/fp.rkt")

(define test-script
  "
   (declare-const x\n
   (_ BitVec 8))\n
   (declare-const y (_ BitVec 8))\n
   (assert (or (bvult (bvadd x (bvmul (_ bv2 8) y)) (_ bv4 8))\n
               (= (bvadd x (bvmul (_ bv2 8) y)) (_ bv4 8))))\n
   (assert (or (bvult y (_ bv2 8))\n
               (= y (_ bv2 8))))\n
   (assert (not (bvult (bvadd x y) (_ bv1 8))))")

#|
(define let-script "(assert
 (not (fp.isNaN ((_ to_fp 11 53) a_ackermann!0))))
(assert
 (let ((?x8 ((_ to_fp 11 53) a_ackermann!0)))
 (let ((?x14 (fp.mul roundNearestTiesToEven ?x8 ((_ to_fp 11 53) (_ bv4607182418800017408 64)))))
 (not (fp.eq ?x14 ((_ to_fp 11 53) (_ bv0 64)))))))
(assert
 (let ((?x30 (fp.add roundNearestTiesToEven (fp.mul roundNearestTiesToEven ((_ to_fp 11 53) a_ackermann!0) ((_ to_fp 11 53) (_ bv4652104065864433664 64))) ((_ to_fp 11 53) (_ bv4638321995473517281 64)))))
(let ((?x8 ((_ to_fp 11 53) a_ackermann!0)))
(let ((?x14 (fp.mul roundNearestTiesToEven ?x8 ((_ to_fp 11 53) (_ bv4607182418800017408 64)))))
(let ((?x22 (fp.add roundNearestTiesToEven ((_ to_fp 11 53) (_ bv0 64)) (fp.mul roundNearestTiesToEven ?x14 ((_ to_fp 11 53) (_ bv4652104065864433664 64))))))
(let ((?x28 (fp.add roundNearestTiesToEven (fp.add roundNearestTiesToEven ?x22 ((_ to_fp 11 53) (_ bv4638321995473517281 64))) ((_ to_fp 11 53) (_ bv4638919777955306537 64)))))
(not (fp.eq ?x28 (fp.add roundNearestTiesToEven ?x30 ((_ to_fp 11 53) (_ bv4638919777955306537 64)))))))))))
")
|#
(define let-script
  "(assert
 (let ((?x8 ((_ to_fp 11 53) h1_ackermann!0)))
 (fp.geq ?x8 ((_ to_fp 11 53) (_ bv4487126258331716666 64)))))
(assert
 (let ((?x8 ((_ to_fp 11 53) h1_ackermann!0)))
 (fp.leq ?x8 ((_ to_fp 11 53) (_ bv4487186704622697397 64)))))
(assert
 (let ((?x52 ((_ to_fp 11 53) (_ bv4607182418800017408 64))))
 (let ((?x41 (bvadd (_ bv4293918720 32) (bvor (bvand (bvand ((_ extract 63 32) fresh_to_ieee_bv_!2) (_ bv2147483647 32)) (_ b
v1048575 32)) (_ bv1072693248 32)))))
 (let ((?x45 (concat ((_ extract 15 8) ?x41) (concat ((_ extract 7 0) ?x41) ((_ extract 31 0) fresh_to_ieee_bv_!1)))))
 (let ((?x50 ((_ to_fp 11 53) (concat ((_ extract 31 24) ?x41) (concat ((_ extract 23 16) ?x41) ?x45)))))
 (let ((?x55 (fp.div roundNearestTiesToEven ?x52 (fp.add roundNearestTiesToEven ?x50 ?x52))))
 (let ((?x53 (fp.sub roundNearestTiesToEven ?x50 ?x52)))
 (let ((?x56 (fp.mul roundNearestTiesToEven ?x53 ?x55)))
 (= ?x56 ((_ to_fp 11 53) fresh_to_ieee_bv_!3))))))))))
")

;(define sexp (string->sexp let-script))
;(remove-let-bindings sexp)

;(define test-formula (unnest (formula->nnf (get-formula (string->sexp test-script)))))
;(define test-vars (get-var-info (string->sexp test-script)))
;(define kenken-formula (unnest (formula->nnf (get-formula (file->sexp "../test3.smt2")))))
;(define kenken-vars (get-var-info (file->sexp "../test3.smt2")))

(define fp-test-script
  (file->sexp "../smt-comp/QF_FP/schanda/spark/incorrect_reordering.smt2"))
;(define fp-test-script (file->sexp "../smt-comp/QF_FP/griggio/fmcad12/test_v5_r5_vr5_c1_s9855.smt2"))
;(define fp-test-formula (simplify (unnest (formula->nnf (remove-let-bindings (get-formula fp-test-script))))))
(define fp-test-formula
  (simplify (unnest (formula->nnf (get-formula fp-test-script)))))
;(remove-equalities (cdr (remove-let-bindings (unnest (get-formula fp-test-formula)))) #f)
;(exact->inexact ((score 1/2 (initialize/Assignment (get-var-info fp-test-script))) (remove-fpconst fp-test-formula)))

(define answer
  (hash-set (hash-set (hash-set (make-immutable-hash)
                                'x0
                                (real->FloatingPoint 7.05307148e-38 8 24))
                      'x1
                      (real->FloatingPoint -4.55127728e-40 8 24))
            'x2
            (real->FloatingPoint -2.25255895 8 24)))

;(sls (get-var-info fp-test-script) (remove-fpconst fp-test-formula) 1/2 500 0.0001)
