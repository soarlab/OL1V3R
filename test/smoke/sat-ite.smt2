; smoke test: satisfiable, exercises a term-level if-then-else
(set-logic QF_FP)
(declare-const x (_ FloatingPoint 8 24))
(declare-const b Bool)
(assert (not (fp.isNaN x)))
(assert (fp.eq (ite b x (fp.neg x)) x))
(check-sat)
