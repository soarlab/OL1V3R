; smoke test: satisfiable, exercises fp arithmetic with a non-default rounding mode
(set-logic QF_FP)
(declare-const x (_ FloatingPoint 8 24))
(assert (fp.isNormal x))
(assert (fp.gt (fp.mul roundTowardPositive x x) (_ +zero 8 24)))
(check-sat)
