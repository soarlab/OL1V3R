# OL1V3R

[![CI](https://github.com/soarlab/OL1V3R/actions/workflows/ci.yml/badge.svg)](https://github.com/soarlab/OL1V3R/actions/workflows/ci.yml)

A theory-level **stochastic local search (SLS)** solver for the SMT theory of
floating-point arithmetic (`QF_FP`, plus some `QF_BV`), a reimplementation of the
ideas in *"Stochastic Local Search for Satisfiability Modulo Theories"*. It
searches the space of variable assignments directly, guided by a structure-aware
objective, rather than bit-blasting to SAT.

It is **incomplete and satisfiability-only**: it answers `sat` (with a model) when
it finds a satisfying assignment, and `unknown` otherwise. It never reports
`unsat` — on an unsatisfiable formula it simply searches until its step budget or
the time limit runs out.

Capabilities:

- IEEE-754 floating point of arbitrary `(exp, sig)` widths, computed bit-faithfully.
- Honors the SMT rounding-mode constants, and **enumerates `RoundingMode`
  variables** over them. (`roundNearestTiesToAway` is not supported — Racket's
  bigfloat has no ties-to-away mode — so instances requiring it are not solved.)
- Term-level `if-then-else`.

## Dependencies

- **[Racket](https://racket-lang.org/)** (8.x or newer).
- **[Z3](https://github.com/Z3Prover/z3)** on `PATH` — used for the default
  `simplify` preprocessing and for `--elim-eqs` / `--try-real-models`. Pass
  `--no-simplify` (and omit those flags) to run without Z3.

## Build

```sh
raco make main.rkt        # compile to bytecode (recommended before benchmarking)
```

## Usage

```sh
racket main.rkt [options] <file.smt2>
```

A good configuration for hard instances:

```sh
racket main.rkt --vns --elim-eqs --try-real-models --step 100000000 --print-models file.smt2
```

Key options (`racket main.rkt -h` for the full list):

| option | meaning |
| --- | --- |
| `--vns` | variable-neighborhood search |
| `--elim-eqs` | additional Z3 `solve-eqs` preprocessing |
| `--try-real-models` | seed the search from a Z3 real-relaxation model |
| `--step <n>` | number of search steps (default is small — set it large) |
| `--seed <n>` | RNG seed (the search is deterministic for a fixed seed) |
| `--no-simplify` | skip the default Z3 `simplify` preprocessing |
| `--print-models` | print the model on `sat` |

> Note: `--step` defaults to a small value; hard instances need a large budget
> (e.g. `100000000`).

## Tests

```sh
racket test/data/fp-test.rkt          # rackunit unit tests
# end-to-end smoke tests:
racket main.rkt --vns --elim-eqs --try-real-models --step 1000000 test/smoke/sat-rm.smt2
racket main.rkt --vns --elim-eqs --try-real-models --step 1000000 test/smoke/sat-ite.smt2
```

CI (GitHub Actions, `.github/workflows/ci.yml`) compiles the project, runs the
unit tests, and runs the smoke tests on every push and pull request.
