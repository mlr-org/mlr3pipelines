# Independent CNF solver and rule audit

Run from the repository root. This directory does not change production
sources. `SOURCE.sha256` identifies the investigated CNF files.

```
python3 -m venv attic/cnf_verify3/independent_solver/.venv
attic/cnf_verify3/independent_solver/.venv/bin/pip install -r attic/cnf_verify3/independent_solver/requirements.txt
attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/independent_solver/rule_proofs.py
attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/independent_solver/calibration.py
```

R requires `checkmate`, `mlr3misc`, and `jsonlite`; the host's R 3.6.3 can run
all scripts. The bridge constructs atoms and clauses explicitly, avoiding the
known pre-R-4.3 mixed-operator dispatch limitation.

Start with `NOTES.md` for chronology and findings, and `PROOFS.md` for general
correctness exclusions and their assumptions. The minimized JSON records
contain valid constructor inputs, first-pass outputs, and second-pass outputs:

* `minimized_sse1.json`: unchanged non-subset bit misses a new SSE1 restriction;
* `minimized_first_order_phase_sse1.json`: the same SSE1 issue already occurs
  before second-order simplification, through elimination of its repair donor;
* `minimized_sse2.json`: reverse count 1-to-2 transition misses SSE2 dispatch;
* `directed_oneend_shrink_min.json`: already-subset range shrink misses SSE2;
* `minimized_subsumption.json`: transient asymmetric matrices skip equality
  subsumption during unit registration.

`oracle.py` contains independent Boolean SAT, MDD, and direct-evaluation
semantics. `r_bridge.R` sources the six real CNF files and offers a persistent
JSON-lines interface. Its optional source-copy instrumentation records the
actual witnesses for local rewrites and unit skips. `trace_simplifier.R`
records complete nested helper-call states for a supplied JSON input:

```
Rscript attic/cnf_verify3/independent_solver/trace_simplifier.R attic/cnf_verify3/independent_solver/minimized_sse1.json
```

The large experiments are independent implementations and generators:

```
attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/independent_solver/structured_families.py --trials 1600 --seed 187882
attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/independent_solver/fixed_point.py --trials 30000
attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/independent_solver/membership_cells.py
attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/independent_solver/exhaustive_two_symbol.py --workers 8
```

The fixed-point search intentionally stops after ten representative residual
rule opportunities. It distinguishes truth preservation from saturation.
The exhaustive two-symbol run covers all at-most-three-clause formulas modulo
membership-cell multiplicity and value order; it can take several minutes.
See its proof scope in `NOTES.md` before interpreting that finite experiment
as an exclusion for larger domains.

`UNIT_CONTAINMENT_ARGUMENT.md` describes why comparison entries stale during
nested unit propagation can safely defer ancestor restrictions, despite
missing equality subsumption. `lifecycle_probe.py` checks its caller-state
assumptions, while `minimized_deferred_skip.json` is a five-clause satisfiable
example showing that raw physical containment can temporarily fail at an
inner registration and recover when its ancestor returns.

`TWO_CLAUSE_PROOF.md` proves soundness and first-order saturation for normalized
inputs with at most two clauses and arbitrarily many symbols/domain values.
`two_clause_probe.py` is its 108,480-execution finite falsification probe.
`first_order_phase_probe.py` explicitly falsifies the attempted extension to
larger formulas, and `first_order_sse1_production_replay.json` confirms that
the four-clause minimized gap survives all later production phases.

`HLA_DOMAIN_REFUTATION.md` and `domain_refutation.py` reformulate clause deletion
as domain propagation under a negated target and emit independently checked
certificates. The 10,000-formula probe found only three known unit-subsumption
leftovers among 62,322 output targets. `SSE2_COVERAGE_PROOF.md` proves that the
static oneend/twoend filters cover every independent useful second-order rule;
the confirmed second-order failures are dynamic scheduling gaps.

`minimized_oneend_symbol_removal.json` covers the additional whole-literal
deletion route into that scheduling gap. `CANDIDATE_REPAIRS.md` explains
source-copy repairs, their local correctness arguments, and paired validation.
`SEMANTIC_PRESERVATION_MAP.md` assembles the proposed canonical-input kernel
proof with all runtime/representation limits explicit. `chain_resource_probe.R`
follows the root's chain design to demonstrate order-dependent recursion
failures on both R 3.6 and current R 4.6.
