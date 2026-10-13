# Independent solver round 1 report

Investigation started 2026-09-06 08:18 UTC; this report written at 09:12 UTC.
Production files match `SOURCE.sha256` and have not been changed by this agent.

## Four reproducible completeness defects

All outputs remain semantically equivalent. Each first result admits a
specific implemented rule; a second simplification pass applies it.

| Trigger omitted | Reduced input | Evidence |
|---|---|---|
| Donor shrinks; existing SSE1 non-subset bit stays TRUE | 4 clauses, 3 symbols, domains 3/2/3 | `minimized_sse1.json`, `trace_sse1.json` |
| Reverse donor-to-target count rises from 1 to 2 after manual queue creation | 6 clauses, 3 symbols, domains 4/3/4 | `minimized_sse2.json`, `trace_sse2.json` |
| Oneend donor shrinks an already-subset range, reducing a resolution union | 5 clauses, 4 symbols, domains 2/4/2/2 | `directed_oneend_shrink_min.json` |
| Recursive unit registration sees a stale reverse TRUE bit and skips equality | 3 clauses, 2 symbols, domains 4/3 | `minimized_subsumption.json`, `trace_subsumption.json` |

The third case was derived algebraically after identifying the coarse event
model; it did not come from additional random sampling. The first case also
reproduces the old experiment 210 finding previously labeled non-confluence.
Non-confluence does not explain failure to reach an applicable local rule.

The fourth case survives the earlier unit-merge fix. A diagnostic source-copy
variant requiring the skipped clause's current range length to be strictly
smaller than the current unit domain repairs all 96 order variants of the
small selector family while leaving the unrelated SSE gaps present. No
production fix is proposed as part of this investigation branch.

## Computational exclusion for all small clause/symbol shapes, arbitrary domains

Completed **520,200 executions** covering every at-most-three-clause formula
over at-most-two symbols, modulo value-membership equivalence and value order.
Each symbol has at most eight distinct membership classes; every nonempty
subset of those eight patterns was considered, for both symbols, with every
within-clause symbol order. Clause permutations are already represented by
bit-coordinate permutations.

Every output passed both an ordinary one-hot Boolean SAT equivalence query
and a separately implemented reduced ordered multivalued decision diagram.
There were no production errors or semantic discrepancies. Of 107,968 unit
skips, 107,936 were strict subsets and 32 were mistaken equality skips; the
same 32 outputs retained directly subsumed clauses. No skipped range was
outside its current unit domain in this complete space.

The unbounded-domain interpretation uses the proved fact that intersection,
union, complement, and the simplifier's nested-set equality tests commute with
surjective value refinement. This is a precise bounded-clause/bounded-symbol
exclusion; it does not cover four-clause or three-symbol inputs, malformed
representations, or the known constructor-list API failures.

## Independent formulations and calibration

The new harness uses Python-generated data and JSON transport into the actual
R constructors. It does not source prior R test harnesses or generators.
Boolean SAT uses one atom per symbol/value with ordinary exactly-one clauses;
MDD conjunction uses Shannon decomposition and canonical DAG nodes. Direct
truth tables calibrate both algorithms on small instances.

* 10,024 independent equivalence comparisons calibrated, including 5,448
  unequal pairs; 522 production outputs also matched direct truth tables.
* 1,600 new structured instances: 400 each of non-unit HLA chains, unit HLA
  chains, second-order bundles, and permutation cycles. All passed SAT and
  MDD; up to 92 symbols, 120 clauses, and about `10^83` valuations.
* Audited 39,140 SSE1, 107,691 SSE2, 509,592 non-unit HLA, and 12,807 unit HLA
  events in that structured run. Seven SSE2 events needed current-unit
  context; no event violated the contextually sufficient rule premises.
* 493 nonuniform value refinements preserved the exact output sets.
* An additional exhaustive 12,240-case selector family found 48 equivalent
  equality-subsumption outputs and no semantic errors.

## Proof progress and the next challenge

`PROOFS.md` gives unbounded-domain proofs of every algebraic rewrite schema,
with executable pointwise abstraction checks; HLA donor single-use safety;
HLA loop termination; conditional HTE unreachability; isolation of virtual
matrix updates between targets; and membership-class refinement invariance.

`UNIT_CONTAINMENT_ARGUMENT.md` develops a birth-time certificate argument for
physical unit containment despite transiently stale matrix bits. A FALSE
comparison is sound under units registered before the source clause became a
unit. Induction over those prior constraints appears to close the delayed
propagation obligations while allowing the observed equality gap. It still
needs independent challenge of every inactive-clause caller guard and a
formal treatment of certificate updates.

Promising next directions are larger clause/symbol shape families around
those certificate lifecycle guards, and distinguishing additional omitted
event types from the four already demonstrated. Repeating the earlier random
soundness distributions would add substantially less information.
