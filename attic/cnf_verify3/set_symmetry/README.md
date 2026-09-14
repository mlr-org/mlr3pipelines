# Source-level finite-set symmetry review

`PROOF.md` proves that the unchanged simplifier commutes with per-symbol
injective value renaming, arbitrary nonempty unequal fiber expansion, and
independent reordering of values inside domains and literal ranges. It also
proves the full equal-membership-cell quotient for arbitrary formula shape.
Clause and symbol occurrence order are retained. Source decisions, callbacks,
and iteration schedules coincide; primitive work and memory use need not.

The proof audits all eight value-cardinality predicates and all structural
length/order dependencies. It is independent of semantic correctness and
local saturation. Its separate positional extension includes duplicate-name
selector outputs and their structural errors without asserting their
semantic correctness. Ordinary duplicate-free domains/ranges, consistent
character equality, immutable universes, and adequate resources are explicit
requirements. Accepted constructors do not enforce all these requirements.

## New executable evidence

All source files remain unchanged. The source hash in every result is
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.

`checks.R` chooses 15 directed inputs and 600 new seeded inputs. Each is
compared against five transformations: injective renaming, unequal splitting,
independent within-vector order, splitting plus order, and full membership
quotienting. Split sizes cycle through 2, 3, 4, and 1. Random inputs have
2–6 symbols, domains of size 3–6, 1–12 clauses, and widths 1–4. These are
targeted/empirical checks, not an exhaustive space or the proof of arbitrary
fiber sizes.

Both R 3.6.3 and R 4.6.1 passed independently:

- **615 inputs and 3,075 paired transformations per R version**;
- **3,901,722 source events compared per version** across original and
  transformed observed executions;
- **2,485 original domain values merged into membership cells** across the
  baseline bank, with full-fiber output preservation checked before projection;
- exact unobserved/observed output agreement, using one shared universe per
  comparison, and agreement with the ordinary public constructor route;
- separate TRUE, FALSE, and empty-conjunction controls.

The observer has 290 source sites: 108 `if` sites, 68 short-circuit operand
sites, 23 loop-sequence and 23 loop-iteration sites, two repeat sites, 13
helper entries, nine `all`/`any` sites, and 44 comparison sites. It records
actual values and exact event order. It does not inspect primitive-internal
membership iterations. This is an instrumentation catalog, not a claim that
every possible event path has been tested.

Three deliberate in-memory source changes add output-preserving decisions
based on unrelated range cardinalities, a value spelling hidden inside an
always-true short circuit, and the first value of a range. Each changes the
recorded schedule under its corresponding transformation while leaving the
projected output unchanged. All three controls were detected. A deliberate
partial fiber is rejected before projection, preventing a second possible
false-positive metamorphic comparison.

`replay_controls.R` independently reduces the effective-unit guard's FALSE
outcome to a four-clause input and adds it to the 15 directed controls.
The 16 complete baseline traces (3,747 events) are saved separately on both
R versions and are **identical across versions**, including exact returned
vectors. Each control also tests fixed block expansion and elementwise
renaming as exact ordered-vector identities, domain-order-only identity,
and unequal splitting with independent range/domain permutation.

The reduced event control has domains `V1={q1,q2,q3,q4}` and
`V2={q1,q2,q3}`, with ordered clauses:

```
(V1=q1 OR V2=q1)
(V2=q1 OR V1=q2)
(V1=q1 OR V2=q3)
(V1=q3 OR V2 in {q3,q1}).
```

Its purpose is to exercise the length guard at line 115 with an effective
domain strictly smaller than the saved incoming unit. It is not presented
as a new simplifier bug. All eight cardinality predicates are executed by
the bank. Both outcomes of the six non-HLA predicates were seen; the two
HLA full-domain predicates were FALSE, consistent with the independent
unreachability proof.

`occurrence_controls.R` uses accepted positive matrix selectors to preserve
duplicate occurrence lists, including the previously reported two-clause
error and stale positional output. It compares complete source traces,
positional outputs, warnings, and errors under unequal splitting/permutation
and membership quotienting. **42 inputs and 84 paired transformations passed
on each R version**, with 187,365 observed events per version. One baseline
error was reproduced under both transformations; 41 cases returned normally.
All 42 complete baseline traces and outcomes also agree across R versions.
Their finite results are recorded separately in `occurrence_controls_r36.json`
and `occurrence_controls_r46.json`. To keep an exact accepted selector route,
copies of one stored range receive the same permutation; the canonical bank
already tests fully independent within-range permutation.

One cross-version reporting difference was diagnosed and excluded correctly:
R 3.6's `dput` prints a matrix attribute as `.Dim`, while R 4.6 prints `dim`.
Re-evaluating those saved texts gives identical matrices. Paired trace checks
compare actual R objects and never used that text comparison. The separate
cross-version control comparison also compares actual saved objects.

## Reproduction

From the repository root:

```sh
Rscript attic/cnf_verify3/set_symmetry/checks.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/set_symmetry/checks.R
Rscript attic/cnf_verify3/set_symmetry/replay_controls.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/set_symmetry/replay_controls.R
Rscript attic/cnf_verify3/set_symmetry/occurrence_controls.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/set_symmetry/occurrence_controls.R
Rscript attic/cnf_verify3/set_symmetry/compare_versions.R
```

`harness.R` is the independent source observer and transform/projection
implementation. The JSON files give compact results; RDS files preserve
the exact input bank, source site catalog, first source-event witnesses,
and the complete compact control traces. Logs capture successful runs.
`replay_controls.R` expects the full default `checks_r36.rds` bank because
the uncommon unit-merge guard is selected from its saved source events.

No production or test file was edited, and this stream made no commits.
