# Boolean occurrence totality review

Completed 2026-09-06. Work is confined to this directory; production source
was neither changed nor committed.

`PROOF.md` supplies the source-level indexing and finite-execution extension
for proper Boolean clauses with homogeneous repeated singleton occurrences.
It composes with the earlier reviewed normal-return semantic result under
ordinary fixed-universe, finite-storage, representable-arithmetic, and
sufficient-resource assumptions. It is not a theorem about every accepted
`Cnf*` representation or every public operator path.

The critical correction is that **unit-HLA lazy count equals row cardinality
is false in this scope**. A four-clause public witness leaves an orphan
`(X1=1 OR X1=1)` beside the unit `X1=0`. Its hypothetical lazy row is entirely
FALSE, while its actual initial count is two. The source's count-one search
exits safely. The proof discharges this boundary by showing that no unit-HLA
starting donor exists. Simply importing the canonical lazy-row proof would
leave a real missing obligation.

The repeated-occurrence extension also needs an occurrence-dependent cost
measure. Fixed-two-clause inputs have `N+3` helper roots, and fixed-three-clause
inputs have a registering frame with `N+1` direct children as copy count N
grows. The canonical clause-count-only helper bounds do not extend. Every
fixed finite input still has finite loops and strict recursive occurrence
descent.

## Reproduce

Run from the repository root:

```sh
Rscript attic/cnf_verify3/boolean_occurrence_totality/reproduce.R
Rscript attic/cnf_verify3/boolean_occurrence_totality/review_totality.R
Rscript attic/cnf_verify3/boolean_occurrence_totality/cost_controls.R

bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/boolean_occurrence_totality/reproduce.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/boolean_occurrence_totality/review_totality.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/boolean_occurrence_totality/cost_controls.R

Rscript attic/cnf_verify3/boolean_occurrence_totality/verify_results.R
```

The existing `cnf-review-r46` environment is documented in
`../review_semantics/R46_ENVIRONMENT.md`. Native R is 3.6.3; the container is
R 4.6.1. The public-path checks load real `checkmate` and source the six CNF
files into a private environment. They invoke class methods explicitly so
the R-before-4.3 mixed-Ops dispatch limitation is not involved.

The original witness search is separately reproducible with:

```sh
Rscript attic/cnf_verify3/boolean_occurrence_totality/find_all_false.R
```

That lightweight search found a witness on its second seeded candidate,
then greedily deleted clauses and occurrences. `all_false_fixture.rds` and
its text form preserve the reduced state. This is a local reduction, not a
claim of global minimality. The final public reproduction does not depend
on reading the search output.

## Results on both R versions

Both versions passed the **same 5,259 public-input cases**, with exactly
matching observation counts, mutation rejection messages, and source MD5s.
This is one deterministic portfolio replayed twice:

* 14 directed controls, including zero/one available nonunits, units,
  duplicate units, repeated-name SSE2, virtual HLA, future matrices, an old
  snapshot reaching a retained other-symbol unit, and the all-FALSE witness;
* 162 repeated/reversed variants of three directed occurrence families;
* 15 long-copy controls, reaching physical width 384;
* all 4,368 ordered lists of one through three clauses from the 16 proper
  signed occurrence words of physical width one or two on two symbols;
* 700 fresh random inputs using seed `6090622`, 2–6 symbols and 2–8 clauses,
  with independently shuffled homogeneous repetitions.

Each observed result is compared identically against the unchanged kernel
and against the direct public formula constructor, preserving duplicate
positions and order. Numeric and character matrix selectors are both
exercised. This portfolio is an indexing/control check; it does not claim
an independent truth-table oracle for every sampled formula. The small
public witness separately checks all eight assignments positionally.

| Observation | Each R version |
| --- | ---: |
| Public input cases and exact result comparisons | 5,259 |
| Scalar source condition checks | 608,283 |
| Named local helper activations | 68,356 |
| Registry boundary checks | 87,380 |
| Cache births with no existing orphan | 8,878 |
| Initialized physical row/count checks | 435,958 |
| Matrix boundary checks | 58,480 |
| Boundaries containing duplicated registry indices | 81,807 |
| Copy deletions retaining more copies | 1,338 |
| Orphan boundary observations | 3,469 |
| Two-position pivots with equal names | 2,495 |
| Repeated first-match positions in SSE2 | 7,700 |
| Zero SSE2 match positions | 615 |
| Old unit snapshot visits | 1,735 |
| Unit-cache skips, all current orphans | 383 |
| Optional future-index short circuits | 322 |
| Future unit registrations | 161 |
| Retained other-symbol unit in an old snapshot | 1 |
| Nonunit-HLA local count checks | 6,118 |
| Nonunit-HLA decrements | 172 |
| Nonunit-HLA self-row exclusions | 36 |
| Unit-HLA initializations, all unable to select a donor | 3,604 |
| Hypothetical lazy count/cardinality mismatches | 49 |
| Hypothetical all-FALSE lazy rows | 23 |
| Strict active restriction-ancestor decreases | 1,323 |
| Maximum source helper depth in this portfolio | 13 |

The runs took 54.8 seconds in native R and 59.2 seconds in the current-R
container while running concurrently. Counts are observations at selected
boundaries; they are not counts of independent states or a coverage proof.
The oneend two-name selection hook was not reached in this portfolio;
its cardinality is discharged from the same physical row-count source
invariant, not from an observed event.

`reproduce.R` additionally passed **20 reduced ordinary-R shape checks** on
each version: first-name list/matrix operations, duplicate and zero selectors,
retained snapshot copies, empty selected-name vectors, all-FALSE guarded
matches, and empty matrix/queue dimensions. It reproduces the exact private
forced-donor failure at source line 754:

```
attempt to select less than one element in get1index
```

The unchanged source returns normally, preserving the unsatisfiable truth
function. Forcing the donor is a diagnostic modification, not a production
counterexample.

`cost_controls.R` passed nine further input executions per version with
matching results: three copy counts for each fixed-clause cost family and
three reversed duplicate-unit chains. The chains reach **helper depth 144**;
the longest checks 2,256 strict restriction-ancestor decreases. These small
controls use direct ordinary kernel lists with the public-representable
homogeneous occurrence shape. The boundary portfolio and these controls
are separate; their reported counts are not combined.

## Observer controls and limits

The observer is constructed here from the unchanged source and does not
source the prior campaigns' generators, oracles, or instrumentation. It
uses the documented old snapshot and same-name SSE2 examples as directed
controls, and independently generated input families for the other checks.
Read-only hooks are added to an in-memory copy, and all original `if` and
short-circuit operands are wrapped with single-evaluation scalar checks.
The latter preserve short-circuit evaluation. The source lexical inventory
has the same 340 indexing, 108 `if`, and 34 short-circuit sites as the
canonical source review. Its CSV is generated independently from R's parser.

Five private source corruptions are rejected on each R version:

| Private modification | Observed rejection |
| --- | --- |
| Replace first-order pivot with `character(0)` | Physical pivot cardinality check |
| Decrement a deleted-column count twice | Physical row/count equality check |
| Force unit HLA to select the orphan donor | Unit-HLA body nonreachability check |
| Decrement a nonunit-HLA local count twice | Local physical count equality check |
| Remove the future-index guard before a NULL matrix read | Native `invalid argument type` |

The positive portfolio counts exclude these corruption executions. The
observer also checks the source helper-stack balance and a conservative
`13*(W0+1)` height bound. Observations support the source proof's concrete
obligations; they do not replace its all-size prefix argument or the
separately reviewed normal-return semantic theorem.

Initial smoke testing corrected two harness assumptions: base R's
`abs(NULL)` is not a numeric empty input, and the public empty-list formula
has universe NULL. No source defect was involved. The smoke artifacts are
not used as additional evidence or added to the final portfolio counts.

## Files

* `PROOF.md`: contract, prefix indexing extension, unit-HLA argument,
  occurrence descent, and exact cost exclusions.
* `reproduce.R`, `reproduction_{r36,r46}.{log,rds}`: public all-FALSE witness,
  independent eight-row truth tables, 20 reduced shapes, and native failure
  when the selection guard is deliberately bypassed in a private copy.
* `review_totality.R`, `observations_{r36,r46}.{log,rds}`: focused public
  portfolio, counters, first fixtures, private corruption controls.
* `cost_controls.R`, `cost_controls_{r36,r46}.{log,rds}`: occurrence cost
  families and deeper duplicate-unit chains.
* `verify_results.R`, `verification.log`: cross-version agreement and source
  identity checks over the completed artifacts.
* `sites_{r36,r46}.csv`, `SOURCE_HASHES.sha256`: lexical positions and the
  unchanged source identity.
* `find_all_false.R`, `all_false_*`: original seeded witness search and
  positional reduced fixture.

No package-wide tests were run, no production fix was attempted, and no
files outside this directory were written by this review.
