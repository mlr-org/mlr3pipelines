# Ordinary Boolean formulas with repeated selector occurrences

This stream made no production edits or commits. All constructions use the
unchanged sources in `SOURCE_HASHES.sha256`, real checkmate, public CNF
constructors, and one-row matrix selectors. Signed integer occurrence
words preserve order and repetition; results and states use positional
lists and RDS. No symbol-keyed JSON oracle is used.

No Boolean semantic counterexample was found. `PROOF_ATTEMPT.md` gives a
reviewable source-level argument for arbitrary size in the stated ordinary
two-value, same-singleton-copy input scope. That argument is separate from
the finite evidence below. It is not a machine-checked proof of R, nor an
exclusion covering arbitrary malformed constructors or multivalued ranges.

## The invariant that survived directed attacks

The canonical per-column invariant is false with duplicate occurrences.
After first-copy removal, the first cached column can be FALSE while a
live identical copy remains; the clause can also be absent from the symbol
registry. The useful replacement groups all cached columns with one name:

> For distinct live non-units with initialized pair counts, all-FALSE
> columns for a symbol imply raw containment of that symbol's current
> literal. Every trailing duplicate-name column stays TRUE.

An orphaned live occurrence always has a duplicate column at its cache
birth. Before cache birth only unit propagation can change the clause, and
the cache skip is disabled there, so its old registry snapshot consumes
every opposite copy before the matrix is created. After birth, deleting a
copy can clear registration but cannot erase the trailing TRUE columns.
The proof records why both halves are needed.

The two-exception case cannot silently treat column names as distinct.
`(1,2), (1,1,3), (-1,-1,3)` reaches a same-name twoend call in which the
other donor has exceptions `(S1,S2)`: repeating `match(S1, ...)` really does
count one bit twice and admit the unrelated S2 exception. The source
argument handles this by enumerating the Boolean union and disjointness
outcomes, rather than asserting a false distinct-symbol certificate.

## A failed stronger hypothesis: Boolean unit skips are reachable

The following signed occurrence words give a short directed example:

```r
list(c(-2L, 3L), c(-1L, -3L), c(3L, 2L, 1L, 1L), c(2L, -1L))
```

S1 becomes the unit `0`. One propagation visit removes the first opposite
S1 copy from the fourth stored clause and clears all of that clause's S1
registrations. A later occurrence of the same index in the old registry
snapshot sees the cleared primary bit and skips the remaining opposite
copy. The saved state has the live clause `(S3=1 OR S1=1)`, with no S1
registration. This refutes the initial claim that Boolean unit-cache skips
are unreachable; it does not refute semantic preservation.

The narrower source claim is that a skip cannot occur while the candidate
is **currently** registered. The proof therefore concludes only that no
live non-unit remains registered for a unit symbol at the HLA boundary.
Orphan copies can remain. Unit HLA initializes each donor count as its
physical width minus current registry membership. Every physical non-unit
has width at least two, and that membership is absent, so no starting
count is one and the loop cannot start. This handles later possible unit
deletion without assuming physical unit containment of orphan copies.

`reproduce.R` checks the skip, same-name exception, first-copy orphan,
eventual orphan-clause deletion, and non-unit HLA deletion in the presence
of an orphan using unchanged and instrumented public constructor calls.

## Directed finite evidence

`run_directed.R` varies first-copy deletion neighborhoods, both signs of
neighboring clauses, input and occurrence orders, independent padding of
second-order resolution clauses, same-name exception donors, implication
cycles, derived units, pseudounit contradictions and long repeated words.
`run_hla.R` varies implication-cycle and hidden-literal ladder patterns,
literal signs, padding positions and clause/occurrence reversals.

These are deterministic directed families, not an exhaustive enumeration
of every formula up to their largest sizes. Their union reaches six
symbols, seven clauses and physical width 48. Each run independently
compares the selected original literal words, the constructed positional
clauses, the unchanged simplifier output, and the instrumented output.

| Check/event | Deletion and resolution families | HLA families | Total |
| --- | ---: | ---: | ---: |
| Formulas | 1,514 | 1,872 | 3,386 |
| Assignment rows | 24,224 | 119,808 | 144,032 |
| Recursive semantic-boundary checks | 58,388 | 149,746 | 208,134 |
| Cached-pair invariant checks | 292,998 | 3,194,170 | 3,487,168 |
| First-copy deletions leaving another copy | 1,718 | 618 | 2,336 |
| Observed orphan occurrences at checked boundaries | 8,914 | 14,748 | 23,662 |
| Later orphan-clause deletions | 239 | 56 | 295 |
| Same-name twoend calls | 2,426 | 644 | 3,070 |
| Same-name calls admitting an unrelated exception | 366 | 196 | 562 |
| Non-unit HLA donors | 1,347 | 2,716 | 4,063 |
| Non-unit HLA deletions | 94 | 32 | 126 |
| Unit-cache skips of old snapshot entries | 2 | 77 | 79 |
| Unit-HLA starting donors | 0 | 0 | 0 |
| Semantic mismatches or runtime errors | 0 | 0 | 0 |

Native R 3.6.3 and current R 4.6.1 ran the same 3,386 formulas successfully.
The extra-exception event and the explicit current-membership assertion
were added after the native HLA run; their HLA counts/checks in this table
come from the R 4.6.1 replay. All other counts agree between versions.
`*_r46.rds` contains the final instrumentation for both families.

The checker additionally observed that equal surviving Boolean literals
never have mutually TRUE primary bits in both matrix directions. The
same-name proof does not need that stronger observation. Assertions also
check singleton/equal surviving ranges, orphan cache-birth multiplicity,
row counts, permanent trailing TRUE columns, grouped FALSE containment,
and the absence of registered unit-symbol non-units before HLA. Every
completed instrumented output must be identical to an independently run
unchanged output, including every ordered duplicate occurrence.

The focused `test_audit.R` suite passed **54 testthat expectations** on R
4.6.1. It checks the independent oracle against a hand formula, deliberately
unequal duplicates, lossless RDS round trips, long public selector words,
the exact difficult source paths, unit propagation, and saved finite
coverage. No whole-package test run was requested or performed by this
stream.

## Reproduction

From the repository root:

```sh
Rscript attic/cnf_verify3/boolean_occurrences/reproduce.R
Rscript attic/cnf_verify3/boolean_occurrences/run_directed.R
Rscript attic/cnf_verify3/boolean_occurrences/run_hla.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/boolean_occurrences/test_audit.R
```

The existing isolated current-R environment is documented in
`../review_semantics/R46_ENVIRONMENT.md`. Separate replay artifacts can be
created using:

```sh
podman exec -e CNF_BOOLEAN_SUFFIX=_r46 cnf-review-r46 Rscript \
  attic/cnf_verify3/boolean_occurrences/run_directed.R
podman exec -e CNF_BOOLEAN_SUFFIX=_r46 cnf-review-r46 Rscript \
  attic/cnf_verify3/boolean_occurrences/run_hla.R
```
