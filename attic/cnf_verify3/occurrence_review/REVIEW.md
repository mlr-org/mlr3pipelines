# Independent review of the Boolean occurrence argument

The occurrence-specific semantic argument survives this review, with the
scope and HLA qualification below. I found no Boolean semantic
counterexample and no reachable failure of its grouped-column, orphan-birth,
same-name SSE2, or unit-registry claims. This is a source-level review with
finite supporting checks, not a machine-checked proof of R.

The reviewed sources are unchanged and recorded in `SOURCE_HASHES.sha256`.
All new work is confined to this directory. There are no production edits
or commits.

## Exact conclusion and required scope

The conclusion supported by the argument is positional truth-table
preservation for a direct `CnfFormula(list_of_proper_clauses)` call, where
all clauses share **one unchanged universe**, each domain is an ordinary
two-element set of distinct nonmissing character values, and every stored
copy of a name in an initial clause has the same singleton range. Public
constructor plus accepted numeric/character selector paths establish that
representation condition. Repeated positions remain relevant to the
algorithm even though identical copies are semantically idempotent.

The shared-universe condition should be explicit in the theorem statement:
separately valid public clauses from different universes are rejected at
`R/CnfFormula.R:164`. The ordinary-domain and proper-clause qualifications
exclude the known multivalued counterexamples, logical-NA selectors and
unrelated constructor/operator issues. The empty input list independently
returns the correct TRUE constant.

The proof establishes semantic preservation of every completed rewrite and
the returned result, together with finite descent of the abstract
algorithm. It additionally excludes the particular duplicate-driven
unit-HLA exception-count runtime error: unit HLA cannot begin in this
scope. It should **not** be summarized as an unrestricted theorem that all
R API calls or all finite-sized requests are error-free. A complete R
total-correctness statement would need its argument-validity, scalar/index,
and finite-resource assumptions stated as well. No runtime errors occurred
in the valid-scope tests here.

## HLA qualification: a stronger raw invariant is false

The raw grouped implication in Section 3 must be confined to the
pre-HLA phase. During a target's HLA pass it becomes containment in that
target's **virtual** clause. At the start of a later target's pass, only
the cache column addressing that later target needs the original raw
invariant. Earlier targets' columns can retain FALSE bits learned from
their virtual expansions, even if those targets survive unchanged in
`entries`.

This distinction is reachable without malformed values or even duplicated
names. Use the public signed words

```r
list(c(1L, 2L), c(1L, 3L), c(-3L, 4L))
```

When the first target is expanded using the second clause, its virtual
range for X3 becomes `"no"`. The third donor's X3 comparison becomes FALSE,
although the stored first target still has no X3 range. All three stored
clauses remain active. A variant repeating X1 in the first target reaches
the same distinction. `inspect_hla.R` and `hla_column_scope.rds` preserve
the states, including every repeated occurrence.

This refutes a global raw-cache claim extending through HLA; it does not
refute the proposed semantic result. The source updates only column
`meta_idx`, the current target, at `R/CnfFormula_simplify.R:709`, and the
next target uses its own column. Direct virtual-clause equivalence and
grouped-containment checks pass at every observed HLA iteration. Section 6
already points in this direction; making the phase/column qualification
explicit would prevent an invalid strengthening of Section 3.

## Review of the difficult source obligations

| Obligation | Review result |
| --- | --- |
| Homogeneous surviving ranges | Singleton restriction only keeps or deletes the first occurrence, or deletes the whole clause. The nonempty range-update route is unreachable. Unit merging either retains the singleton or reports a real contradiction. |
| Trailing repeated-name columns | For distinct pairs, initialization starts every trailing column TRUE. Pair initialization uses scalar character names, removal uses first `match()`, and non-unit HLA also uses scalar character names. Each changes only the first matching column. The deliberately cleared diagonal is excluded. The lazy unit-HLA vector is separate and is never initialized by a starting donor here. |
| Orphan before cache birth | A not-yet-born clause cannot be targeted by initialized pair counts. Unit propagation's metadata guard disables its cache skip, and an old registry snapshot has enough entries to consume all opposite copies before the synchronous call returns. In earlier preprocessing the explicit name intersection also preserves repetitions, followed by fresh registration. |
| Orphan after cache birth | A surviving orphan requires at least two copies at birth. Its permanent trailing TRUE bit prevents an all-FALSE grouped certificate. If a distinct donor has only one birth column for a still-present name, it remains registered and receives the reverse TRUE update when the target loses that name. |
| Equal-name SSE2 pivots | The repeated `match()` at line 436 really can admit an unrelated exception. The Boolean union case split is necessary. In the only nontrivial opposite-singleton restriction, disjointness forces one donor to lack that name; the twoend donor then subsumes the target remainder, or the other donor's FALSE primary and repeated-match equality force a zero row count. This makes the removal sound. |
| Nested units and old snapshots | A currently registered candidate cannot have the asymmetric pair of Boolean primary bits needed for the skip: creating that asymmetry requires removal of the candidate's symbol, which clears registration. Old snapshots can still skip orphan copies. Those skipped disjuncts remain false under the retained unit. |
| Later deletion of donors or orphan targets | Rewrites are checked against the current conjunction; HLA excludes the current target from its donors. Sequential target deletion updates the remaining-clause list and registry. An orphan has no missing registry cleanup to perform. No proof needs an already deleted donor. |
| Unit-HLA initialization and termination | No active non-unit remains registered for a current unit symbol. The initial count is therefore its full physical width, at least two. The loop cannot start, so no unit deletion invalidates an earlier unit-relative argument. Every recursive change removes a stored occurrence or clause, or reports contradiction; finite snapshots and one-use HLA donors cannot cause an infinite no-op loop. |

## Independent finite observations

`review_events.R` sources the six unchanged production files into a
separate environment and adds observers to an in-memory copy of the
simplifier. It does not source the original proof campaign's oracle or
instrumentation. Every observed output must be identical to a separate
unchanged-source call, including ordered duplicate occurrences.

There are two independent truth-table evaluators: one interprets signed
literal words and one iterates every physical range-list position. The
oracle test deliberately supplies unequal duplicate ranges forming a
tautology; a first-name-only evaluator would fail that control. Clauses and
states are serialized as positional R objects/RDS, never symbol-keyed JSON.

The native R 3.6.3 campaign contains **2,067 formulas**: 552 variations of
seven directed controls, 15 long-copy controls, and 1,500 deterministic
seeded random inputs. It reaches six symbols, eight clauses and physical
width 192. Numeric and character matrix selectors are both exercised.
These are directed and sampled families, not an exhaustive finite bound.

| Checked condition or event | Count |
| --- | ---: |
| Assignment rows | 52,962 |
| Recursive semantic boundaries | 65,780 |
| Pre-HLA cached-pair checks | 839,680 |
| HLA virtual conjunction checks | 8,643 |
| HLA cached-pair checks | 31,598 |
| First-copy removals leaving copies | 1,770 |
| Observed orphan boundaries | 7,006 |
| Later orphan-clause deletions | 626 |
| Non-unit HLA deletions | 13 |
| Same-name SSE2 calls | 3,464 |
| Same-name calls with an unrelated exception | 132 |
| Unit-cache skips from old snapshots | 63 |
| Unit-HLA initializations, all unable to start | 598 |
| Semantic, invariant, or runtime failures | 0 |

The run took 167.1 seconds. `events.rds`, `events.txt` and `events.log`
record the result. The focused R 4.6.1 tests passed **29 expectations** and
check the independent oracle,
lossless repeated selectors and RDS, exact skip/SSE2 events, saved campaign
coverage, HLA's virtual-target qualification, and the shared-universe
precondition. The additional current-R HLA control checks two formulas,
11 virtual conjunction states and 22 cached pairs.

Reproduce from the repository root:

```sh
Rscript attic/cnf_verify3/occurrence_review/review_events.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/occurrence_review/inspect_hla.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/occurrence_review/test_review.R
```

The count `hla_containment_needs_virtual_target` was added after the full
native campaign, so its positive observation is in the focused
`hla_column_scope` artifacts. The assertions on virtual semantics,
grouped containment, trailing columns and exact row counts were present
throughout the 2,067-formula run.
