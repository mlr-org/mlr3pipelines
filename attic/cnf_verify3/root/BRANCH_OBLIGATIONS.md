# Conditional-outcome coverage and source exclusions

This study concerns the unchanged `R/CnfFormula_simplify.R` identified by
SHA-256 `7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
The source contains **108 executable `if` sites** in the kernel and its local
helpers. Across two completed workloads, 212 of their 216 Boolean outcomes
were observed. The four remaining TRUE outcomes have the source-level
exclusions below. An independent review is in progress in `../branch_review/`.

This is outcome coverage for `if` conditions. It is not coverage of every
short-circuit operand, iteration count, execution path, or combination of
states; those substantially stronger statements do not follow from it.

## 1. Measurement and instrumentation

`branch_obligations.R` parses the actual function body and wraps each `if`
condition with a recorder in an in-memory function copy. The recorder forces
the original condition once, records its value, and returns that same value.
It does not rewrite assignments or replace any set operation. Nested helper
bodies are traversed too; labels record both their owner and exact condition.
No production source or namespace binding is replaced.

Every input runs through both the unchanged function and the observed copy,
with the same universe, and the complete returned R objects must be identical.
The first exact input for every observed outcome is saved. Thus the recorded
outcomes have reproducible witnesses, and the observer's finite behavioral
comparison is separate from the mathematical argument that it preserves each
condition's value. Added observer frames can change resource thresholds, so
this instrument is not used to measure stack capacity.

| Workload | Inputs | Condition evaluations | Outcomes observed |
| --- | ---: | ---: | ---: |
| Mixed width, 2–8 symbols, 2–6 values, 2–24 clauses, plus 17 controls | 10,017 | 4,835,057 | 201 |
| Dense, 2–6 symbols, 2–6 values, 8–40 clauses, plus 17 controls | 20,017 | 57,799,347 | 211 |
| Combined observations | 30,034 | 62,634,404 | 212 |

The 17 controls are seven saved canonical examples in both clause orders,
plus scalar TRUE, scalar FALSE, and an empty formula list. The random
workloads use seeds 9061301 and 9061302; every third generated formula has a
planted model. All proper input ranges are nonempty unique subsets of their
domains. The dense generator's length-one numeric `sample()` when there are
two symbols permits width one as well as two; this detail is retained in the
saved generator and reproduction command, rather than describing that
workload as exclusively nonunit.

Commands from the repository root:

```sh
Rscript attic/cnf_verify3/root/branch_obligations.R
CNF_BRANCH_OUT=attic/cnf_verify3/root/branch_dense CNF_BRANCH_PROFILE=dense CNF_BRANCH_TRIALS=20000 CNF_BRANCH_SEED=9061302 Rscript attic/cnf_verify3/root/branch_obligations.R
```

The `.tsv` files contain readable catalogs, `.rds` files contain first-input
witnesses and source/runtime provenance, and `.log` files record checkpoints.
`branch_combined.tsv` sums matching site IDs, owners and conditions from the
two complete tables. Every site's FALSE outcome was observed. A condition
yielding NA or more than one value would be saved separately and stop the
run; none did. Neither workload produced an observer/output discrepancy.

## 2. The empty-clause test in symbol deletion cannot succeed

**Site 26, production line 239:** `if (!length(clause)) return(TRUE)` follows
`clause[[symbol]] = NULL` in `eliminate_symbol_from_clause()`.

There is exactly one caller of this helper: the empty-intersection branch of
`apply_domain_restriction()`. On any call that reaches that branch, the target
has at least two symbols. Here is the caller case split establishing this
without assuming that the defensive branch itself will catch an error:

| Restriction caller | Why a present target symbol belongs to a live nonunit |
| --- | --- |
| Initial nonunit preprocessing | The loop begins beyond all initial units. A conversion to a unit returns NULL and breaks the current symbol loop; while the current clause remains a nonunit there are no comparison callbacks yet. Future clauses have not been registered and cannot be changed by earlier propagation. |
| Unit registration's registry snapshot | A candidate later becoming a unit on the propagating symbol merges into the existing representative and is marked eliminated, so the loop skips it. If it becomes a unit on another symbol, the propagating symbol is absent and the restriction returns before intersection. The retained representative was withdrawn from the registry before registration. |
| First-order relation handler | Its complete caller graph maintains live nonunit operands before entering the handler. No recursive call intervenes between selecting the pivot and the restriction. The detailed caller checks are in `../proof_state/LIFECYCLE_PROOF.md`. |
| Second-order trial | Both handlers recheck the target and their active donors before every trial, and after recursive changes before proceeding to another trial. The restriction immediately follows its raw set-premise checks. |

A canonical clause has unique symbol names. Deleting one present name from a
clause of length at least two leaves at least one name. Hence the test is
FALSE whenever reached. If the result has one name, the following branch
registers a unit; it does not call symbol deletion on that singleton again.
If the old snapshot's symbol is absent, the restriction has already returned
without reaching this test.

This exclusion depends on the full caller/lifecycle invariant and canonical
names. It is not a claim about arbitrary direct calls to an extracted local
helper or malformed accepted selector output.

## 3. The ordinary clause-deletion helper is never called on a unit

**Site 75, production line 473:** `if (is_unit[[clause_idx]]) stop(...)` in
`eliminate_clause_update_sr()`.

All four production call sites are covered:

1. Range restriction at line 156: the preceding caller split supplies a
   nonunit whenever the selected symbol exists. The containment branch
   performs no callback before deleting its target.
2. Direct subsumption at line 310: `on_updated_subset_relations()` has live
   nonunit operands at entry. It has performed no recursive operation before
   its count-zero branch.
3. Nonunit HLA at line 698: its targets come from the final nonunit partition.
   The pairwise phases have ended; HLA makes only virtual additions and whole
   target deletions, so it cannot turn a nonunit into a unit.
4. Nonunit HLA at line 714: the same target invariant applies to hidden
   subsumption discovered after a virtual addition.

The HLA target partition has the right size because every registered symbol
has exactly one retained unit representative before HLA. Units only start
disappearing in the later separate unit-HLA loop, which marks elimination
directly and never calls this helper. Its metadata therefore does not create
an additional unit caller.

The guard is useful as a consistency assertion, but its TRUE branch is
unreachable under the reviewed lifecycle. This is stronger evidence than
its absence in millions of tests.

## 4. Neither immediate HLA full-domain test can succeed

**Sites 98 and 104, production lines 694 and 756.** A selected donor `D` has
exactly one raw non-subset comparison against the current virtual target `T`,
at a pivot `s`. Therefore some value `v` belongs to `D_s` and not to `T_s`.
The extension is

```
T'_s = T_s union (Omega_s minus D_s).
```

That same `v` still does not belong to `T'_s`. Thus `T'_s` is a proper subset
of its domain and cannot have the domain's length when its values are unique.
The argument applies to both loops. It requires the exact quiescent initial
rows and the exact monotone virtual updates; for the unit loop, the reviewed
lazy-row lemma supplies the delayed initialization premise.

The duplicate-domain extension is also established elsewhere: virtual ranges
are submultisets of the stored domain, so missing a value still prevents
length equality. The algebra alone must not be used to assume accurate
selection counts; the source premises are proved in
`../review_semantics/REVIEW.md` and `../review_hla/REVIEW.md`.

Hidden **subsumption** remains reachable and useful. The exclusion concerns
these two immediate full-domain branches, not the entire HLA procedure.

## 5. A further consequence: where contradiction signals originate

For a nonlogical canonical input, every successful contradiction signal
originates at `register_unit()` line 102, where two same-symbol unit ranges
have empty intersection. The only other source of a fresh TRUE signal in a
helper is the excluded empty-clause test at line 239. Every other TRUE return
in a helper forwards a TRUE received from another helper; the final FALSE
returns likewise forward such a signal.

The finite caller graph and the source termination argument rule out a
signal with no originating event. Hence every normally returned FALSE on
such an input comes with a concrete conflicting pair of registered unit
constraints. Those units may themselves have been derived by earlier SSE
or propagation; this statement does not say that contradictory units had to
appear in the original input. Scalar FALSE is handled separately by the
initial logical-input return.

This local origin classification complements the semantic theorem. It does
not imply completeness of contradiction detection: canonical contradictions
without a derivable unit conflict can remain unchanged.
