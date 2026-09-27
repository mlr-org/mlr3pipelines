# Ordinary indexing and shape safety of the finite-set kernel

Source audit, 2026-09-06. Source: `R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
All line numbers below refer to that source. No production change is proposed.

## Result and scope

I find no canonical-input counterexample to ordinary indexing/value-shape
safety. The obligations below can be discharged by a prefix induction on the
source's execution. In particular, **successful execution is not a premise**:
at each next indexing operation or scalar test, the preceding invariant and
the source guards establish that operation's arguments have the required
shape. Combining this safety argument with an independently established
finite-execution argument excludes ordinary subscript, missing-index,
wrong-rank, vector-condition and undefined-local-variable failures under the
contract below. This is a human source proof, not a mechanization of R.

The contract is a fixed finite symbol-to-domain map, ordinary nonmissing unique
character domain values, and ordinary lists/vectors/environments with primitive
indexing and membership semantics. Every occurring symbol has one valid,
nonmissing, nonempty name; names in a clause are unique. Actual nonconstant
clauses are nonempty named lists, and each literal is a nonempty proper subset
of its symbol's domain. Constants have already been normalized to scalar TRUE
or FALSE; an empty conjunction list is also allowed. All symbol lookups into
the universe are defined. Arithmetic and indices are representable and the
execution has sufficient allocation and stack resources. There are no external
callbacks that change these objects or overload these primitives.

These are kernel assumptions. They do not follow merely from an object
carrying a `Cnf*` class, as the independently recorded matrix-selector defect
demonstrates. Resource exhaustion, malformed/overloaded objects, overflow,
encoding inconsistencies, completeness and fixed-point claims are outside this
result. This argument does not erase any existing scheduling finding.

There is one important distinction from a blanket "no NA indices" claim:
lines 382 and 424 intentionally apply **NA-producing logical vector `[`**
selectors. Those operations are defined. Lines 383 and 425 remove their NA
results before any scalar `[[` or matrix-coordinate use. The theorem excludes
unsafe consumed indices, not these deliberate intermediate values.

The earlier lifecycle, contextual-matrix and lazy-HLA arguments were read and
checked against the source. The shape proof needs substantially less than the
semantic theorem: in many places **count equals number of TRUE bits**, rather
than semantic exactness of those bits, is sufficient. The separate semantic
theorem remains necessary for correctness of the transformations.

## 1. Stable index spaces and clause lifecycle

Let `m = length(entries)` after the harmless ordering at line 48. The outer
entry list never changes length. Each actual write either replaces one
existing clause/range or changes one existing elimination flag. `is_unit` and
`eliminated` always have length `m` and contain defined logical values.
An entry's inactive status (`eliminated || is_unit`) never becomes false again.
A candidate merged into an older unit may retain `is_unit = FALSE`, but its
`eliminated` flag is TRUE before it can cause recursive work.

Actual clauses only lose symbols and values. Nonempty intersections replace
character ranges; an empty intersection takes the literal-removal branch
before being stored. Symbol deletion keeps a nonempty clause, returns a
contradiction immediately on an empty result, or converts a non-unit to a
singleton. No resumed recursive frame writes an old clause snapshot back:
the actual `entries` writes at 167 and 244 precede callbacks. Thus actual
clause names remain unique and valid, and every surviving range remains an
ordinary nonempty proper character set. Virtual HLA clauses are local copies;
named assignment appends/overwrites a single symbol without changing `entries`.

Before line 530, `symbol_registry[s]` contains exactly the already-registered
live non-units containing `s`. The current preprocessing clause is added only
after its unit restrictions finish. It is therefore harmless for removal to
read an absent registry binding: environment lookup returns NULL, and
`NULL[logical(0)]` is defined. After preprocessing, the registry contains exactly
all live non-unit occurrences. Its indices are distinct, in `1:m`, and carry
the named symbol at insertion. Deletion/conversion removes all relevant
occurrences before callbacks; no clause is inserted a second time.

At line 530, let `A = available` and `n = length(A)`. These are distinct actual
indices of all then-live non-units. The two maps remain fixed:

```
A[k] in 1:m,               k in 1:n,
available_inverse[A[k]] = k,
available_inverse[i] = NA for i outside A.
```

Every later live non-unit belongs to `A`. Every later candidate registered as
a unit also belongs to `A`: original units are registered only in the initial
unique queue, preprocessing units are never registered again, and subsequent
registration is only the non-unit-to-singleton transition. The call-path
argument for this last statement appears in Section 4 below.

Consequently every registry index after line 530 has a defined inverse.
Snapshots can retain a removed occurrence, but still retain an ordinary valid
index and its fixed inverse. Removal never makes an index numerically invalid.

Exact registry/lifecycle relationships here are asserted at the boundaries
that schedule or consume other-clause work. A finite transition can briefly
be between its own writes: for example, a just-created singleton is removed
from its registries before `register_unit` sets its flag, and an eliminated
clause loses registry occurrences after its elimination flag is set. There
is no intervening inference callback. Within such a transition, the fixed
index ranges, saved scalar names and ordinary vector types already establish
the intervening writes' validity; full registry exactness is reestablished
before another scheduling boundary. Treating the pending singleton as an
ordinary active non-unit between these writes would be an incorrect invariant.

## 2. Allocated matrices are different from initialized pairs

`is_not_subset_of` is a list of length `n`. At first its slots are NULL.
At a live outer iteration `p`, line 561 assigns `M[p]` a logical matrix of
shape `n` by `K[p]`, where `K[p]` is the clause's width at that instant and is
at least two. Its column names are the clause's then-unique symbol names.
The matrix is never resized or replaced again. Its entries remain TRUE/FALSE;
only individual logical values are changed. Every symbol that this clause
retains later belongs to its fixed column-name set. Deleted symbols retain
their columns, which the non-unit deletion path clears to FALSE.

Let `p = meta_idx_outer`. At any callback boundary during that outer iteration:

* An active index `k <= p` has an allocated matrix. For `k < p`, if it had
  been inactive when its iteration was skipped, it could not now be active.
  For `k = p`, allocation precedes every callback.
* An index `k > p` can still be active and have a NULL slot. The explicit
  future-index guards are therefore necessary; activity alone is insufficient.
* The value of `p` does not change while recursive callbacks or one of their
  saved `for` snapshots is running. It advances only after the outer body
  completely returns. This is essential for stale unit-propagation snapshots.

`not_subset_count` and `second_order_enabled_matrix` initially have shape
`n` by `n`. Before HLA, the former is numeric/integer, and the latter logical.
For distinct indices, a non-NA count means the pair has been initialized.
Both endpoint matrices then exist. Pair construction initializes both counts
at 608–610 with no intervening callback. No count is ever reset to NA.
All diagonal counts stay NA; every callback scheduler excludes them either
by construction or an explicit non-NA mask. The FALSE diagonal matrix rows
never become TRUE.

At each inference/selection boundary, an initialized count equals the sum of
the corresponding logical row. This is a bookkeeping invariant even when a
comparison has a temporarily stale semantic interpretation:

1. Construction uses `sum` of the row directly.
2. Reverse changes at 185–188 and 270–272 test FALSE before setting TRUE and
   incrementing once.
3. Forward changes at 220–222 recheck TRUE after recursion before clearing
   and decrementing once.
4. Symbol deletion snapshots exactly its TRUE initialized rows, clears the
   entire column and decrements those counts before invoking a callback.

Thus a count is a defined integer-valued number from zero through the fixed
row width. In particular, **count one selects exactly one column name**, and
**count two selects exactly two**. Matrix logical rows cannot contain NA.
Inactive slots may contain stale semantic information, but their numeric and
logical shapes do not change. A conversion to unit leaves its old count/bit
bookkeeping unchanged; it is not evidence for deleting its old columns.

The paired bit/count writes have no callback between them. In the few ordinary
evaluation steps between these writes, count equality is temporarily suspended;
the fixed matrix/vector indices justify the following repair assignment
without needing equality as a premise. No exceptional-name selection or
inference consumes the half-repaired state. This is the explicit small-step
discharge needed to use the boundary invariant in a first-failing-operation
argument.

At the end of pair construction all pairs of surviving non-units have
initialized counts. Each such pair was visited when the later endpoint's
iteration ran; if either endpoint had been inactive then, it could not
survive now. During the second-order phase the active set only decreases.
The matrix created by `not_subset_count != 2L` preserves dimensions. Its NA
cells are uninitialized/diagonal pairs; actual reads at 327, 354 and 361 use
initialized distinct pairs. The manual queue is a genuine two-column matrix
from `which(..., arr.ind = TRUE)`, including when it has zero rows.

## 3. Zero and one non-unit boundaries

The potentially descending `seq.int` at 499 is guarded by the all-units return
at 491. With zero entries, that same equality returns the empty conjunction
before the sequence is constructed. Otherwise the sequence's lower bound is
at most its upper bound, and both are valid actual indices.

Unit preprocessing can leave **zero available non-units even when the input
contained non-units**. Allocation then creates zero-length matrix lists and
0-by-0 count/enable matrices. The `seq_along(available)` loop and second-order
queue have zero iterations. `meta_idx_outer` need not be assigned: no later
helper that reads it is called. Non-unit HLA has no targets. Unit HLA uses
empty vectors/lists, `match(TRUE, logical(0))` returns a scalar NA, and the
explicit guard breaks before a donor lookup or a force of `roe_inverse`.

With exactly one available non-unit, its matrix is `1` by `K`, with `K >= 2`.
Its only row is FALSE; the 1-by-1 count is NA. There are no distinct pair calls.
Non-unit HLA's donor slice is `integer(0)`, with equally empty counts and
`was_used`; the same match guard exits. Unit HLA can still act using that one
non-unit, with ordinary vectors of length one. `[, symbol]` dropping a matrix
column to a vector of length one is intentional; scalar `[[1]]` is defined.
No source path requires an `n > 1` condition that these loops fail to supply.

These claims use the exact ordinary-R boundary shapes, not the absence of
errors in a test run. The harness also exercises these cases directly.

## 4. Recursive helpers and snapshots

### Registration and a stale propagation snapshot

`register_unit` is called only by the initial unit queue (485–488) or by
literal deletion that just produced a singleton (245–252). Initial queue
indices are unique and its registry is empty, so no recursive early visit to
a future initial unit is possible. A newly born singleton is removed from
both symbol registries and then marked unit or eliminated before it propagates.

Each `unit`, `nu = names(unit)`, registry lookup, and domain write at 88–104
therefore has a singleton valid name/index. A previous representative `ur`
is an existing actual index, and its nonempty domain exists. The only empty
intersection returns TRUE before subsequent propagation.

Consider the saved `symbol_registry[nu]` sequence at line 124. At snapshot
creation every element is a live non-unit containing `nu`. If a recursive
call removes it, line 128 skips it. If it becomes a unit on `nu`, it merges
into the already registered representative and is eliminated, so it is also
skipped. If it becomes a unit on another symbol, it has lost `nu`; the
restriction's scalar `match` at 148 gives NA, and line 149 returns before any
range access. This proves that a registered unit cannot be changed or
registered again by a stale propagation frame.

The optional matrix optimization introduces a finer obligation: line 133
may index that other-symbol unit's **old matrix before** the missing-symbol
guard runs. It is still safe. If the snapshot element had `k <= p`, its matrix
was allocated while it was active at snapshot creation; it still exists,
and contains `nu` because the clause contained `nu` then. If `k > p`, line 133's
first short-circuit operand prevents the access. No callback advances `p`,
so a never-allocated future slot cannot cross this boundary in a saved loop.
The registering candidate itself has a valid inverse by Section 1; its own
matrix is used only for `k <= p`, and its retained name was in its allocated
column set. `inso_column` is assigned exactly when `use_inso` becomes TRUE.

The pair count need not be initialized for this optimization. Allocated matrix
slots already have ordinary logical entries in every row. Safety must not
confuse this fact with a semantic claim about an uninitialized pair.

A newly constructed canonical boundary witness exercises the other-symbol
singleton path. Let `X={0,1,2}` and `Y=Z=W={0,1}`, and use the clauses in order:

```
(X=2 OR Y=0), (Y=1 OR X=0), (X=2 OR Z=0),
(X in {0,1} OR W=0), (X in {0,1} OR W=1).
```

First-order work on the last pair creates the unit `X in {0,1}`. The nested
propagation sequence X → Y → X converts the third clause to `Z=0` before the
first X frame reaches it in its original snapshot. Its old matrix still has
the X column; the subsequent restriction returns at the missing-symbol guard.
The result is `X=0 AND Y=0 AND Z=0`, with W free, exactly as direct evaluation
of the input requires. This is a boundary control for the preceding argument,
not a discovered defect or a premise of the proof.

### Restrictions and literal deletion

All restriction callers supply a scalar symbol. Registration and preprocessing
iterate scalar name vectors. First-order SSE uses the count-one row selection.
Second-order calls use one of two explicitly selected scalar names and the
oneend handler's length-one guard. `match(symbol, names(clause))` therefore
returns one integer or NA; the NA guard precedes positional `clause[[...]]`.
An absent named donor range elsewhere returns NULL as intended; this is a
defined list operation, not an invalid index.

The local intersection occurs before any recursive call, so its saved numeric
position is valid throughout that local work. A nonempty stored change is
followed by the matrix-NULL and future-index guards at 170/176. A current
non-unit literal deletion has the corresponding 255/261 guards. Once they
pass, every current or just-deleted symbol is in the fixed matrix columns.
This explains why `match` at 194 and 263 cannot be NA even after the clause
has shortened: it searches the frozen matrix's names, not current positions.

The deletion function's local `sr` deserves attention. Its first value is the
removed-symbol registry snapshot. The unit branch overwrites `sr` with the
remaining-symbol registry at 249, but then returns at 251/252. The non-unit
branch reaching `available_inverse[sr]` at 267 still has the original snapshot.

Reverse-refresh loops at 183 and 269 have no callbacks inside. Every selected
source contains the symbol and has an allocated matrix, via the registry and
non-NA pair mask. Forward loops can recurse; they recheck target activity at
210/283 and source activity immediately after each callback at 226/288.
Lost actual target/source ranges can be read by name as NULL safely. Their
frozen matrix columns and fixed numerical coordinates still exist. The
reentrancy bit check at 220 prevents negative counts and protects later
count-one scalar selection.

### Pair handlers and second-order candidates

Every actual `on_updated_subset_relations` call has an initialized distinct
pair and live non-unit operands. The call sites are 224, 285, 612, 620 and
646. The first two have the activity checks just described; construction has
553/575 and post-callback 613/615/622; the manual queue has 644. No mutation
intervenes between each final guard and call. Its `rowsum` is assigned at
entry. Values above the enabled threshold return; count two selects the
twoend path, zero deletes the target, and only count one reaches 314.

The first-order restriction can change both operands recursively. A NULL
return ends target processing, TRUE ends the whole computation, and source
activity is checked at 329 before second-order work. The already saved pivot
continues to belong to the allocated column-name set even if it has since
disappeared from the actual source. The oneend handler then rechecks the bit
at 386 before a trial; an empty candidate list simply does no work.

The `on_update_range` candidate list is masked by non-NA counts. It may retain
targets that later become inactive, but both handlers recheck target activity
on entry (381/412). A handler returns NULL when its main source becomes
inactive, which stops this outer loop. The enable-matrix coordinates were
initialized before those snapshot candidates were selected.

Candidate lists at 382 and 424 intentionally use logical comparisons that
can be NA for future pairs. Ordinary vector `[` then yields NA elements.
This is a **defined intermediate value**, not a failure: 383 and 425 remove
all NA candidate indices before any `available[[candidate]]` access. Empty
lists and length-one lists need no different treatment.

The oneend handler checks each donor's activity/count after earlier recursion,
then checks that removing its proposed intersection name leaves exactly one
target name (396) and that this name exists in the target (397). The twoend
handler starts from count two, so `symbols_twoend[[1]]`, `[[2]]` and their
opposite orientation are defined. The two names are a fixed local snapshot.
Every candidate rechecks the main count and its two bits (427), donor activity
(431), and target-symbol presence (440). The `match(..., nomatch = 0L)` at
436 returns exactly two ordinary positions, possibly zero; zeros are legal
matrix `[` coordinates and are ignored. `sum` of the resulting zero-, one-
or two-element selection is scalar.

An old registry candidate can lose the intersection symbol and remain live.
The source deliberately tolerates its absent named-list range as NULL at
462–464. The comments claiming presence in both donors are stronger than
needed for indexing or the finite-set rule. The target pivot is present by
397/440, so 453's named matrix column exists. Both donor row coordinates are
fixed valid meta-indices. Each actual `try_sse_2nd_order` call therefore has
defined matrix access and a scalar target pivot. The raw `all`/`any` results
are scalar even for empty ranges.

These call paths also exclude the deliberate `stop` at 473: clause elimination
only receives a live non-unit. During HLA only the current non-unit target is
passed to this helper. A semantic FALSE return is not that error branch.

## 5. Non-unit HLA and the absent self inverse

Before HLA, every live non-unit is an element of `A`, has an allocated matrix,
and has initialized counts against every other live non-unit. Exactly one
live unit exists per `unit_domains` binding; the representative registry is
never used after unit-HLA deletions. Sorting by clause length therefore puts
all live non-units first and exactly `length(unit_domains)` live units last.
The nonnegative split at 658 and guarded sequence at 659 have correct bounds.

During non-unit HLA only the current target can be eliminated. The `for`
sequence is a snapshot, but no future target can be eliminated by an earlier
target's loop. `remaining_nonunit_entries` and the symbol registry lose the
current target together; the current `remaining_other_entries` contains
exactly all other current non-units. No donor range changes in this phase.

For each target, local counts and `was_used` have exactly one entry per donor.
The initial count agrees with the matching row's number of TRUE bits. Earlier
HLA targets changed other row coordinates in each donor matrix, so this
target's original row/count agreement is unaffected. Every current-target
bit clear decrements exactly its donor's local count. The selected match is
guarded against NA at 687, then count one at 690 supplies exactly one symbol.
All universe/name accesses consequently use a scalar valid domain name.

The symbol-registry update loop at 705 includes the target itself if the
target actually contains the pivot. Its self row is FALSE and has remained
FALSE since 569, so it cannot enter the body that computes `roe_idx`.
Every other registry element is in `remaining_other_entries` exactly once.
Hence the unguarded `match` at 710 is non-NA precisely when consumed at 711.
This is the reason the missing self-donor index is safe; an extra NA check is
not an unstated assumption. Literal ranges of absent names are legitimate
NULL values while extending the virtual clause.

Deleting the target removes its registry occurrences and immediately breaks
the loop. Otherwise the selected donor's `was_used` coordinate is valid and
local named assignment extends the virtual clause. Its single-symbol update
does not resize a matrix, mutate a donor, or invalidate a cached index.

## 6. Unit HLA, lazy rows and the change of count type

After non-unit HLA, `remaining_nonunit_entries` is **immutable through return**.
The symbol registry contains only these indices and also remains immutable.
The promise at 730 computes the inverse of exactly this fixed list. It is
forced only in 764 when a symbol registry index is encountered. That index
belongs to the list once, so `roe_inverse[[updating_clause_idx]]` is an existing
nonmissing scalar position. Delayed evaluation introduces no different index
domain, even if the first force occurs after several virtual extensions or
after earlier units were eliminated.

Each unit target starts as an actual singleton, so `unitsymbol` is a scalar
valid name. The count variable at 738 is deliberately overwritten with an
ordinary vector of length `r = length(remaining_nonunit_entries)`; so are
`was_used` and the lazy comparison list. No subsequent call invokes a helper
that consumes the previous count matrix. The remaining operations are the
two character helpers, local assignments, direct unit elimination flags and
the final return helper. A matrix-to-vector type change cannot leak backward
into a pair callback because all those recursive frames have already returned.

For an unallocated donor row, its count still has the initial value

```
length(donor) - as.integer(unit_symbol %in% names(donor)).
```

Unique donor names make this exactly the number of TRUE values in
`names(donor) != unitsymbol`. Every later decrement at 774 first allocates the
row (765–769) and clears exactly one TRUE bit (771–773). An allocated row
therefore always has exactly the recorded number of TRUE bits. No row is
deallocated or recreated. This proves the **shape-only lazy-row lemma**:
when count one selects an unallocated row, its initial row has exactly one
TRUE; when it selects an allocated row, the maintained row has exactly one
TRUE. Thus 753 always yields one symbol, regardless of whether the row was
constructed late. No premise about a completed successful HLA execution, or
even raw semantic exactness, is used for this cardinality argument.

The separate semantic lazy-row lemma is still needed to claim that these
bits accurately describe the current virtual target. It follows from every
earlier pivot visiting and allocating all donors containing that symbol, as
reviewed in `../review_semantics/REVIEW.md`, Section 6. That stronger fact is
compatible with, but unnecessary for, the indexing discharge here.

An update row's `[[symbol]]` at 771 exists because the registry lists only
donors containing that symbol, and that donor's named row was allocated from
its unchanged names. A count reaching zero eliminates only the current unit
and exits that loop. Future unit targets are unaffected. Named list lookup
of an absent virtual-target symbol returns NULL and is safely extended by
the character union. The guarded `match` at 746 handles `r = 0` as well.

## 7. Definition-before-use and scalar conditions

| Binding or condition family | Source-level reason |
| --- | --- |
| `available`, inverse, count matrices and `second_order_enabled` in early helpers | `is_not_subset_of` is NULL throughout initial registration/preprocessing; the helpers return or short-circuit before these reads. The matrices and flag are assigned before the first pair callback. |
| `meta_idx_outer` | First callback with non-NULL matrices is inside the allocated outer body. In the later second-order phase, any actual pair implies `n >= 2` and the outer loop has assigned its final bound. With `n = 0` no relevant callback runs. |
| `inso_column` | `use_inso` starts FALSE and is set to TRUE only in the branch that immediately assigns this column, before the propagation loop. |
| `second_order_enabled_matrix` at 327 | Reached only after an initialized count-one case and, when the ordinary restriction branch is used, the explicit enabled check at 325. It already exists even while the flag is FALSE. |
| `sr` on the non-unit deletion route | The unit-only reassignment has an unconditional return; the continuation uses the removed-symbol snapshot. |
| `rowsum`, exceptional symbols, trial return flags | Assigned before use in each local scope; no branch skips assignment and falls into a read. Count bookkeeping supplies single/two-name selections. |
| `remaining_unit_entries` when there are no units | The assignment's false branch is NULL, an existing defined value; `for (... in NULL)` has zero iterations. |
| `roe_inverse` | A promise is created before the unit loop, over a list that is then fixed, and is used only for members of that list. |
| `if (adr)` / `if (ousr)` / `if (hs2oo)` / `if (ts2o)` | Helpers return only TRUE, FALSE or NULL; each plain scalar use is preceded by its NULL return/break guard, or the caller uses `identical(..., TRUE)`. The registry-removal helper's value is never used as one of these flags. |
| `if (is.na(match_result))` | Every such direct condition uses a scalar query (`symbol` or TRUE), so match returns exactly one index/NA. The vector match in SSE2 instead feeds vector indexing and scalar `sum`/`all`. |
| Remaining `if`, `&&`, `||` operands | Scalar length/count comparisons, scalar flag/matrix element lookups, scalar name membership, or `all`, `any`, `is.null`, `identical`. Their argument index shapes are discharged above. No vector-valued source expression is deliberately consumed as a condition. |

The static inventory contains all 108 `if` sites and all 34 short-circuit
operator sites, as well as 340 `[`/`[[` sites. It is an independently generated
indexing inventory, not a copy of the prior branch-outcome instrumentation.
The ordinary-source detector rejects vector conditions even on R versions
that would historically accept the first element with a warning.

## 8. Limits of this discharge

There is no remaining source-level indexing obligation identified in this
contract. The proof explicitly relies on the inspected callback graph,
monotonic lifecycle, fixed matrix column sets, count/bit bookkeeping and
ordinary primitive value behavior. A future edit that adds a callback within
a previously atomic refresh, resizes a matrix, reactivates a clause, reuses a
unit index, changes the registry domain during unit HLA, or updates only one
of a bit/count pair must revisit the corresponding argument.

The executable evidence is finite and is reported separately in `README.md`.
It checks the source argument and the instrumentation's boundaries; it is not
the premise that establishes general safety. No successful test campaign is
being substituted for one of the discharges above.

More precisely, suppose a first ordinary argument-validity failure existed.
All preceding operations would be a valid finite execution prefix. The
initial shapes, each guarded call path, and the finite between-write
transitions above propagate the necessary invariant up to that operation.
Its corresponding inventory obligation then gives valid indices, a defined
scalar condition, or the required rank, contradicting that it was the first
failure. No previously proved statement conditional on whole-function normal
return is used in this induction. The earlier proofs supply reviewed source
context; registry lifecycle and count bookkeeping are rederived here as
prefix properties. Separately, ordinary primitive totality, unmodified input
evaluation, representable arithmetic and sufficient resources are assumptions
about the execution environment, not conclusions of the argument-validity
proof. Finite termination is also a separate source argument.
