# Totality for homogeneous repeated Boolean occurrences

Source review, 2026-09-06. All line numbers refer to unchanged
`R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
The six source hashes are in `SOURCE_HASHES.sha256`.

## Conclusion and exact contract

The Boolean occurrence class has a complete source-level indexing and
finite-execution extension under the ordinary finite-operation contract
below. The canonical indexing proof cannot simply be applied unchanged:
its registry and unit-HLA invariants are false here. Replacing those
invariants as specified in this note closes the consuming-operation
obligations. I found no remaining indexing or termination gap in this
scope. This is a human source argument, not a mechanized proof of R.
The subsequent [independent dependency review](../proof_dependency_review/REVIEW.md)
accepts this extension and separates the structural unit-HLA no-start argument
from the stronger semantic orphan and containment lemmas.

Input is a finite direct list of proper clauses in one unchanged universe.
Every occurring symbol has a valid nonempty, nonmissing name and an ordinary
two-element domain of distinct nonmissing character values. Every physical
range is a singleton; repeated copies of a name within one initial clause
all hold the same singleton. Clauses and ranges are ordinary lists and
vectors. The direct public constructor plus accepted numeric/character
matrix selectors can produce this representation. Interpret disjunctions
positionally, including repeated occurrences.

As in the canonical contract, set/equality/index primitives have their
ordinary consistent behavior, arithmetic and storage indices are
representable, and sufficient allocation and stack resources are available.
No custom dispatch, active binding, caller evaluation, universe mutation,
or inconsistent character encoding changes the operations. These premises
do not assume that the particular next source index is valid, that execution
terminates, or that the whole call returns normally. The empty input list
separately returns TRUE.

Combining the indexing and termination arguments here with the reviewed
normal-return semantics in `../occurrence_review/REVIEW.md` proves positional
truth-function equality of the input and the normal finite returned formula.
The source-level semantic argument is imported with its current-target
virtual-HLA qualification; it is not independently re-proved here.

The conclusion excludes non-Boolean ranges, unequal copies of one name,
missing/empty/overloaded ranges or names, logical-NA selectors, inconsistent
universes, arbitrary nested formula/constant/operator paths, interpreter
resource failure, and overflow. It makes no completeness, canonical-output,
fixed-point, or clause-count-only cost claim. The existing multivalued
selector counterexamples are unaffected.

## 1. The relevant changes to the canonical proof

I read the occurrence proof and its independent review, the canonical
`../index_contract/PROOF.md`, its independent indexing review, and the
total-correctness composition and composition review. The following are
necessary substitutions, not additional assumptions that every execution
already satisfies.

| Canonical statement | Boolean occurrence replacement |
| --- | --- |
| Each clause has unique names. | Clause names may repeat; all copies retain the same singleton. Matrix columns are physical cache-birth occurrences. |
| A symbol registry is a set of all live nonunits containing that symbol. | It is a finite multiset of valid live nonunit indices. Current membership is sound, but retained orphan occurrences may be absent from it. |
| One symbol removal deletes that symbol. | Named list deletion removes its first occurrence. It clears all current registrations of that clause for the symbol and only the first matching cached column. |
| Count one/two identifies one/two distinct exceptional names. | It identifies one/two physical column positions; two names may agree. Scalarity of each separately consumed name is unchanged. |
| Unit-HLA initial count equals the TRUE cardinality of its lazy row. | This equality can fail, even with an all-FALSE hypothetical row. No unit-HLA starting donor exists, so that row is never constructed or consumed. |
| Registry/candidate snapshots have at most the clause count in entries. | Their length is bounded by finite physical occurrence count, possibly much larger than the clause count. |

The last row matters for cost bounds. It does not invalidate finite loops.
The current registry must also be distinguished from a previously captured
R `for` sequence throughout the source proof.

## 2. Prefix lifecycle and physical storage

Let `m` be the original number of entries after sorting, and let `W0` be
the sum of their physical clause widths. No outer entry slot is added,
removed, renumbered, or reactivated. Every actual singleton restriction
either does nothing, eliminates its target, removes its first matching
occurrence, or reports contradiction. A nonempty strict range change is
impossible: intersecting a singleton can only retain it or make it empty.
In particular, lines 166–231 and `on_update_range()` are unreachable.
Unit merging keeps the same singleton or returns contradiction. Surviving
physical copies therefore stay homogeneous and no source write adds a
physical occurrence. Virtual HLA updates do not write ranges to `entries`.

Initial registration at 517–524 appends an index once per physical occurrence.
After that, no operation adds a registration for that nonunit. Removing any
copy of a symbol removes **all** its current registry indices at 242.
Whole-clause elimination removes registrations for all surviving names,
including repeated names; later repetitions simply remove from an already
cleaned vector. Unit conversion removes the remaining name's registrations
before entering `register_unit()`.

Consequently, at scheduling boundaries, every current registry member is
a valid live nonunit containing the named symbol. For any such member,
its multiplicity equals the number of its current physical copies of that
symbol. A live clause may instead have zero membership despite retaining
copies: it is then an orphan for that name. Registry vectors and their
snapshots have length at most `W0`, are ordinary integer vectors, and have
no missing entries. Neither duplication nor later removal makes a saved
numerical index invalid.

The pending-singleton and elimination transitions need the same small-step
qualification as the canonical proof. The assignment at 244 can temporarily
leave a singleton with `is_unit=FALSE`; registry removal at 250 precedes
its registration at 251. Elimination sets its flag at 474 before clearing
its registries at 475–477. No inference callback consumes either incomplete
transition. Fixed indices and scalar saved names justify the intervening
primitive operations.

At 530, `available` and its inverse are fixed distinct actual/meta index
maps. Every later active nonunit and every later newly registered unit
comes from this set. Original units are registered only in the initial
queue; later registration occurs only on a physical nonunit-to-singleton
transition. A clause cannot make that transition twice. The unit domain
map has one live representative per binding, so the nonunit/unit split at
658–659 remains valid despite pseudounits such as `(X OR X)` being physical
nonunits.

## 3. Birth, duplicate columns, and physical counters

Every active index at or below the current outer bound has an allocated
matrix. Future active indices may have NULL slots. Pair-count initialization
still certifies allocation of both endpoints, and diagonal counts remain
NA. Each allocated matrix keeps its original dimensions and names forever,
now including duplicates. Every subsequently retained name appears among
these frozen columns. No primitive matrix operation in this source requires
the names to be unique.

It is useful to retain the reviewed birth lemma: a live nonunit cannot be
orphaned at its own cache birth. Before birth, an initialized-pair handler
cannot target it; its counts are NA. Only unit propagation can remove its
occurrences. The future-index guard disables cache skipping, and the saved
registry sequence has one entry per copy. Each opposite-copy visit removes
one copy, while same-valued propagation deletes the clause. Recursive calls
can remove further copies or make it inactive, but cannot add copies or
advance the outer index. A synchronous propagation frame must return before
cache birth is reached. Earlier preprocessing similarly visits every copy
in the name intersection and registers each surviving nonunit afresh.

Thus an orphan surviving after birth had at least two birth columns of its
name. For distinct rows, every trailing duplicate-name column remains TRUE:
initialization makes it TRUE, and subsequent named matrix accesses and
`match` select only the **first** column. The self row is separately initialized
FALSE and remains FALSE.

The indexing fact needed here is purely physical: at each pre-HLA inference
or selection boundary, every initialized `not_subset_count[i,j]` equals
the number of TRUE bits in row `j` of matrix `i`. Repeated names do not break
the repair argument:

* Pair creation computes each count by an actual row sum (608–610).
* A literal deletion snapshots precisely the initialized TRUE rows of the
  first matched column, clears that column, and decrements just those counts
  (263–266). A later deletion of another physical copy cannot decrement
  an already-FALSE primary column again.
* The reverse loop at 269 may visit the same registry index repeatedly.
  Its FALSE test precedes each TRUE assignment and increment. Only the first
  visit can increment that bit; subsequent duplicate visits see TRUE.
* Range-update repairs are unreachable in this Boolean specialization.

The bit write and its count repair contain no inference callback between
them. The equality is suspended only for that known pending delta; fixed
indices justify the repair itself. Count-one/two selection never consumes
the intermediate state. A duplicate counter increment/decrement would
invalidate this argument, which the private controls explicitly detect.

It follows that count one selects exactly one physical column name, and
count two selects exactly two. Their names may agree. This is enough for
the consuming scalar `[[` and conditional operations, without any
distinct-name semantic premise.

## 4. Consumed indices and stale snapshots

The remaining canonical indexing discharges survive with the preceding
multiset/physical-counter replacements:

| Consuming source boundary | Reason valid in this scope |
| --- | --- |
| Registration, 88–122 | The candidate is a physical singleton; `names(unit)` is one valid name. Any post-allocation candidate has a fixed inverse. Its old matrix is used only when allocated, and scalar character column selection returns the first named column, hence an ordinary vector of `length(available)`. |
| Unit propagation, 124–135 | Snapshot entries may repeat but are valid actual/meta indices. An eliminated candidate is skipped. A same-symbol newborn unit merges or returns contradiction; a retained other-symbol unit has lost the old name. For an index at/below the fixed outer bound, its old matrix still contains the old name; a future index short-circuits before the NULL matrix access. The later missing-symbol guard handles the other-symbol unit. |
| Restriction/deletion, 148–163 and 237–273 | Every caller supplies a scalar symbol. `match()` is one position or NA, with a guard before positional access. A current/deleted name has a nonmissing match in an allocated frozen column set. First-name list deletion is defined even if other copies survive. The unit branch's overwritten `sr` returns before the nonunit branch consumes its old-symbol snapshot. |
| Pair callbacks | The five original scheduling sites retain their initialized-pair and active-operand guards. Duplicate candidates do not bypass a guard; repeated visits may instead do no work. The diagonal remains excluded. The internal unit-deletion `stop` at 473 is unreachable. |
| First-order pivot, 314/322 | Physical count one gives a one-element nonmissing character vector, even if the selected column has a duplicate name. An absent named donor range returns ordinary NULL. |
| Oneend and twoend handlers, 382–441 | Logical vector `[` may produce NA candidates; the explicit filters at 383/425 remove them before `available[[...]]`. Count two gives two valid physical positions. Oneend filters to exactly one target name before a trial. Twoend selects each of its two saved names separately; equality of these names does not make either vector-valued. |
| Repeated/zero two-name match, 436–437 | `match(..., nomatch=0L)` produces two ordinary coordinates, possibly equal or zero. Matrix `[` permits repeated coordinates and ignores zeros. `sum()` stays scalar even on an empty selection. The sum can have a different semantic interpretation when names agree; the occurrence semantics proof already handles that case. |
| Second-order trial, 453–467 | The guarded target pivot is present in its actual clause and frozen matrix columns. Donor rows have fixed indices, even when two donor indices coincide. An old donor can lack an intersection symbol; its named list lookup is then valid NULL. The all/any tests remain scalar on empty sets. |
| Zero/one allocated nonunit | The early all-unit guard and zero-length sequences behave as in the canonical proof. Empty count matrices preserve their rank, `which(..., arr.ind=TRUE)` is a genuine two-column empty queue, and guarded `match(TRUE, logical(0))` exits before any missing donor index. A one-row matrix selected by one character name intentionally drops to a length-one vector. |

This table covers the non-HLA consumers affected by occurrence multiplicity.
The lexical inventory still contains 340 indexing tokens, 108 `if` sites,
34 short-circuit sites, and one explicit internal `stop`. The unaffected
definition-before-use and scalar-result discharges in canonical proof
Sections 3, 4 and 7 require fixed index ranges, guarded activity, and physical
row counts; they do not require registry uniqueness. `inso_column` is assigned
exactly when `use_inso` becomes TRUE. No callback advances `meta_idx_outer`.
The matrix-to-vector transition occurs only after all pair helpers return.
The Boolean-unreachable range-update route introduces no new consumer.

## 5. Nonunit HLA remains indexed by physical columns

At each target's start, its donor vector consists of distinct remaining
nonunit indices other than itself. The target's initial local count equals
the corresponding physical row count in each donor matrix. Earlier HLA
targets changed different row coordinates; their possibly virtual FALSE
bits are not used to initialize this target's counts.

Count one therefore selects one scalar name at 690. A named virtual-clause
lookup takes the first copy; that operation is defined regardless of any
other copies. The complementary range construction is an ordinary finite
character operation. It need not replace all physical copies for indexing
safety; its semantic justification belongs to the reviewed Boolean argument.

During updates at 705–711, current registry indices can repeat but every
member is either the current target or one of the distinct donors. The
target's self row is FALSE, so it cannot reach the missing self-donor match.
Every other member has a unique nonmissing position in the donor vector.
The bit test ensures only the first duplicate visit can clear the first
named column and decrement its counter. Thus local count/bit equality holds
after repair. Orphans missing from the registry cause no invalid index or
spurious decrement. If the target is eliminated, its remaining registered
occurrences are removed and the loop exits. Only the current target can be
deleted in this phase.

No global raw-cache or global count/matrix equality is asserted after HLA
starts: the selected **current target's** local count and virtual comparison
row are the relevant objects. This preserves the earlier semantic review's
phase qualification.

## 6. Unit HLA: prove that the body cannot start

The canonical lazy-row count theorem is false here. It must be replaced
before line 753 is claimed to select one name.

First consider a unit propagation visit whose target is **currently**
registered for the unit symbol `s`. It has never lost an `s` occurrence
since registration. Its first cached `s` bit toward the registering unit
can be FALSE only from an explicit equal-singleton comparison: a source
removal would have cleared its registry membership, and nonempty range
updates are impossible. That equality also set the registering unit's
reverse first `s` bit FALSE. Before HLA, making this reverse bit TRUE would
require removal of an `s` copy from the candidate, which again clears its
membership. New allocation/uninitialized pairs cannot provide the asymmetric
TRUE/FALSE pair either. The diagonal candidate is absent from a unit's
registry snapshot because the newborn unit was removed before registration.

Therefore the optimization at 133 cannot skip a currently registered
candidate. An actual visit either deletes its clause (same singleton),
deletes an opposite copy and clears membership, or finds that it has become
inactive. A later duplicate of the same index in the old snapshot **can**
be skipped, leaving an orphan. That is allowed; it cannot restore membership.

No subsequent nonunit registration recreates membership for a current unit
symbol. After the matrix phase begins, no nonunit is ever registered again.
During earlier preprocessing, the captured intersection contains every
copy of each current unit symbol. A new unit can arise during processing
only by making the current clause itself a unit; that clause then skips
nonunit registration. Its synchronous nested propagation cleans older
registered clauses. This also handles units generated during preprocessing.

Consequently, whenever execution reaches unit HLA, every current unit symbol
has an empty current nonunit registry. Reaching that source point implies
the earlier synchronous propagation frames have returned, a fact about the
already executed prefix, not a premise about whole-function normal return.
Nonunit HLA deletes registrations and cannot add any. Every surviving donor
has physical width at least two, so line 738 initializes

```
not_subset_count = physical donor widths >= 2.
```

The selection mask `not_subset_count == 1L & !was_used` is therefore entirely
FALSE, including the empty-vector case. `match(TRUE, ...)` is scalar NA, and
746 breaks. The lazy rows at 750–753, their potentially empty exceptional-name
selection, the delayed inverse at 764, and the remaining unit-HLA body are
unreachable. No unit is eliminated here. This establishes their safety by
nonreachability rather than by assuming an invalid lazy-row equality.

### A local counterexample to the canonical lazy-row invariant

Use the four public signed words, where a positive integer means `Xi=1`:

```r
list(c(-1L, 2L), c(3L, -2L), c(1L, 1L, 1L), c(-3L, -1L))
```

Both inspected R versions reach unit HLA with unit `X1=0`, a live orphan
donor `(X1=1 OR X1=1)`, and no registry membership for X1. The two surviving
donors' counts are `(2,2)`. For the orphan, a hypothetical lazy row is

```r
c(X1 = FALSE, X1 = FALSE)
```

Its TRUE sum is zero, while its initial count is two. The actual source
exits without constructing that row. Forcing that donor at line 745 in a
private source copy makes line 754 consume `character(0)` as a list `[[`
index, producing `attempt to select less than one element in get1index` on
both versions. This is a corruption control, **not** a failure of the
unchanged source. `reproduce.R` constructs the accepted public inputs and
preserves every physical position. The original and returned formula are
both false on all eight assignments.

## 7. Finite execution without a successful-return premise

Every source `for` sequence is a captured finite vector. Registry snapshots
can be longer because of repeated occurrences, but their finite lengths
cannot grow during an existing `for`. Neither the two fixed orientations
in SSE2 nor the finite matrix queue creates an unbounded local loop.

Let `W` be the sum of the physical lengths of **all stored** clauses, including
eliminated entries. `W` starts at `W0` and never increases. A literal deletion
commits `W := W-1` at 244 before it can enter registration or another pair
callback. A contradiction before that write returns without a child callback.
Unit merges do not increase W; clause elimination changes only flags and
registries. No nonempty range update exists in this scope.

Cut the two classes of helper edges from `eliminate_symbol_from_clause` to
`register_unit` and `on_updated_subset_relations`. Every such edge follows
that strict deletion write. The remaining reachable helper graph is acyclic:
pair handlers may reach an SSE2 trial, a restriction, and finally a deletion,
but they cannot cycle back without crossing a cut edge. The deferred union
forced by `char_intersect` adds only the leaf `char_intersect -> char_union`
edge. It cannot invoke inference before the deletion write.

Thus an active helper path has at most `W0` progress-bearing edges. Each
intervening acyclic segment has at most the 13 named local helpers. One
deliberately loose prefix bound is

```
active local helper depth <= 13 * (W0 + 1).
```

This counts source helpers, not every R evaluator or primitive frame. The
observer also checks the stronger local fact that every active restriction
ancestor of a new restriction has seen a strict physical occurrence decrease
since its entry. The argument concerns each valid finite prefix; it does
not assume that a currently active parent eventually returns.

Every helper has finitely many direct children, because its captured loops
are finite. A finitely branching invocation tree with bounded height is
finite, by induction on height. The top-level loops produce finitely many
such roots. Nonunit HLA is the remaining `repeat`: every nonbreaking
iteration sets the selected previously unused donor's `was_used` bit, so
it can iterate at most its donor count plus the final failed selection.
Its update loop has no inference recursion. Unit HLA exits on its first
selection by Section 6. Primitive operations between these finite nodes
are total under the ordinary operation/resource premises. No infinite
source execution remains.

The prefix indexing discharges exclude a first ordinary argument/shape
failure: any preceding valid finite prefix establishes the relevant scalar,
rank, fixed coordinate, or guarded nonreachability for the next operation.
Finite execution and ordinary primitive totality then force normal return.
Only now is the reviewed normal-return semantic theorem applied.

## 8. The canonical clause-only cost bounds do not transfer

There is no need to inherit the canonical `B(m)`/`D(m)` or signature-class
bound to prove finite execution here. Their snapshot-length premise is
false when occurrence width is unbounded.

For a concrete family with only two clauses, take `X1=0` and the clause
with N copies of `X1=1` followed by `X2=1`. Preprocessing calls the restriction
helper once per opposite copy. The exact number of top-level helper roots
is `N+3`, unbounded at fixed `m=2`.

With three clauses `(X1=0 OR X3=0)`, `(X1=0 OR X3=1)`, and the same N-copy
target, a derived unit propagates before the target's matrix is allocated.
Its registry snapshot has N target copies plus the other original donor;
the registering frame has `N+1` direct restriction children. This exceeds
the canonical `B(3)=9` once N is large enough. Both families are checked
in `cost_controls.R`; the raw entries it uses are the same homogeneous
representation produced by the public selector construction in `reproduce.R`.

These are finite-cost counterexamples to transferring a clause-only bound,
not indexing errors or nontermination. The occurrence-dependent potential
above is sufficient for totality of every fixed finite request.
