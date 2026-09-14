# Boolean clauses with repeated occurrences: source-level argument

This argument concerns the six unchanged CNF files recorded in
`SOURCE_HASHES.sha256`. It is a source-level proof attempt for review, not a
machine-checked proof of R. The accompanying finite checks are evidence for
its specific invariants, not an exhaustive substitute for the argument.

## Scope and proposed conclusion

Take any finite number of symbols with fixed ordinary two-element domains
of distinct nonmissing character values. Take any finite list of proper
`CnfClause`s made by the public constructors followed by accepted repeated
numeric/character selector indices. Every physical occurrence of a given
symbol in one initial clause consequently has the same singleton range.
Keep all occurrences positional when interpreting its disjunction.

The proposed conclusion is that `CnfFormula` preserves this conjunction's
Boolean truth table at arbitrary clause count, symbol count and occurrence
width. The result may retain duplicated symbol names. The conclusion is
about this direct list-of-proper-clauses path, not arbitrary nested formula
constructor combinations, universe mutation, logical-NA selectors, byte
encoding mixtures, custom S3 behavior, or non-Boolean ranges. The previously
documented multivalued selector counterexamples are outside this scope.

The argument below also excludes the duplicate-driven unit-HLA symbol-count
runtime error in this Boolean scope. It does not claim that every R-level
constructor/constant/operator combination is free of existing API errors.

## 1. Stored occurrences never change their Boolean value

Initially every proper range is a singleton. For such a range, intersection
with any restricting set is either the original singleton or empty.
`apply_domain_restriction()` therefore does one of only four things:

1. returns because the symbol is absent;
2. eliminates the whole target through its coverage branch;
3. returns unchanged;
4. removes the first matching physical occurrence.

The nonempty range-update branch at lines 166 onward is unreachable. In
particular `on_update_range()` cannot be reached through that branch.
`register_unit()` merges singleton ranges to either the identical singleton
or a contradiction. The final HLA loops change a local virtual clause only;
they do not write expanded ranges into `entries`.

Thus surviving physical occurrences keep their original singleton value,
and all surviving copies of one name in one stored clause still agree.
Deleting a copy while another copy remains is semantically a no-op. There
are no stale unequal ranges analogous to the ternary selector example.

This fact alone is insufficient: removal clears the whole registry entry
for that clause and only its first matching cached column. Sections 2–7
address precisely those effects.

## 2. Cache birth and orphaned occurrences

Call a live non-unit occurrence of symbol `s` an *orphan* if its clause index
is absent from the current `symbol_registry[[s]]`. Distinguish the current
registry from a previously captured loop snapshot; those are not the same.
Call the creation of a clause's `is_not_subset_of` matrix its *cache birth*.

The following property is central:

> If a live non-unit clause has an orphaned `s` occurrence after cache birth,
> its cache has at least two columns named `s`.

All non-unit clauses are initially registered once per surviving occurrence.
After registration there is no operation that adds a physical occurrence.
Removing any `s` occurrence clears all registrations of that clause for
`s`. Consequently an orphan created after cache birth must have had at
least two copies at that birth.

It remains to exclude an orphan already present at cache birth, because a
clause that lost one of two copies earlier could otherwise be born with a
single cached column and no registry membership. Before a clause's cache
is created, pairwise/SSE handlers cannot target it: its pair counts are
`NA`, and the relevant callback loops exclude those counts. Only unit
propagation can remove occurrences from such an unprocessed clause.

For that unit propagation the matrix skip is disabled for the unprocessed
clause by `s_clause_idx_meta <= meta_idx_outer`. A registry snapshot contains
one index per copy. Every opposite copy is therefore removed in turn until
none remains, unless the clause is eliminated, becomes a unit, or a
contradiction ends the simplification. Same-valued copies cause immediate
whole-clause elimination. Recursive unit registration does not invalidate
this argument: the outer snapshot remains, no call adds a copy, and the
unprocessed-clause skip remains disabled. The outer loop cannot advance to
the clause's cache birth until this synchronous propagation returns.

In the earlier preprocessing loop, before the matrices exist, the explicit
`clause_symbol_isct` list also contains every duplicate occurrence; any
remaining non-unit is registered afresh after that loop. Thus it cannot
carry an unregistered occurrence into the matrix phase either.

## 3. The useful cache invariant groups columns by name

For distinct live non-unit clauses `A` and `B` with an initialized pair
count, let `M[A,B]` be the corresponding row of A's matrix. Its columns are
the occurrences at A's cache birth, even after A later becomes shorter.

The correct invariant is:

> If every column named `s` in `M[A,B]` is FALSE, A's current `s` literal is
> contained in B's current `s` literal. An absent range means the empty set.

For a name absent from A's cache, A cannot acquire that name later and the
same inclusion is vacuous. The invariant is a **raw** inclusion here; a
projection through live units is unnecessary for this Boolean specialization.

Every column after the first column of a repeated name remains TRUE for
distinct clauses. Matrix creation initializes it TRUE; all subsequent
updates use a single name or `match(name, colnames(...))`, which select the
first column. The all-FALSE premise can therefore hold only for a name
with a single cache-birth column.

For such a single-column name:

- Initial FALSE assignments are explicit equal-singleton comparisons.
- Removing that name from A makes A's range empty, justifying clearing the
  column. No nonempty range update exists in the Boolean specialization.
- If B loses its last `s` occurrence while staying a non-unit, every live A
  that still contains `s` is registered for it. Otherwise Section 2 would
  require A to have duplicate birth columns, contrary to this case.
  The reverse-update loop therefore sets A's initialized comparison to
  TRUE before invoking callbacks. Duplicate registry indices cannot cause
  an extra count increment because the code checks the bit first.
- If B loses only one of several copies, its literal is unchanged. A
  reverse TRUE update is conservative and does not invalidate a FALSE
  implication elsewhere.
- Clauses that become units or are eliminated leave these active pair
  obligations before recursive pairwise work uses them.

The maintained `not_subset_count` is the sum of physical cached bits, not
the count of distinct exceptional names. Clearing an already FALSE first
column does not decrement it again. The permanent trailing columns can
overcount genuine exceptions, but cannot erase an exceptional symbol from
the grouped all-FALSE interpretation.

A useful subsidiary fact is that if a live initialized clause no longer
contains `s`, its **first** cached `s` column is FALSE. The last-occurrence
removal clears that column; if no such birth column existed, name matching
uses zero and contributes no bit. This fact is used in Section 5.

## 4. First-order rewrites remain sound

A row sum of zero implies full literal containment by Section 3, so
subsumption deletion is sound.

A row sum of one has exactly one exceptional physical column, hence one
exceptional symbol `s`. Every other symbol of the donor is contained in
the target. The usual self-subsumption argument therefore applies to that
single symbol. A duplicate target occurrence removal is additionally a
no-op while another equal copy remains.

The implementation's early coverage branch also behaves correctly when
the exceptional cached symbol is now absent from the donor: restriction
by the empty set causes whole-target elimination, but the donor then has
no exceptional literal at all and subsumes the target. With a singleton
restriction equal to the target literal the donor likewise subsumes the
target. Opposite singleton restriction removes one target occurrence and
is the ordinary Boolean self-subsumption rewrite when it is the last copy.

Unit propagation is sound independently of cache precision. Same-valued
target literals justify whole-clause elimination. Opposite literal copies
are false under the unit and can be removed. Skipping any such removal can
leave a false disjunct in place, but cannot change the conjunction's meaning
while the unit remains. Section 7 separately establishes why unit deletion
does not later invalidate this reasoning.

## 5. Second-order rewrites, including equal exception names

For two **distinct** exceptional names `x,y`, the twoend donor is contained
in the target outside those names by Section 3. In the other donor, the
test comparing the entire row count with the sum over the two distinct
matched primary columns implies that no other TRUE column exists. A
trailing duplicate column would add a positive count outside those two
positions and fail the equality. Thus that donor also has no exception
outside `x,y`. The ordinary two-donor resolution argument, using the actual
disjointness test outside the target's `x` range, justifies restricting
target `y` to the union of the donor `y` ranges. Removing only one duplicate
copy remains a no-op; the coverage/empty-union cases are subsumption cases
of the same rule.

The same-name case requires a different argument. It is really reachable.
For example, the public clauses represented by the signed words

```
(1, 2), (1, 1, 3), (-1, -1, 3)
```

make a twoend row whose exceptional column names are `(S1,S1)`. The first
donor's exceptional names can be `(S1,S2)`. Repeating `match(S1, ...)`
counts its first bit twice and admits that unrelated S2 exception.
Consequently a proof that silently deduplicates the two exceptional names
would be wrong.

Let `x` be their common name, D the twoend donor, E the other donor, and let
the target's actual `x` literal be the singleton `{a}`; `x` is present in
the target by the handler guard. D is contained in the target outside x.
The actual `try_sse_2nd_order` restriction uses `D_x union E_x`, after
checking that D and E share no x value outside `{a}`.

Enumerate the possible Boolean union:

- If it is the full domain, the target range is unchanged.
- If it is `{a}`, both donors' x ranges are either empty or `{a}`. D
  subsumes the target, so the coverage branch's target deletion is sound.
- If it is empty, D has no x literal and subsumes the target remainder;
  whole-target deletion is sound.
- If it is the opposite singleton `{b}`, disjointness forces at least one
  donor's x range to be empty. If D's is empty, D is contained in the target
  remainder. If only E's is empty, E's first cached x bit is FALSE by the
  subsidiary fact in Section 3. Both repeated matched positions therefore
  contribute zero. The handler's row-count equality forces E's entire row
  count to be zero, so E also subsumes the target. Since E lacks x, it
  subsumes the target remainder. Removing the target x occurrence is sound.

Those cases cover every actual restriction, including donor coincidence
and a donor taken from an old registry snapshot that has since lost x.
The oneend handler does not itself send equal exception names through its
distinct-target-name filter; the analysis above concerns the twoend path.

## 6. Non-unit HLA and eventual donor/target deletion

At the start of a target's non-unit HLA pass, Section 3 applies to its
physical cached rows. Interpret each row count by its TRUE columns, with
the permanent duplicate columns retained. A row count of one identifies
at most one exceptional **symbol**, and every other donor literal is
contained in the target.

The virtual target grows its first named range. Its other copies retain
their original singleton, which is contained in the growing first range.
The virtual first named range therefore represents the full positional
disjunction for that symbol; no unequal stale stored-range issue arises.
Adding the complement of the donor's exceptional range is the standard
hidden-literal step and preserves the target's meaning in the context of
the other live donor clauses.

Every later FALSE update is a direct containment test against that virtual
range. Missing registry updates for orphan donors only leave extra TRUE
bits; they do not introduce false grouped containments. Duplicate registry
indices cannot double-decrement a counter because the primary bit is set
FALSE on the first iteration and checked on every iteration. A row count
of zero therefore certifies a live donor contained in the virtual target.
The whole-domain test likewise certifies a virtual tautology using the
ordinary distinct-value Boolean domain.

Either condition makes the target redundant in the context of the other
current clauses. Its deletion is consequently sound, including when the
target itself bears orphan occurrences. Later targets use the updated
remaining-clause list. Clause deletion removes every surviving registered
name; an already orphaned name has no registry entry to clean. No frozen
proof may continue to use an eliminated donor, and the implementation's
remaining-entry list and registry updates satisfy that requirement.

## 7. Unit-cache skips are possible, but unit HLA has no starting donor

An initially tempting stronger claim was that Boolean cache skips are
impossible. Directed tests refute it. A unit's registry loop captures one
index per occurrence. Removing the first opposite copy clears the source's
first column and all current registration for that symbol. A later entry
of the old snapshot can then pass the skip test and leave another opposite
copy in the live clause. `directed.rds` preserves the exact public input.

The necessary narrower claim is:

> A Boolean unit-cache skip cannot occur while the candidate is still
> currently registered for that symbol.

Such a candidate has never lost any occurrence of the symbol since
registration. Its primary FALSE bit toward the registering unit can only
come from an explicit equal-singleton comparison; it cannot come from
clearing that source symbol. The unit's singleton is unchanged throughout
its life. The reverse unit-to-candidate bit was also set FALSE by that
comparison and can only have been made TRUE by removal of the candidate's
symbol, which would have removed its current registry membership. Hence
the two Boolean conditions required for a skip cannot both hold while the
candidate remains registered. A not-yet-initialized pair cannot furnish
the source FALSE bit without the same removal event.

On an actual visit to a still-registered candidate, the unit either
eliminates the clause (equal singleton), removes an opposite copy and
clears registration, or the candidate has already become inactive.
No later operation adds non-unit registry membership. Nested registrations
may work from old snapshots, but cannot create new membership either.
After all synchronous propagation returns, no live non-unit is registered
for any current unit symbol. Orphan occurrences of those symbols may and
do remain.

In unit HLA, every remaining non-unit has physical length at least two.
The initial count is that physical length minus the Boolean fact of
current registry membership for the unit symbol. That membership is false
for every remaining non-unit, so all starting counts are at least two.
No donor has count one, the HLA repeat loop cannot begin, and the unit is
never eliminated through this phase. There is consequently no circular
reliance on a unit that is subsequently deleted and no route to the
multivalued duplicate-name unit-HLA symbol-count error.

## 8. Termination, lifecycle and status of the argument

No helper adds a stored occurrence or clause. Every recursively mutating
step removes an occurrence, eliminates a clause, or signals contradiction.
No-op restrictions do not invoke the otherwise relevant range-update
recursion. Registry snapshots, pair loops and HLA donor-use loops are all
finite. Existing active-clause guards and the uninitialized diagonal count
prevent using a clause to subsume itself; duplicate registry indices add
repeated iterations but do not bypass those guards.

The delicate claims have been separated rather than inherited from the
canonical proof: cache birth, permanent trailing columns, grouped FALSE
containment, equal-name second-order exceptions, old registry snapshots,
and unit-HLA initialization. A remaining review objection should identify
which of those source claims fails and a reachable state that contradicts
it. The finite instrumentation checks them at recursive decision boundaries
and compares every completed instrumented result to an independent run of
the unchanged simplifier. That supports this argument without making a
finite all-size exclusion claim.
