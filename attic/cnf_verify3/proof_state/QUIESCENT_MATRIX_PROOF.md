# Why live comparison matrices are raw-exact before HLA

This extends `LIFECYCLE_PROOF.md` and is a source-level proof argument, subject
to its explicit well-formed finite-set assumptions. Public objects carrying a
`Cnf*` class do not automatically meet those assumptions: the representation
agent independently found that logical-`NA` clause subsetting can construct a
clause with an `NA` symbol name and a `NULL` range. Such accepted malformed
objects lie outside this proof.

The first proof established that every consumed FALSE bit is sound under
current units, and that all live non-unit ranges are physically inside the
units before HLA. That already establishes one direction of raw correctness
at that boundary:

```
M[A,B,s] = FALSE  =>  A_s subset B_s.
```

The missing direction concerns conservative TRUE bits that temporarily remain
TRUE after their source shrinks. The following pending-update argument closes
that direction for live, initialized non-unit pairs.

## 1. Only a source shrink can make a correct TRUE bit become wrong

Suppose `M[A,B,s]` is TRUE because `A_s` is not a subset of `B_s`.

- Shrinking B, or deleting s from B, cannot turn that non-inclusion into an
  inclusion: the target only gets smaller.
- Removing either clause or turning it into a unit removes the pair from the
  live non-unit obligations; no clause ever becomes active again.
- Changing a different symbol cannot affect this comparison.
- Changing the global unit domain does not itself change either *raw* set.

Thus a TRUE bit can become raw-inaccurate only when A's range at s shrinks.
Every such actual stored change occurs either in `apply_domain_restriction()`
or in `eliminate_symbol_from_clause()`. Updating an older unit on a merge is
irrelevant because that source is already inactive as a non-unit.

## 2. Every such source change creates a bounded outstanding update

For nonempty strict range restriction, `apply_domain_restriction()` writes the
new source range at line 167. Before it invokes any callback, it computes
`rows_to_check` (line 205) from the initialized TRUE comparisons whose target
is registered for the symbol. This list contains every potentially wrong TRUE
bit immediately created by the source change:

- A target not containing s has empty range there, so the source's still
  nonempty range cannot be a subset of it.
- A pair whose count is `NA` is not yet initialized. Its eventual construction
  compares the current raw sets directly.
- An inactive target has no live obligation.

For every listed target that remains active, the loop tests the current raw
sets and flips TRUE to FALSE when the inclusion now holds. It uses the current
`entries` values, not the original local clause snapshot (lines 215–216).
The second check at line 220 avoids decrementing a bit that a recursive child
has already cleared. An early non-contradictory return from this loop occurs
only if the source becomes inactive (line 226), which discharges all of its
live-pair obligations.

For deletion of s from a source that remains a non-unit, the entire source
column is cleared before callbacks (line 265). There is no outstanding TRUE
debt for that symbol afterward. Conversion into a unit removes the source
from the live obligations before pairwise callbacks start.

## 3. Recursive changes cannot create an unowned TRUE debt

Consider a TRUE bit introduced after an older source-update loop took its
snapshot. Such a bit is introduced only by a reverse update following target
restriction or deletion. Those reverse updates test the current raw source
range, so the bit is correct at introduction.

If it later becomes incorrect, Section 1 says that the source must have shrunk
again. That subsequent source change invokes its own nested update operation,
which owns the new obligation under Section 2. The old snapshot need not
anticipate it. Nested operations either discharge their obligations or make
the source inactive. On unwinding, older operations resume their finite row
lists and discharge whatever obligations remain.

This can be stated as an inductive execution invariant:

> Every raw-inaccurate TRUE bit for a live initialized pair is covered by an
> active range-update frame for that source and symbol, or by the immediate
> symbol-deletion transition before its column has been cleared.

At actual recursive callback boundaries the immediate deletion transition has
already repaired its column. Only active range-update frames remain as debt
owners. No such frame exists after the pairwise and second-order loops have
fully returned. Therefore no raw-inaccurate TRUE bit remains before HLA.

This reasoning is independent of whether the scheduler revisits every
newly useful SSE rule. A cache may be exact while a rule corresponding to its
state is never scheduled; that is how the confirmed completeness gaps coexist
with correct final comparison matrices.

## 4. Count consistency is maintained independently

For every initialized pair, `not_subset_count[A,B]` remains the sum of the row
of `is_not_subset_of[[A]]` describing B.

- Initial construction computes both sums explicitly (lines 608–610).
- Reverse FALSE-to-TRUE changes increment once, guarded by the previous FALSE
  bit (lines 185–188 and 270–272).
- Forward TRUE-to-FALSE changes decrement once, with the reentrancy recheck
  immediately before mutation (lines 220–222).
- Deleting a source symbol snapshots exactly the currently TRUE initialized
  rows, clears the column, and subtracts one from exactly those counts before
  invoking callbacks (lines 264–266).

No other recursive operation writes the ordinary count matrix. The later
unit-HLA phase deliberately reuses the local variable name for a different
vector, after the ordinary matrix is no longer consumed.

The diagonal count stays `NA` and is excluded from inference scheduling; its
FALSE matrix row is not interpreted as a deletion witness. Future pairs stay
`NA` until direct raw comparison constructs them. Removed source-symbol columns
stay FALSE even for future rows, which correctly represents their empty range.

## 5. Consequence for HLA initialization and preservation

Combining physical unit containment, contextual FALSE soundness, the TRUE-debt
argument, and count consistency gives exact raw comparisons and exact counts
for every live non-unit pair when HLA starts.

For one non-unit HLA target, the initial local donor counts are copied from
that exact global matrix. Extending one symbol changes no other comparison.
The symbol registry enumerates every live donor containing the changed symbol;
each newly contained donor range flips one TRUE bit and decrements its local
count. No FALSE can become TRUE because the virtual target only expands.
The current target's self-comparison stays FALSE and never tries to decrement
a nonexistent self-donor counter. The local count and virtual comparison row
therefore remain exact by induction through every donor selection.

The matrix writes are confined to the current target's row inside each donor
matrix. A later target uses a different row and the unchanged global count
matrix, so its initialization is still raw-exact. Eliminated earlier targets
are removed from the remaining donor list and the symbol registry.

For unit HLA, physical containment independently justifies the initial count
`clause length - contains_unit_symbol`. This is a raw comparison to the unit
being tested, not a circular assumption of that unit. Fresh local comparison
lists then undergo the same one-symbol monotone update argument.

Consequently the donor-selection consistency premise of HTE unreachability
is supported by this lifecycle argument, rather than left solely as an
empirical assumption. A selected donor has a value at its exceptional symbol
that the virtual target lacks; adding the donor's complement cannot add that
value, so the immediate full-domain branch cannot fire on well-formed input.

## Remaining limits

This is not a verified operational semantics for R. It relies on ordinary
R value/copy behaviour for lists and character vectors, the inspected source
call graph, the explicit input representation contract, and normal finite
index arithmetic. Resource failures such as the reproduced recursive stack
overflow are not excluded. Completeness and fixed-point claims remain false.

The pending-update invariant is intentionally formulated for *live initialized
non-unit pairs*. Applying it to old inactive unit matrices, uninitialized
future rows, or post-HLA virtual target rows would be an incorrect extension.
