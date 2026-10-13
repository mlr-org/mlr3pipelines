# Four clauses retain an SSE1 scheduling gap at every initial width at least three

Reviewed 2026-09-06 against unchanged `R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
This directory contains a source-level family proof, public reproductions,
and independent truth/rule checks. It makes no production change.

The completed [independent source review](../wide_four_review/REVIEW.md)
reconstructs the sole SSE2 queue entry and HLA steps, checks the initialized
versus default-TRUE padding columns, and supplies the first-divergence
argument for the retained-core invariant. Its separate small controls pass
on both R versions.

## 1. Exact family and conclusion

Use separate ASCII-named symbols `X`, `Y` with domains
`{0,1,4,5}`. In this exact input order and within-clause core-symbol order,
start with:

```
A = (Y in {0,5} OR X in {0,5})
B = (X=1 OR Y in {0,1,4})
C = (X=0 OR Y in {4,0,5})
D = (Y=0 OR X in {4,5}).
```

This is the input from
`../independent_solver/minimized_first_order_phase_sse1.json`, with `X0=X`
and `X1=Y`. Only that file's domains and input clauses are used by the new
checks. Its saved diagnostic `result` is not the current production result:
the actual production HLA phase deletes a third surviving clause.

For any positive integer `k`, add fresh distinct symbols `P1,...,Pk`, each
with domain `{off,on}`, and append the same ordered literal block

```
P1=on OR ... OR Pk=on
```

to **every** clause. Write its disjunction as `Q_k`. There is one unchanged
ordinary universe per formula. All values/names are ASCII; all domains are
nonempty and unique; all literals are nonempty proper ranges; every clause
has distinct names. No selector, external mutation or custom method is used.

**Theorem.** For every finite supported `k >= 1`, all four input clauses
have exactly the same support and initial width `k+2`. Production returns,
in the indicated order, exactly:

```
first:
  (Y=0 OR X=5 OR Q_k)
  AND (X=0 OR Y in {4,0} OR Q_k)

second:
  (Y=0 OR X=5 OR Q_k)
  AND (Y in {4,0} OR Q_k).
```

The second call is productive: it removes the entire `X=0` literal from
the second clause. All padding literals remain exactly unchanged, in their
original order, in both clauses and both returned payloads. These claims
include the displayed order `{4,0}`, not merely equality of range sets.

Thus for **every width `w >= 3`**, four common-support clauses of initial
width `w` can leave a productive first-order self-subsumption rule after
one call. Together with the separately reviewed three-clause width-at-least-
three theorem, this makes the clause-count boundary sharp for that class.
This is an existence result with a specific input order. It does not claim
failure for every ordering or every four-clause formula.

The source theorem assumes supported finite vector/index arithmetic and
sufficient resources in each compared execution. “Arbitrary width” is a
mathematical family statement, not a promise that a concrete machine can
allocate every member.

## 2. The leftover rule and exact truth

Let the two first-output clauses be `A'` and `C'`. At `Y`, donor `A'` has
`{0} subset {4,0}` in target `C'`; at every padding symbol the ranges are
identical. Their only exceptional donor symbol is `X`, where `{5}` is not
contained in `{0}`. SSE1 therefore restricts the target to

```
{0} intersect {5} = empty,
```

which deletes its `X` literal. This is a direct literal-set rule, independent
of any cached subset bit or source instrumentation. `padding_sse1()` checks
every ordered donor/target pair and finds exactly that opportunity. It finds
no productive SSE1 restriction in the second output.

For the unpadded input `F`, the exact core truth table has five models out
of sixteen: every `X` when `Y=0`, plus `(X=5,Y=4)`. Both displayed stripped
outputs have exactly those five models.

Distributivity gives, for every padding assignment,

```
AND_i (C_i OR Q_k) = (AND_i C_i) OR Q_k = F OR Q_k.
```

Both output clauses contain the same complete padding block, so the same
factorization applies to their conjunction. Exact equality on the sixteen
core assignments consequently proves truth preservation on **all**
`16 * 2^k` assignments. The exact model count is `16 * 2^k - 11`: only the
all-`off` padding assignment permits any of the eleven false core rows.

The large-width tests do not pretend to enumerate `2^256` padding patterns.
They verify the complete common block in every output clause, evaluate the
entire core truth table, and separately test every individual padding literal
as the sole true one, as well as all-false and all-true padding patterns.
The displayed algebra discharges the remaining assignments.

## 3. Why compare one padding literal with many, not zero with many

The unpadded production example has the same first and second core outputs,
but does not have the same source schedule. In its first pass, deleting `X`
from `B` creates a `Y` unit, invoking `register_unit` and unit propagation.
In its second pass, deleting `X` from `C'` similarly creates a unit.

With positive padding those clauses still contain `Y` and all pads. Their
width is at least two, so they remain nonunits. The first-pass restrictions
formerly achieved by unit propagation arise through the ordinary nonunit
comparison updates instead. Unit HLA is also replaced by ordinary nonunit
HLA. The observed core transition counts are therefore different:

| Padding | First-pass core/HLA records | Second-pass records | Unit registrations across both passes |
| --- | --- | --- | --- |
| zero | 42 | 9 | 2 |
| any tested positive count | 58 | 16 | 0 |

A blanket assertion that padding preserves the unpadded callback schedule
would be false. The proof instead establishes the one-padding execution
concretely, then simulates it with every positive number of pads.

## 4. Complete relevant one-padding trajectory

In this section each clause implicitly contains the one unchanged literal
`P1=on`. Initial equal widths preserve input order `A,B,C,D` at source line
48. The following table records every change to an actual core range or an
elimination flag in the first pass. Unlisted callbacks perform no actual
range or clause change.

| Step | Productive change | Reason in the source |
| --- | --- | --- |
| 1 | `A_X: {0,5} -> {5}` | `D` is contained in `A` except at `X`; first-order restriction by `{4,5}`. |
| 2 | Remove `B_X={1}` | `D` is contained in `B` except at `X`; intersection with `{4,5}` is empty. `B` remains the nonunit `(Y in {0,1,4} OR P1=on)`. |
| 3 | `A_Y: {0,5} -> {0}` | Removing `B_X` reduces `B`'s count against `A` to one; its callback restricts `A_Y` by `{0,1,4}`. |
| 4 | Eliminate `D` | The updated `A` now subsumes `D`. |
| 5 | `C_Y: {4,0,5} -> {4,0}` | The removal of `B_X` also exposes the ordinary callback restricting `C_Y` by `{0,1,4}`. |
| 6 | Eliminate `B` | Nonunit HLA, detailed below. |

After step 5 the surviving actual cores are

```
A' = (Y=0 OR X=5)
B' = (Y in {0,1,4})
C' = (X=0 OR Y in {4,0}).
```

No actual core is empty, and no clause has become a unit. The second-order
phase makes no further actual restriction. Its candidate symbols, tests,
and callbacks are retained in the saved trace; the statement is about the
actual source result, not about arbitrary stronger mathematical resolution.

The HLA target order at line 657 is `A', C', B'`: widths are `3,3,2`, with
the original order breaking the equal-width tie. The virtual steps are:

| Target | Donor | Exceptional symbol | New virtual range | Outcome |
| --- | --- | --- | --- | --- |
| `A'` | `B'` | `Y` | `{0,5}` | No deletion; `B'` still has missing `1,4`, and `C'` still has missing `4` at `Y` and `0` at `X`. |
| `C'` | `A'` | `X` | `{0,1,4}` | `5` remains absent, so `A'` cannot subsume this virtual target. |
| same virtual `C'` | `B'` | `Y` | `{4,0,5}` | `1` remains absent, so `B'` still cannot subsume it; no deletion. |
| `B'` | `A'` | `X` | `{0,1,4}` | `C'` is now contained at every symbol, so `B'` is deleted. |

Every HLA range is proper in its original domain; no hidden-tautology branch
is taken. Padding is never the exceptional symbol. Source HLA writes only
these local virtual ranges, so the returned actual ranges retain their
pre-HLA order and contents. `entries[!eliminated]` returns `A',C'`.

The second call starts with two equal-width nonunits, `A',C'`. Its first
productive SSE1 restriction deletes `C'_X={0}` using `A'_X={5}`. The target
retains `Y in {4,0}` and the pad, hence remains nonunit. No other actual
change occurs. HLA extends `A'_Y` virtually to `{0,1,5}` using the shortened
target, then extends the shortened target's absent `X` virtually to
`{0,1,4}` using `A'`; neither donor becomes contained, and neither target
is deleted. This proves the two displayed one-padding outputs.

`one_padding_state_changes.json` gives all actual-state changes and virtual
steps from the complete saved run. The source's actual range writes occur
inside the observed restriction/unit handlers; nonunit HLA deletion uses
the observed deletion helper. Initial sorting is fixed as above and unit
HLA is absent. Entry/exit checkpoints therefore capture every actual-state
change for this finite execution, including nested callbacks.

## 5. Positive-padding simulation lemma

Replace the single common pad `P1=on` in that execution by any positive
number `k` of fresh common pads, appended after the same core symbol
sequence. Relate states at the boundaries of common-padding-only work:

1. Actual clauses and local core ranges, in order, are equal after removing
   the common padding block. Eliminated flags, clause indices, available
   maps, row counters, work queues and pending core callbacks are equal.
2. Every actual and virtual padding range is its original `{on}`. All pad
   registry entries hold the same clause-index vector as the single-pad
   entry. Core registry entries are unchanged.
3. Core comparison columns are identical. Every completed pair has FALSE
   in all padding columns, since both clauses have the same nonempty pad
   range. Uninitialized padding blocks correspond as repeated default TRUE
   columns; they are not used as completed-pair row counts.
4. Every live actual clause retains at least one core symbol. It has width
   `core_width+k`, so there are no units in either compared execution.

The one-padding trajectory in Section 4 establishes these properties at
entry and shows the required nonempty-core property through both calls.
The following source audit closes the induction for arbitrary `k`.

### Width sorting and structural branches

Both sorts use only physical clause widths (lines 48 and 657). Replacing
one pad by `k` pads adds the same `k-1` to every relevant width, preserving
all strict comparisons and ties. Initial order and HLA target order are
therefore exactly the same. In this family the first and second calls both
start with equal-width clauses, and the later short clause is always last
in the descending HLA ordering.

The source's emptiness and `length(clause)==1` branches (lines 239/245) are
FALSE whenever evaluated: at least one core symbol and at least one pad
remain. Initial `is_unit` is entirely FALSE; `unit_queue` is empty;
`unit_domains` remains empty; registration and both unit-only paths are
absent. Every clause/index-vector/matrix-row count is identical. Raw matrix
column counts grow, which only allocates the additional common columns.
The source's exceptional-symbol masks exclude padding and keep their
original lengths. Range-cardinality predicates on core symbols are unchanged.

### Actual comparisons and recursive callbacks

For two clauses `E,T`, their padding ranges are equal at every fresh symbol.
Thus the set of exceptional donor symbols is exactly the core set:

```
{s : E_s is not a subset of T_s}
```

contains no padding symbol. Completed comparison rows consequently gain
only FALSE bits, so their sums and the source's count-zero, count-one and
count-two choices are identical. Padding may add iterations to initial
symbol matching and column comparison; each extra iteration makes the
same TRUE subset checks and writes the corresponding FALSE bits. Neither
comparison-direction handler is called until the full pair comparison has
completed, so partially initialized padding blocks cannot alter a callback.

Elsewhere, source reads of whole-row sums are either those just-completed
pair sums or restrictions to core exceptional columns. The explicit
`not_subset_count` NA/availability guards exclude uninitialized pair rows.
The extra default TRUE columns therefore never become an extra count-one
or count-two premise.

First-order restriction selects the same core symbol and intersects the
same concrete ranges. Empty intersections delete the same core symbol;
nonempty intersections write the same core range. The one-padding run's
retained-core property then proves the same nonunit outcome, so the next
callback is the same nonunit handler. Every core matrix flip, guarded
counter update, eliminated flag, and recursive return consequently agrees.
In particular this coupling includes stale snapshots and unchanged bits;
it does not assume a complete scheduler.

Second-order handlers select pivots from TRUE comparison bits, so no pad
can become a pivot. Their relevant donor/target core predicates, overlap
outside the target, donor union used for restriction, and structural
candidate order are identical. The pads add no eligible source pivot or
new source candidate count. This is a statement about the actual kernel's
tests; it does not claim that every syntactically possible stronger
resolution presentation is literally the same expression after padding.

Deleting a clause removes its index from every common pad's registry.
Those extra registry writes are independent, introduce no callback, and
leave the core entries and the common pad index vector in correspondence.

### HLA

Every target starts with all original pad ranges. A donor is always
contained at every pad, hence its unique exceptional symbol is in the core.
The selected core range expansion uses exactly the same original core
domain and donor range. Padding never expands or vanishes. All donor-count
updates, used-donor flags, full-domain comparisons, and hidden-subsumption
decisions therefore agree with the one-padding run. The additional original
padding domains are never read as HLA pivot domains.

Source HLA deletes the same clauses and never writes its virtual ranges
back into actual entries. There are no unit HLA targets. Thus Section 4's
finite virtual trajectory repeats for every positive `k` and returns the
same ordered actual cores with the complete common pad block.

This proves the simulation by induction over core operations, with finite
extra blocks of padding-only comparison/registry work between them. It is
a **stuttering simulation**, not equal raw instruction counts or equal
full character-loop traces. Applying it to both one-padding calls proves
the family theorem.

## 6. Why the scheduling gap remains

The initial `A`-to-`C` comparison is visited while `A_X={0,5}`. Its only
exceptional symbol is `X`; restricting `C_X={0}` at that time does nothing.
Later `D` shrinks `A_X` to `{5}`. That changes the productive restriction
without changing the exceptional `X` bit: it was TRUE and remains TRUE.
The source callback machinery does not revisit every already-count-one
pair when its exceptional donor range shrinks during the first-order phase.

The later changes at `Y` also do not create a new FALSE transition for the
already-contained `A_Y`-to-`C_Y` relation. HLA uses virtual additions and
does not perform the missed actual `X` restriction. It deletes `B` while
leaving `A',C'`. Adding common FALSE comparison columns cannot create the
missing notification, which is precisely what the source simulation proves.

This finding concerns saturation and a productive second pass. Every tested
and proved family member preserves truth. It does not assert a semantic
kernel bug, incomplete termination, or a new repair to the scheduler.
