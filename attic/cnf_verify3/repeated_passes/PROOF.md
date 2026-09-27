# Repeated simplification: finite convergence, with no constant pass bound

Written 2026-09-06 for `R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
The representation assumptions are the canonical finite-set assumptions of
the semantic-preservation review. Each invocation considered below returns
normally; resource failures are outside these statements.

**Results.** There is no input-independent constant number of passes that
suffices. For every positive integer `n`, the explicit family below makes
exactly one value deletion in each of its first `n` passes and is then fixed.
Every clause remains present. Merely reversing the order of its target clauses
makes the same reductions finish in one pass. For arbitrary valid input, the
initial total number of stored value occurrences bounds the number of
productive passes.

## 1. Meaning of a pass and of stability

Fix the universe. One pass means one invocation of `simplify_cnf()` on the
previous invocation's stored formula. Passing its returned `CnfFormula` object
directly is possible: there is no `[.CnfFormula` method that would insert an
additional simplification. The Python bridge serializes the same stored
clauses between invocations, without reconstructing or simplifying them first.

Two useful notions of stability are different:

* **Clause/value stability:** the multiset of clauses is unchanged when symbol
  and value order within a clause, and order among clauses, are ignored.
  Multiplicity is retained so that deleting a duplicate is counted as actual
  work. All constructed family clauses are distinct, so set and multiset
  stability coincide for those examples.
* **Storage stability:** clause, symbol, and value order are unchanged as well.
  Universe metadata is fixed throughout.

Call a pass **productive** if it changes the clause/value multiset. This is
syntactic progress; every pass still preserves the same logical model set.
The minimization experiment below has three productive passes followed by a
fourth pass that only sorts clauses, so reporting only raw object inequality
would overstate the number of useful passes.

## 2. A general finite bound

For a nonconstant formula define

```
V(F) = sum over current clauses C and their symbols s of |C_s|.
```

Set `V(TRUE) = V(FALSE) = 0`. Label each input clause with a ghost identity to
follow it through sorting and elimination. The implementation never creates
an actual clause, adds an actual symbol, or adds an actual value:

* Range restriction and symbol deletion only remove existing values.
* A unit merge intersects the retained old unit and deletes the new candidate.
  It does not replace the old unit by a larger range.
* HLA additions affect a local virtual clause only. A successful HLA operation
  deletes its target; it never writes an enlarged actual clause back.
* A logical return is terminal and subsequent invocations preserve it.

Every surviving clause has a nonempty range, so deletion of a whole clause
also removes at least one value occurrence. Consequently every productive
pass strictly decreases the nonnegative integer `V`, and there can be at most
`V(F_initial)` productive passes. This bound is deliberately simple, and is
not claimed to be tight.

A nonproductive pass cannot hide new useful work for a later pass. If `V`
does not decrease, no actual clause/range mutation took place. The only
possible storage change is the initial stable sort by clause length. All
local caches and virtual HLA clauses are fresh on the next invocation. The
now-sorted input therefore executes the same remaining operations and is an
exact storage fixed point. In particular, productive passes form an initial
consecutive segment; there cannot be a nonproductive pause followed by a later
productive pass.

Thus at most `V(F_initial) + 1` applications produce an exact fixed-point
representation, including any final sort-only application. A loop that waits
to *observe* equal consecutive stored outputs may use one further verification
call. Comparing normalized clause/value multisets can stop at the first
nonproductive call; that call's returned representation is already sorted.

This argument uses monotonicity of actual storage, not monotonicity of the
logical formula under implication: all intermediate formulas are equivalent.
It also does not assert that the fixed point is a complete or canonical
logical normal form.

## 3. The family

Fix `n >= 1`. Let `T` and `S0,...,S(n+1)` be binary symbols with values `0,1`.
Let `G` have values `g0,...,gn`, and write

```
P_i = {g0,...,g(i-1)},      1 <= i <= n.
```

Define the following `2n + 1` clauses:

```
A0 = (S0=0 | S1=0)

Bi = (Si=1 | T=1 | G=gi),                   0 <= i < n

Ai = (Si=0 | S(i+1)=0 | T=0 | G in P_i),    1 <= i <= n.
```

Supply them in this precise order:

```
A0, B0, B1, ..., B(n-1), An, A(n-1), ..., A1.
```

All ranges are nonempty proper subsets of their domains, so public clause
construction does not convert any clause into a constant. The varying size
of `P_i` changes no clause's number of symbols: every initial `Ai`, `i >= 1`,
has length four. Hence the initial length sort preserves their decreasing
index order. Each `Bi` has length three, and `A0` has length two.

Let `F_k`, for `0 <= k <= n`, denote the same clause/value set except that
`T=0` has been removed from exactly `A1,...,Ak`. The initial formula is `F_0`.
There are never units in these states: a shortened `Ai` still has three
symbols.

The formula is satisfiable: take `T=1` and every `Si=0`, with any `G` value.
The initial value-occurrence measure is

```
V(F_0) = 2 + 6n + n(n+1)/2,
V(F_k) = V(F_0) - k.
```

## 4. Every clause remains indispensable

These witnesses work in every state `F_k`, so no sound subsumption, HLA, or
other whole-clause elimination can remove a clause.

To falsify only `Ai`, including `A0`, take `T=1`, `G=gn`, set `Si` and
`S(i+1)` to `1`, and all other selector symbols to `0`. The two adjacent edge
clauses, when present, each have their other selector equal to `0`. Every
`Bi` holds through `T=1`. The selected `Ai` has no satisfied literal.

To falsify only `B0`, take `T=0`, `G=g1`, `S0=S1=0`, and all other selectors
equal to `1`. The seed and `A1` hold through their selectors, and every
`Ai` with `i >= 2` contains `g1`. `B1`, if present, holds through `G=g1`.
All other blockers hold through their selector. The domain contains `g1`
also when `n=1`.

To falsify only `Bi` with `i >= 1`, take `T=0`, `G=g0`, `S0=Si=0`, and all
other selectors equal to `1`. The seed holds through `S0`; every `Ai` contains
`g0`; `B0` holds through `g0`; and every other blocker holds through its
selector. The selected blocker's three literals are false.

These explicit witnesses also prevent indirect HLA deletion. Under the other
clauses, every virtual HLA extension is equivalent to its starting target.
A witness falsifying that target while satisfying every donor therefore
continues to falsify the virtual target, so it can become neither a tautology
nor subsumed by a retained donor.

## 5. Classification of all possible productive local rewrites

The local rules' algebraic soundness is sufficient here; no completion or
idempotence property is assumed. For a donor `D` and target `C`, let `E_D`
be the set of symbols on which the donor range is not contained in the target.
First-order restriction at `t` needs `E_D subset {t}`. Second-order
restriction using intersection symbol `s` and restriction symbol `t` needs
both donors' exceptional sets inside `{s,t}`, together with
`D_s intersect E_s subset C_s`.

### 5.1 Almost every literal value is protected by a direct model

Every selector literal of every `Ai`, and every individual `G` value in an
`Ai`, is necessary even in the full current conjunction. For a selector
literal, take `T=1`, `G=gn`, let that selector be `0`, the other selector in
the target be `1`, and all remaining selectors be `0`. For a `G=g_j` value
of `Ai`, take `T=1`, `G=g_j`, set both target selectors to `1`, and all other
selectors to `0`. In each case the target holds only through the selected
literal value, and every other clause holds. These witnesses cover the seed's
selector literals as well.

Every blocker's `T=1` literal is necessary: take `T=1`, `G=gn`, and all
selectors `0`. Its selector literal is necessary by the same assignments as
the clause-deletion witnesses above, except set its selector to `1`. The
`G=g0` literal in `B0` is necessary with `T=0`, `G=g0`, `S0=0`, and all other
selectors `1`. The `G=g1` literal in `B1`, when present, is necessary with
`T=0`, `G=g1`, `S1=0`, and all other selectors `1`.

Consequently a sound local rule can change only an unremoved `T=0` literal
in an `Ai`, or possibly `G=gi` in a blocker with `i >= 2`. The latter values
are not protected by such a full-formula model, but they have the following
syntactic obstruction.

### 5.2 No local rule can remove a blocker's guard

For restriction of `Bi` at `G`, an `A` donor has two distinct selector
exceptions: its selector values are `0`, whereas the target's sole selector
value is `1`. These two exceptions cannot both be outside pivot `G` in an
SSE1 or SSE2 inference.

Every other blocker `Bj` has exceptional set `{Sj,G}` against `Bi`. An SSE1
rule is impossible. In SSE2, two such blockers must have the same selector
`Sj` to fit the same allowed pair `{s,G}`, so they must be the same blocker.
Its self-intersection `{1}` on `Sj` is not contained in the target's absent
`Sj` range. Thus it cannot supply the required intersection premise either.
No guard in any blocker changes.

### 5.3 The only useful rule for `Ai` is its immediate predecessor

Consider a target `Ai` with `T=0` still present. No first-order donor can
restrict `T`: an `A` donor other than the target has an uncontained selector,
and every `B` donor has an uncontained selector value `1`.

For second-order restriction at `T`, a blocker `Bj` must use `Sj` as the
intersection symbol, since its selector is always an exception. Its guard
must satisfy `gj in P_i`, so `j < i`.

An `A_h` paired with it must have every selector other than `Sj` present in
the target. Among distinct `A` clauses, this leaves only the two adjacent
possibilities: `A(i-1)` with outside selector `S(i-1)`, or `A(i+1)` with
outside selector `S(i+2)`. The latter requires blocker index `i+2`, which
violates `j < i`. Hence the unique possible mixed pair is

```
A(i-1), B(i-1),  intersection symbol S(i-1),  restriction symbol T.
```

The selector ranges are `{0}` and `{1}`, so their intersection is empty.
The target-pivot union is `{0,1}` while `A(i-1)` still has `T=0`, and becomes
`{1}` after that literal has been removed. Thus this rule is useful exactly
when the predecessor has already shortened. The seed `A0` has no `T` literal
from the beginning.

Two blocker donors cannot give another rule: their selectors must coincide,
so the donors would coincide, and their nonempty selector self-intersection
fails the target containment premise. Two `A` donors cannot give another
rule either. Each non-self `A` donor must be adjacent to the target to have
only one selector outside it. The two adjacent clauses have different outside
selectors, and repeating one adjacent donor leaves a nonempty `{0}`
self-intersection on a selector absent from the target.

Therefore at state `F_k`, `k < n`, the **only useful local restriction** is
the two-donor deletion of `T=0` from `A(k+1)`. At `F_n` none remains. There are
no possible whole-clause eliminations by Section 4, and no units to propagate.

## 6. The production scheduler performs exactly one such rule per pass

Assume the preceding passes produced `F_k`, with `k < n`. Sorting by clause
length at the next invocation produces the order

```
A0, B0,...,B(n-1), A1,...,Ak, An,A(n-1),...,A(k+1).
```

The already-shortened `A` clauses have length three, and preserve their
creation order after the blockers. All unshortened `A` clauses have length
four and preserve their decreasing index order. First-order comparison
construction changes nothing, by Section 5.

The manual second-order queue at lines 628–646 is constructed from the count
matrix using `which(..., arr.ind = TRUE)`. R enumerates this matrix in column
order, so target indices increase through the queue. In particular, the
unshortened targets are visited in decreasing subscript order. Each pair
`Bi -> A(i+1)` has exactly the two exceptions `{Si,T}`, before and after its
predecessor's shortening, so it is in this manual queue.

For targets `An,...,A(k+2)`, the only possible predecessor still has its `T=0`
literal when that target's queue column is visited. The rule is a no-op.
Eventually the queue reaches `A(k+1)`, whose predecessor is already short
(or is the seed). The correct donor is in the `Si` registry, all off-pivot
comparisons hold, and both inverse guards pass: `{0}` is contained in neither
the predecessor's empty `T` range nor the blocker's `{1}`. The rule deletes
the target's `T=0` literal.

Now consider the callbacks triggered by that deletion. The new donor's old
`T` range `{0}` was already contained in every still-unshortened target's
`T` range `{0}`. All these comparison bits were therefore FALSE. At lines
264–285, `eliminate_symbol_from_clause()` dispatches only rows whose removed
symbol bit changed from TRUE to FALSE. It does not revisit any unshortened
target, and it has no final `on_update_range()` callback.

The callbacks it *does* dispatch concern blockers, the seed, or already
shortened targets. Section 5 excludes productive rewrites of all of them.
Thus there is no indirect cascade. The newly enabled target `A(k+2)` has
already had its manual queue column visited and is not requeued. HLA deletes
nothing because every clause remains indispensable.

Exactly one value occurrence has therefore been removed, yielding `F(k+1)`.
This proves the induction. At `F_n`, no rule changes anything and all clauses
are already sorted by their final length. The `n`th returned formula is an
exact storage fixed point; a further invocation verifies equality.

The use of an increasing guard *range* is what keeps all unshortened targets
at the same length while preventing short resolution shortcuts through
unrelated blockers. Increasing the number of guard symbols instead would
change the length sort and destroy this particular schedule.

## 7. Forward-order control

If the initial target order is instead `A1,...,An`, the manual queue visits
them in that increasing order. It first shortens `A1`, then reaches the
still-pending queue column for `A2`, whose predecessor is now short, and so
on. The omitted callback is irrelevant because each next target's own queued
visit is still ahead. All `n` deletions finish in one pass, and the final
clause/value set is the same `F_n`.

This confirms that the long repeated-pass sequence is a scheduling effect.
The corresponding local-reduction dependency chain can be traversed in a
single invocation by the existing implementation with a different input order.

## 8. Concrete verification and the minimized three-pass member

The persistent R adapter invokes the unchanged kernel in R 4.6.1. Every
intermediate result in `chain_pilot.json` and `family_scale.json` was checked
against the initial formula by the independent one-hot SAT and MDD oracles.
Observed reverse-order pass counts are exactly `n` for
`n=1,2,3,4,5,8,12,16,24,32`. The forward-order controls at
`n=3,8,16,24,32` need one productive pass and have the same final normalized
clause/value set.

`check_family_rules.py` independently enumerates every SSE1 and SSE2 premise
using Python sets on all prefix states for `n=1,...,10`, including repeated
donors. It verifies the claimed unique useful rule in 65 states and checks
945 explicit clause-irredundancy witnesses. It also compares direct-kernel
and public-constructor first-pass results for these ten family members.
These finite checks support the symbolic classification and source audit;
they do not replace the induction over arbitrary `n`.

Greedy deletion minimization of the `n=3` member, while retaining proper
literal ranges and at least three productive passes, gives the following
seven clauses. `G` has domain `{g0,g1,g2,g3}`; `T,S0,S1,S2` are binary.

```
(S0=0 | S1=0)
(S0=1 | T=1 | G=g0)
(S1=1 | T=1 | G=g1)
(S2=1 | T=1)
(T=0 | G in {g0,g1,g2})
(S2=0 | T=0 | G in {g0,g1})
(S1=0 | S2=0 | T=0 | G=g0)
```

Passes 1, 2, and 3 respectively remove `T=0` from the last, penultimate, and
antepenultimate clauses. The third removal creates the unit
`G in {g0,g1,g2}`. Pass 4 only moves that unit into its sorted position;
pass 5 confirms exact stored equality. The measures are

```
22 -> 21 -> 20 -> 19 -> 19 -> 19.
```

All intermediate formulas were SAT/MDD checked; the domain product contains
only 64 valuations. `minimized_three_pass.json` contains the full traces and
the source-reported events. This is a deletion-minimal result of the recorded
greedy search, not a claim of globally minimum clause or domain counts.

`reproduce_three.R` additionally passes each actual returned `CnfFormula`
object directly into the next call, without serialization, checks all 64
valuations after each call, and confirms the productive/order-only split.
Run it from the repository root with

```
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/repeated_passes/reproduce_three.R
```

No production files were changed and no commits were created for this work.

Independent review: `../quotient_review/REPEATED_PASS_REVIEW.md` challenged
the scheduling induction, callback exclusion, witnesses, and convergence
argument and found no substantive gap. Its fresh public-package construction
checked `n=1,...,6`, 27 frontier states, 251 clause witnesses, 876 protected
literal-value witnesses, 39 public calls, and 51,168 output truth comparisons.
It separately repeated the minimized 64-world example through all five calls.
