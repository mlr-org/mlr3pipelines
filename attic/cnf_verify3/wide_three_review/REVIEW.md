# Independent review: three initially wide clauses

Reviewed 2026-09-06 against `R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
This review concerns Sections 3–5 of
`../wide_three_saturation/WIDE_THREE_SATURATION.md`. No production source or
package test was changed, and no commit was created.

## Verdict and scope

I found no substantive gap in the extension from common triples to exactly
three input clauses of arbitrary initial widths at least three. The initial
width argument, the exclusion of pending reverse initialization, and the
arbitrary-outside-symbol debt/tail argument can all be reconstructed from the
current source. The proof is conditional on the stated ordinary finite-set
representation, consistent list/environment symbol identity, local soundness,
lifecycle/comparison-owner lemmas, and sufficient resources for normal return.
It does not establish the same theorem for every accepted R character encoding.

The common-three-symbol case and the static SSE2/HLA reductions remain
dependencies on the already reviewed results. I checked the new source
scheduling arguments directly; the controls below are not their proof.

One minor precision improvement would help: where Section 5 says that an
operation “must be SSE1 from B,” read this as **an SSE1-shaped restriction
from B**. The production twoend loop can use the same donor twice. Such a
call has the same bound and set premises as SSE1: its intersection premise
provides the missing off-target containment. Section 2 already records this
degeneracy, and none of the later reasoning requires the restriction to have
entered through the ordinary SSE1 call site specifically.

## 1. Why the new cases contain no units

For a prescribed symbol value at `q`, each original support minus `q` has
at least two symbols. Three such supports admit distinct representatives
unless their union has only two symbols. That exception forces all three
original supports to equal the same triple containing `q`. Thus, outside
the separately reviewed common-triple case, every value of every symbol
extends to a model of the initial formula.

A produced unit would have a nonempty proper range, because actual ranges
only shrink from proper input ranges. Local soundness would make this unit
a nontrivial unary consequence, contradicting the full projections above.
This excludes all unit branches throughout the new cases, including nested
unit propagation and the return before comparison repairs at lines 245–252.

## 2. The omitted SSE1 relay really is confined to initialization

Write `A -> T` for an eventual productive sole-`t` pair and choose a retained
value `v in T_t \ A_t`. Ordinary owners exist on initialization, a source
count decrease to zero/one, and a reverse zero-to-one increase with a still
pending zero visit. Whole-symbol removal clears the source bit before its
visit batch. A current count-one ordinary handler uses its current pivot and
range at lines 298–322. Consequently an omitted surviving restriction needs
a loss of `v` from `A_t` while its established `t` bit remains TRUE.

Before that loss, `A` and `T` contain `v`. The third ghost `B` never contained
it: all-three incidence cannot suffer a first loss. Every off-`t` range of
`B` is then frozen, including absent coordinates. An SSE1 restriction from
`A` or `T` is blocked at `t`; an SSE2 restriction either has that obstruction
or encounters the common `v` at intersection pivot `t`.

The loss therefore uses `B_t` as an SSE1 bound and implies

```
B_q subset A_q subset T_q, for all q != t.
```

These containments also held at any earlier pair initialization: `B_q` is
still original, and `A_q,T_q` only shrink. A completed `B -> T` ordinary
initialization would already have removed `T` or `v`. A completed `B -> A`
ordinary initialization would likewise already have removed `A` or its
copy of `v`. In particular the order `A,B,T` is impossible: its earlier
`B/A` initialization finishes before `A/T` is initialized.

The other qualification is a pending reverse `B -> T` initialization. Both
counts are installed at 608–610, so this is different from an NA pair. If
`B -> T` is the first direction, its restriction removes the witness before
recursion. Otherwise the first direction is `T -> B`; its only possible
ordinary exception is `t`, since `T_t` contains `v` absent from `B_t`.
The source update opened directly by this first direction changes `B_t`.

A nested `B -> A` visit from that update has just cleared `B`'s `t` bit.
Count zero deletes `A`. Count one has an off-`t` exception `q`; that exception
is raw-correct and permanent until the supposed loss, since `B_q` is frozen
and `A_q` can only shrink. It precludes the later sole-`t` `B -> A` operation
needed to remove `v`. Larger counts are skipped while SSE2 is disabled.
If `B_t` is removed completely, exactly the same cleared-column argument
applies; there is no unit escape. Reverse repairs of incoming comparisons
do not invoke ordinary callbacks. No fourth clause can supply another route.

Hence `B/T` must still be NA when the relevant loss occurs, while `A/T` is
already established and `B/A` is being initialized. The only possible orders
are `T,A,B` and `A,T,B`. The former reaches `B/T` before `B/A`; the pending
case was just excluded. Only `A,T,B` remains.

This argument does not assume that `B` survives to the final output. Rather,
it proves that the decisive loss and its resolution occur inside this one
initialization pair, before an unrelated later retirement could intervene.

## 3. Initial widths force equal original supports

At a loss with surviving `A`, the bound `B_t` is nonempty: an empty bound
deletes `A` through 153–157. Thus all three original supports contain `t`.
Every original off-`t` symbol of `B` retains its original nonempty range and
is present in the current `A,T` by the relay containments. Therefore

```
S_B subset S_A intersect S_T.
```

The actual initial sort at line 48 and surviving order `A,T,B` give

```
|S_A| <= |S_T| <= |S_B|.
```

These force equality of the original supports. There is no inference about
the ordering of their current widths. Let their common original width be
`m`; the new non-common-triple branch actually has `m >= 4`, although the
remaining relay argument only needs `m >= 3`.

There is also no hidden dependence on exactly two off-`t` symbols:

* With no strict off-`t` `A/B` comparison, inner `A -> B` runs first. It
  leaves the reverse pair at zero and deletes `A`, either recursively or
  when the reverse initializer reads the refreshed count. `B/T` remains NA.
* Let `k >= 1` be the number of strict off-`t` `A/B` comparisons. Then the
  earlier `A -> B` count is at least two and is skipped. The reverse
  `B -> A` restriction at `t` may remove `v`. A contained bound deletes
  `A` immediately. Otherwise put `A'_t = A_t intersect B_t`.
* If `A'_t subset T_t`, the repaired `A -> T` count is zero. `T` has index
  two and `B` index three, so the source scan or deletion batch visits `T`
  first and deletes it without a prior distinct-row callback.
* Otherwise only `A -> B` needs a forward repair, leaving count `k`.
  `k >= 2` is skipped before enablement; `k = 1` uses a bound strictly
  containing the target `B_q`, so line 160 returns without recursion.

Complete pivot deletion leaves `m-1 >= 2` frozen, nonempty off-`t` ranges.
There is no unit return. Initialization then reaches `B/T`; the strict
off-`t` comparison makes the first `T -> B` direction ineligible, and the
reverse `B -> T` direction removes `T` or `v`. This closes the relay.

## 4. Arbitrarily many outside comparisons have the same last owner

For a final independent SSE2 witness, let `t` be the restricted coordinate,
`s` the intersection coordinate, and choose
`v in T_t \ (A_t union B_t)`. The incidence/dominance argument makes `v`
originally private to `T`. Every possible bound on `T_t` omits `v`, so `T_t`
never changes while this witness survives. For each coordinate `q != t`,
every legal bound on `A_q` contains `B_q`, and symmetrically. Hence
`A_q intersect B_q` is constant. In particular the final intersection
premise at `s` was true throughout the execution.

After the static reductions, final donor exception sets are `{s,t}` and
either `{s}` or `{s,t}`. Take the last transition of any raw donor-to-target
comparison after initialization, or the enablement boundary if there is no
later transition. After that point the raw roles have their final values.
The inverse comparisons at `t` are permanently TRUE because of `v`.

The important monotonicity is now of **cached excess bits**, not of actual
ranges or all comparison bits throughout the run. Every final exception
must be cached TRUE by raw FALSE soundness. An extra TRUE at an outside
coordinate, or at the eventual oneend donor's `t`, denotes current
containment and has a source-update owner. Once cleared after stabilization,
it cannot be set TRUE again: reverse setters require an actual noncontainment
which the stabilized role excludes.

If there is any such debt, choose the chronologically last repair among both
donor rows. Its decrement leaves its donor with exactly its final count one
or two, and the partner is exact already. The repair owns an ordinary visit
at 224 or 285. A batched visit can be pending during other callbacks, but
the final raw roles cannot change and no new debt can appear. Its visit thus
sees the required current role when reached.

This works for any finite number of outside coordinates. It does not need
one selected “last outside symbol” to dominate the other pending frames.
If a count-one visit is suppressed only at its second-order flag, that
pair's initial manual queue entry still lies ahead. A count-two ordinary
dispatch itself bypasses the flag. Final surviving operands cannot trigger
retirement exits. At the resulting exact-role visit, registry membership,
the raw intersection check, both twoend orientations, and the inverse guards
lead to a restriction removing `v`.

## 5. A target-side pivot transition cannot evade all those visits

The remaining transition is a shrink/removal of `T_s` that makes `A_s`
exceptional. A union bound containing `A_s` cannot cause it. The operation
therefore has the SSE1-shaped bound `B_s`, with `B_t subset T_t` and all
off-`s` containments. The donor newly made exceptional is the final twoend
`A`; the other donor is permanently oneend at `s`. Immediately beforehand,
`A` has the raw sole-`t` role.

Outside debt is already covered by the last-repair argument. With no outside
debt, distinguish the cached `s` bit before this target change:

**FALSE.** An ordinary count-one visit is pending, since a completed one
would have removed `v`. Initialization is before enablement and the later
twoend queue covers that case. In the enabled phase a nonempty source repair
invokes its ordinary handler immediately. The permanent `t` exception rules
out a pending zero-to-one owner. Thus the only pending count-one source is
a whole-symbol deletion batch, for some removed coordinate `r` outside
`{s,t}`.

Every allowed restriction of `A_r` requires `B_s subset A_s`. An SSE1 bound
from `T` is blocked by its private `t` value. An SSE1 bound from `B` requires
the inclusion. A `B,T` SSE2 bound must use intersection pivot `t`, so its
off-pivot premise requires the same inclusion at `s`. A repeated donor
reduces to these same cases. This reasoning is unchanged if many other
outside coordinates are present. Dominance is persistent, so just before
the target change,

```
B_s subset A_s subset T_s.
```

Using `B_s` as the bound then deletes `T` at line 153; it cannot produce the
claimed surviving pivot exception. Final `A_s,A_t` are nonempty, so the
source cannot have taken the unit early return from the removal of `r`.

**TRUE.** The bit is inaccurate immediately before the target transition.
Choose the last strict write of `A_s` before that transition. It is nonempty
since `A_s` survives finally. With this fixed current source range, every
earlier `T_s` is a superset of the current `T_s`, which still contains it.
Thus the source frame cannot already have examined `T`'s row: that inclusion
would have cleared its TRUE, and target narrowing could not restore a TRUE
before the first ensuing loss of inclusion. A later source shrink would
instead provide a later chosen frame.

The owning frame is therefore still pending with `T` on its forward scan
and its range-notification tail at line 231 still ahead. Its snapshot
includes `T`, since the pair is initialized, both operands survive, and
`T_s` contains nonempty `A_s`. The target change can make the TRUE accurate,
so a later bit repair is not promised. What remains promised is this exact
frame's later `on_update_range(A,s)` call. After stabilization `s` is an
actual exception present in `A`; line 349 includes `T` even if `T_s` was
removed. Any remaining outside/partner debt has a later final repair; with
exact rows the twoend handler reaches the witness. A disabled flag retains
the corresponding manual queue owner.

This is the needed notification ownership argument. Eventual matrix
exactness alone would not prove it, since the stale TRUE can become exact
without a clear. Adding outside symbols introduces more possible repairs,
but cannot move this source frame's tail before its still-pending target row.

## 6. Executable controls and limits

`checks.py` constructs 22 deterministic bases: common-support relays at
widths four and seven with zero, one, and two strict off-pivot comparisons,
partial and complete pivot removal bounds; unequal-width relay candidates
whose smallest support must sort first; and saved scheduling-boundary seeds
extended with six additional symbols using three different support patterns.
The latter include common width nine, equal widths with unequal supports,
and unequal widths. All resulting inputs have initial widths at least three
and are outside the common-triple case.

Each base uses all six input clause permutations and two explicit independent
symbol/range order arrangements. A local raw-set checker checks final direct,
SSE1, unrestricted two-donor SSE2, and domain-propagation/HLA opportunities.
It compares first and second results as clause multisets modulo symbol/value
order, preserving duplicate clauses. A scalar Cartesian evaluator compares
both outputs with the original. It does not use CNF `all.equal()`.

The previously reviewed direct-kernel bridge runs unchanged production and
requires each selected observed execution to be `identical()` to a separate
plain execution. Both R 3.6.3 and R 4.6.1 complete:

| Per runtime | Count |
| --- | ---: |
| Positive arrangements | 264 |
| Truth-table assignment rows, each comparing input and both outputs | 385,536 |
| Additional observed calls identical to plain calls | 22 |
| Observed checkpoints without a unit | 618 |
| Enabled conservative-TRUE observations | 26 |
| Count-one entries pending in enabled deletion batches | 2 |
| Outside-scope four-clause SSE1 gap rejected | 1 |

All positive outputs are semantically unchanged, locally saturated under the
raw checker, and unproductive on a second call. The negative control has a
detected SSE1 remainder and a productive second call. The observation counts
are repeated execution observations, not distinct abstract scheduler states.
In particular these controls do not exhaust simultaneous debt across arbitrary
outside coordinates; Section 4 supplies that source argument.

Reproduce from the repository root:

```sh
python3 attic/cnf_verify3/wide_three_review/checks.py
python3 attic/cnf_verify3/wide_three_review/checks.py --current-r
```

`checks_r36.json` and `checks_r46.json` retain every constructed base and the
per-base observations. The theorem remains a manual source-level argument,
not a mechanized proof of R callback semantics or an unrestricted public-API
encoding theorem.
