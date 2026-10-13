# Three clauses of initial width at least three

2026-09-06. Reviewed unchanged production source:
`R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
This directory contains analysis and controls only.

## Conclusion and contract

The first-call local-saturation argument extends to **any three proper
canonical input clauses whose initial widths are each at least three**.
Their supports need not coincide; widths, clause order, within-clause symbol
order, range order, and ordinary finite-domain sizes are unrestricted.
The subsequent [independent source review](../wide_three_review/REVIEW.md)
supports the widened initialization and deferred-tail arguments and supplies
264 additional directed arrangements per R version.

This is a source-level theorem conditional on the ordinary representation,
local soundness, lifecycle, comparison-owner, and normal-return premises of
`../review_semantics/REVIEW.md`. Domains are fixed, finite, and nonempty;
ranges are finite ordinary sets; each initial range is nonempty and proper;
symbol names within each clause are distinct; and indexing/count arithmetic
does not overflow. In addition, **symbol identity must agree across named
lists and all environment binding lookup/enumeration operations**, and value
identity must agree across the set primitives. This is an explicit ordinary-
set semantic premise, not a claim about every R character encoding or locale.
Missing symbols mean empty ranges. The theorem does not cover malformed
selector storage, character-identity mismatches, external mutation, or resource
failure.

The local rules are actual unit propagation and unit subsumption, direct
subsumption, SSE1, the full two-donor SSE2 set rule, and domain-propagation
refutation under a target's negation (the reviewed HLA deletion predicate).
The first output has no productive instance of any of these rules. A second
call can sort surviving clauses after their widths changed in the first
call, but cannot remove another clause, symbol, or value. Clause multiplicity
is retained when comparing outputs modulo order. Neither complete logical
entailment, canonical form, output confluence, nor exact ordered-object
idempotence is claimed.

The common-three-symbol case is the already reviewed theorem in
`../three_full_review/REVIEW.md`. The new proof below handles every other
support pattern; its main simplification is that **no unit can ever appear**.
The SSE1 relay reduces to common original supports of arbitrary width, and
the enabled SSE2 pending-visit argument works with arbitrarily many symbols
outside its two pivots.

## 1. Matching and the exact unary exception

Write `S_i` for the original support of clause `i`, and `C_i,q` for its
original range at `q`, with absent ranges empty. Three supports, each of
size at least three, have distinct representatives: the union of any one,
two, or three supports has at least the number of supports selected. Assign
each representative a value from its clause's nonempty range. This gives a
model of the input, so every input in this class is satisfiable.

Now fix any symbol `q` to any value in its domain. To satisfy the three
clauses using other symbols, consider `S_i - {q}`. Every such support has
size at least two. All one- and two-support matching conditions therefore
hold. The three-support condition can fail only if their union has size
two. Because all original supports had size at least three, that failure
is equivalent to all three original supports being exactly the same triple
`{q,r,s}`. If they are not one common triple, these residual supports have
distinct representatives and the fixed value extends to a full model.

Thus outside the common-triple case the projection of the model set on
**every** symbol is its complete domain. No nontrivial unary consequence
exists. Every represented unit produced by sound monotone restriction would
be a proper unary consequence, so no intermediate or final clause can become
a unit. This excludes unit merge, propagation, the unit early return, and
the nested unit-equality scheduling omission throughout the new case.

The exceptional case has an exact semantic characterization. If all three
supports equal `{q,r,s}`, let

```
W_q = C_0,q union C_1,q union C_2,q.
```

Every value in `W_q` extends to a model: satisfy one clause using `q`, and
the other two using distinct symbols `r,s`. A value outside `W_q` extends
to a model exactly when at least two original ranges overlap at `r` or at
`s`. Sufficiency follows by satisfying two clauses at that shared value and
the third on the other symbol. Necessity follows because three clauses
satisfied using only two symbols must have two clauses satisfied on the
same symbol. Consequently

```
projection_q(F) = W_q   if the r ranges are pairwise disjoint and
                           the s ranges are pairwise disjoint;
                D_q   otherwise.
```

A proper unary consequence on `q` therefore exists exactly when both
disjointness conditions hold and `W_q` is proper in `D_q`. If production
actually represents a `q` unit `U` descended from clause `i`, soundness and
monotonicity give

```
W_q subset U subset C_i,q subset W_q,
```

so `U = C_i,q = W_q`. Its retained range never narrowed, and every other
original `q` range was already contained in it. Any same-symbol unit merge
is equal. The reviewed common-triple proof additionally checks the exact
equality-skip branch and rules out a surviving unit-equality clause. These
facts are sufficient here; the projection criterion alone does not claim
that the simplifier must materialize every entailed unit.

At most one symbol can have a represented proper unit in the common-triple
case: its original range equal to `W_q` overlaps the other two nonempty
`q` ranges, whereas a proper unary consequence on either other symbol would
require the `q` family to be pairwise disjoint. More than one semantic unary
consequence can nevertheless exist when none is representable as a monotone
descendant unit. Symbols outside the common triple always have full-domain
projection.

## 2. Value incidence does not require common supports

Keep each original clause's ghost identity while its ranges shrink. For
three live ghosts `A,B,T`, fix a symbol `t` and value `v` that survives in
`T_t`. In the new case no unit changes occur.

* If all three original `t` ranges contained `v`, there can be no first
  loss: every SSE1 bound has a donor containing `v`, and every SSE2 bound
  is a union of the other donors' ranges containing `v`.
* If `A_t,T_t` contain `v` but `B_t` does not, every off-`t` range of `B`
  is frozen. An SSE1 restriction by `A` or `T` fails containment at `t`.
  An SSE2 restriction either fails the same containment or uses `t` as its
  intersection pivot, where the common value `v` defeats the premise.
  This assertion includes ranges originally absent from `B`.
* If `A` is the next ghost to lose `v`, the operation must be SSE1 from
  `B` at `t`: a bound from `T` or the `B,T` union contains `v`. This gives
  `B_q subset A_q` for every `q != t`. That dominance persists while all
  three ghosts and `v` survive. Later bounds on `A_q` are `B_q` or a
  union containing `B_q`; a bound from `T` alone is blocked at `t`.

Degenerate production SSE2 calls with the same donor twice reduce to an
SSE1 bound and change none of these implications. Clause retirement ends a
three-live-ghost witness; it never creates a replacement ghost.

For a final independent SSE2 rule, choose
`v in T_t minus (A_t union B_t)`, with intersection pivot `s`. The static
classification requires both donors to have an actual `s` exception. If
`v` originally belonged to `A,T`, the third incidence fact gives permanent
`B_s subset A_s`; the intersection premise would then imply
`B_s subset T_s`, a contradiction. The symmetric case is identical, and
all-three incidence was already excluded. Therefore a final independent
SSE2 witness was **originally private to `T`**.

Two further invariants hold while this final triple and `v` survive:

1. `T_t` never changes. Any actual bound at `t` omits `v` and would remove it.
2. For **every** symbol `q != t`, `A_q intersect B_q` is constant. A
   restriction of `A_q` using `T` alone is blocked at `t`. A restriction
   using `B,T` must use `t` as intersection pivot, and its bound contains
   `B_q`. Intersecting `A_q` with a bound containing `B_q` preserves the
   donor intersection. The argument is symmetric for `B`.

Hence the final intersection premise held from the beginning: donor
intersection is constant and every earlier `T_s` contains the final `T_s`.
Once `B_s subset A_s`, it also remains so. None of these facts assumes that
all supports coincide or that there is only one off-pivot symbol.

## 3. The SSE1 relay forces common original supports

Use the ordinary-visit classification in
`../scheduler_review/REVIEW.md`, Sections 2–3, **without** its diagnostic
added scans. Initialization and every source decrease to count zero or one
own an ordinary visit. A reverse zero-to-one increase preserves its pending
ordinary owner. Whole-symbol removal clears the corresponding source bit;
it does not leave an unchanged exceptional bit. A handler reads the current
count/pivot before applying SSE1. Therefore a final omitted sole-`t` SSE1
restriction can only originate when `A_t` loses a retained target value
`v`, while its previously established exceptional bit remains TRUE.

At the relevant loss, the third ghost `B` has never contained `v`, its
off-`t` ranges are still their original sets, and it restricts `A_t` by
SSE1. The already-established `A`-to-`T` role gives

```
B_q subset A_q subset T_q       for every q != t.
```

Any completed earlier ordinary `B`-to-`T` visit would have deleted `T` or
removed `v`. Its zero/one role was already present at initialization:
`B`'s off-`t` ranges are frozen and `T` only shrinks. A completed earlier
`B`-to-`A` initialization likewise would already have removed `v` from `A`.
This second observation explicitly excludes the order `A,B,T`: when `T`
later initializes against `A`, its putative loss-producing `B/A` visit
cannot still be ahead of it.

The reverse-initialization pending case also cannot hide this loss.
If the first direction is `T`-to-`B`, its only possible exception is `t`
because `T_t` contains `v` absent from `B_t`. Its update changes only
`B_t`. A nested `B`-to-`A` ordinary visit from that source update has just
cleared the `t` comparison. Count zero deletes `A`. Count one at an
off-`t` symbol `q` means `B_q` is not contained in `A_q`; that exception is
permanent until the supposed first loss, because `B_q` is frozen and
`A_q` only shrinks. It prevents `B` from being a sole-`t` donor for that
loss. Larger counts are skipped before SSE2 enablement. If `B`-to-`T` is
the first initialization direction, it removes `T` or `v` before recursing.

Thus at the loss the `B/T` pair is uninitialized, the `A/T` pair has already
been visited, and `B/A` is being initialized. `B` must be the last original
clause in the initialization order. The order `T,A,B` reaches `B/T` first
and removes the witness, leaving only `A,T,B`.

Now the initial width sort matters. `B_t` is nonempty at a surviving
loss: an empty restricting range deletes `A` directly at lines 153–157.
Both `A` and `T` originally contain `t` because they contained `v`.
Every other original symbol of `B` remains nonempty and is contained in
the current `A` and `T`. Therefore

```
S_B subset S_A intersect S_T;
|S_A| <= |S_T| <= |S_B|.
```

These force `S_A = S_T = S_B`, of a common width `m >= 3`.
This is an original-support statement. No claim is made that the current
width ordering remains sorted after simplification.

The remaining source argument is the reviewed common-support relay with
`m-1` off-`t` symbols in place of two. Initialization tries inner `A`-to-
outer `B` first (608–615). If every off-`t` range is equal, it restricts
`B_t` into `A_t`, then the initialized reverse zero pair deletes `A`.
`B/T` is still NA, so `B`'s source scan has only the initialized `A` row.
Complete pivot deletion leaves `m-1 >= 2` frozen nonempty ranges and cannot
take the unit early return.

Otherwise there are `k >= 1` strict off-`t` `A/B` comparisons. The earlier
`A`-to-`B` count is at least two and is skipped in initialization. The
reverse `B`-to-`A` handler can remove `v`. If its bound already lies inside
old `A_t`, the helper deletes `A`. In the remaining case put
`A'_t = A_t intersect B_t`:

* If `A'_t subset T_t`, `A`-to-`T` becomes zero. The source scan reaches
  `T` (index 2) before `B` (index 3) and immediately deletes `T`. Complete
  pivot removal has the same batch order, and leaves at least two nonempty
  off-`t` literals.
* Otherwise `A`-to-`T` retains its exceptional bit. The only new forward
  repair is `A`-to-`B` at `t`, leaving count `k`. If `k >= 2`, it is skipped
  while second order is disabled. If `k = 1`, the off-`t` bound strictly
  contains `B`'s current range and the helper returns unchanged at line 160.
  There is no recursive operation before initialization reaches `B/T`.

At that later pair, the strict off-`t` comparison also makes `T`-to-`B`
have at least two exceptions, so it is skipped. `B`-to-`T` is zero/one
and removes `T` or `v`. The presumed SSE1 remainder is impossible. No relay
argument about arbitrary later SSE2 callbacks is required.

Direct-subsumption remainders are excluded by the same ordinary count-zero
owners. An owner whose target or source retires no longer describes a final
pair; a final live zero count cannot survive completion of all its owners.

## 4. Stabilization with arbitrarily many outside symbols

After direct/SSE1 and the static SSE2/HLA reductions, a final independent
SSE2 remainder has donors with exception sets `{s,t}` and either `{s}` or
`{s,t}`. Let `A` be a final twoend donor and let `Q` contain every original
symbol outside `{s,t}`. The final raw donor comparisons on every `q in Q`
are containments. Both donor `s` comparisons are exceptions. `A`'s final
`t` exception has held throughout, since `T_t` is constant.

Choose the last actual transition of any raw donor-to-`T` comparison,
or the boundary just before SSE2 enablement if there is no later transition.
This is a finite choice because actual range changes are finite on normal
return. After it, all relevant **raw roles** are final. Section 2 already
makes the intersection premise and excluded value valid throughout.

The last transition into a final role can only be:

* a donor shrink at some `q in Q`, or at `t` for the eventual oneend
  donor, changing an exception into containment; or
* a shrink/removal of `T_s` creating a donor's final `s` exception.

Target shrinking cannot create a donor containment, donor shrinking cannot
create a donor exception, and `T_t` cannot shrink. These observations cover
all symbols, including symbols absent from an original support.

After stabilization, every extra cached TRUE is containment debt with a
pending live source update. The **last** remaining repair in a donor row
decreases its count to the final one or two and supplies the ordinary visit
at line 224 or 285. If the partner still has debt, its later last repair
supplies another visit. There may be many outside symbols, but every one
falls into this same finite last-repair argument. A source-range notification
skipped by a FALSE pair-enable flag still has that pair's later manual queue
visit ahead of it; ordinary count-two dispatch itself does not test this flag.

At a stable-role visit, both donors occur in the `s` registry, all outside
comparisons are FALSE, the one/two counts are exact, and the inverse guards
at `t` are TRUE because `v` remains outside both donors. The twoend handler
tests both pivot orientations (416–445) and reaches the actual intersection
check and useful restriction (453–467). Nested callbacks cannot invalidate
the stabilized roles, intersection premise, or final survivors. Thus a visit
removes `v`, contradicting the proposed final witness.

If no role changes after initialization, the exact initial pair enters the
manual queue at 628–640. The only remaining issue is whether a last target-
side `s` transition can avoid every such visit. The next section closes it.

## 5. The deferred SSE2 tail still has an owner

Suppose shrinking `T_s` newly makes `A_s` exceptional. A bound
`A_s union B_s` preserves earlier `A_s subset T_s`, so the responsible
operation must have the SSE1 bound and premises from `B`. This includes a
degenerate SSE2 call using `B` twice; the reasoning does not require one
particular helper call site. Its off-`s` premise puts `B_t` inside
`T_t` permanently. Immediately before this last raw-role transition, `A`
has the raw sole-`t` role, with every `q in Q` contained in `T`.
Any useful ordinary `A`-to-`T` visit would already remove the private `v`.

If any outside bit is still extra TRUE, its later last repair is the visit
from Section 4. Assume every outside bit is FALSE, and distinguish `s`.

**The cached `s` bit is FALSE.** An exact count-one ordinary visit must be
outstanding. Initialization owners occur before SSE2 enablement and are
covered by the queue at this later twoend role. After enablement, the only
place a newly installed count-one visit waits across another callback is
the whole-symbol deletion batch (264–285): a nonempty source decrement
enters its ordinary handler without intervening recursion (221–224).
A zero-to-one owner is impossible for `A`, whose `t` exception is permanent.

The removed symbol is some `r in Q`. It cannot be `s` or `t`, whose source
ranges survive finally. **Any** legal restriction of `A_r` forces
`B_s subset A_s`: SSE1 from `T` is blocked at `t`; SSE1 from `B` needs that
containment; SSE2 from `B,T` must use `t` as intersection pivot and needs
the same off-pivot containment at `s`. This argument does not require that
`r` is the only symbol in `Q`. Section 2 makes this dominance persistent.
Just before the alleged target change, `A_s subset T_s`, so also
`B_s subset T_s`. `B` cannot productively shrink `T_s` into a new `A`
exception: a bound already contained in `T_s` deletes `T` at line 153.
The final witness therefore cannot pass through this pending-batch escape.
The source still has its final nonempty `s,t` literals, so the unit early
return is explicitly unavailable.

**The cached `s` bit is TRUE.** Before the target change it is conservative
debt. Choose the latest strict nonempty shrink of `A_s` whose update has
not repaired that bit. At the recursive boundary it has already formed its
forward row list (178–205). `T` belongs to that list because it is active,
initialized, and contains the nonempty current `A_s`. Its row has not yet
been passed: an examined current inclusion would have cleared the bit;
a later source shrink creating the inclusion owns a newer update instead.

Shrinking or completely removing `T_s` can now make the existing TRUE
raw-correct, so no later bit repair is required at `s`. Nevertheless the
owning source update has not yet reached its **tail**, `on_update_range(A,s)`
at line 231. At that later tail, `s` remains present in `A` and exceptional
against `T`, so line 349 includes `T`; target presence of `s` is not required.
If its row or partner still has excessive TRUEs, their later last repairs
provide the stable-role visit. Otherwise the twoend notification reaches both
orientations. A disabled flag instead retains the manual queue owner.

This identifies an owner whose notification snapshot is still ahead of the
target transition. Mere eventual comparison exactness would be insufficient:
the target change may make the stale TRUE accurate without clearing it.
Adding arbitrarily many `q in Q` changes only the number of potential later
last repairs, not this required source-tail ownership.

## 6. HLA, orders, and proof obligations

The reviewed static SSE2 classification and HLA theorem need quiescent
exact comparisons, complete registries, no live zero count, and physical
unit containment. These are supplied by the reviewed lifecycle lemmas and
Sections 1–5. In the new support cases units are absent. In the common-triple
case the earlier review supplies physical containment and absence of unit
equality leftovers. HLA changes only virtual ranges and removes actual
clauses. Later donor removal cannot create a domain-propagation refutation
that was unavailable with the earlier larger donor collection. It likewise
cannot create a fresh range rule between surviving clauses.

Clause initialization order is constrained only by the actual initial
width sort. No fixed within-clause symbol order is used: pair initialization
finishes all coordinate comparisons before callbacks, source rows run in
clause-index order, and both twoend pivot orientations are covered. The
proof is therefore an all-order argument, not an inference from a finite
aligned-order quotient or the controls below.

The exact extension obligations and their discharge are:

| Obligation | Discharge |
| --- | --- |
| Exclude narrowing units for unequal/wider supports | Fixed-symbol matching in Section 1; an actual unit would contradict its full-domain projection. |
| Account for a relay whose donor is initialized last despite unequal widths | Frozen original support inclusion plus the initial width inequalities force equality, Section 3. |
| Do not confuse a pending reverse initializer with an uninitialized pair | Pending source t-repair either retires an operand or creates a permanent off-t obstruction; the `A,B,T` order is excluded separately, Section 3. |
| Replace the two frozen off-t literals by arbitrarily many | Complete t deletion leaves m−1 ≥ 2; k=1 gives a no-op covering bound and k≥2 is skipped before SSE2, Section 3. |
| Handle many conservative outside TRUEs in enabled SSE2 | Last raw-role transition followed by the last actual debt repairs, Section 4. |
| Do not lose an ordinary count-one visit when a new pivot exception appears | A removed r can be any member of Q; its rule forces persistent B_s ⊆ A_s and blocks the target transition, Section 5. |
| A pivot TRUE can become accurate without ever being cleared | The latest pending nonempty source update still owns its target row and later range-notification tail, Section 5. |
| Missing target pivot or originally missing donor coordinates | Empty-set semantics; a final exceptional source pivot is present, and the notification does not require it in the target, Sections 2, 4–5. |
| Compose local rules without claiming complete entailment | Existing static SSE2/HLA reductions at the quiescent boundary, this section. |

These are manual source-level arguments. The controls below challenge their
algebraic and execution premises but are not a mechanized scheduler proof.

## 7. Reproducible controls and their exact scope

`proof_checks.py` is separate from production execution. It checks all
78,310 ordered triples of supports of size at least three on universes of
three through six symbols. Each original triple has a matching. It checks
543,821 fixed-symbol residual matchings, including a fresh symbol absent
from all clauses; all 105 failures are exactly the common-triple exception.
All 64 sorted relay-support cases have equal supports.

An additional 373,248 direct scalar projection checks exhaust the two
off-symbol range families over a three-value domain and all eight membership
patterns of a fixed `q` value. All 36 excluded fixed-value cases have empty
`q` membership and both off-symbol families pairwise disjoint, exactly as
the projection characterization states.

The same script asks Z3 for countermodels to 454 local set implications,
using three, four, and five coordinates and arbitrary eight-bit ranges,
including empty ranges. All are UNSAT. They check the frozen single-absence
donor, constant donor intersections, persistent pivot dominance, the
outside-coordinate removal premise, and the target union-bound premise.
Each local coordinate involves only three pre-state sets and deterministic
set operations, so an ordinary finite countermodel has at most eight
membership atoms per coordinate. This is an exact local set quotient; it
does not establish arbitrary-width callback scheduling.

`controls.py` reuses the previously calibrated raw-set rule checker and
unchanged-production bridge in `../three_full_review/`. Its bank has no
random cases. It takes two saved enabled scheduling-boundary cases and two
explicit first-order relay cases, appends proper Boolean literals on one
or two new symbols according to selected incidence masks, and applies all
six clause orders with six explicit within-clause order arrangements.
There are 60 variants and 2,160 arrangements on each R version:

| Arrangements per runtime | Number |
| --- | ---: |
| Original common triple | 144 |
| Common support of width four or five | 288 |
| Different original supports, all widths at least three | 1,728 |

Both R 3.6.3 and R 4.6.1 pass 4,324 plain calls, 60 additional observed
calls whose outputs are `identical()` to plain calls, 507,744 positive
truth-table valuation rows, and independent final direct/SSE1/SSE2/HLA
audits for every positive arrangement. No positive case has a productive
second call or a semantic difference. The known three-binary-clause unit
equality gap and four-clause SSE1 gap are rejected by the same final checker
and have productive second calls; those two controls are outside this
theorem's scope.

Per runtime the observations check constant donor intersections 26,205
times, private target-range constancy 7,899 times, matrix count consistency
9,034 times, and absence of units outside the common triple 1,672 times.
They observe 114 enabled conservative TRUEs and eight count-one entries
pending in enabled deletion batches. Unequal-support variants themselves
contribute 60 conservative TRUEs and two pending count-one entries; common
supports of width four/five contribute 36 and four respectively. Thus the
wider controls exercise the deferred boundaries rather than only static
cases. These are observations, not distinct abstract states.

The root's separate 20,000-case two-runtime exploration and its SAT/MDD
checks are recorded in `../root/three_wide_clauses_r36.json` and
`../root/three_wide_clauses_r46.json`; they are not included in these counts
or used as the theorem's proof.

Reproduce from the repository root:

```sh
attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/wide_three_saturation/proof_checks.py
python3 attic/cnf_verify3/wide_three_saturation/controls.py
python3 attic/cnf_verify3/wide_three_saturation/controls.py --current-r
```

Machine-readable output is in `proof_checks.json`, `controls_r36.json`, and
`controls_r46.json`; command completion output is saved in the runtime logs.
No production source, package tests, other stream, or git commit was changed.
