# Independent review of the diagnostic SSE scheduling arguments

Reviewed 2026-09-06. This review follows the actual source-copy construction in
`../independent_solver/r_bridge.R`, not only the two proposed proofs. It changes
neither production nor the bridge. The production source has SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`; the bridge has
SHA-256 `ddce0652915f7823954500e93482e548e868993cf485df567f67d144140acc1c`.

**Verdict:** the specific extra notifications suffice for the claimed
normal-return SSE saturation, under the stated canonical finite-set, lifecycle,
cache, and HLA prerequisites. The arguments can be completed without another
notification site. The important additions to the prose are an explicit
count-zero pending-visit invariant and a stronger stabilization lemma for all
comparison bits of a final three-clause witness. Neither theorem follows from
cache exactness alone. This is a source-level mathematical review, not a
mechanized semantics of R or a resource-bounded termination theorem.

The precise conclusions are:

* `sse1_changed_range`, and variants containing it, leave no useful SSE1
  restriction between surviving clauses. Direct subsumption between nonunits
  is also exhausted. The unchanged unit-skip condition can still leave direct
  **unit** subsumption, which is a separate rule.
* `sse2_rescan`, and variants containing it, leave no useful SSE2 pattern with
  a twoend nonunit donor after the first-order rules are exhausted.
* Combining this with the static coverage/HLA reductions gives full SSE2
  saturation when no direct unit-subsumption opportunity remains. The strict
  unit-length guard in `combined_rescan` supplies that missing condition.

## 1. Scope and inherited invariants

Use the explicit contract in `../review_semantics/REVIEW.md`: finite ordinary
sets of unique character values, unique nonmissing symbol names in each clause,
fixed nonempty domains, ordinary list/copy and membership semantics, no count
overflow, and normal completion. The full SSE2/HLA composition additionally
uses constructor-normalized inputs whose literal ranges are nonempty proper
subsets of their original domains, as `HLA_SATURATION_PROOF.md` explicitly
requires. The weaker truth-preservation contract must not silently replace
that prerequisite. Absent literals denote the empty set. Scalar logical
results have no surviving-clause rule obligation.

Before HLA, let `U_s(q)` be the effective unit domain at execution time `q`,
or the original domain when there is no registered unit. Let `U_s*` denote its
value at the completed pairwise/second-order boundary, before HLA can remove
unit clauses. The necessary previously reviewed invariants are:

1. No nonunit is created or reactivated. Actual ranges and symbol sets only
   shrink; eliminated clauses and units leave all live nonunit obligations.
2. For live initialized pairs, a consumed FALSE bit certifies
   `A_s intersect U_s(q) subset T_s intersect U_s(q)`.
3. Counts equal the number of TRUE bits. An uninitialized or diagonal count
   is `NA` and is never submitted as an inference pair.
4. Every raw-inaccurate TRUE bit has an outstanding source-range update that
   will repair it if the source and target survive. Symbol deletion clears
   its entire source column before recursion. This is the actual pending-frame
   lemma from `../proof_state/QUIESCENT_MATRIX_PROOF.md`.
5. On complete unwinding, every live nonunit range is physically inside `U_s*`,
   and its initialized comparison matrices are raw-exact. There are no actual
   range changes during HLA; virtual HLA matrix writes are target-local.

The new callers preserve the lifecycle prerequisites. The added SSE1 loop
checks both operands, count one, and the changed-symbol bit immediately before
every call. The outgoing SSE2 loop checks the changed source before every
iteration and the target immediately before dispatch. The incoming loop checks
the changed target and the candidate source before dispatch. A handler's NULL
result can name a dead source rather than a dead target; the rescan does not
misinterpret it. Its next iteration and its after-loop guard test the changed
clause directly. No self-pair can enter these lists because its count is `NA`.

The exact insertion sites also matter. SSE1 is added after the nonempty strict
range update's ordinary comparison repairs. SSE2 replaces the final range
handler at that site and also replaces the final FALSE return of the
remaining-nonunit symbol-deletion helper. Early returns before either insertion
mean no range change, an uninitialized source, an inactive source, or global
contradiction. None loses an initialized surviving-pair obligation.

## 2. Count-zero work is not a stored deletion decision

For a live initialized ordered pair `(A,T)`, use `k` for its current count.
At callback boundaries the following invariant holds:

> If `k = 0`, an ordinary visit for `(A,T)` is outstanding. That visit has not
> already consumed the zero state; its eventual handler reads the live count.

There are only three ways the ordinary pair matrix can acquire a zero count:

* **Pair initialization.** Both ordered counts are installed before the first
  ordered handler runs (production lines 608-610). The inner-to-outer direction
  is considered first, then the outer-to-inner direction reads its count again
  at line 618. That second visit is genuinely pending while the first handler
  recurses. If either operand becomes inactive, the pair has no survivor
  obligation; otherwise the second direction is reached.
* **Nonempty source restriction.** A TRUE-to-FALSE bit change installs the new
  count at line 222 and calls `on_updated_subset_relations(..., FALSE)` at
  line 224. There is no recursive callback between installing count zero and
  entering that handler.
* **Source literal deletion.** All affected counts are decremented together at
  line 266. Their ordinary callbacks are then processed by the finite
  `rows_changed` loop at lines 277-288. Earlier entries can recurse while a
  later zero pair is still waiting. The later pair remains on this list; its
  current count is tested at line 284, and the handler reads it again at 298.

Reverse target updates only increase counts. Their writes cannot create zero.
No other pre-HLA operation creates a zero count. A handler that actually enters
with zero eliminates the target before any recursive call (lines 309-311).
Consequently a live zero state cannot remain after its owning visit is consumed.

This proves the important zero-to-one exception explicitly. Suppose shrinking
T makes a pending zero pair acquire one exception at `s`. The existing visit
does **not** carry the command "delete T": it carries the ordered pair. Its
later `rowsum` read is one, so with `second_order_only = FALSE` it attempts the
current SSE1 restriction at the current unique TRUE symbol. If the count rises
above the current dispatch threshold before that visit is reached, skipping
is harmless to this one-exception obligation. A later decrease to one or zero
supplies a fresh ordinary visit. No count increase is mistaken for evidence
that work was already discharged.

The manual second-order queue cannot strand a zero pair: its zero branch also
deletes before checking `second_order_only`. More fundamentally, the ordinary
owner above already exists independently of that queue.

## 3. Complete SSE1 obligation argument

For a live initialized count-one pair let `s` be its unique TRUE symbol. Call
its restriction discharged at time `q` if

```
T_s intersect U_s(q) subset A_s intersect U_s(q).
```

The useful strengthened invariant is: every count-one pair is discharged, or
owns an outstanding ordinary visit, or owns a pending added scan for a strict
source change at its current exceptional symbol. If a pending scan's relevant
role is lost before its snapshot/visit, a future newly eligible role must
acquire its own owner through the transitions below. Together with the
count-zero invariant, this is preserved by every state transition:

* Initialization at count zero/one creates an ordinary visit.
* A source-comparison decrease to zero/one creates an ordinary visit, including
  decreases from a conservatively excessive count and batched symbol deletion.
* A strict nonempty source-range change at the existing exception may destroy
  the discharged inclusion even if no comparison bit changes. Its added scan
  owns exactly this event. Deleting that entire source literal clears the
  exception, so the ordinary count-decrease visit suffices.
* A source change at another symbol does not change the discharged inclusion
  at `s`. Any newly eligible role caused by that change has a count decrease
  and thus an ordinary owner.
* A target change at `s` preserves the discharged inclusion. A target change
  elsewhere can add an exception and disable the role. The only target change
  that creates count one is a zero-to-one transition, whose ordinary owner is
  preserved by Section 2.
* A smaller ambient unit domain intersects both sides of a discharged
  inclusion with a common set, so preserves it. Subsequent physical updates
  fall into the already listed source/target cases. No raw-containment claim
  is made at a transient skip.
* Clause removal or conversion to a unit discharges its live nonunit
  obligations permanently.

A change of the unique pivot without a new owner is impossible: one-symbol
count updates must pass through zero or two. Passing through zero retains its
ordinary owner; returning from two to one creates a fresh owner. When a literal
deletion changes many *pairs* at once, it still clears only one bit of any
particular ordered pair.

The added scan's snapshot cannot lose the obligation. At snapshot time, every
initialized active count-one pair at its changed symbol is included. If a pair
becomes eligible afterward, it did so by initialization, a count decrease, or
zero-to-one with an existing owner. If the donor range changes again after a
snapshot or after a visit, the later source operation owns its own added scan.
An older scan never has to anticipate a later mutation. The only early
non-contradictory abandonment of the owner's list occurs when its source is
inactive; target inactivity likewise removes the corresponding obligation.

An ordinary count-one handler reads the current pivot and donor range and
calls `apply_domain_restriction` with `second_order_only = FALSE`. Before that
helper can recurse, it has either eliminated the target, removed the literal,
written the intersection, or established that the intersection is a no-op.
In every surviving case this establishes the discharged inclusion at that
instant. The helper's R promises do not introduce a concurrent state change:
the donor range is forced for the intersection before recursive work begins.
A subsequent recursive donor change carries its own obligation as above.

At complete pairwise unwinding there are no outstanding initialization or
source-update frames. Thus count zero is absent for surviving pairs, and every
count-one pair is discharged. Physical unit containment and raw-exact matrices
turn this into precisely the desired raw direct-subsumption/SSE1 property.
HLA does not shrink actual ranges or add clauses, so later deletions cannot
create a new operation between surviving pairs.

Actual unit donors add no missing SSE1 restriction: their only exceptional
symbol can be their unit symbol, where physical containment has already made
the restriction a no-op. For a nonunit donor and a unit target, a unique
exception has to be at a symbol absent from the target; that restriction is
also a no-op. Direct unit subsumption is the separate strictness condition.

## 4. A stabilization lemma for the final SSE2 witness

Consider three distinct nonunit clauses A,B,T that survive the entire call.
Let `tau` be their chronologically last actual range/symbol change after
second-order enablement, if one exists. At relevant callback boundaries after
this change's nonrecursive reverse repairs, all three raw clauses have their
final values and remain active.

For any ordered pair C,D among these three and any symbol `r`, suppose the
final raw comparison is TRUE. Choose a final witness

```
v in C_r \ D_r.
```

Physical containment at the eventual boundary gives `v in U_r*`. Monotonicity
of unit domains gives `U_r* subset U_r(q)` at every earlier pairwise time `q`.
Since the clauses already have their final ranges after `tau`, this same value
refutes contextual containment at every such callback boundary. The FALSE
certificate invariant therefore forbids the bit from being FALSE there.

This applies to **all** final TRUE bits, not only the inverse pruning bits at
the SSE2 restriction symbol. Consequently each relevant row after `tau` is

```
its exact final TRUE set + possibly some conservative extra TRUE bits.
```

The extra TRUE bits are raw-inaccurate and are repaired by the pending-frame
lemma. Once cleared, they cannot be set TRUE again: the only reverse setters
test the current raw ranges, and those ranges are fixed. Final TRUE bits cannot
be cleared. Thus the relevant exception sets approach their final sets solely
by removing surplus bits, and their roles are stable after the last such
repair. This is stronger than the proposed proof's informal phrase "too many
exceptions" and supplies its needed justification.

Use `U*` from before HLA, even if some units are later removed by unit HLA.
Those later removals do not change the already fixed raw clauses or invalidate
the earlier physical-containment fact.

## 5. Finding a notification after the last relevant change/debt

By the static coverage reduction, the genuinely remaining pattern is

```
E_B = {s,t}, and E_A is {s} or {s,t},
A_s intersect B_s subset T_s,
T_t \ (A_t union B_t) is nonempty.
```

First suppose `tau` exists.

* If the changed clause is A or B, its outgoing rescan visits its count-one
  and count-two roles, independent of which literal changed. At count one it
  obtains the actual current unique pivot; at count two it passes `NULL` for
  the orientation restriction. The whole-literal-deletion insertion gives the
  same coverage when the last change is symbol removal.
* If the changed clause is T, its incoming rescan visits every count-two donor.
  This covers target-induced count increases, including a newly twoend B.

If both donor roles are already final when this scan selects/visits the pair,
it has a valid ordinary route to the desired handler. If a relevant role is
hidden or distorted by surplus TRUE bits, select the last removal of a surplus
bit among the two donor-to-T rows that occurs after `tau`. Section 4 ensures
that after it, both donor roles are final and stay final. The row whose last
surplus bit was removed has final count one or two and therefore receives an
ordinary callback:

* A nonempty range update calls immediately after its decrement, with the
  current threshold two because second order is enabled.
* A symbol-deletion batch has already decremented every row before invoking
  its queued callbacks. The corresponding pending callback is reached after
  the relevant last decrement, and reads the now-stable final count.

If the selected scan/ordinary callback snapshots its partners before the other
role has settled, that other row's later last surplus-bit removal supplies a
new callback. Equivalently one can select the chronologically last relevant
decrement from the outset. If an older frame resumes after the bits were
already cleared by recursion and skips its redundant decrement, the nested
frame that performed the actual last decrement already owns the visit.

If there is **no** actual change of A,B,T after enablement, the matrices are
raw-exact at the initial phase boundary. The twoend B-to-T pair is in the initial
manual queue. Its guarded queue entry is reached while both operands remain
active, and both roles already have their final values. No last-debt argument
is needed in this branch because the initial first-order work is fully unwound
before queue construction.

## 6. Manual-queue flags cannot swallow that notification

At enablement, exactly the active count-two pairs are assigned FALSE and put
in `sse_to_trigger`. For a pair of final surviving clauses:

* A FALSE flag means its initial manual entry has not started yet. The queue
  sets the flag TRUE **before** calling its handler (line 645), and no later
  statement resets it to FALSE.
* An entry skipped by the lifecycle guard has an inactive operand forever, so
  cannot belong to our final triple.
* Partner enumeration inside the SSE2 handlers does not consult the partner's
  manual flag. Only the principal pair's eligibility can cause a deferral.

Therefore an added rescan deferred at a FALSE flag has a later manual visit of
that same pair. If its stable final count is one, the queued
`on_updated_subset_relations(..., TRUE)` takes the oneend branch, with its actual
final pivot and its now-TRUE flag. If the final count is two, it takes the
twoend branch. Either visit reaches the roles after they have stabilized.

There is a useful source detail the proposed proof leaves implicit:
`on_updated_subset_relations()` dispatches count two at lines 300-304 **before**
consulting any manual flag. Thus a count-decrease callback to two is not
deferred at all. Only its oneend branch and the explicit extra rescans can
defer. This is extra coverage, not a missing case.

If a pending manual entry runs too early, before the partner's last surplus
comparison disappears, that later comparison repair still owns its ordinary
callback. The selected *last* relevant repair occurs when both roles are final;
any deferral from that repair is to a strictly later unvisited queue entry.
This excludes a circular "each visit happened before the other was ready"
argument once the clauses have their final ranges.

## 7. The identified handler reaches the triple

After the selected stable-role visit (or its pending manual successor):

* Both donors have symbol `s`, hence are still in its symbol registry.
* T has `t`, because there is a value to remove.
* A's oneend/twoend count and exceptional columns meet the exact partner
  filter in the handler. A oneend call supplies pivot `s`; a twoend call tries
  both orientations, including `(intersection=s, restriction=t)`.
* Every final value in `T_t \ (A_t union B_t)` is a witness forcing both
  inverse comparison bits at `t` to be TRUE by Section 4.
* The direct intersection test succeeds by the final SSE2 premise.

Handler partner lists and exceptional-symbol snapshots cannot become stale
with respect to this triple: losing an operand contradicts its final survival,
and changing one of its ranges contradicts the choice of `tau`. Other clauses
may change recursively, but cannot remove this surviving donor from the symbol
registry or invalidate these fixed roles. A nested callback might itself apply
the desired change to T; that already gives the same contradiction.

The helper therefore calls `apply_domain_restriction` with
`A_t union B_t`. A nonempty excluded value makes that operation proper unless
the target is deleted outright. Either outcome contradicts the stipulated
unchanged final surviving T. Hence the twoend-pattern opportunity cannot
remain.

The static non-twoend and actual-unit cases then reduce to direct subsumption,
SSE1, or HLA deletion as proved in `SSE2_COVERAGE_PROOF.md`. Invoking the reviewed
HLA saturation theorem and the strict-unit properness condition completes the
full `combined_rescan` statement. This review does not extend the theorem to
arbitrary logical entailment, confluence across clause orders, or an arbitrary
future optimized rescan implementation.

## 8. Evidence and limits of this review

The result above is based on exhaustive source-transition classification, not
on extrapolating a successful finite fuzz run. `probe_zero_to_one.R` adds
read-only hooks to the exact `combined_rescan` source text and searches for the
transient event discussed in Section 2; it also checks that its hooks leave the
diagnostic output exactly unchanged. Its findings, if any, are supporting
examples and are not needed for the proof. In 5,000 fresh generated dense cases
(seed 649813), no useful zero-to-one transition was observed; the output-equality
checks all passed. This is explicitly not an unreachability claim.

`callback_controls.R` extracts the unchanged `on_updated_subset_relations`
definition and manual queue statements from the production function body. All
four controls passed under R 3.6.3:

1. A manually pending zero-to-one state calls the current oneend restriction
   instead of the zero-count deletion. Its before/after truth functions also
   agree on all 12 valuations of the small synthetic clause pair.
2. A queued pair whose current role is oneend first defers at its FALSE flag,
   then the exact queue enables it before its current oneend handler executes.
3. The ordinary count-two branch dispatches even with a FALSE manual flag.
4. The manual `second_order_only` flag does not suppress zero-count deletion.

These controls deliberately construct callback states and stub downstream
actions. They verify exact dispatch behavior, **not reachability** of those
states from a valid full simplification call. The completed source argument
above supplies the actual ownership/coverage reasoning.

Reproduce the recorded local checks from the repository root with:

```sh
Rscript attic/cnf_verify3/scheduler_review/callback_controls.R
Rscript attic/cnf_verify3/scheduler_review/probe_zero_to_one.R 5000
```

Logs are saved beside these files. Production and the diagnostic implementation
have not been modified, and no commit was made by this review.
