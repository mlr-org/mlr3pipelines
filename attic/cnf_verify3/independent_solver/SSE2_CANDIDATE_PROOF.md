# Proposed saturation argument for the diagnostic second-order rescans

This is a source-level argument for the `sse2_rescan` / `combined_rescan`
variants in `r_bridge.R`. It does not describe the unchanged production
scheduler, which has counterexamples. Assume finite constructor-normalized
set inputs, normal completion, the reviewed lifecycle/cache invariants, and
the proposed first-order saturation theorem in `SSE1_CANDIDATE_PROOF.md`.
For the unconditional final-saturation claim, also assume the strict-length
unit guard included in `combined_rescan`, whose final-properness proof is in
`CANDIDATE_REPAIRS.md`. Without that guard, the claim below is conditional on
there being no remaining direct unit-subsumption opportunity.

The aim is to exclude a useful independent second-order restriction in the
returned formula. Local soundness of every extra callback is separate and
already follows from the existing inference handler's premises.

The [independent scheduling review](../scheduler_review/REVIEW.md), completed
2026-09-06, validates the exact source-copy variants under these prerequisites.
It strengthens the last-change step to stabilize every final TRUE exception
among the three survivors and audits callback/manual-queue coverage directly.

## 1. Reduce a final opportunity to an enumerated donor pattern

Take surviving distinct donors A,B and target T, an intersection symbol s,
and a target-restriction symbol t. Suppose the independent SSE2 premises hold
and T_t contains a value outside A_t union B_t.

The static set classification in `SSE2_COVERAGE_PROOF.md` exhausts the possible
donor exception patterns relative to T. Cases not containing a twoend donor
reduce to direct subsumption, a stronger first-order restriction, or a
domain-refutation/HLA deletion. The first two are excluded by the earlier
phases, the unit guard and the proposed first-order theorem. The HLA theorem
excludes the third once direct unit subsumption is excluded. More strongly,
the oneend/oneend construction with nonunit donors gives a refutation using
only those two donors, which the HLA loop detects even if an unrelated unit
equality remains. The static unit-case argument reduces all cases involving
actual unit operands to these earlier-rule cases as well.

It therefore suffices to consider a twoend donor, say B, with exceptional
symbols exactly {s,t}, and another donor A having either one exception s or
the same two exceptions. Both donors are in the registry for s, and T contains
t. The useful excluded value proves the two inverse pruning comparisons at t
are TRUE. The actual intersection premise makes `try_sse_2nd_order()` succeed.

## 2. Inspect the last range change among the three final clauses

All actual ranges only shrink. Consider the chronologically last actual range
change of A, B or T after the second-order phase was enabled. If there is no
such change, consider the phase's initial manual queue instead. From the chosen
last change onward, all three clauses have their final ranges and remain active.

If the changed clause is a donor, its added outgoing rescan includes every
initialized live target for which it currently has count one or two. A oneend
call uses its actual exceptional symbol; a twoend call tries both orientations.
The changed symbol need not itself be exceptional. This is what covers a
change in an already-contained range. The same rescan is called after complete
literal removal when the changed clause remains nonunit.

If the changed clause is T, its added incoming rescan includes every current
twoend donor. This covers count increases caused by shrinking the target,
including a oneend-to-twoend transition that the production code merely stores.

If the three ranges already had their final values when the phase started,
the exact initial comparison matrix places the twoend pair B,T in the manual
queue. The queue visits every such live pair. A pair deferred by its disabled
flag is still covered by its pending manual-queue visit; an active queued pair
cannot remain disabled after the whole queue returns.

## 3. Account for conservative TRUE bits during that last change

An added rescan might temporarily see too many exceptions because an older
source-update frame has not yet cleared all newly contained ranges. This is
the only way the final eligible donor patterns can be hidden after their last
actual range change. FALSE comparisons remain contextually sound throughout.

The raw-exactness proof assigns every inaccurate TRUE bit to a pending source
update. Such bits are eventually cleared on normal unwinding. When the last
relevant extra TRUE bit in a donor-to-T row is cleared, its count decreases to
the final one or two. The existing ordinary callback then invokes the oneend
or twoend handler. If its partner still has too many exceptions, the partner's
later last decrease supplies another such visit. Thus the last of these
relevant count decreases sees both final donor roles.

The needed inverse comparisons at t cannot be falsely FALSE after the last
three-clause range change. A final excluded value v belongs to T_t and to the
final effective unit domain, but belongs to neither donor's t range. Since
current unit domains contain the final unit domains, a FALSE bit would violate
its contextual containment certificate at v. Thus the inverse pruning guard
cannot hide this final useful restriction while waiting for unrelated work.

If a needed visit is deferred by the manual-queue flag, its later queue entry
again sees the final ranges and roles. These alternatives exhaust whether
the added scan sees final counts immediately, sees a temporarily excessive
count, or waits for the initial queue.

## 4. The triggered handler cannot miss the final triple

The static coverage proof checks the handler's symbol registry, exceptional
symbol matching, count filter, inverse comparisons, and both pivot orientations.
At the visit identified above, all those conditions hold for the final triple.
Every handler loop rechecks active operands and current counts. A nested
operation cannot invalidate a final operand or change its ranges: that would
contradict the choice of the last three-clause range change and the fact that
all three operands survive.

The handler therefore reaches the independent restriction and properly changes
T_t or removes T. Either contradicts the supposition that these were the final
surviving ranges. No such final opportunity can remain.

## Status and limits

This proof proposal needs independent review, especially the transition from
conservative comparison debt to a guaranteed final eligible handler visit.
It relies on the specific broad rescans in the diagnostic bridge, not on an
unspecified optimized future repair. Removing the incoming scan, restricting
outgoing visits to the changed exceptional symbol, or omitting the complete
literal-deletion call site invalidates the argument and has saved examples.

Together with first-order saturation, HLA deletion saturation, and the strict-
length unit-skip properness proof, this would establish saturation for the
implemented rule family in `combined_rescan`. It would not prove canonical
output across clause orders, complete entailment, or resource-bounded success.
The existing 5,000-formula paired diagnostic found no residual rule and stable
second-pass clause/range sets; those observations are separate evidence.
