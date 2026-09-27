# Why the extra source-range callback should saturate first-order SSE

This document concerns only the diagnostic `sse1_changed_range` source-copy
variant in `r_bridge.R`, and variants that include it. Production is unchanged.
It extends the local soundness argument in `CANDIDATE_REPAIRS.md` into a proposed
normal-return saturation proof. The independent lifecycle/cache lemmas and
their canonical finite-set assumptions remain prerequisites.

The [independent scheduling review](../scheduler_review/REVIEW.md), completed
2026-09-06, validates this conclusion and supplies the explicit count-zero
pending-visit invariant needed for the zero-to-one case. Its stronger
obligation formulation is the reviewed version of this argument.

## Desired quiescent property

For every two distinct surviving nonunit clauses A,T, if A is contained in T
outside symbol s, then T_s is contained in A_s. Otherwise first-order SSE could
properly restrict T_s. Direct subsumption is handled separately: if A is
contained at every symbol, T is redundant and should have been removed.

Units need no additional SSE1 rule at quiescence. A unit donor can restrict
only its own symbol, where final physical unit containment already holds.
A nonunit donor with one exceptional symbol against a unit target must have
that exception at an absent target symbol, so its restriction is a no-op.

## Separate range obligations from comparison obligations

The comparison-count cache can be exact while a useful restriction remains
unperformed. In particular, a count-one pair has two relevant pieces of state:
its exceptional symbol s, and its donor's actual range A_s. The original code
observes a change in the first piece but can miss a change in the second.

Use current ambient units when interpreting transient ranges. A FALSE bit is
then a valid containment certificate. After normal unwinding, physical unit
containment and exact raw matrices identify this contextual property with the
desired raw property above.

A pending SSE1 obligation is covered by one of these existing or added visits:

1. Initialization of an ordered pair, which invokes the ordinary handler for
   count zero or one.
2. A source-comparison decrease to count zero or one, whose range-update or
   symbol-deletion frame invokes the ordinary handler.
3. The added source-range callback, which revisits initialized active count-one
   targets whose exceptional symbol is the just-restricted source symbol.

Every pending visit is finite. A source update can abandon its remaining list
only after its source becomes inactive; targets that become inactive likewise
have no surviving-pair obligation. The candidate rechecks source, target,
count and exceptional bit immediately before each added handler call.

## Exhausting the ways a previously discharged pair can need work

Suppose a pair has no outstanding visit, and consider one state change.

**Source range shrinks at its exceptional symbol.** The comparison may remain
TRUE and its count may remain one, while T_s is no longer contained in A_s.
This is precisely the new visit in item 3. If the comparison instead becomes
FALSE, item 2 covers count zero. Removing the whole exceptional literal also
clears its TRUE bit and uses item 2; it does not require the new nonempty-range
call site.

**Source range shrinks elsewhere.** It cannot invalidate a previously applied
restriction at the exceptional symbol. It can turn a larger comparison count
into zero or one; the TRUE-to-FALSE transition then uses item 2. If that bit
was already FALSE, neither the pivot range nor the existing containment
premises become weaker, so no new SSE1 work is enabled.

**Target range shrinks.** It cannot turn a failed source containment into a
successful one. For an already count-one pair at s, shrinking T_s preserves
the discharged condition T_s contained in A_s. Shrinking another target range
can only increase the number of exceptional symbols and disable the rule.

There is one transient qualification: a target shrink can change a count-zero
pair into count one before its pending deletion callback runs. That pair
already has the visit from item 1 or 2. The callback reads the current count,
so it performs the current SSE1 rule instead of relying on the old zero.
If it temporarily becomes count two or larger, any later decrease back to one
creates a fresh visit. A zero-count pair cannot exist without such a pending
visit while both operands remain active.

**Ambient unit domains shrink.** Intersecting both sides with a smaller common
domain preserves every already discharged containment. The physical clause
updates may be deferred, but the comparison certificate is interpreted under
the new units. Their eventual raw changes either preserve this contextual
property or create one of the already listed source-update visits.

**A clause becomes a unit or is removed.** It ceases to be an active nonunit
operand. No clause later becomes active again, and no new nonunit is created.
There is therefore no new surviving-pair obligation in this case.

**Second-order phase enablement or an HLA deletion.** Enablement itself changes
no ranges. Any subsequent actual second-order restriction is one of the source
or target changes above. HLA only deletes whole actual clauses, so it cannot
create an SSE1 restriction between two surviving clauses.

## Why nested callbacks do not invalidate the argument

The added scan runs after the ordinary comparison repairs for its source
change. Conservative stale TRUE bits may temporarily hide a count-one pair;
when their owning source-update frame clears them, item 2 supplies the visit.
If a recursive operation changes the source range again after an older scan
took its target snapshot, that newer source operation owns its own item-3
scan. A new reverse TRUE bit can disable a rule, but cannot create an uncovered
useful count-one rule except the already covered zero-to-one case.

The handler intersects T_s with the donor range read for that invocation.
If recursion subsequently shrinks A_s again, the nested source update creates
its own obligation; the old invocation need not predict that later range.
On normal unwinding no update frame or initialization visit remains. Hence
there can be no uncovered useful first-order restriction in a surviving pair.

## Current status and limits

This is a proposed source-level scheduler proof, awaiting an independent
challenge of the obligation bookkeeping. It is not an executable model of R.
The 5,000-case candidate probe found no residual SSE1 operation and both
semantic oracles agreed, while unrelated SSE2 and unit-equality examples
remained. Those observations support the diagnosis but do not replace the
event classification above.

The proof does not establish second-order saturation. A oneend donor's
already-contained range can change the second-order union bound, and a target
change can create a new twoend role. Both require the separate rescan variant.
