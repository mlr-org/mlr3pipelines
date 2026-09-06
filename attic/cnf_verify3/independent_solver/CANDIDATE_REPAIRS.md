# Diagnostic repairs and what can be proved about them

These variants exist only in the source-copy R bridge. No production source
has been changed. Their purpose is to challenge the diagnosed mechanisms and
make a future repair reviewable, rather than to declare the investigation done.

## A. Refuse a unit skip unless the current range is strictly shorter

The `unit_skip_length` variant adds the following conjunct to the existing
matrix-based unit-skip predicate:

```r
length(entries[[s_clause_idx]][[nu]]) < length(unit_domains[[nu]])
```

This only prevents skips; the calls it enables are the same ordinary unit
restrictions used when no matrix optimization is available. Therefore the
unit-rule soundness proof still applies. The probe in `unit_skip_candidate.py`
repairs all 96 clause/symbol order variants of the minimized selector family.

There is also a general properness argument. For a symbol s, consider its
chronologically last unit registration. After that registration introduces
its effective domain U, no later registration can further tighten U by the
definition of "last". Any clause remaining live and containing s was either
in this registration's symbol-registry snapshot or was a future preprocessing
clause that will receive U before entering the registry.

For a snapshot clause, an explicit restriction either eliminates it because
its intersection contains all U, or leaves a range strictly shorter than U.
If the new guard skips the clause, its current range is already strictly
shorter than U. Subsequent ranges only shrink. A future preprocessing clause
likewise receives an explicit restriction and is either eliminated or left
strictly shorter. The independent birth-order proof still supplies actual
containment in U after all ancestor work returns. Thus every surviving range
is a **proper** subset of U at the end, and no clause equal to/containing the
unit can remain through the stale strictness optimization.

The argument is about finite canonical sets and normal completion. It uses
the inspected lifecycle fact that clauses never appear as new non-units or
acquire a symbol after registry construction. It also relies on the length
guard reading the current effective `unit_domains[[nu]]` inside each iteration,
not an earlier registering-clause snapshot.

## B. Revisit oneend targets after the donor range actually changes

The `sse1_changed_range` variant appends a small loop to
`apply_domain_restriction()`, after its existing comparison updates and before
its optional second-order range handler. It selects initialized live targets
whose count is one and whose exceptional symbol is the just-changed symbol.
For each, it rechecks liveness, count, and symbol and calls
`on_updated_subset_relations(..., FALSE)`. It stops if the source becomes
inactive or a contradiction is signalled.

This covers precisely the confirmed missing event: the donor's exceptional
range became smaller while its non-subset bit stayed TRUE. It runs even during
the initial first-order phase; otherwise the four-clause/two-symbol witness
would remain unfixed. A no-op restriction returns before firing further
callbacks, so this does not create recursion from unchanged ranges.

Each additional inference is sound by the existing contextual FALSE-bit
invariant: a count-one source is contained in its target outside the selected
symbol, after intersecting with current units. Active-operand checks protect
the same caller contract as existing callbacks. The termination measure is
unchanged because continued recursive state changes still remove actual
values/symbols/clauses. This is a local correctness argument for adding the
calls; a complete global SSE1 saturation proof for the candidate remains a
separate obligation.

`sse1_candidate_probe.py` checks every saved mechanism fixture and paired
baseline/candidate executions on dense formulas, long HLA chains, second-order
bundles, and finite-state cycles. SAT and MDD check every candidate output.
It records any residual SSE1 opportunity separately from residual SSE2 and
unit subsumption, rather than mistaking truth preservation for completion.

The 5,000-case run passed both semantic oracles and left no SSE1 opportunities.
The independent SSE2 and unit-equality mechanism fixtures remained, as expected
from the scope of this change.

## C. Rescan affected second-order roles after every actual range change

The `sse2_rescan` diagnostic builds on B and scans all currently eligible
oneend/twoend roles of a changed clause, using the actual exceptional pivot
for a oneend clause even if another, already-subset range changed. It also
checks incoming twoend donors whose target changed. The scan is invoked after
complete symbol deletion as well as nonempty range restriction, and respects
the existing not-yet-triggered manual-queue flags.

This is intentionally a transparent diagnostic rescan, not a proposed final
optimization. It checks source/target liveness after every potentially
recursive operation and continues using the existing logically justified
inference handlers. The `combined_rescan` variant adds A's unit guard too.
All seven saved fixtures then have no local rule left pending.

`combined_rescan_probe.py` completed 5,000 further dense/structured cases with
SAT and MDD equivalence, no residual direct/subsumption/SSE operations, no
domain-refutation HLA redundancy, and stable second-pass clause/range sets.
Fourteen outputs changed relative to baseline. These observations support the
diagnosis; a universal saturation proof for the altered scheduler has not yet
been completed. No production source has been modified by these experiments.
