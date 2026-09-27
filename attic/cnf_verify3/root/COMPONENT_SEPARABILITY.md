# Independent symbol components do not change each other's simplification

Reviewed source simulation, 2026-09-06. The separate
[independent source review](../normalization_component_review/REVIEW.md) and
[observations](../normalization_component_review/RESULTS.md) are complete.
This addresses a different possible
cause of scheduling differences: adding unrelated constraints to a formula.
The component relation is on **input symbol names**, not on equal value labels.
Different symbols may freely share the same character domain values.

## Claim and contract

Partition the ordered proper input clauses into groups such that no symbol
appears in more than one group. Preserve each group's induced clause order
and every clause's internal symbol/range order. Use ordinary canonical finite
storage in one unchanged universe, with the usual indexing/arithmetic/resource
premises. Empty groups and scalar TRUE are neutral cases; scalar FALSE is a
separate terminal case.

Simplify the whole list once, or each group separately with its own induced
ordered input. Then:

1. The whole call returns FALSE if and only if at least one separate call
   returns FALSE. This is about what the actual heuristic recognizes, not
   merely whether the separate formulas are contradictory.
2. If no call returns FALSE, project the whole result to the clauses of any
   one group. Its ordered clauses and ordered literal vectors are exactly
   the separate result, with universe attributes interpreted as the same
   symbol/domain association. No surviving clause mixes groups.
3. Repeated calls have the same componentwise relation. If all components
   keep returning proper results, the number of productive whole calls is
   the maximum of their separate productive-call counts. Pure storage
   sorting retains the separate sort-only qualification.

This permits arbitrary global interleaving of the groups' input clauses.
It does not say the complete callback event streams have identical lengths:
cross-group pairs consume extra allocation, comparisons and harmless trials.
It does not predict equal runtime or memory, nor transfer a particular
machine's successful stack threshold to a larger combined input.

The same argument excludes influence from additional unused symbols in the
universe: only symbol domains requested by actual or virtual clauses are
looked up. The kernel never enumerates all universe bindings to choose work.

## 1. Stable ordering and local index embeddings

The initial `order(lengths(entries))` preserves the original relative order
of clauses with equal widths. Restricting that sorted order to one group
equals sorting its induced input alone. Assign each original clause a ghost
identity and map its local index to its larger global index. Available-list
and metadata index order is also preserved by this embedding.

Every registry at symbol s contains only the group owning s, in its original
induced order. Initial units and subsequent unit representatives therefore
merge and propagate within one group. The unit-domain guard reads one symbol's
range, never values at other symbols. The global number of unit bindings is
used only to split the final sorted list into unit/nonunit targets; it does
not alter the logical rules.

After preprocessing, pair initialization visits local pairs in the same order
as a separate run, with irrelevant other-group visits interspersed. For a
productive callback in group G during this stage, the current outer clause
must also be in G: Section 2 excludes productive cross-group calls. Thus
`meta_idx <= meta_idx_outer` has the same truth value on G's embedded indices.
In the later second-order phase, both global and separate outer bounds have
already reached their respective final indices, so that comparison is again
the same for G. This addresses an order-sensitive guard rather than merely
assuming that changed numerical indices are harmless.

## 2. Cross-group pair work cannot change a clause

Before HLA, a live nonunit donor D has at least two actual symbols. Against
a target from another group, every present donor range is nonempty and the
target has no such symbol. Its row therefore has one TRUE column per present
symbol, and FALSE only at donor columns already deleted. Its count is its
current width, at least two. Unit-based contextual comparisons cannot clear
an existing cross-group symbol: there is no target range there at all.

No cross-group direct subsumption or SSE1 call is enabled. A cross-group
count-two pair can reach a twoend handler, but both candidate restriction
symbols belong to D and are absent from the target. Lines 419/420 skip both
orientations before reading another donor or trying a restriction.

In a same-group second-order operation, both selected pivots come from that
group. The symbol registry can only supply other donors from the same group.
An absent range is still a symbol of this group's vocabulary, never a reason
to recruit an unrelated group. The ordinary oneend/twoend callbacks and all
productive actual changes therefore stay inside one group.

Changes inside G can repair its donors' comparison rows against other groups,
but such rows remain inert by the same width argument. Deletion/conversion
guards remove inactive clauses before they could act as a width-one nonunit.
Those extra repairs cannot change any row, range, queue eligibility or unit
binding used by a same-group pair.

## 3. The manual queue preserves local scheduling

The second-order flag is enabled only after the complete first-order sweep.
The preceding per-group states match the states after the corresponding
separate first-order sweeps. The queue comes from column-major
`which(not_subset_count == 2, arr.ind=TRUE)`. An increasing embedding of local
indices preserves this queue's relative order for same-group pairs.

Cross-group queued pairs return without a change as above. Same-group pairs
have the same counts, flags, exceptional symbol order, candidate registry
order and actual ranges as in the separate run. Recursive changes and their
continuations consequently match, including the existing omissions of later
useful notifications. A component that needs multiple productive calls keeps
that behavior; unrelated groups neither repair nor worsen it.

## 4. HLA remains in the target's group

At HLA entry the relevant live nonunit comparison rows are raw-exact. A
virtual target initially contains only its own group's symbols. A donor from
another group has at least two present literals and therefore at least two
exceptions against it. Such a donor cannot be selected by the count-one rule.
Every selected donor and its new virtual pivot instead belong to the target's
group. Inductively the virtual target can never acquire an outside symbol.

The registry refresh for a virtual pivot visits only donors in the same group.
Cross-group local HLA counts remain their unchanged widths. Ignoring them when
finding the first eligible donor gives exactly the separate eligible sequence.
The same is true for a unit target: an unrelated nonunit has initial count
equal to its width, since it does not contain the unit symbol, and is never
visited by the target's virtual symbol updates.

Global decreasing-width target order restricts to the same local target order.
Deleting a target changes only its group's remaining-donor list and registry.
Thus all HLA decisions, actual deletions and componentwise returned order agree.
The cross-group matrix storage can be larger without becoming an inference
premise for the local result.

## 5. False returns and repeated calls

The simulation holds until the first FALSE return. Its triggering unit merge
or restriction is wholly inside one group, whose separate execution takes
the same path. Conversely, if a separate call returns FALSE, the global
execution reaches that local step unless another group has already returned
FALSE. Finite interspersed inert work cannot prevent that scheduled step.
This establishes the stated recognition equivalence without assuming that
the simplifier recognizes all contradictory formulas.

For nonconstant outputs, the same partition remains valid after the call.
Apply the source simulation again to each induced output. A component cannot
resume productive changes after a nonproductive call: the reviewed repeated-
pass argument makes the sorted output of that call an exact fixed point.
Whole storage loses a value/clause if and only if at least one component does.
The productive passes therefore stop at the largest component count.

## 6. Independent executable comparison and remaining review

`component_separability.py` builds separate canonical components and arbitrarily
interleaves their clauses. It compares a production whole call with separate
production calls on the induced component orders, preserving literal-vector
and clause order. It repeats both sides together through the final fixed
point, checks false-recognition equivalence and the maximum-pass identity,
and samples independent SAT/MDD semantic comparisons. Saved scheduling-gap
fixtures are included to challenge an accidental assumption of first-pass
saturation. Both native R 3.6.3 and container R 4.6.1 passed the same 2,400
inputs: 6,125 whole calls, 12,706 isolated calls, 11,045 exact ordered-component
comparisons, and 26 separate SAT/MDD semantic comparisons. There were 531
recognized contradictory cases and 1,869 nonconstant fixed points; among the
latter, 30 needed no productive call, 1,822 needed one and 17 needed two.
Seven saved scheduling fixtures were used. These finite counts are not a
probability distribution or a claim of exhaustive component/order coverage.

This is a source proposal requiring independent review of the index embedding,
cross-component contextual rows, manual queue, and unit-HLA handling. The finite
run is supporting evidence and does not itself establish the universal claim.
No production change is proposed here.
