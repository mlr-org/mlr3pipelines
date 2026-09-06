# Composing a source-level semantic preservation proof

This document assembles the independently reviewed lemmas into a proposed
complete proof for the simplification kernel. It is a source-level argument,
not a mechanized semantics of R or a claim about every public object bearing
a `Cnf*` class. The representation investigations have concrete accepted
public paths that violate the kernel's required input representation.

## Precise proposed theorem

Let the input to `simplify_cnf()` be a scalar logical constant, or a finite list
of nonempty clauses. Each clause has unique registered symbol names and each
literal range is a nonempty unique ordinary character set inside its fixed,
nonempty finite domain. Assume ordinary total character membership, ordinary
R list/copy semantics, no external mutation of the universe during execution,
and index/count arithmetic within the runtime's supported bounds.

**If the simplifier completes normally, its returned formula has exactly the
same satisfying valuations as the input.** The source-level termination
measure also rules out mathematical infinite descent on these finite inputs,
but runtime stack and allocation failures are explicitly outside the normal-
completion theorem. Reverse implication chains reproduce such failures.

The theorem asserts neither a fixed point nor a canonical form. Several
accepted canonical inputs demonstrably need another simplification pass.

## State being interpreted

At each semantic operation, interpret the conjunction of all currently
non-eliminated entries. A registered unit is represented by its retained
clause, and `unit_domains` holds that clause's current range. Merging units
intersects their ranges and removes the redundant registering clause. The
intersection of all registered units is therefore a valid ambient assumption
for later operations in the pairwise phases.

The selected operation can be logically sound even when the raw stored ranges
of some other clauses have not yet been physically intersected with a new
unit. The correct transient interpretation is relative to those ambient unit
constraints. A proposed invariant requiring all raw comparisons to be exact
inside nested registration would be false.

## Operation-by-operation obligations

| Operation | Sufficient premise and its source |
|---|---|
| Return a logical input or drop initial TRUE clauses | Direct conjunction semantics. |
| Merge two units / report their empty intersection | Same-symbol membership is conjunction by set intersection. `register_unit` introduces each clause index once; the retained representative matches `unit_domains`. |
| Restrict a clause under a unit | Intersecting a range with an asserted unit preserves its meaning in context; if the unit is contained in the clause, deletion is subsumption. |
| Skip physical unit propagation through the matrix optimization | The FALSE retained-symbol comparison has a certificate depending only on units born before this registering index; well-founded discharge after ancestor work gives final physical containment. The stale reverse TRUE may lose equality elimination but cannot invalidate that containment argument. |
| Direct subsumption | Every consumed FALSE comparison is contextually sound, so count zero gives donor containment at every symbol under current units. |
| First-order SSE | Count one gives contextual containment at every non-pivot symbol. The target's pivot range is intersected with the donor's range; the pointwise rule is an equivalence under the retained donor and units. |
| Second-order SSE | Exact count sums and contextually sound FALSE bits give off-pivot containments. The direct raw intersection test is stronger than the corresponding contextual intersection test. The target restriction by the donor-range union is therefore sound under the retained donors and units. Inverse TRUE guards only prune attempts; they are not a logical premise. |
| Empty symbol / empty clause result | Removing an empty literal preserves a disjunction. If a sound restriction empties the whole clause, the current conjunction is contradictory; the propagated TRUE status correctly causes a FALSE return. The empty clause need not be written back before that immediate return. |
| Non-unit virtual HLA extension / target deletion | The quiescent matrix theorem gives exact raw initial comparisons and counts. Monotone local updates preserve exactness. Each extension is equivalent under a retained, distinct donor; a contained donor makes the virtual target redundant. Only the target is removed during its own virtual proof. |
| Unit virtual HLA extension / target deletion | Final physical unit containment gives the initial raw comparison counts, without assuming the very unit whose redundancy is being proved. Fresh local rows are then maintained exactly by monotone updates. Donors are surviving non-units. |
| Return surviving entries | Every completed semantic operation preserved the interpreted conjunction; eliminated entries were redundant at the time they were removed. Sequential composition gives input/output equivalence. |

## Why callback machinery supplies the needed premises

`../proof_state/LIFECYCLE_PROOF.md` inspects every registration call site and
every recursive helper operand guard. In particular, an inactive donor or
target is not submitted to an actual SSE inference; a stale symbol-registry
snapshot may submit an absent symbol to a restriction helper, which returns
without action before modifying anything. A registering unit uses its own
fresh index for the skip certificate, even when merging into an older stored
representative.

The same proof establishes contextual FALSE certificates. Shrinking a source
preserves inclusion; non-unit target restrictions repair reverse FALSE bits
before callbacks; unit restrictions preserve inclusion after intersecting both
sides with the same new ambient constraint. Symbol deletion repairs the
corresponding empty-range comparisons before callbacks.

`../proof_state/QUIESCENT_MATRIX_PROOF.md` handles conservative TRUE bits.
Only source shrink can make a formerly correct TRUE bit wrong. Every such
source update owns a finite pending list covering all affected initialized
live targets. Recursive updates either discharge their own obligations or
make the source inactive; all obligations disappear on normal unwinding.
Combined with final physical unit containment and exact count increments/
decrements, this gives raw exactness before HLA. Virtual changes for one HLA
target touch only that target's matrix row and cannot affect a later target's
initial raw comparisons.

`PROOFS.md` gives the pointwise algebra for all local rewrites, single-use HLA,
HTE branch unreachability, and finite descent. `rule_proofs.py` exhausts each
pointwise Boolean schema, and the independent proof-state certificate checker
exhausts its local transition/induction obligations. These finite Boolean
checks are universal for the corresponding set identities because every
concrete valuation maps to one of the checked membership patterns.

## Independent evidence and remaining interpretation limits

The one-hot SAT oracle and canonical decision-diagram oracle share neither
the simplifier's clause update rules nor its caches. A third checker uses
domain-refutation certificates for hidden clause redundancy. Their calibration
and complete two-symbol/three-clause membership-class enumeration provide
separate evidence; the proof does not infer universal correctness merely from
those trial counts.

The load-bearing limitations remain ordinary R value semantics, the inspected
source graph, and the exact representation precondition. Logical-NA and matrix
clause selectors can produce public classed objects outside that precondition;
those are implementation defects requiring their own handling. This theorem
also does not guarantee successful execution on long but simple inputs:
current R fails a reversed 1,024-symbol implication chain with node-stack
overflow, while the forward order succeeds through 4,096 symbols.

This composition should receive an independent final challenge before being
presented as a completed universal kernel proof. A future source change must
recheck the caller graph and cache-maintenance premises; the archived source
hashes identify the version analyzed here.
