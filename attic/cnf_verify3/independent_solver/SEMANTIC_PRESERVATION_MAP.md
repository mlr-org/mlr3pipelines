# Composing a source-level semantic preservation proof

This document assembles the independently reviewed lemmas into a
normal-return proof for the simplification kernel. It is a source-level argument,
not a mechanized semantics of R or a claim about every public object bearing
a `Cnf*` class. The representation investigations have concrete accepted
public paths that violate the kernel's required input representation.

## Precise theorem and review status

Let the input to `simplify_cnf()` be scalar `TRUE` or `FALSE`, or a finite list
of nonempty clauses (the list itself may be empty). Each clause has unique,
nonmissing, registered symbol names and each literal range is a nonempty
unique ordinary character set inside its fixed,
nonempty finite domain. Assume ordinary total character membership, ordinary
R list/copy/indexing semantics without user-defined replacement behavior,
no external mutation of the universe during execution,
and index/count arithmetic within the runtime's supported bounds.
Symbol identity must also agree across character membership, list indexing,
environment insertion/direct lookup, and environment name enumeration. In
particular, enumerating the unit-domain map must preserve the clause symbol
keys. This premise was implicit in the source's mathematical symbol-map
interpretation and is now explicit after the
[native-name counterexample](../character_identity_review/FINDING.md): valid
Unicode names under `LC_CTYPE=C` can violate it even with canonical clause
shapes and successful ordinary public construction. The new example normally
returns a changed truth function. It refutes a strengthening to all ordinary
R character names, not the pointwise set rules or this explicit map contract.

**If the simplifier completes normally, its returned formula has exactly the
same satisfying valuations as the input.** The source-level termination
measure also rules out mathematical infinite descent on these finite inputs,
but runtime stack and allocation failures are explicitly outside the normal-
completion theorem. Reverse implication chains reproduce such failures.

The theorem asserts neither a fixed point nor a canonical form. Several
accepted canonical inputs demonstrably need another simplification pass.

The independent challenge in `../review_semantics/REVIEW.md` found no circular
premise or semantic counterexample in this composition. It supplied the frozen
birth argument and lazy-row lemma incorporated below. The separate
`../review_hla/REVIEW.md` challenges the HLA and static second-order components.
These reviews support the theorem at the level of the inspected R source;
neither review is a formal verification of the R runtime. The source hashes in
`SOURCE.sha256` delimit the reviewed version. Full-domain literal ranges need
not be excluded for this semantic theorem, although the public clause
constructor normally converts them to a logical TRUE clause.

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
constraints. This interpretation applies through the pairwise phases, before
HLA begins. The final unit-HLA loop can delete a unit without clearing
`unit_domains`; that loop instead reasons from actual non-unit donors and fresh
raw comparisons. A proposed invariant requiring all raw comparisons to be exact
inside nested registration would be false.

## Operation-by-operation obligations

| Operation | Sufficient premise and its source |
|---|---|
| Return scalar TRUE/FALSE or an empty formula list | Direct conjunction semantics; an empty conjunction is TRUE. Initial TRUE-clause filtering belongs to the constructor, not a general kernel pass. |
| Merge two units / report their empty intersection | Same-symbol membership is conjunction by set intersection. `register_unit` introduces each clause index once; the retained representative matches `unit_domains`. |
| Restrict a clause under a unit | Intersecting a range with an asserted unit preserves its meaning in context; if the unit is contained in the clause, deletion is subsumption. |
| Skip physical unit propagation through the matrix optimization | The FALSE retained-symbol comparison has a certificate depending only on units born before this registering index; well-founded discharge after ancestor work gives final physical containment. The stale reverse TRUE may lose equality elimination but cannot invalidate that containment argument. |
| Direct subsumption | Every consumed FALSE comparison is contextually sound, so count zero gives donor containment at every symbol under current units. |
| First-order SSE | Count one gives contextual containment at every non-pivot symbol. The target's pivot range is intersected with the donor's range; the pointwise rule is an equivalence under the retained donor and units. |
| Second-order SSE | Exact count sums and contextually sound FALSE bits give off-pivot containments. The direct raw intersection test is stronger than the corresponding contextual intersection test. The target restriction by the donor-range union is therefore sound under the retained donors and units. Inverse TRUE guards only prune attempts; they are not a logical premise. |
| Empty symbol / empty clause result | Removing an empty literal preserves a disjunction. If a sound restriction empties the whole clause, the current conjunction is contradictory; the propagated TRUE status correctly causes a FALSE return. The empty clause need not be written back before that immediate return. |
| Non-unit virtual HLA extension / target deletion | The quiescent matrix theorem gives exact raw initial comparisons and counts. Monotone local updates preserve exactness. Each extension is equivalent under a retained, distinct donor; a contained donor makes the virtual target redundant. Only the target is removed during its own virtual proof. |
| Unit virtual HLA extension / target deletion | Final physical unit containment gives the initial raw comparison counts, without assuming the very unit whose redundancy is being proved. The lazy-row lemma below justifies delayed row allocation; monotone updates then keep rows exact. Donors are surviving non-units. |
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

For completeness, the simpler frozen-birth derivation is as follows. At the
birth of a unit candidate on symbol `s`, let `R` be its saved candidate range
and `E` the intersection of units on `s` born strictly earlier. Every incoming
FALSE bit used by its skip test supplies `C_s intersect E subset R`. The bit
is frozen against that fresh candidate's index, not against a subsequently
changed older representative. Later source shrinkage preserves the statement.
Induction in chronological birth order, separately for each symbol, establishes
final containment in every earlier unit constraint and discharges each such
certificate. Stack ancestry is not the induction order; a nested `X,Y,X`
sequence is therefore harmless. Equality can remain, which is the documented
unit-subsumption scheduling defect, but final physical containment follows.

For unit HLA, a donor whose local row is still unallocated cannot mention a
previously processed virtual pivot: each such pivot visited all live donors
containing it through the symbol registry and allocated their rows. Its
comparisons are therefore still the initial ones when the initial row formula
is evaluated lazily. Subsequent virtual extensions only clear comparisons
after direct subset checks. This supplies the additional source obligation
that is absent from non-unit HLA, whose rows are allocated in advance.

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

The independent challenges are now recorded in the two reviews linked above.
A future source change must recheck the caller graph and cache-maintenance
premises. The theorem's conditional scope is essential: it does not establish
correctness of all public normalization paths, all character encodings, arbitrary
classed objects, fixed-point scheduling, or resource-bounded execution.
