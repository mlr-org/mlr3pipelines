# A deletion-saturation theorem for the HLA phase

This strengthens the algebraic correspondence in `HLA_DOMAIN_REFUTATION.md`.
It concerns the inspected source, constructor-normalized finite kernel inputs,
normal completion, and ordinary set semantics. In particular, every literal
range is a nonempty **proper** subset of its nonempty domain, symbol names
within a clause are unique, and every range and domain is a unique set. This
is a stronger premise than the truth-preservation theorem, which may allow
full-domain ranges because preserving a tautological unit's truth is sound.
It uses the independently reviewed
physical-unit-containment and quiescent-matrix lemmas in
`../proof_state/LIFECYCLE_PROOF.md` and `../proof_state/QUIESCENT_MATRIX_PROOF.md`.
It does not assume first- or second-order literal saturation; both are false.

## The proposed theorem

Let F be a normally returned, nonconstant formula. If domain propagation in
F without a clause T, under the assumption that T is false, derives a
contradiction, then **a surviving unit clause directly subsumes T**.

Thus the HLA phase leaves no domain-refutation redundancy except a possible
instance of the already confirmed unit-equality subsumption gap. In
particular, no surviving unit is redundant by this predicate. If the output
has no clause containing the range of a surviving unit, it is saturated for
domain-refutation clause deletion. This predicate is not general entailment.

The theorem is a source-level proof proposal, not a machine-checked semantics
of the R program. The final section identifies its exact scheduling premises.

## 1. Domain propagation has an order-independent refutation predicate

For each symbol s, maintain a possible-value set P_s. Initially P_s is the
full domain minus T_s, and is nonempty by the proper-range premise. For every
donor D, count its possible literals,
meaning symbols s for which D_s intersects P_s.

* Zero possible literals gives a conflict.
* Exactly one, at s, restricts P_s to P_s intersect D_s.
* Two or more makes no restriction.

Treat conflict as the bottom state. Every donor operation is contracting and
monotone on the finite product lattice of possible-value sets. To check the
only nontrivial monotonicity case, suppose a larger state has one possible
literal at s. A smaller state cannot gain a possible literal elsewhere; it
either conflicts or restricts the same s by the same D_s. The smaller result
is therefore contained in the larger result.

It follows directly by induction that any common fixed point below the
initial domains is contained in every state reached by propagation. A fair
sequence of proper restrictions terminates, and its terminal state is the
greatest such common fixed point, unless conflict has occurred. Consequently
one fair order refutes exactly when every fair order refutes. Adding donors
can only make the terminal domains smaller and cannot invalidate a refutation.

A donor used once for a proper one-symbol restriction can never properly
restrict again: its other literals stay impossible, and its selected domain
is already inside its allowed range. It may later become conflicting, which
must still be checked. Thus using each donor once is enough, provided all
later conflicts are detected.

## 2. Omitting units cannot hide a new propagation step on a nonunit donor

Let U be any selected collection of surviving unit clauses other than T.
There is at most one unit per symbol. Every surviving nonunit donor D has
D_s contained in U_s whenever U has a unit at s. This is physical containment,
not an assumption of the target whose redundancy is being tested.

Compare two propagation states: P, which ignores U, and Q, which has first
applied U. Maintain Q_s = P_s intersect U_s at unit symbols and Q_s = P_s
elsewhere. A nonunit donor has exactly the same possible-literal symbols in
both states, because D_s intersects P_s exactly when it intersects Q_s.
Restricting either state by that donor preserves the relation between them.

If no unit in U is initially contained in T, every Q_s is initially nonempty.
A nonunit propagation step also cannot empty Q_s without an already detected
donor conflict: its one selected literal is possible in both states. Therefore
the unit clauses themselves never produce an extra conflict. Propagation
with U refutes exactly when propagation without U refutes.

This argument applies to both nonunit and unit targets. For a unit target,
every other unit is on a different symbol, so initial unit subsumption is
impossible. It also works with a subset of the units present earlier in the
execution. That matters when some units are themselves removed later.

## 3. The source HLA loop is complete for its nonunit donors

Use the complement change of variables C_s = domain_s minus P_s. A donor
has one possible literal at s exactly when its comparison count to virtual
C is one. The production extension by the complement of D_s is precisely
the corresponding restriction of P_s.

At nonunit HLA entry, the independently reviewed quiescent-matrix theorem
gives exact comparison rows and counts for all live nonunit pairs. No distinct
live nonunit pair has count zero: initialization invokes the ordinary handler
for zero; every subsequent decrease to zero invokes it as well, possibly
after a finite pending callback list. If that list is abandoned, its source
has become inactive, so it is no longer a live donor. A target decrease only
increases reverse counts. Therefore a live zero count cannot remain when all
the callback stacks have returned.

For a unit target, its freshly constructed donor counts are exact by physical
containment. They cannot initially be zero: every donor has at least two
symbols, and at most the target's one symbol is already contained.

During either HLA loop, an extension can only turn TRUE comparisons to FALSE.
The symbol registry covers every live donor containing the changed symbol;
the loop tests every such donor, including previously used ones, and decrements
exactly the comparisons that become contained. A newly zero count immediately
deletes the target. Otherwise the loop selects an unused count-one donor until
none remains. The once-used argument in Section 1 shows that the resulting
domain state is a common fixed point of all donors. Hence the target survives
exactly when the nonunit-donor propagation predicate fails to refute it.

This needs no acyclicity assumption. Cycles can create further unit donors,
and each newly count-one donor is selected by the repeated scan. The finite
number of donors bounds the number of selected extensions.

## 4. A single target sweep suffices

During HLA, actual nonunit clause ranges never change. Only entire clauses
are removed; the virtual extensions are local to the current target. A later
target's matrix row is untouched by earlier virtual extensions.

Suppose a final surviving target T has a domain-refutation proof from the
other final clauses and is not directly subsumed by a final unit. Section 2
removes all final unit donors from that proof. The remaining proof uses only
final nonunit donors, which were a subset of the nonunit donors available
when T was visited. Section 1 makes refutability monotone under adding donors,
and Section 3 makes the source target procedure complete. T would therefore
have been deleted when visited, a contradiction.

For a final surviving unit target the same reasoning applies, and direct
subsumption by a different final unit is impossible. Consequently every
residual domain-refutation redundancy is a nonunit target directly subsumed
by a final unit. Because final physical ranges are inside units, this means
an equal range at the unit's symbol, not a strictly larger one.

## Proof boundaries and cross-checks

The load-bearing source premises are exact initial live matrices/counts,
physical unit containment, complete symbol registries, no surviving nonunit
count-zero pair at HLA entry, exact monotone virtual updates, and local virtual
row isolation. The independent lifecycle/cache review supplies the first,
second, third, fifth and sixth; the zero-count argument above inspects every
count construction/decrement site. These premises exclude accepted malformed
public objects with repeated/NA symbol names or NULL literal ranges.

`domain_refutation_probe.py` independently checked 62,322 returned targets in
10,000 formulas. Its only three refutations were the known unit-equality
cases. That is falsification evidence for this theorem, not its justification.
The diagnostic strict-length unit-skip guard has a separate final-properness
proof in `CANDIDATE_REPAIRS.md`; together with the present argument it would
exclude even these residual deletion opportunities under the same assumptions.
