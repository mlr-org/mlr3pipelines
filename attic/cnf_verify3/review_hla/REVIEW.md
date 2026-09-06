# Independent review of finite-domain HLA deletion completeness

The unrestricted statement “the implementation removes every clause whose
negation is refuted by domain propagation in the other clauses” is false.
There are two distinct qualifications:

1. Even an ideal sequential deletion pass need not remove every clause that
   was redundant against the original donor set. Deleting one clause can make
   another necessary. The meaningful saturation property concerns the final
   surviving formula.
2. For that final formula the production code has the confirmed unit-equality
   gap. A nonunit containing exactly a surviving unit's range can survive,
   although the unit alone refutes the nonunit's negation.

My independent source review supports a precise stronger characterization of
the proposal in `../independent_solver/HLA_SATURATION_PROOF.md`:

> For a normally returned nonconstant formula F on canonical finite domains,
> domain propagation in F minus T under not-T refutes a surviving clause T
> **if and only if** a different surviving unit directly subsumes T.

The forward implication is the substantive theorem proved below. The reverse
implication is immediate: not-T makes that unit impossible. Physical unit
containment sharpens subsumption here to equality at the unit's symbol. Thus
no surviving unit is redundant by this predicate. The only possible residual
refutations are nonunits containing a surviving unit verbatim at one symbol.

This is a source-level argument, not a mechanically verified R operational
semantics proof. Its assumptions and executable cross-checks are explicit
below. All new artifacts in this review are confined to this directory;
production files were neither edited nor committed.

## 1. Input contract and the exact propagation predicate

Each symbol s has a finite nonempty domain Omega_s. Each stored clause C is
a disjunction of predicates `x_s in C_s`; an absent symbol has C_s empty.
Every stored nonconstant clause has at least one symbol. Every present range
is a nonempty proper subset of Omega_s, and both ranges
and domains contain unique values. A clause has at most one range per symbol.

These are kernel representation assumptions. `CnfAtom()` removes empty and
full-domain atoms, and `CnfClause()` merges repeated symbols and removes
tautological clauses. A hand-built object carrying the right R class need
not satisfy this contract. Malformed names, NULL ranges, duplicate domains,
or externally changed universe environments are not covered here.

To test target T, initially retain the possible domain

    P_s = Omega_s minus T_s.

Every P_s is nonempty by properness. For donor D, let

    L_D(P) = {s : D_s intersects P_s}.

There are exactly three propagation cases:

* L_D(P) empty: conflict.
* L_D(P) = {s}: replace P_s by its intersection with D_s.
* At least two possible symbols: no restriction.

This is also **generalized arc consistency for each individual clausal
constraint**, not merely an analogy to Boolean unit propagation. To see
this independently, take the satisfying tuples of D inside the box product(P)
and project those tuples onto each coordinate. With no possible literal there
are no tuples. With one possible symbol s, the s projection is P_s intersect
D_s and every other coordinate retains all its values. With at least two
possible symbols, any value of any coordinate has support by satisfying a
literal at a different possible symbol. Hence every projection equals P.

`support_projection()` enumerates the tuples directly and confirms this
claim against the propagation operator on all 2,352 clause/state pairs over
two ternary symbols. It does not use the count-one characterization internally.

The rule is complete for this individual-constraint support filtering. It is
not complete for logical entailment by a conjunction of clauses. For example,
the four Boolean clauses

    (x or y or z), (x or y or not-z),
    (x or not-y or z), (x or not-y or not-z)

jointly imply x. Under not-x every donor still has two possible symbols, so
this propagation procedure stops without contradiction. Direct valuation
enumeration in `mathematical_checks.py` checks both claims.

The cited paper calls its arbitrary-donor operation ALA; HLA in that paper
uses binary donors. Its Boolean ALA/refutation equivalence appears in Lemma
7. This source uses “HLA” for its finite-domain arbitrary-donor version. The
set argument in this review establishes the finite-domain result independently
of importing that Boolean lemma. See [Heule, Järvisalo and Biere, *Clause
Elimination Procedures for CNF Formulas*, Sections 4.1–4.2 and Lemma
7](https://fmv.jku.at/papers/HeuleJarvisaloBiere-LPAR10.pdf).

## 2. Order independence of refutation for a fixed donor set

Order domain boxes by coordinatewise inclusion. Add one bottom state for
conflict; boxes with any empty coordinate can be identified with bottom.
Each donor induces a contracting monotone operation f_D on this finite poset.

Here is the full monotonicity case split for Q contained in P:

* If P has no possible D literal, neither does Q, so both results are bottom.
* If P has just s, Q can only have s or no possible literal. In the former
  case its restriction `Q_s intersect D_s` is contained in `P_s intersect D_s`;
  in the latter its result is bottom.
* If P has at least two possible D literals, f_D(P)=P. The contracting result
  f_D(Q) is contained in Q, which is contained in P.

Take any common fixed point Q below the initial P0. Induction on a propagation
sequence shows Q is contained in every visited state: from Q contained in P,
monotonicity gives `Q=f_D(Q)` contained in f_D(P). A fair sequence of proper
restrictions must terminate by finiteness. If it terminates without conflict,
its terminal state is itself a common fixed point and therefore the greatest
common fixed point below P0. If it reaches bottom, no nonbottom common fixed
point below P0 can exist. Thus the terminal box, including bottom, is
independent of the fair order.

Adding donors cannot destroy refutability. Any fixed point of the larger
donor collection would also be a fixed point of the original donor collection;
if the latter has no nonbottom fixed point below P0, neither does the former.
This monotonicity concerns a fixed target and fixed raw donor clauses. It
must not be confused with claiming that deletion preserves every original
propagation consequence.

## 3. Complement coordinates and the one-use donor invariant

Write the virtual target as `V_s = Omega_s minus P_s`. Then

    D_s intersects P_s  iff  D_s is not a subset of V_s.

The non-subset count is exactly the number of possible donor literals. For a
selected donor with sole possible symbol s, domain restriction gives

    Omega_s minus (P_s intersect D_s)
      = V_s union (Omega_s minus D_s).

This is precisely the set in production lines 693 and 755. Adding only values
absent from the old virtual range is a duplicate-free way to form the union.

There is a useful strengthening of the existing proof notes. After donor D
is selected at s, including a selection that changes nothing,

    P_s is nonempty and P_s is a subset of D_s;
    D_v intersects P_v is empty for every v other than s.

Subsequent restrictions preserve both facts as long as they preserve nonempty
domains. Every selected count-one restriction does preserve nonemptiness,
because its selected literal was possible. Consequently, before some other
donor reports conflict:

* D always has exactly the same one exceptional symbol s.
* Reusing D always changes nothing.
* **D cannot itself become count zero.**

The existing notes say a used donor may later become conflicting, and the
source comment at line 706 suggests checking such cases is required. Under
exact consistent bookkeeping this is stronger than needed:
there is no path to a conflicting used donor. Continuing to check used donors
is harmless, but those hidden-subsumption witnesses cannot occur on a valid
execution. The algebraic single-use conclusion remains correct.

The same nonemptiness argument makes the immediate HTE tests at lines 694
and 756 unreachable. Choose a value in `D_s minus V_s`, which exists by
selection. It belongs neither to old V_s nor to the complement of D_s, so
the extended virtual range cannot cover Omega_s. Contradiction is instead
recognized when an as-yet-unused donor loses its last possible literal.

There is no assumption of acyclic donor dependencies. Other donors may reduce
the same coordinate several times, and a donor previously having several
possible symbols may become selectable later. The repeated scan finds it.
At most one iteration per donor is necessary, including no-op selections.
On normal loop termination every unused donor has at least two possibilities,
and every used donor is a no-op by the invariant. The state is therefore a
common fixed point of every donor.

## 4. What omitting actual unit donors requires

The production HLA loops use only surviving nonunit donors. This is not
soundly justified by saying units were “already propagated” without giving
a physical invariant: the unit under test is excluded from its own premises.

Let U_s be the range of a selected context unit on s, and assume every
nonunit donor's raw D_s is contained in U_s. Compare P, propagated using only
nonunits, with Q initialized and propagated using these units as well:

    Q_s = P_s intersect U_s at context-unit symbols;
    Q_s = P_s at other symbols.

Since D_s is contained in U_s, D_s intersects Q_s exactly when it intersects
P_s. Both states therefore give every nonunit donor the same possible-symbol
set. A one-symbol restriction preserves their stated relationship. If no
context unit directly subsumes T, Q initially has no empty domain. A later
selected nonunit restriction cannot empty Q: its selected intersection was
nonempty in Q as well as P. The context units themselves remain no-ops.

It follows that, **unless a context unit directly subsumes T**, nonunit-only
propagation refutes exactly when propagation including the units refutes.
The full propagated boxes need not be equal; their precise relationship is
the intersection formula above.

Two boundaries matter:

* Without physical containment the result is false. Let T be `x=0`, let the
  context unit be `y=0`, and let D be `(x=0 or y=1)`. D alone does not refute
  not-T, whereas D together with the unit does. Here D's y range is not inside
  the unit. The executable checks reproduce this counterexample.
* With physical containment but direct unit subsumption, ignoring that unit
  can miss an immediate contradiction in the initial box. This is exactly the
  surviving production equality gap, not a donor-chain scheduling defect.

For a unit target, every other surviving unit is on a different symbol, so
direct unit subsumption is impossible. Raw nonunit containment in the target
unit's range also correctly initializes its comparisons: that range is a
property of the stored donor, not an assumption of the target's truth.
The code uses the full `universe`, rather than `unit_domains`, when taking
complements in both loops. Substituting the target's unit domain for the full
universe would introduce precisely the circular assumption being avoided.

## 5. Independent audit of the source premises

Line references are to `R/CnfFormula_simplify.R` with SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
I read the complete simplifier and the existing lifecycle/cache arguments.
The following is my independent derivation of the load-bearing premises,
not an inference from the test statistics.

### 5.1 Live registry coverage and permanent inactivity

Initial unit representatives are registered at lines 485–487. Preprocessing
processes nonunits before inserting their remaining symbols into the registry
at lines 515–520. Later operations can remove ranges, symbols or clauses but
never add actual symbols or grow actual ranges.

Symbol removal deletes the clause from that symbol's registry first (240–244).
If it becomes a unit, its remaining symbol is removed too before registration
(245–251). Whole-clause removal cleans every present symbol (472–478).
Registration marks a new representative as a unit (95), or eliminates it on
merging with an existing representative (104), before recursive propagation.
No operation reactivates a clause. Thus at a quiescent boundary each registry
contains exactly the live nonunits with a nonempty range at its symbol.

The fixed `available` list and inverse indices contain all clauses that can
ever remain nonunits during the matrix phases. Every finally live pair is
initialized in the nested pair loop: an early break is caused by the outer
clause becoming inactive; a skipped inner clause is inactive forever.

### 5.2 Physical unit containment after recursive propagation returns

A non-skipped unit propagation explicitly intersects the clause with the
current effective domain or removes it (135, 152–167). A skip at 133 relies
on a FALSE incoming comparison into the registering clause's retained unit
symbol. Such a FALSE comparison has a birth-order certificate:

    current(C_s) intersect earlier_unit_constraints_s
      is a subset of the registering candidate's birth range.

A direct raw subset comparison establishes the certificate. Shrinking C
preserves it. A nonunit restriction of the other range repairs reverse FALSE
comparisons before callbacks (178–192). If a unit propagation shrinks that
other range without reverse repairs, the propagating unit already exists
before the other clause can become a unit, and is one of the earlier constraints.

After the candidate becomes a unit, ordinary pairwise update loops no longer
treat it as a nonunit target. The incoming retained-symbol certificate cannot
acquire a dependence on that candidate's own newly asserted range. A whole
source-column clearing after a symbol is removed is harmless because the
source's actual range then is empty.

Induct over candidate unit births. A clause either receives an explicit
restriction, or its skip certificate plus physical containment in all earlier
constraints proves containment in this birth constraint as well. Every
propagation frame returns eventually on a normally completed execution. If
a child registration tightens a unit domain during a parent frame, the child
supplies its own later birth constraint; the parent's saved local unit range
does not lose its earlier certificate. The effective-range guard at 115
disables skips when a merge already made the effective range smaller than
the candidate's own birth range.

The same clause index cannot be reborn as a unit. Initial units are outside
`available`; a later unit is removed from the symbol registry before registration.
A stale same-symbol propagation snapshot sees an eliminated merged candidate;
a stale different-symbol snapshot finds that its old symbol is absent and
returns at 149. SSE operands are checked active before each rewrite. A stored
old representative can shrink at 103, but that old index is not registered
again. Hence registration does not reuse an inactive unit's stale matrix as
if it belonged to a fresh candidate.

Clauses not registered yet during preprocessing receive all current units
when their own iteration starts. While matrices are NULL, a still-nonunit
range change cannot create unrelated recursive registrations; a registration
caused by that clause means the clause itself became a unit and its preprocessing
symbol loop stops. A newly created unit cannot silently evade a future live
nonunit. These observations cover the preprocessing boundary of the induction.

This proves containment, **not strict containment**. The reverse TRUE bit
used by the skip need not already have been refreshed during a recursive
source shrink. Therefore it does not certify strictness. That is why the
unit-equality example is compatible with the physical invariant.

### 5.3 Exact live pair matrices and counts before HLA

FALSE comparisons consumed by a live pairwise rewrite are sound after
intersecting both sides with all registered unit constraints. Direct pair
construction establishes this (587–605); source shrinking preserves it;
nonunit target shrinking refreshes reverse comparisons before callbacks;
unit target shrinking preserves it under the common new context; and symbol
deletion clears the source column and refreshes reverse entries before callbacks.
Inactive pairs do not carry a live comparison obligation. Physical containment
from 5.2 then turns every such FALSE certificate into raw inclusion at HLA entry.

For TRUE comparisons, only a shrink of the source at that symbol can make a
raw-correct non-inclusion incorrect. Target shrinking cannot do that. A strict
nonempty source restriction writes the range at 167 and snapshots every
initialized TRUE comparison to a live registered target at 205. An absent
target range cannot contain this nonempty source; uninitialized pairs will
be compared directly later. Every relevant incorrect TRUE is thus on this
frame's finite pending list.

The loop tests current stored ranges at 215–216. It rechecks the bit before
decrementing it (220–222), so a nested frame clearing the same bit does not
double-decrement. An early noncontradictory exit from this loop occurs only
when the source becomes inactive (226). New TRUE bits introduced by a nested
target shrink are raw-correct when installed. If a later source shrink makes
one wrong, that later source-update frame owns the new pending obligation.
Complete symbol deletion clears all affected source bits before callbacks
(264–266). Therefore every incorrect live TRUE has an outstanding owning
frame, and no such TRUE can remain after every callback has returned.

Counts remain exact sums independently: pair construction sums both rows
(608–610); guarded FALSE-to-TRUE operations increment once (185–188,
270–272); guarded TRUE-to-FALSE operations decrement once (220–222); whole
symbol deletion snapshots and decrements exactly the affected initialized
rows (264–266). The diagonal count remains NA and is never an inference pair.

### 5.4 No distinct live nonunit pair starts HLA with count zero

Exactness by itself is insufficient: the HLA selection skips initial zero
counts. This premise needs its own scheduling argument.

An initialized zero is immediately offered to the ordinary handler in both
pair orientations (611–620). A later per-range decrement to zero invokes
the handler at 224; its threshold test cannot skip zero. Whole-symbol deletion
may set several counts to zero before processing them, but each remains on
the finite pending callback list (277–285). If a target shrinks before its
callback, its count may rise; a final zero cannot be dismissed for that reason.
The list can be abandoned only when the source becomes inactive (288) or a
global contradiction is returned. Skipped inactive targets do not survive.

Every final live zero therefore either receives the handler at 309–311,
which eliminates its target, or would require a still-outstanding callback.
The latter is impossible when HLA starts. This proof does not require every
newly useful first-/second-order literal restriction to have been revisited.
Those stronger scheduling properties can fail while all live zero pairs
have been consumed.

An isolated count-one algorithm without this premise is incomplete: donor
`(x in {0} or y in {0})` already subsumes target
`(x in {0} or y in {0,1})` on ternary domains. It has no exceptional symbol,
so a selector looking only for count one does nothing. The mathematical
checks deliberately disable the initial-zero check and reproduce that miss.

### 5.5 Per-target virtual bookkeeping, row isolation and unit initialization

For a nonunit target, local counts are copied from the exact matrix at 673.
Extending V_s can only clear TRUE comparisons involving s. The registry
enumerates every donor having a nonempty range at s; the membership check
at 707–709 clears exactly the newly contained ones and decrements their local
counts at 711. A new zero causes immediate deletion. The current target's
self-row is FALSE and stays FALSE because its original range is contained
in its expanding virtual range. Hence it cannot index a missing self-donor
counter through `match()`.

The only matrix writes during nonunit HLA have the current target row index
inside a donor matrix. A later target uses another row. The global count
matrix is not changed by these virtual updates; actual `entries` ranges
are not overwritten. Consequently a later target still starts from exact
original raw comparisons. Deletions remove earlier targets from both the
donor list and registries without changing remaining donors.

There is exactly one surviving unit representative per constrained symbol,
so sorting by clause length and subtracting `length(unit_domains)` correctly
separates nonunits and units at 657–659. Unit targets use freshly constructed
counts. Every nonunit donor is contained at the target's unit symbol, while
all its other present symbols are exceptional to that unit target. Thus
`length(donor) - contains_target_symbol` at 738 is exact. It is at least one:
the donor has at least two symbols. Its lazy comparison lists encode the same
fact and undergo the same one-symbol updates as in the nonunit loop.

Units are not donors in either loop. Deleting one unit therefore does not
invalidate another unit's outstanding raw nonunit proof. Not updating the
unit registry during the final unit loop is harmless because it is no longer
used to construct donor premises or to restrict stored clauses.

Together with Sections 2–3, these facts establish that each target is deleted
exactly when the nonunit donor set present at its visit refutes its negation.

## 6. Why a single target sweep proves final deletion saturation

Assume a final surviving target T is refutable from the other final clauses,
and no final unit directly subsumes it. By Section 4, final nonunit donors
alone refute it. Every one of those raw nonunit donors was also available
when T was visited: HLA removes entire clauses and changes no actual ranges.
The visit may have had more donors, never fewer. By donor monotonicity from
Section 2, T was refutable at that visit. The complete target procedure from
Section 5 would have removed it, a contradiction.

This argument works equally for final unit targets. Another final unit cannot
subsume such a target, because the units have different symbols. It also
covers the early return containing only unique proper units: each target's
negation remains compatible with units on different independent symbols.

Thus the only residual case is direct unit subsumption of a nonunit. If a
unit range U_s is contained in T_s, the final physical containment in Section
5.2 also gives T_s contained in U_s, so the two ranges are equal. This proves
the iff characterization at the beginning of this review.

One sweep does **not** imply a unique surviving subset. On Boolean variables,
write

    A=(x or y), B=(x or z), C=(not-y or z), D=(y or not-z).

A is initially refutable using B and D; B is initially refutable using A
and C. If A is deleted, B becomes necessary, and vice versa. Exhausting all
24 target orders gives the two different saturated outputs `{A,C,D}` and
`{B,C,D}`. Production returns `{B,C,D}` in the displayed input order. This
is a concrete distinction between final saturation and “remove all initially
refutable clauses.”

## 7. Finite domains, multiple restrictions and SSE2 coverage

None of the preceding proofs uses a two-value law such as “not one value
means the other value.” All steps are subset, intersection and complement
identities on arbitrary finite sets. A selected donor may restrict a domain
to several values, and another donor may later shrink that same domain again.
The one-use rule applies to donor clauses, not to symbols.

Here is a production-executable genuinely multivalued unit-deletion example.
Let x have domain `{0,1,2,3}` and y,z each have `{0,1,2}`. The target is
`T=(x in {0,1})`; its nonunit donors are

    D0=(x=0 or y in {0,1})
    D1=(y in {1,2} or z=0)
    D2=(y in {0,2} or z=1)
    D3=(x=1 or z=2).

Under not-T, D0 restricts y to `{0,1}`; D3 restricts z to `{2}`; D1 then
restricts y to `{1}`; D2 becomes impossible. D0 remains satisfied throughout
the second restriction to y, illustrating the stronger used-donor invariant.
Both donor x ranges are proper subsets of T's range, so ordinary unit
propagation does not remove those donors in advance. Production removes T
and returns exactly the four unchanged donors.

The existing static SSE2 case split is correct under its explicit fixed-state
premises. For a useful restriction at t using intersection symbol s, each
donor's exception set lies inside `{s,t}`. An empty exception set gives
subsumption; an exception set `{t}` already gives an at-least-as-strong SSE1
restriction; two exception sets `{s}` give a two-donor refutation by HLA:

    P_s starts outside T_s;
    donor A restricts P_s inside A_s;
    A_s intersect B_s is inside T_s, so B becomes impossible.

The remaining arrangements contain a two-exception donor. The twoend handler
tries both pivot orientations (416–420), enumerates the other donor through
the intersection-symbol registry (421–425), and verifies that its exceptional
columns lie in those same two symbols (436–438). Usefulness supplies a value
of T_t outside both donor ranges, making both inverse guards at 453 true.
The explicit intersection test is exactly the set premise. Thus the static
pruning does not miss another independent useful arrangement.

Unit donors reduce to the same earlier cases by physical containment: a unit
at t leaves no target value outside its range; a unit at s contains the other
donor's s range, reducing the intersection premise to a simple donor subset;
a unit at another symbol directly subsumes the target. For a unit target at
t, donor t ranges are already contained, so both donors can only be oneend
at s and are covered by the two-donor refutation above.

This does not repair the independently established dynamic SSE1/SSE2 event
scheduling gaps. Exact fixed-state candidate coverage and final HLA clause
deletion saturation are compatible with remaining useful literal restrictions.

Finally, a careless Boolean encoding changes the propagation predicate. On a
ternary q, the three unary constraints `q in {0,1}`, `q in {1,2}`, and
`q in {0,2}` are immediately inconsistent by finite-domain intersection.
Replace q's values by three Boolean indicators with one at-least-one clause,
pairwise at-most-one clauses, and the three positive two-indicator clauses:
there is no Boolean unit, so ordinary Boolean UP reports no conflict. The
encoding is logically correct, but its elementary UP is weaker. The executable
checks verify both unsatisfiability and the absence of Boolean UP assignments.

## 8. Exact counterexample to unconditional production completeness

Let y have `{0,1,2}` and x have `{0,1,2,3}`, with each clause stored in y,x
symbol order:

    y=0 or x in {0,1}
    y=1 or x in {0,2}
    y=2 or x in {0,3}.

Production returns

    x=0
    y=2 or x=0.

The second clause is refuted under its negation by the first unit immediately.
Every input literal is nonempty and proper; this is inside the theorem's
representation assumptions. A second simplifier pass removes the residual
nonunit. Changing the stored symbol order of each clause to x,y also makes
the first pass return only x=0. These facts are asserted in `examples.R`.

`trace_unit_equality.R` independently records the actual skip at line 133:

    registering unit: x=0
    current effective unit range: {0}
    skipped target: y=2 or x=0
    cached unit-not-subset-target bit: TRUE
    cached target-not-subset-unit bit: FALSE
    raw target range equals current unit range: TRUE.

Thus the effective-range merge guard has correctly allowed use of the unit's
own range; the incorrect inference of strictness comes specifically from
the stale reverse TRUE bit. The trace output is identical to an uninstrumented
run. This directly locates the missed removal before HLA, rather than merely
inferring a root cause from the redundant output.

This example refutes unconditional final deletion completeness but confirms
the exact surviving-unit exception. No change to one-use donor scheduling is
needed to explain it. An initially unit-subsumed target has an initial empty
domain after including units, while production HLA ignores those units.

## 9. Executable evidence and limits

Run the following from the repository root:

    python3 attic/cnf_verify3/review_hla/mathematical_checks.py
    python3 attic/cnf_verify3/review_hla/production_checks.py
    Rscript attic/cnf_verify3/review_hla/examples.R
    Rscript attic/cnf_verify3/review_hla/trace_unit_equality.R

The Python code uses only the standard library; the R code needs only base R
and sources the single production simplifier. No earlier research implementation
or generator is imported. `production_bridge.R` constructs an instrumented
function in memory; it never writes production source. Each instrumented
output is also checked identical to an uninstrumented call on the same input.

The mathematical checks exhaust:

| Obligation | Cases |
|---|---:|
| Support projection equals the donor operator | 2,352 |
| Donor monotonicity on comparable nonempty boxes | 17,328 |
| Two ternary symbols, target and unordered donor pair | 54,144 |
| Both donor orders compared with repeated support propagation | 108,288 |
| Refutations also checked by direct valuation entailment | 15,480 |
| Unit omission with raw donor containment and no initial unit subsumption | 82,890 |

The production audit ran 2,015 formulas, including all six orderings and both
literal-coordinate orders of the equality family, the directed deletion-order
and multivalued examples, and 2,000 seeded formulas on two through six symbols
with domain sizes two through five. It checked:

| Source obligation | Checks |
|---|---:|
| HLA entry boundaries reached | 1,352 |
| Exact live pair matrices/counts and positive initial counts | 17,070 |
| Physical unit containment pairs | 4,406 |
| Exact symbol registry membership | 6,041 |
| Per-target outcome versus independent repeated domain propagation | 6,118 |
| Exact virtual comparisons/counts at loop iterations | 44,425 |
| Strong used-donor nonconflict/no-op invariant | 7,658 |
| Final surviving clauses independently tested for refutation | 5,974 |

Every checked premise passed. The six residual refutations were precisely the
six directed y,x-order equality-family runs; every one was a nonunit directly
subsumed by its surviving unit. Results are in `mathematical_results.json`
and `production_results.json`. The production source hash was unchanged
before and after the audit.

Finite checks support but do not prove the unbounded source theorem. The proof
depends on the inspected call graph, ordinary R list/vector copy behavior,
the representation contract, and normal completion. It does not guarantee
resource bounds, absence of practical recursion failure, arbitrary malformed
public-object behavior, general entailment completeness, unique output,
idempotence of the full simplifier, or saturation of earlier literal rules.

The only correction I would make to the reviewed HLA proof prose is to
strengthen, rather than weaken, the used-donor statement: in a consistent
nonconflicting run a used donor cannot later be the conflict donor. The
remaining stated logical and implementation premises withstand this review.
