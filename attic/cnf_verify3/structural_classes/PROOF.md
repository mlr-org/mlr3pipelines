# Structural classes: saturation and contradiction recognition are different

Written 2026-09-06 for `R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
Assume canonical finite sets: nonempty finite domains, unique symbols within
each clause, and nonempty proper literal ranges. Constants are normalized in
the usual way. All implementation claims concern normal completion.

## 1. Explicit predicates and results

In the **incidence graph**, one vertex represents each stored clause and one
represents each symbol; a clause is adjacent to exactly its symbols. Separate
clauses remain separate vertices, even if they have the same support. An
incidence forest is acyclic. An incidence pseudoforest has at most one cycle
in each connected component.

For clauses with at most two symbols, the **primal multigraph** replaces each
nonunit clause by an edge between its two symbols. It must retain parallel
edges. Collapsing parallel edges can hide incidence cycles and invalidate a
structural claim.

The following predicates concern the returned clause/value multiset:

* **UP:** no empty unit intersection, no nonunit value outside a live unit,
  and no nonunit clause directly subsumed by a live unit.
* **SSE1:** no direct subsumption and no useful one-donor range restriction.
* **SSE2:** no useful restriction justified by the two-donor set premises in
  `../independent_solver/SSE2_COVERAGE_PROOF.md`, including the arrangements
  covered by earlier rules or by HLA.
* **HLA:** for every target, propagating domains in all other clauses under
  its negation does not refute. This is the exact deletion predicate defined
  in `../independent_solver/domain_refutation.py`; it is not general entailment.
* **SAT recognition:** the implementation returns logical FALSE exactly when
  the input is unsatisfiable.

The results are:

| Input class | First-pass UP/SSE1/SSE2/HLA saturation | First-pass SAT recognition |
| --- | --- | --- |
| Incidence forests, arbitrary finite domains/ranges | Yes; every surviving clause and every literal value is indispensable | Yes; initial unit propagation already decides contradiction |
| Incidence pseudoforests, arbitrary finite domains/ranges | No additional general saturation claim here | Yes; initial unit propagation already decides contradiction |
| At most two symbols per clause, with ranges for each symbol pairwise disjoint or equal | Yes | No |
| Boolean clauses of length at most two | Yes, as a special case of the preceding row | No |
| Boolean renamable Horn clauses, arbitrary lengths | No additional general saturation claim here | Yes; initial unit propagation already decides contradiction |
| Boolean positive-variable implications, with arbitrary positive/negative units | Yes, from the binary theorem | Yes, from the Horn theorem |

“First-pass” saturation ignores clause ordering. A later invocation may sort
a newly created unit into its initial length position, but these classes need
no further productive pass under the stated positive saturation results.

`BOOLEAN_2CNF_GRAPH.md` further characterizes the complete final unit set and
contradiction verdict for Boolean 2-CNF: take initial units together with all
literals reached from their complements by at most three input implication
edges, then close under ordinary implication reachability. The final units
are exactly this closure, unless it contains both polarities and FALSE is
returned. This gives an exact inference-strength boundary for this class.

## 2. Initial unit propagation reaches proper physical containment

This property applies to the initial unit stage for arbitrary canonical
input, before the comparison matrices exist. Initial units are merged by
intersection. Each already processed nonunit is in the symbol registry;
each newly registered or tightened unit immediately visits all such clauses.
Each not-yet-processed nonunit is restricted against the current unit domains
when its main-loop turn arrives.

At this stage `is_not_subset_of` is NULL, so the cached skip condition is
unavailable. On each visit, `apply_domain_restriction` either deletes a clause
whose literal contains the complete effective unit range, or intersects that
literal with the unit and leaves it a proper subset. A new unit recursively
does the same to the processed prefix. After the final registration of a
symbol's effective domain, every surviving previously registered clause is
visited; subsequent clauses are checked on insertion. Thus, on normal exit
from this initial stage:

* there is one nonempty unit range per constrained symbol;
* every nonunit has at least two nonempty literal ranges;
* every nonunit range at a unit symbol is a **proper** subset of that unit;
* every other range is still a proper subset of its original domain.

Write `P_s` for the effective unit range, or the original domain if no unit
exists. Then every nonunit range is nonempty and proper within `P_s`.
All these updates only delete clause nodes or incidence edges, so an initial
forest or pseudoforest retains that graph property.

This initial-stage statement does not strengthen the general final-output
containment theorem: the known equality-skip issue can occur after matrices
exist in unrestricted formulas. The forest proof below precludes entering
any such later range-changing path.

## 3. Incidence forests: a model-extension theorem

Consider a forest of nonunit clauses, each with nonempty ranges inside some
nonempty allowed domains `P_s`. Every assignment of an arbitrary allowed value
to a chosen root symbol extends to a model of its entire connected component.

To prove this, orient the tree away from that root. Every clause has one
parent symbol and at least one child symbol, because its degree is at least
two. If the parent value satisfies the clause, choose allowed values for its
children arbitrarily. Otherwise choose one child value from that clause's
nonempty range and choose its other child values arbitrarily. Continue away
from the root. Distinct branches never assign the same symbol, so all these
choices coexist. The process terminates on a finite tree and satisfies every
clause. A component with no specified root is handled by choosing one.

Applied after initial unit propagation, this gives a satisfying assignment of
every nonconstant residual forest, with all units also satisfied. Therefore
an unsatisfiable forest must have returned FALSE already in the initial unit
stage. No later inference is needed for contradiction recognition.

### 3.1 Every residual clause is essential

Remove a nonunit target `C`. Its distinct symbols lie in distinct components
of the remaining nonunit forest: a path connecting two of them would complete
a cycle through `C`. For each target symbol `s`, choose a value in
`P_s minus C_s`, which exists by proper containment. Apply the extension
theorem independently in each component. This satisfies every other clause
and every unit while falsifying `C`.

For a unit target at `s`, remove that unit, choose a value outside its range
in the original domain, and use the extension theorem rooted at `s`. The
other units constrain different symbols. The nonunit ranges remain nonempty
inside the allowed domains, so all other clauses can still be satisfied.

Thus no whole clause is redundant, even by unrestricted semantic entailment.
In particular, no subsumption or sound HLA deletion is possible.

### 3.2 Every individual literal value is essential

For a nonunit target `C` and one of its values `v in C_s`, assign `s=v` and
assign every other symbol `t` of `C` a value in `P_t minus C_t`. Delete the
target temporarily and extend these root assignments independently as above.
The resulting valuation satisfies the full formula, and satisfies `C` only
through the selected occurrence of `v`. Removing that value from the literal
would make the valuation false.

For a unit target, set its symbol to the selected value and extend from that
root. This likewise witnesses the necessity of each unit-range value.

Consequently no later sound value restriction can make progress. The
implementation is already saturated for UP, SSE1, SSE2, and HLA immediately
after initial unit propagation. Its later matrices and HLA loops may be
built, but no actual clause/range mutation can occur. The final result is
irreducible even under arbitrary equivalence-preserving single clause or
single value deletion, a stronger property than the advertised local rules.

## 4. Pseudoforests: a constructive satisfiability certificate

The SAT-recognition argument extends further than the preceding irreducibility
argument. Any incidence pseudoforest in which every clause has at least two
symbols has a matching that assigns each clause a distinct incident symbol.

In a tree component, root at a symbol and match each clause to one of its
child symbols. No symbol can be selected by two clauses, since it has only
one parent clause. In a component with a cycle, alternate clause and symbol
vertices around the cycle and match each cycle clause to the next cycle
symbol. Orient every attached tree away from that cycle. Match each off-cycle
clause to a child symbol. Such a child exists because its degree is at least
two, and no branch selection conflicts with the cycle matching.

For each matched symbol, choose a value from its matched clause's nonempty
range. Choose all unmatched symbols arbitrarily from their allowed domains.
Every clause now holds through its assigned symbol. Applying this certificate
to the residual nonunits after initial unit propagation also satisfies every
unit, because all selected ranges lie inside their effective domains.

Thus any nonconstant residual pseudoforest is satisfiable. Every unsatisfiable
canonical input pseudoforest must be recognized during initial unit
propagation. This does not imply that all surviving values are necessary: a
single implication cycle can force a value without producing a unit in the
stored output. Section 7 gives the exact example.

## 5. Binary clauses with no possible proper nonempty range change

Assume each clause has at most two symbols, and for each symbol all its input
literal ranges are pairwise disjoint or equal. Equivalently, each literal
selects one block of a fixed partition of that symbol's domain; unused domain
values can form an additional block. Boolean proper literals automatically
satisfy this condition.

Intersections of two such ranges are either the original block or empty.
Intersecting a block with the union of two donor blocks has the same property.
Thus unit propagation, SSE1, and SSE2 can never properly shrink a range while
leaving it nonempty. Every actual nonunit range change deletes a complete
literal and immediately converts the two-symbol clause into a unit. A unit
merge either preserves its block or reports contradiction.

Therefore every nonunit that survives to the end has exactly its original
two ranges throughout the call. The live nonunit pool only loses members.
This eliminates the mechanism behind the general scheduling gaps: no future
range shrink of a retained donor or target can enable an opportunity after
its queue visit.

The source's literal-deletion helper takes its unit branch and returns before
the nonunit matrix-update callbacks. Thus even nested propagation cannot
change comparisons between two still-live binary clauses; their initialized
comparisons remain exact raw comparisons throughout. The contextual subset
qualification needed for general wider clauses introduces no extra inference
case here.

### 5.1 Unit propagation has no equality-skip gap in this class

A nonempty range at a symbol remains its original block while that symbol
is present. Two equal blocks therefore have always been equal. Once their
pair comparison is initialized, both directional subset bits are FALSE at
that symbol and remain FALSE while the symbol remains present.

An uninitialized pair has both relevant default bits TRUE; it cannot satisfy
the skip condition requiring one TRUE and the opposite FALSE. Initialization
sets the equal-block pair's two bits together. Earlier outgoing updates cannot
manufacture an asymmetric equality at an uninitialized pair: they visit only
initialized counts, and this class has no proper nonempty range change.
Deleting the symbol removes that clause from its registry instead.

Hence registering a unit does not skip an equal target range. Such a clause
is deleted. A disjoint target range is deleted as a literal, yielding another
unit if the clause was binary. Completed propagation leaves **no nonunit
occurrence at any unit symbol**. This also applies to Boolean clauses of
arbitrary length, since their nonempty singleton ranges cannot change.

### 5.2 No useful first- or second-order restriction survives

Consider any pair of final surviving nonunits. Their actual ranges were
unchanged when the initial pair loop considered both directions. A direct
subsumption or useful SSE1 would have removed the target from the nonunit pool.
Neither can remain. Relevant final units also finish all their propagation,
as shown above.

Now consider an SSE2 opportunity among final surviving nonunits. Their fixed
ranges existed when the manual queue was built. By the independently audited
static coverage case split:

1. A zero-exception donor supplies direct subsumption.
2. A donor with only the restriction-pivot exception supplies stronger SSE1.
3. Two donors with only the intersection-pivot exception supply HLA deletion.
4. Every other useful configuration contains a two-exception donor and is
   visited by the manual queue in one of its pivot orientations.

Cases 1 and 2 are already excluded. In case 4, all three final clauses remain
live at the queued visit, with the same ranges and comparisons; the helper's
registry, exception-count, inverse-containment, and intersection guards pass.
The offered deletion would remove the target from the final nonunit pool, a
contradiction. Case 3 is excluded by complete HLA deletion below.

Unit-donor or unit-target arrangements reduce to the same earlier rules, as
in the unit section of `SSE2_COVERAGE_PROOF.md`. Here the stronger absence of
unit symbols from all nonunits also excludes them directly.

### 5.3 HLA and repeated-call stability

HLA operates on exact fixed live nonunit ranges. Under negation of each
target, its repeated virtual extensions are precisely domain propagation in
the other nonunit clauses. Every newly eligible donor is revisited until
there is no proper restriction or a contradiction is detected. This is the
source-loop argument in `HLA_SATURATION_PROOF.md`.

Later HLA deletions only remove donors and cannot create a refutation that
was unavailable when a retained target was visited. Units create no omitted
case because no nonunit contains their symbols. Thus every final target is
HLA saturated. Since all local rules are saturated and the next invocation
starts from the same stored ranges, no second productive pass is possible.
Only clause sorting may differ after late unit births.

The condition on ranges matters. The existing four-clause witness on two
four-valued symbols in
`../independent_solver/minimized_first_order_phase_sse1.json` consists entirely
of two-symbol clauses but retains a productive first-order rule after a full
production pass. Its overlapping ranges can shrink without a unit birth.
Its collapsed primal graph is a single edge, while its incidence graph has
cycle rank three because all four distinct clauses share both symbols.

## 6. Boolean Horn and ordinary positive-variable implications

Fix a polarity for each Boolean symbol. A Horn clause has at most one positive
literal in those polarities. Every literal deletion and clause deletion
preserves that property.

After initial unit propagation, all unit-symbol occurrences have disappeared
from nonunits by the singleton argument in Section 5.1. Assign each unit its
required value and every remaining symbol the negative/default value. Every
remaining nonunit clause has at least two literals and at most one positive
literal, so it has a negative literal satisfied by this assignment. This is
a model of the residual formula.

Therefore initial unit propagation recognizes every unsatisfiable Boolean
Horn input. The same proof applies to a renamable Horn formula: use its fixed
choice of complemented symbol polarities throughout. The implementation need
not find the renaming for this existence proof.

In particular, clauses `not x or y`, representing implications between
positive variables, together with any positive or negative units satisfy both
the binary saturation theorem and this Horn contradiction-recognition theorem.
Arbitrary **signed-literal** implication graphs instead represent all Boolean
2-CNF and are subject to the counterexample below.

## 7. A precise binary obstruction: opposed chordless implication cycles

All symbols in the following eight-clause formula are Boolean:

```
( x  | a1)
(!a1 | a2)
(!a2 | a3)
(!a3 | x )
(!x  | b1)
(!b1 | b2)
(!b2 | b3)
(!b3 | !x)
```

The first cycle forces `x`: assuming `!x` successively forces `a1,a2,a3`,
and the last clause of that cycle conflicts. The second cycle analogously
forces `!x`. The conjunction is unsatisfiable.

Every clause is indispensable. If a clause in the first cycle is deleted,
set `x=FALSE` to satisfy the second cycle, and satisfy the broken first
implication chain by changing from TRUE to FALSE across its missing edge.
If its first edge is missing choose all `a` values FALSE; if its last edge
is missing choose them all TRUE. The symmetric assignments work when deleting
a clause from the second cycle. Thus deleting any single clause restores
satisfiability, so HLA cannot delete any clause soundly.

There is also no useful SSE1/SSE2 inference. For two-symbol clauses with one
clause per support edge, useful SSE1 needs two clauses on the same two symbols.
Useful genuinely two-donor SSE2 needs a triangle of support edges: resolving
two binary donors at their common symbol must leave their other symbols in
the binary target. If those other symbols coincide, the donors would be
parallel edges; if distinct, the target closes their triangle. Earlier-rule
arrangements have the same parallel-edge obstruction or delete a redundant
target through HLA.

The displayed primal graph has two chordless squares sharing `x`, no parallel
edges, and no triangle. Its incidence graph has two independent cycles. Thus
it is already saturated for every implemented rule while still unsatisfiable.
The production simplifier returns it unchanged, and no number of repeated
applications helps.

The construction extends to opposed cycles of any lengths `p,q >= 4`. A
single such cycle is satisfiable but forces its root literal, which the
implementation does not extract. This supplies the promised pseudoforest
example separating contradiction recognition from complete implication/value
extraction. If either opposed cycle has length three, the two-donor rule can
derive its root unit, and ordinary propagation then refutes the other cycle;
the observed `3+3` and `3+4` controls return FALSE.

This obstruction concerns inference strength rather than update scheduling.
It is distinct from the unbounded useful-pass construction in
`../repeated_passes/PROOF.md`, whose missed rule becomes available only after
a retained nonunit donor changes.

## 8. Executable checks and limits

`probe.py` invokes the unchanged production R kernel in the prepared R 4.6.1
container. It uses direct truth tables and independent set-rule/HLA predicates.

* All 4,096 subsets of the 12 binary clauses over three Boolean symbols were
  tested in both forward and reverse order: 8,192 inputs. Every result was
  truth-equivalent, UP/SSE1/SSE2/HLA saturated, and unchanged in clause/value
  multiset by a second call. All 3,398 unsatisfiable ordered inputs in this
  finite calibration were recognized; the seven-symbol obstruction shows why
  that count cannot establish a general SAT-completeness claim.
* The two-edge path over three ternary symbols was tested under every one of
  its `6^4 = 1,296` nonempty proper range labelings and four boundary-unit
  conditions: 5,184 inputs. All 12 contradictions were recognized. Every
  surviving output passed all local predicates and a second-call check.
  Direct truth tables supplied 13,272 clause-essentiality and 29,019
  literal-value-essentiality checks.
* Opposed cycles of sizes `3+3`, `3+4`, `4+4`, `4+5`, `5+5`, and `4+8` were
  checked by both one-hot SAT and MDD. Each was unsatisfiable, and deleting
  any one clause restored satisfiability by SAT. The four cases with both
  lengths at least four remained unchanged and passed all saturation tests.

Full results are in `probe_results.json`, `probe.log`, and
`opposed_cycles.json`. These checks falsify proposed extensions and calibrate
the arguments; the structural proofs cover unbounded sizes and domains.

`controls.py` adds 1,024 two-symbol cases whose literal ranges are disjoint
two-value blocks, 192 forests containing larger clauses, 2,592 signed Horn
cases spanning all 16 polarity choices and 81 unit boundary conditions, and
1,512 overlapping-range ternary pseudoforests. All relevant claims passed.
The pseudoforest controls also happened to have no residual local rule or
productive second call, but the theorem above deliberately claims only SAT
recognition for the full arbitrary-range pseudoforest class.

`implication_graph.py` separately predicted the exact returned Boolean unit
set and contradiction verdict in 8,238 executions. Its theorem and detailed
results are in `BOOLEAN_2CNF_GRAPH.md` and `implication_graph_results.json`.

No production sources or package metadata were changed, and no commits were
created for this subtask.
