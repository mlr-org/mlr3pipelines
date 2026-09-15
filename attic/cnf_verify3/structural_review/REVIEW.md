# Independent review of four structural CNF theorems

Reviewed 2026-09-06. The claims reviewed are incidence-forest saturation,
incidence-pseudoforest contradiction recognition, Boolean renamable-Horn
contradiction recognition, and equal-or-disjoint-range binary saturation in
`../structural_classes/PROOF.md`. The associated UP, comparison, SSE2, and HLA
source paths were inspected directly, alongside the cited static SSE2/HLA
proofs. No production source was modified and no commit was made.

The checked simplifier has SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
Executable checks used the installed package described in
`../execution_modes/NOTES.md`: R 4.6.1 (2026-06-24), package `0.11.0.9000`.

## Conclusions

All four stated results hold under the assumptions below. I found no
counterexample or missing logical case within their stated scope.

| Class | Independently justified conclusion |
| --- | --- |
| Incidence forest | Initial UP either refutes or leaves every actual clause and every literal value indispensable. Thus all later sound deletion/restriction rules are saturated. |
| Incidence pseudoforest | Initial UP either refutes or leaves a formula with a constructive model. Thus contradiction recognition is complete. |
| Boolean renamable Horn | Initial UP either refutes or leaves the usual default-polarity model. The implementation need not discover the renaming. |
| At most two symbols per clause; all ranges at each symbol pairwise equal or disjoint | Final UP/SSE1/SSE2/HLA saturation holds, and a second call cannot make productive progress. This does not give complete contradiction recognition. |

The stronger forest irreducibility statement does not extend to a single
incidence cycle. Binary saturation without the range condition also fails.
The existing opposed-cycle example correctly separates local saturation from
contradiction recognition. These boundaries were independently replayed.

## Exact assumptions and meanings

The input to the kernel is a fixed finite-domain formula with one nonmissing
symbol name per clause entry, one entry per symbol, and nonempty unique
character ranges contained properly in nonempty unique finite domains.
The universe and domain memberships remain fixed throughout a call or a
repeated-call comparison. Constants have already been normalized. The
implementation claims assume normal completion and ordinary R string/set
equality and index behavior.

For forests and pseudoforests the graph is the bipartite incidence graph of
the **stored** formula: each distinct stored clause occurrence has its own
vertex. A unit is a degree-one clause vertex. Duplicate clauses are not
silently merged when deciding this premise. For binary clauses, the equivalent
primal graph must be a multigraph: two clauses with the same two symbols form
an incidence cycle. Collapsing parallel support edges would enlarge the input
class incorrectly.

For the binary theorem, equality or disjointness is required between every
pair of input ranges of a given symbol, including ranges in units. Distinct
input blocks need not have equal cardinalities or exhaust the domain. Different
symbols may use entirely different partitions. The theorem requires at most
two **symbols**, not two individual values, per clause.

Horn means Boolean singleton literals with at most one positive literal per
clause after one fixed choice of polarity for each symbol. It does not mean
arbitrary finite-domain implications or arbitrary signed Boolean 2-CNF.

Saturation means absence of useful actual clause/value deletions under the
listed UP/SSE1/SSE2 predicates and domain-propagation refutation under each
target's negation. HLA here is that latter predicate, not arbitrary semantic
entailment. A final sort-only call may change raw order without changing the
clause/value multiset. None of these conclusions covers the malformed
accepted selectors in `../r_values/NOTES.md`, arbitrary nested-constructor
normalization, or externally changed universes.

## The initial-UP invariant is sufficient

The source first merges initial units, then processes nonunits in length
order. A processed nonunit is registered at each live symbol; a later unit
registration recursively propagates its current effective range into that
processed prefix. A clause not processed yet is intersected with the current
unit domains on its own main-loop turn. No comparison matrices exist during
this stage, so the later cached skip cannot occur.

`apply_domain_restriction()` intersects a literal with the effective unit.
If that intersection equals the effective unit, the whole clause is subsumed
and deleted. Otherwise any surviving literal is a proper subset of the unit.
An empty literal is removed; a resulting unit immediately propagates. Unit
merges retain their intersection and either preserve a nonempty range or
report contradiction. Finiteness and monotone deletion ensure these recursive
updates terminate on a normally returning call.

Consequently the completed initial stage has one nonempty unit range per
constrained symbol, and every residual nonunit literal is nonempty and proper
inside its symbol's effective allowed domain. For an unconstrained symbol
that domain is its original domain. Updates only remove incidence edges or
clause vertices, so they preserve forest and pseudoforest membership.

This proves exactly what the structural arguments need. It does not assume
that the same properness invariant holds after every later cached update in
unrestricted inputs.

## Forest proof, including units and individual values

In a nonunit incidence tree, root at any symbol with any prescribed allowed
value. Each clause has a parent symbol and at least one child symbol. If the
parent already satisfies it, choose its children arbitrarily; otherwise choose
one child from that clause's nonempty range. Tree branches cannot assign the
same symbol twice. This extends the prescribed root value to a full model.

Removing a target nonunit separates each of its incident symbols into
different remaining components. Choose outside its literal range at each of
those symbols; properness ensures these values exist. Independent extensions
satisfy all other clauses and units while falsifying the target. To show a
particular value indispensable, instead fix its symbol to that value and
all other target symbols outside their ranges. The target then holds through
exactly the selected value.

For a target unit, deleting it broadens its symbol's allowed domain back to
the original domain. All nonunit ranges remain nonempty inside the broadened
domain. A root value outside the deleted unit and the same extension argument
show clause indispensability. A root value inside the unit shows each of its
values indispensable. Other units constrain different symbols and remain
part of the allowed domains. This is the step most likely to be obscured if
the original effective domain at the removed unit is incorrectly retained.

The residual forest is therefore satisfiable and has no equivalent result
obtainable by one actual clause or literal-value deletion. Since all actual
later mutations consist of sound deletions/restrictions, none can start.
This gives both initial-stage contradiction recognition and all advertised
first-pass saturation properties, independently of scheduler completeness.

## Pseudoforest proof

Every clause of a residual nonunit pseudoforest has degree at least two.
In a tree component, root at a symbol and match every clause to one child
symbol. Each symbol has only one parent clause, so these matches are distinct.
In a component with a cycle, match cycle clauses bijectively to the next
cycle symbols, then orient attached trees away from that cycle and use the
same child matching. Off-cycle clause degree at least two guarantees an
available child, and no attached branch can reuse a cycle symbol.

Choose each matched symbol from its matched clause's nonempty range, and
unmatched symbols arbitrarily from their allowed domains. Every clause has
its own satisfied literal, and all units are respected. Thus a nonconstant
residual pseudoforest is always satisfiable, even with overlapping ranges
and clauses wider than binary. This matching argument does not imply that
every allowed root value extends, so it does not justify the stronger forest
essentiality conclusion.

## Horn proof

In the initial-UP invariant above, a nonunit range cannot be a nonempty
proper subset of a Boolean unit singleton. Hence no unit symbol remains in
a residual nonunit. This follows already from the initial stage; it does not
need the later cached equality-skip argument.

Fix the renaming that makes the input Horn. Literal and clause deletion
preserve that property. Give every unit its required value and every other
symbol its negative/default value in the fixed polarity. Each residual
nonunit has at least two literals and at most one positive one, so it has a
satisfied negative literal. This is a residual model. It proves complete
contradiction recognition without requiring the implementation to compute
the renaming or to extract every logically forced literal.

## Why the binary range theorem survives the source schedule

For a symbol partition block `B`, intersection with any other input block
is either `B` or empty. Intersection with a union of two blocks has the same
property. Therefore UP, SSE1, and SSE2 never leave a proper nonempty shrink.
A binary clause either keeps both original ranges, is deleted, or loses a
whole literal and becomes a unit. An effective unit stays its original block
or conflicts with a disjoint unit. HLA changes virtual clauses only.

This yields a stronger local source invariant than the general cache proofs:
every pair of nonunits that is live at the end had those exact same ranges
whenever compared earlier. The unit branch of `eliminate_symbol_from_clause()`
returns before its nonunit comparison-update callbacks. Therefore no update
of a still-live binary pair can leave stale comparisons, and its initial
pair-loop comparisons are the exact raw block comparisons throughout.

The cached unit skip requires an asymmetric pair of subset bits. Equal
blocks have two FALSE bits when initialized; a not-yet-initialized shared
symbol has two TRUE bits. The pair loop initializes both directions together
before callbacks, and this class has no later proper nonempty shrink that
could manufacture asymmetric equality. A deleted symbol leaves the registry.
Thus equal blocks cannot be skipped. Equal ranges delete a propagated target
clause; disjoint ranges delete its literal. At completion no nonunit contains
any unit symbol.

A final pair with subsumption or useful SSE1 would have had the same rule
when its initial pair was considered and would have left the live pool.
For SSE2, write each donor's exceptional symbols relative to the target.
The full set-premise split is:

- An empty exceptional set is subsumption.
- Only the restriction pivot exceptional gives the stronger SSE1 rule.
- Only the intersection pivot exceptional in both donors gives HLA deletion:
  extending by the complement of one donor contains the second donor.
- Otherwise one donor has both exceptional pivots. Its pair is in the
  count-two manual queue; the other donor is in the intersection-symbol
  registry, and both pivot orientations are attempted. Usefulness makes both
  inverse-containment guards TRUE, and the intersection premise is exactly
  the source intersection guard.

All final participants remain unchanged and live at the relevant queued
visit, so the last case cannot have been enabled only after that visit.
Unit participants cannot add a missed case: their symbols occur in no
nonunit; two distinct units are on different symbols; and an SSE2 unit target
would require its symbol in a nonunit donor or a previous rule already
excluded. This completes the scheduler exclusion without assuming general
SSE2 saturation.

For HLA, complement the virtual target ranges into possible-value domains.
An exception count of one is exactly a one-possible-literal donor, and its
virtual extension is the associated domain intersection. Final live binary
pair comparisons at HLA entry are exact. Every newly contained range updates
all registered donors, including used donors, and a zero count immediately
deletes the target. A once-used donor cannot later make another proper
restriction: its other literals remain impossible and its chosen domain is
already inside its range. It can later conflict, which the zero-count test
still catches. Thus the repeated count-one scan reaches the full propagation
fixed point or refutes.

Earlier targets' virtual changes affect only their own target rows; later
actual HLA deletions only remove donors. Removing donors cannot create a
new refutation for an earlier retained target. Units are irrelevant to these
nonunit checks because their symbols are absent from nonunits; unit HLA has
no first count-one donor. Hence final HLA saturation follows. With all local
rules saturated and all actual ranges fixed, a second invocation has no
productive operation; only the initial length sort may change storage order.

## Boundary examples verified independently

The single four-clause cycle

```
(x | a1), (!a1 | a2), (!a2 | a3), (!a3 | x)
```

has four models, all with `x=TRUE`. Production leaves it unchanged and all
implemented local predicates are saturated. Deleting the `a1` literal from
the first clause gives the unit `x` and preserves its complete truth table.
Thus incidence-pseudoforest membership does not imply literal-value
indispensability or complete forced-value extraction. The example is also
renamable Horn: take `x=FALSE` as x's positive polarity and each `ai=TRUE`
as an internal symbol's positive polarity. Horn contradiction recognition
therefore likewise does not mean complete forced-value extraction.

Joining this cycle to its opposite-polarity copy on three fresh internal
symbols yields the known eight-clause opposed-square formula. It has no
models, but the installed package returns all eight clauses unchanged and
the independent local/HLA predicates find no reduction. Removing any one
clause restores satisfiability. Its connected incidence graph has cycle
rank two, so it does not contradict pseudoforest recognition.

The saved four-clause, two-four-valued-symbol overlapping-range example
`minimized_first_order_phase_sse1.json` was separately replayed. After one
call it contains

```
(X1=0 | X0=5), (X0=0 | X1 in {4,0}).
```

The first clause permits SSE1 removal of `X0=0` from the second. The next
call makes that deletion while preserving all 16 input valuations. Its
input incidence cycle rank is three, despite its collapsed primal graph
being one edge. This falsifies the proposed extension to arbitrary finite
binary ranges and the use of a collapsed simple graph to claim foresthood.

## Independent executable evidence

`check_structural.R` constructs ordinary public clause/formula objects in
the installed package. A research-only copy of its namespace function is
truncated immediately before the `available = ...` assignment, returning
the end of initial UP. No namespace binding or production file is changed.
Both that prefix and the ordinary public result are compared with an
independent positional-OR/scalar-membership truth-table evaluator.

The independent rule checker enumerates donor/target set premises, including
repeated SSE2 donors. The HLA checker repeatedly propagates possible domains
under a target's negation. Neither uses the author's Python predicates or
production matrices/events. Direct rule/refutation controls calibrate them.
Graph cycle ranks are calculated by a separate union-find implementation;
pseudoforest certificates use a general augmenting-path matcher rather than
the proof's tree/cycle matching construction.

| Generated class | Inputs | Full assignments across inputs | Unsatisfiable inputs | Additional checks |
| --- | ---: | ---: | ---: | --- |
| Forests | 240 | 25,336 | 38 | 196 include wide clauses; 585 clause and 1,127 value-essentiality checks |
| Pseudoforests | 240 | 12,817 | 66 | 88 include wide clauses; matching models for all 174 noncontradictory residuals |
| Renamable Horn | 400 | 12,792 | 92 | 378 include wide clauses; fixed-polarity default model checked after UP |
| Equal/disjoint binary | 300 | 118,811 | 201 | Multi-value blocks; final blocks and surviving original binary clauses checked |

All contradictions in the three SAT-recognition classes were found by the
initial prefix. Forests and equal/disjoint binary outputs passed the fresh
UP/SSE1/SSE2/HLA and repeated-call checks. Binary contradictions were not
required to be recognized, consistent with the theorem's scope.

Including the three boundary controls, there are 1,183 cases and 169,916
assignment rows, each checked against both the initial prefix and full public
output. The script performs 1,726 ordinary public formula constructions,
including second-call controls, plus 1,183 prefix-copy executions. All passed.
The additional single-cycle semantic deletion/Horn-polarity check is in
`boundary_checks.R`. These are finite falsification checks supporting the
source arguments; they do not replace the unbounded structural proofs.

Results are saved in `checks.log`, `checks.json`, `checks.rds`, and
`boundary_checks.log`. Run from the repository root:

```
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/structural_review/check_structural.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/structural_review/boundary_checks.R
```

The separate exact Boolean-2-CNF implication-graph characterization is not
needed for any of these four conclusions. This review does not claim to have
exhausted wider Horn saturation or arbitrary-range pseudoforest saturation;
neither is asserted by the reviewed theorem table.
