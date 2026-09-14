# Independent root check of the eight-clause/four-variable boundary

2026-09-06. This reviews the new
[`binary_minimal_boundary` proof and enumeration](../binary_minimal_boundary/README.md)
and supplies a second finite selection formulation. The source behavior used
is the previously independently reviewed Boolean 2-CNF graph theorem.

## Verdict and scope

The exact minimum is eight proper binary clauses and four used variables,
attained simultaneously by an inconsistent four-edge parity cycle. Initial
units, duplicate clauses, unused symbols and separate components cannot
reduce the bound. This concerns failure to recognize contradiction, not
changed truth or a missed application of one of the implemented local rules.

I checked the minimal-core reductions and each loop/symmetry restriction in
the independent standard-library enumerator. They cover every required core.
The source proof for unit-containing cores is particularly useful: after
consistent complete Boolean propagation, residual binary clauses are unchanged
original clauses on entirely unassigned variables, hence a subformula of the
satisfiable core-minus-unit remainder. Its model can be combined with the
propagated assignment, contradicting minimality. No general claim of complete
unit propagation for arbitrary CNF is needed.

## 1. Source-independent graph reductions

In a unit-free minimally unsatisfiable core, choose a strongly connected
component containing opposite literals. Reverse-complement symmetry makes
the component self-complementary. Clauses supplying two opposite directed
paths inside it are already contradictory, so minimality places every core
clause and literal inside that component. Any short seed there would reach
opposite literals, so an unrecognized core has no short complement path.

For completeness, the usual implication-component criterion needs only a
short constructive proof. If no component contains opposite literals, order
the components topologically and declare a literal true when its component
has larger rank than its complement's. Exactly one polarity is true. An edge
`a -> b` cannot have true a and false b: the edge and its reverse-complement
edge would give

```
rank(a) <= rank(b) < rank(not b) <= rank(not a) < rank(a).
```

This is impossible. Thus every implication and every clause is satisfied.
Conversely two opposite paths force a contradiction. This discharges the SCC
criterion used by the core reduction without importing a classification of
minimally unsatisfiable formulas.

A globally shortest complement path cannot repeat any variable except its
endpoint variable. A same-literal repetition removes a closed subwalk; an
opposite-literal repetition supplies a shorter complement path. Its length
is therefore at most the number of used variables. Absence of paths of
length one through three gives the four-variable lower bound.

## 2. Review of the first complete finite enumeration

Every variable in a minimal core has both signs, so its support degree is at
least two and `n<=m`. A repeated clause is unnecessary in a minimal core.
Two clauses on the same variable pair must be exact complements: differing
in exactly one sign supplies a two-edge seed. Hence each unordered pair
has multiplicity at most two. Strong connectivity implies connected support.

The enumerator chooses every set of occupied unordered variable pairs and
every subset to double, with the exact requested total edge count. Filtering
to nondecreasing degrees loses no isomorphism class: variable renaming can
always arrange those degrees. It retains all equal-degree labelings, so it
does not need a more delicate canonical graph construction.

For each first clause on an occupied pair, the code enumerates its two
signs, except for fixing each variable's first encountered sign to zero.
Independent variable complementation always achieves these choices. A doubled
pair's second clause is then its exact complement. This construction preserves
the fact that the first encountered sign belongs to the first clause of
its pair, including pairs which are doubled. The subsequent both-sign filter
is necessary and does not restore any discarded symmetry case.

The short-path test advances **exact-length** adjacency sets for lengths
one, two and three and checks the complement at each length. It need not
retain earlier reachability because every length is checked separately.
The truth bitsets cover all assignments with a falsified clause; their union
is the full assignment set exactly when the conjunction is contradictory.
No simplifier output enters either decision.

These observations support the reported complete exclusion: all 95,743
degree-normalized/fixed-polarity signings for `m<=7`, including 3,442 with no
short seed, have zero contradictory survivors. The positive `m=8,n=4`
enumeration and direct R parity examples calibrate the boundary independently.

## 3. A separate clause-selection formulation

`binary_clause_selection.py` instead gives one Boolean selection variable
to every possible proper binary clause on a fixed variable set. It asserts:

1. Every concrete Boolean assignment falsifies at least one selected clause.
   This directly encodes contradiction by full assignment coverage.
2. For every possible two-/three-edge complement path, its supplying clause
   variables cannot all be selected. Proper binary clauses exclude one-edge
   complement paths automatically.
3. The selected clause count is at most the requested bound.

Only one symmetry constraint is used: the clause `(X0=0 or X1=0)` is
selected. A nonempty proper binary formula can always be renamed and have
its variable polarities changed to contain that clause. In `--minimal-core`
mode every one of the exactly n variables must occur with both signs.
The code tests extracted SAT witnesses by a separate graph reachability
calculation and all concrete truth rows.

With Z3 4.12.2 the completed exclusions are:

| Selection scope | Completed result |
| --- | --- |
| Four variables, optional unused padding | At most 4, 5, 6, 7 clauses: UNSAT; 8 clauses: SAT |
| Five variables, all used with both signs | At most 6 and 7 clauses: UNSAT |
| Six variables, all used with both signs | At most 6 and 7 clauses: UNSAT |
| Seven variables, all used with both signs | At most 6 clauses: UNSAT; 7 clauses: timeout, no solver conclusion |
| Eight variables, optional unused padding | At most 4 and 5 clauses: UNSAT; 6 and 7 clauses: timeout, no solver conclusion; 8 clauses: SAT |

The larger initial encoding was less efficient. Its timeouts are recorded
explicitly in JSON/logs and are **not** exclusion evidence. The separate
complete multigraph enumeration has no such unfinished scope.

There is also a small elementary way to close the only missing seven-variable
case of the second formulation. With seven binary clauses, seven variables,
and both signs of every variable, every variable occurs exactly twice, once
with each sign. Connected support is a degree-two cycle (parallel edges give
the two-variable base case). Choose a direction around that cycle and rename
each variable's values so its occurrence in the outgoing edge is negative.
Its occurrence in the incoming edge is positive. Every clause then has the
form `(not x_i or x_next)`, satisfied by setting all variables alike. This
contradicts the assumed unsatisfiability. Thus `m=n` cannot supply a core.

Combining the four-variable selection exclusion, the five-/six-variable
both-sign exclusions, this degree-two argument, and the elementary variable
minimum gives a second route to the same finite bound. No result from the
multigraph signing generator is required for this second route.

The JSONs and logs retain every solver status and measured duration. The
bound is finite computational mathematics with source-independent calibration;
it is not a proof certificate checked by a separate formal proof kernel.
