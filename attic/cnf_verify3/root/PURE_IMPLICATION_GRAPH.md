# Exact graph behavior for pure Boolean implications

Independently reviewed source argument for the unchanged simplifier,
2026-09-06. A pure implication
clause is `(X_i=0 OR X_j=1)` for distinct Boolean symbols, representing the
directed edge `i -> j`. Allow any finite list of these clauses, including
duplicates, with a fixed two-value universe and canonical ordinary storage.
The empty edge list is TRUE. All claims assume normal completion.

## 1. The kernel performs greedy deletion of transitively redundant edges

Before HLA the only possible actual mutation is duplicate-clause deletion.
Here is the independent reviewer's direct source argument, which does not
need the general semantic theorem. Before a hypothetical first other change,
all surviving clauses remain their original pure implications and pair
comparisons are exact. Direct subsumption requires identical literals.

An SSE1 count-one pair shares one equal literal. The remaining donor literal
has the same opposite polarity as the target's other literal. Its symbol is
therefore absent from the target: otherwise the clauses would be identical
or an endpoint repeated. The requested restriction is an immediate no-op.

For SSE2 write the target literals as `t=v, r=1-v`. A candidate twoend donor
has `t=1-v, s=v`, by its inverse guard and opposite literal polarities. Its
exception at s and the intersection guard require the other donor's s
literal to be `s=1-v`; that donor's other literal is `q=v`. The other inverse
guard excludes `q=t`, and distinct endpoints exclude `q=s`. Outside {s,t},
the only possible target literal has value `1-v`, so containment of `q=v`
is impossible. The candidate premises contradict each other. No successful
SSE2 restriction occurs, including the helper's whole-clause-deletion route.

Both uniform assignments satisfy all the pure implications. They additionally
protect every literal against a sound strengthening, but alone would not
exclude deletion of a redundant whole clause; the source classification above
is needed for that stronger prefix statement.

The pair stage retains the first occurrence of every duplicate and all
remaining clauses keep their two original literals. All relevant live-pair
comparisons at HLA entry are exact.

For a target edge `u -> v`, negating its clause fixes `u=1` and `v=0`.
Propagation through the other implication clauses refutes these assumptions
if and only if those edges contain a path from u to v:

* A path propagates value one from u through to v, conflicting with v=0.
* If there is no path, assign one exactly to the vertices reachable from u
  and zero to the others. No edge leaves the reachable set, so this assignment
  satisfies all other implications and still falsifies the target edge.

The reviewed HLA/domain-propagation theorem identifies exactly that refutation.
Thus HLA deletes an edge exactly when the remaining live edges give an
alternative path between its endpoints. All clauses have equal length two,
so the stable decreasing-length HLA traversal is their input order after
duplicate removal. This is precisely the independent algorithm in
`pure_implication_graph.R`: traverse the edges in that order and delete an
edge whenever its endpoints remain connected by a directed path without it.

The proof uses exact HLA refutation completeness, not an assumption that
arbitrary semantic redundancy is generally recognized by the full CNF kernel.
For this fragment, semantic implication of an edge happens to be reachability.

## 2. Every returned clause and literal is indispensable

Each deletion preserves reachability. A retained edge has no alternative
path when visited; deleting more edges later cannot create a path. Hence the
final graph preserves original reachability and no remaining edge can be
deleted without changing it. The reachable-set assignment above is an
explicit distinguishing valuation for deleting any final clause.

Removing either literal of a final binary clause makes it a unit. The two
uniform assignments still satisfy the original and final formulas, and one
of them falsifies that unit. Every literal value is therefore indispensable
as well. This establishes stronger irreducibility than merely passing the
implemented local-rule checks. A second simplification call cannot make any
productive change; all widths are two, so it cannot reorder this result either.

## 3. Acyclic inputs have an order-independent clause set

For a directed acyclic graph, reachability is a strict partial order. Its
cover pairs are precisely pairs `u < v` with no intermediate w satisfying
`u < w < v`. Each cover pair must be an original edge: any longer witnessing
path would supply an intermediate vertex. That edge cannot be removed from
any reachability-preserving subgraph.

Every non-cover comparable pair has a path made of cover edges, by repeatedly
splitting through an intermediate vertex in the finite order. The mandatory
cover edges remain available throughout HLA. Every non-cover input edge is
therefore deleted whenever it is visited; every cover edge is retained.
The output clause **set** is independent of input order and is exactly this
unique cover-edge graph. Output storage order can still differ.

This is an exact positive class in which semantic confluence of the returned
clause set can be proved. It does not extend to general cyclic implication
graphs or to arbitrary clauses with two literals of the same polarity.

## 4. Genuine normal-form differences remain even after full saturation

For a complete directed graph on `n >= 2` Boolean symbols, the input expresses
that all symbols agree. Let H be any subgraph with the same reachability
from which no edge can be removed while preserving that reachability.
Place all edges outside H first in the input and H's edges last. At every
early HLA visit, H is still available and supplies an alternative path;
all outside edges disappear. When H's edges are visited, none is removable.
Production therefore returns exactly H.

Two families show that this is not the recorded missed-scheduling phenomenon:

* Any directed Hamiltonian cycle is such an H and has n clauses. Fixing its
  first vertex and permuting the others gives `(n-1)!` different returned
  clause sets for the same input edge set, all exact fixed points.
* The bidirected star is also such an H and has `2*(n-1)` clauses. Deleting
  either direction on a leaf edge prevents that leaf from reaching or being
  reached by the center. It too is an exact fixed point with every clause
  and literal indispensable.

For n greater than two the star is larger than a cycle. Thus complete local
saturation and individual-clause irredundancy do not imply a minimum clause
count, unique representation, or input-order-independent output. The package
does not promise those properties; these are limitation controls, not new
semantic defects. They clarify the difference from a returned formula which
still admits a useful rule explicitly implemented by the simplifier.

At n=3 there are exactly five minimal strongly connected simple directed
graphs: the two directed triangles and the three bidirected two-edge trees.
A minimal graph with three edges must have one incoming and one outgoing
edge per vertex, giving a triangle. A graph with four or more edges either
contains a triangle, making extra edges removable, or is one of the three
bidirected trees; five edges necessarily contain a triangle.

## 5. Independent executable controls

`pure_implication_graph.R` compares ordinary public `CnfFormula` construction
with an independent Warshall-reachability greedy deletion algorithm. A
separate private source prefix returns the state immediately before HLA and
checks that only duplicate edges disappeared. Every input/output truth table
is evaluated, every remaining clause receives a deletion witness, and every
output is simplified again to check stability.

The bank contains every ordered subset of the six directed edges on three
vertices (1,957 calls), three input orders for every four-vertex directed graph
(12,288 calls), chosen cycle/reverse-cycle/star outputs through eight vertices,
and a duplicate-edge control. For every acyclic four-vertex graph the three
orders must give the same edge set. For the complete three-vertex graph all
720 orders must yield exactly the five normal forms described above.

Both R 3.6.3 and R 4.6.1 runs passed: 14,261 cases, 213,400 assignment rows,
1,696 DAG cases, and 57,726 clause-essentiality checks per version. The saved
complete-three-vertex histogram includes the 720 permutations plus three
chosen family controls: 723 executions total, with counts 241, 241 for the
triangles and 80, 80, 81 for the trees. It is not a 720-execution probability
distribution. The initial final assertion compared a named vector against
an unnamed expected vector; that research-harness error was corrected with
`unname()` and both full runs repeated successfully.

The [independent review](../fixed_graph_review/REVIEW.md) verifies the source
and graph arguments, including the direct polarity proof above. Its separate
bank adds 1,755 graph cases on each R version, including all ordered edge
multisets through length four on three vertices and 200 cases with many
duplicate edges on four to six vertices. Finite controls and universal source
proofs are separate evidence; no production source is changed.
