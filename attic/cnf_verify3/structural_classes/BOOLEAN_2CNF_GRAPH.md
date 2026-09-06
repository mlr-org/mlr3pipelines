# Exact unit and contradiction characterization for Boolean 2-CNF

This refines the binary saturation theorem in `PROOF.md`. It applies to the
same unchanged source and canonical, normal-execution assumptions, with
exactly two values per symbol and at most two symbols per clause. Its graph
is built from the **input**, before any simplification.

## Statement

Write `bar(l)` for the complement of Boolean literal `l`. For every input
binary clause `(a or b)`, put the edges

```
bar(a) -> b,       bar(b) -> a
```

in a directed graph `G`. Let `I` be the set of initial unit literals and let

```
S = {l : G has a path from bar(l) to l of length at most 3}.
K = every literal reachable in G from I union S,
    including the starting literals themselves.
```

Then:

1. The production simplifier returns FALSE **if and only if** `K` contains
   both polarities of some symbol.
2. Otherwise, its final set of unit literals is **exactly** `K`.

Thus it looks for short paths to obtain new unit seeds, and then propagates
those seeds through paths of arbitrary length. It does not add resolution
edges that shorten longer paths from a literal's complement to itself.
The final unit set and contradiction verdict are independent of clause order
within this class. HLA may still choose different redundant nonunit clauses
to retain.

## 1. The retained binary clauses never change

Every actual literal range is a Boolean singleton. Restricting a range either
does nothing or removes its whole literal, immediately making the binary
clause a unit. The unit then propagates completely. The equal-unit skip is
impossible for immutable singleton ranges, as shown in `PROOF.md`, Section 5.1.

Before HLA, the only ways an input binary clause can cease to be a live
binary clause are:

* elimination because it duplicates another binary clause;
* elimination because a unit satisfies one of its literals;
* conversion to a unit on one of its original literals, by propagation,
  SSE1, or SSE2.

In particular, there is no new binary edge and no changed surviving binary
edge. For a genuine twoend SSE2 call, one donor has a nonempty range on the
restriction symbol. The inverse guards require both donors to omit the
target's Boolean value there. The union therefore contains only the opposite
value, so the call removes the literal and creates a unit. It cannot take
the `empty restringent` clause-elimination route.

This also removes the contextual-cache complication of general finite-domain
executions. Every comparison between two live binary clauses describes
unchanged actual ranges. A literal deletion takes the unit branch of
`eliminate_symbol_from_clause()` and returns before its nonunit matrix-update
callbacks. Nested unit propagation can delete clauses or create further
units, but cannot change a surviving binary range or its comparison. Thus the
raw set premises used below are justified throughout this class, not merely
after a quiescent boundary.

## 2. Every created unit lies in K

Initial units are in `I`. A propagation-created unit follows an input
implication edge from an earlier unit, so induction preserves membership in
`K`.

SSE1 derives `r` from `(r or t)` and `(r or bar(t))`. These clauses give a
two-edge path

```
bar(r) -> t -> r,
```

so `r` is itself in `S`.

Consider a unit `r` created by a genuine SSE2 call on target `(r or t)`.
Choose its twoend donor to contain `bar(t)`, and let `s` be the intersection
symbol. The relevant cases are elementary because all ranges are singletons.

* If the other donor's non-intersection literal is `r`, the donors have the
  form `(p or r)` and `(bar(p) or bar(t))`. Along with the target they give

  ```
  bar(r) -> p -> bar(t) -> r.
  ```

  Hence `r` is in `S`.
* If that donor instead also has `bar(t)`, the donors have opposite literals
  at their intersection symbol and share `bar(t)`. They already provide a
  two-edge seed `bar(t)` by SSE1. The target edge `bar(t) -> r` places `r`
  in `K`. This case also covers an intersection symbol equal to the target's
  surviving symbol: the required containment guard forces the opposite
  donor values there, yielding the same two-clause unit seed.

The off-pivot containment premises allow no other symbol in the second
binary donor. A repeated donor fails the intersection guard. Thus every
SSE2-created unit is in `K`, even in a configuration where a stronger SSE1
would have supplied the unit earlier.

Unit constraints never disappear or change polarity on a noncontradictory
execution. Duplicate units merge without a change; opposite units return
FALSE. Consequently, a returned contradiction implies that `K` contains
opposite literals. If a nonconstant result is returned, all its unit literals
lie in `K`.

## 3. The final units are closed under every input implication

Let `U` be the consistent set of units just before HLA. Completed Boolean
propagation leaves no unit symbol in a live nonunit clause. Every original
binary clause either still has a duplicate representative, or was removed by
a satisfied unit, or produced a unit on one of its own literals. Unit values
cannot subsequently change on a noncontradictory execution.

Therefore each original clause either is already satisfied by `U`, or has
both symbols unassigned by `U`. It cannot have a false literal and an
unassigned other literal: propagation would create the other unit. It cannot
have both literals false without reporting contradiction. This is exactly
closure of `U` under both input implication edges.

The graph also has contraposition symmetry: a path `a -> b` gives the
reversed complemented path `bar(b) -> bar(a)`, because every clause supplies
both edges.

## 4. Every short-path seed is obtained

Suppose `l` is in `S` but not in `U`, and choose a path

```
bar(l) = v0 -> v1 -> ... -> vk = l,     k <= 3.
```

No variable appearing on this path can be assigned by `U`. If `vi` is in
`U`, forward closure gives `l`. If `bar(vi)` is in `U`, contraposition of the
prefix gives a path from `bar(vi)` to `l`, again yielding `l`. Either would
contradict its assumed absence from `U`.

Hence every clause supplying an edge of this path is still represented just
before HLA. None could have been satisfied or turned into a unit, because that
would assign one of the path variables; duplicate deletion retains the same
edge. All these remaining binary clauses have their original ranges.

A proper canonical binary clause never has an edge between two literals on
the same symbol. Thus a path of length two has exactly the two clauses of the
SSE1 pattern above. A path of length three uses three distinct symbols and
has the clauses

```
(l or p),     (bar(p) or q),     (bar(q) or l).
```

Using the last as target, the middle is a twoend donor and the first is a
oneend donor. Their intersection-pivot ranges are disjoint, their off-pivot
literals agree with the target, and both inverse guards at the restriction
pivot pass. The manual queue must visit this unchanged, still-live triple.
It derives `l`.

The first-order pair loop and the static second-order queue therefore exclude
both possibilities for a missing seed. So `S` is contained in `U`. Since `I`
is contained in `U` and `U` is closed under `G`, it follows that `K` is
contained in `U`. Together with Section 2, this gives `U = K`.

If `K` contains opposite literals, no noncontradictory normal execution can
reach this boundary. It must have returned FALSE earlier. This proves both
directions of the contradiction criterion.

## 5. HLA cannot alter the established unit set

No nonunit contains any unit symbol. A unit target's initial virtual clause
has only its unit symbol, while every possible nonunit donor has two other
symbols. All donor exception counts therefore start at two. Unit HLA has no
first extension to apply and cannot delete that unit.

Nonunit HLA deletes entire clauses, never creates units, and never returns a
new contradiction verdict. Hence the pre-HLA set `U = K` is the final set.

## 6. The cycle boundary and validation

A forcing cycle with `p` clauses gives a path of length `p` from the root
literal's complement to the root. If two opposed cycles share only that root
and both have length at least four, neither supplies any seed in `S`; there
are no initial units. Thus `K` is empty, and the simplifier returns the
unsatisfiable nonunit formula unchanged. If either cycle has length three,
its root seed propagates through the other cycle and reaches its complement,
so the implementation reports FALSE even when the other cycle is long.

`implication_graph.py` computes `S` by a bounded graph reachability loop and
`K` by a separate ordinary worklist; it reads no production caches or events.
It compared both the complete returned unit set and the contradiction verdict
in 8,238 current-R executions:

* all 4,096 three-symbol binary-clause subsets;
* each subset again in reverse order with a rotating choice among all 27
  assignments of absent/positive/negative initial units;
* all 36 pairs of opposed forcing-cycle lengths from three through eight;
* satisfiable and contradictory reverse implication chains of lengths
  4, 8, 16, 32, and 64.

All predictions agreed, including 4,851 contradiction verdicts. The checks
counted 27,660 short-seed occurrences and 36,989 closure-unit occurrences.
Results are in `implication_graph_results.json` and `implication_graph.log`.
The earlier direct truth-table and SAT/MDD runs separately checked the
production semantics and the unsatisfiable cycle obstruction.
