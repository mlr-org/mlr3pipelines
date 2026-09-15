# Minimum missed contradiction in proper Boolean binary CNF

Under the assumptions of
[`BOOLEAN_2CNF_GRAPH.md`](../structural_classes/BOOLEAN_2CNF_GRAPH.md) and its
independent [review](../boolean_graph_review/REVIEW.md), the exact minima are

* **eight proper binary clauses**;
* **four used Boolean variables**.

One formula attains both minima. Optional initial unit clauses, duplicate
clauses, unused universe symbols, or additional components cannot lower
either bound. Scalar FALSE and formulas containing an empty clause are
excluded from the missed-contradiction question: they are already recognized.

The variable lower bound below is an elementary graph proof. The clause
lower bound combines an elementary core reduction with a complete, ordinary
Python enumeration of a finite superset of possible cores. It uses no SAT
solver or classification theorem for minimally unsatisfiable 2-CNFs. This is
a finite computational proof of the clause bound, not a claimed general
inequality relating clause count to shortest complement-path length.

## 1. A witness attaining both bounds

Use three equality constraints and one contradictory endpoint inequality:

```text
x1 = x2,   x2 = x3,   x3 = x4,   x4 != x1.
```

Expanded as proper binary clauses, this is

```text
(!x1 OR x2), (x1 OR !x2),
(!x2 OR x3), (x2 OR !x3),
(!x3 OR x4), (x3 OR !x4),
(!x1 OR !x4), (x1 OR x4).
```

Its implication graph is a bidirected cycle on eight literal vertices, with
complementary literals at opposite vertices. Thus every literal has distance
exactly four to its complement. There are no initial units and no seeds of
length at most three, so the exact graph characterization gives `K = empty`.
The production simplifier returns the eight clauses unchanged.

The conjunction is contradictory, since the equalities imply `x1 = x4`.
Deleting any single clause leaves exactly one of the 16 Boolean valuations
as a model. Consequently it is minimally unsatisfiable, and a sound
whole-clause deletion cannot remove a clause. The obstruction is recognition
of a global contradiction, not a missing sound clause deletion.

`validate.R` checks these assertions independently through the unchanged
kernel and public `CnfFormula` construction. The same checks passed on
R 3.6.3 and R 4.6.1. Per runtime they include:

* all truth rows of the two-, three-, and four-variable parity cycles;
* all single-clause deletions of those cycles, each with exactly one model;
* Floyd-Warshall distances showing all literal pairs are connected and each
  complement distance is respectively two, three, or four;
* production FALSE for the two- and three-variable cycles;
* unchanged production output for the four-variable cycle under all 16
  variable sign choices and 24 variable renamings (384 inputs), with a
  deterministic shuffled clause order and symbol order for each;
* every one of the 256 within-clause symbol-order choices for the base
  four-variable cycle.

The two runtime logs record production source MD5
`376aac6eb81334e751e9351f9eddc02f`. There are 1,030 production calls per
runtime: 6 baseline/calibration calls, 768 sign/name calls, and 256 symbol-order
calls. No production source or function binding was instrumented or edited.

## 2. Why an unrecognized contradiction has a seed-free binary core

Let F be an unrecognized contradictory list of proper Boolean unit/binary
clauses. Its graph closure K from initial units and short seeds is consistent
by the exact characterization. Choose an inclusion-minimal unsatisfiable
subformula M. This is a finite deletion argument, and does not require an
algorithm for finding minimum-cardinality cores.

### 2.1 A minimal core containing a unit is unit-refutable

Suppose M contains a unit p. The proper remainder `M minus {p}` is satisfiable
by minimality. Perform complete Boolean unit propagation on M from all its
initial units. If it were consistent, every original clause would either be
satisfied by the propagated assignment or survive unchanged on wholly
unassigned variables: a falsified literal in a proper binary clause makes a
unit, and a satisfied literal removes the clause. Initial units are assigned.

The residual clauses are therefore a subformula of `M minus {p}`, restricted
only by discarding clauses already satisfied by the propagated assignment.
A model of the satisfiable remainder, restricted to unassigned variables,
satisfies this residual. Combining it with the propagated assignment gives
a model of M, a contradiction. Hence initial-unit propagation on M refutes M.
Every input unit and propagation edge of M also belongs to F, so F's K would
be inconsistent. Thus **M has no initial units**.

This argument uses complete Boolean unit propagation, already established
for the production class. It does not assume that unit propagation decides
arbitrary satisfiable-base entailments outside Boolean 2-CNF.

### 2.2 All literals of a binary minimal core lie in one SCC

For every clause `(a OR b)`, the implication graph contains the pair

```text
bar(a) -> b,   bar(b) -> a.
```

Consequently every directed path has a reverse-complemented path. The usual
2-SAT implication criterion supplies an SCC C containing x and bar(x),
since M is contradictory. For every literal v in C, paths between v and x
reverse-complement to paths between bar(v) and bar(x). Since x and bar(x)
are already in the same SCC, bar(v) is in C. Thus C is self-complementary.

Choose paths `x -> bar(x)` and `bar(x) -> x` inside C. Collect the clauses
that supply their edges. These clauses alone are unsatisfiable. Every literal
of each collected clause is in C: if the clause supplies `u -> v`, its
literals are bar(u) and v, both in C by self-complementarity. Minimality of M
forces this collected subformula to equal M. Hence all used literals of M
lie in the same self-complementary SCC, and all of its original clauses lie
inside that SCC.

This is not a claim that every contradictory formula has only one SCC. The
minimal-core reduction is essential.

### 2.3 One short seed anywhere in the core would refute it

If M contained a path of length at most three from bar(l) to l, then l would
be a seed of both M and F. From l, the SCC property supplies paths to both
polarities of every used variable of M. All those edges are also present in
F, so its K would be inconsistent. Therefore M has **no** complement path
of length at most three.

It is consequently sound to exclude *all* short seeds in a search for a
minimal missed contradiction. An arbitrary original input F may still have
consistent short seeds elsewhere. The reduction concerns the existence of
M, not equality of the original input with a globally seed-free formula.

Equivalently, the graph detector is monotone under adding clauses: input
units, short seeds, and their reachable literals can only grow. A subformula
refuted by the graph detector cannot be contained in an unrecognized F.

## 3. Elementary lower bound of four variables

In the graph of M, choose a shortest path from **any** literal to its own
complement, minimizing length over all endpoint literals. Write its length
as g. Such a path exists because the graph is self-complementary and strongly
connected. The preceding reduction gives `g >= 4`.

The path cannot repeat an internal variable. If two occurrences are the
same literal, deleting the directed closed subwalk makes a shorter path with
the same endpoints. If they are opposite literals, the intervening proper
subpath is a shorter complement path. The only allowed repeated variable
is the variable at the two endpoints of the entire path.

There are consequently exactly g different variables on this path, so
`number of variables in M >= g >= 4`. The witness in Section 1 attains four.
Unused symbols in an ambient universe do not participate in this count.

## 4. Finite reduction for the clause lower bound

Suppose an unrecognized F has at most seven proper binary clauses, with any
number of initial units. Its core M from Section 2 contains only binary
clauses, so write `m = |M| <= 7` and let n count its used variables.

The following elementary restrictions cover M:

1. Every variable occurs in both polarities. If one were pure, first model
   the proper remainder after deleting all clauses containing it, then set
   that variable to satisfy those deleted clauses. This would model M.
2. Each variable therefore has at least two clause occurrences. Counting
   occurrences gives `2m >= 2n`, hence `n <= m <= 7`. No general deficiency
   theorem is needed. Proper clauses need two variables, so `n >= 2`.
3. There are no duplicate clauses in M; deleting a duplicate preserves
   unsatisfiability. On a fixed pair of symbols, two different clauses may
   differ in one sign or both. Differing in one sign gives `(r OR p)` and
   `(r OR !p)`, a two-edge complement path forcing r. Hence any two clauses
   on that symbol pair must be exact literal complements. At most two of
   the four possible clauses can then occur on the pair.
4. The undirected support multigraph is connected, because all literals are
   in one SCC. It has n vertices and m edges, multiplicity one or two on
   each unordered symbol pair, no loops, and minimum degree at least two.

The enumerator deliberately does **not** impose minimal unsatisfiability or
strong connectivity. It searches the larger class supplied by restrictions
1-4 and then discards every signing with a complement path of length at most
three. If all remaining formulas are satisfiable, no possible M is omitted.

## 5. Exact enumeration scope and symmetry reductions

`checks.py` uses only Python's standard library. For each `1 <= m <= 7` and
`2 <= n <= m`, it performs these finite loops:

1. Choose e unordered distinct symbol pairs, where
   `ceil(m/2) <= e <= min(m, n*(n-1)/2)`.
2. Choose exactly `m-e` of those e pairs to be doubled. Keep connected
   multigraphs of minimum degree at least two.
3. Keep only nondecreasing vertex-degree sequences. Every candidate has
   such a labelling under a permutation of variables; no specific tie
   ordering is required because all pair subsets are enumerated.
4. For each edge, choose the two signs of its first clause; a doubled edge
   gets the exact complementary clause as its second clause. Fix the first
   occurrence of each variable to polarity zero. Each variable can be
   complemented independently, so every signing is equivalent to one with
   these n choices fixed. All remaining `2e-n` binary sign choices are
   enumerated. A variable's first occurrence belongs to a first clause,
   since the second clause of a doubled edge follows its first.
5. Discard signings having a pure variable. Explicitly construct implication
   adjacency sets and advance exact-length reachable sets three times from
   every literal, discarding a signing if any complement is reached.
6. Test every surviving formula against every Boolean valuation. The actual
   implementation unions precomputed bitsets of falsifying valuations for
   each clause and tests whether the union covers all `2^n` valuations.

These symmetry reductions may retain several isomorphic copies; reported
numbers are candidate signings, **not** isomorphism classes. Duplicate
representations can only add work, and do not weaken the exclusion result.

The bitset evaluator is calibrated against a separate ordinary assignment
loop for all 16 clause subsets over two variables. The independent BFS
distance computation, short-path detector, ordinary truth evaluator, and
bitset evaluator also agree on parity cycles of lengths two, three, and four.

Recorded exhaustive results:

| Clauses m | Multigraphs | Signings after fixed first polarities | Both polarities used | No short seed | Unsatisfiable survivors |
| --- | ---: | ---: | ---: | ---: | ---: |
| 1 | 0 | 0 | 0 | 0 | 0 |
| 2 | 1 | 1 | 1 | 1 | 0 |
| 3 | 1 | 8 | 1 | 1 | 0 |
| 4 | 5 | 58 | 9 | 7 | 0 |
| 5 | 21 | 544 | 57 | 41 | 0 |
| 6 | 118 | 6,564 | 577 | 389 | 0 |
| 7 | 725 | 88,568 | 4,953 | 3,003 | 0 |
| Total | 871 | 95,743 | 5,598 | 3,442 | 0 |

`results.json` and `checks.log` also retain the separate (m,n) counts. The
search completed in approximately 1.9 seconds on the recorded run.

A separate positive enumeration with `m=8, n=4` finds 24 contradictory
candidate representations among 264 short-seed-free signings. Its 14
multigraphs have 1,472 fixed-polarity signings, of which 1,376 use both
polarities of every variable. `results_eight_four.json` stores three example
representations. The 24 count includes representational duplications and
is not asserted to count distinct formulas or isomorphism types.

Thus the finite search excludes every core required by an unrecognized
input with at most seven binary clauses. The eight-clause witness closes
the bound, including for inputs which additionally contain initial units.

## 6. A tempting path-count shortcut is invalid

Do not add the lengths of opposing complement paths and call that the number
of distinct clauses. A clause supplies two implication edges, and opposite
paths can share a clause or use both of its edges. For example,

```text
(x OR y), (!x OR y), (!y OR z), (x OR !z), (!x OR !z)
```

is minimally unsatisfiable with five clauses. A contradictory directed cycle
can contain both implication edges supplied by `(!y OR z)`. The calibration
checks that this example is contradictory and every clause is indispensable.
The enumeration proof avoids any clause-disjointness assumption on paths.

As research context, the full graph classification is developed by
[Abbasizanjani and Kullmann](https://arxiv.org/html/2003.03639). No classification
result from that paper is used for the bounds above; the finite proof uses
the explicitly stated elementary reductions.

## 7. Reproduction

From the repository root:

```sh
python3 attic/cnf_verify3/binary_minimal_boundary/checks.py
python3 attic/cnf_verify3/binary_minimal_boundary/checks.py \
  --min-clauses 8 --max-clauses 8 --min-variables 4 --max-variables 4 \
  --output results_eight_four.json
Rscript attic/cnf_verify3/binary_minimal_boundary/validate.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/binary_minimal_boundary/validate.R
```

The current-R launcher reuses the existing isolated environment described in
[`R46_ENVIRONMENT.md`](../review_semantics/R46_ENVIRONMENT.md). The saved logs
are `validate_r36.log` and `validate_r46.log`. No package source, package
tests, or other audit stream was changed by this work.
