# Independent review of the exact Boolean 2-CNF graph theorem

Reviewed 2026-09-06 against
[BOOLEAN_2CNF_GRAPH.md](../structural_classes/BOOLEAN_2CNF_GRAPH.md) and unchanged
`R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
All new artifacts are in this directory. Production files are unchanged.

**Verdict:** the graph characterization is valid for a normalized list of
proper Boolean units and binary clauses, on normal return. The four principal
obligations all admit source-level proofs; no Boolean counterexample was
found. Two qualifications should be explicit in the statement:

* Scalar FALSE, or an input containing a normalized FALSE clause, needs its
  own immediate-FALSE case. Its binary implication graph can be empty, so
  the literal statement "FALSE iff K contains opposite literals" cannot
  apply to that representation without this exception. The sibling's graph
  oracle already special-cases scalar constants.
* Raw cache exactness concerns **initialized comparisons between still-live
  binary clauses**, not all allocated entries. Uninitialized default TRUE
  bits need not equal the raw relation. A unit's former binary row also keeps
  irrelevant removed-symbol columns. Neither qualification harms the proof.

The Boolean restriction matters. Executable multivalued examples below
falsify a direct extension of the graph criterion and show a created unit
being removed by final HLA outside the Boolean class.

## 1. Scope and formulation used in this review

Every symbol has exactly two distinct values. Write a literal as a signed
symbol l and its unique opposite as bar(l). A nonconstant clause has one or
two literals on distinct symbols; its ranges are nonempty proper Boolean
ranges, hence singletons. The list may contain duplicates. TRUE clauses have
been removed; a FALSE input is handled separately as above. The empty list
is TRUE and has no graph seeds or units.

For each input binary clause `(a OR b)`, G contains

```
bar(a) -> b,    bar(b) -> a.
```

Let I contain every initial unit literal. Define S by a path of at most three
edges from bar(l) to l, and K as reachability closure of I union S in G,
including its starting vertices. The graph concerns original input clauses,
not the changing list of surviving clauses.

The claimed result is:

```
FALSE is returned iff K contains l and bar(l) for some l;
otherwise the final set of represented unit literals is exactly K.
```

Unit *literals* are sets here. Duplicate unit clause indices can be eliminated
while an equivalent representative survives. The theorem does not preserve
particular unit indices, output order, or every nonunit clause.

The normal-return conditions include finite canonical ordinary R lists and
character values, unique registered symbol names, a fixed universe, ordinary
set/membership and replacement behavior, live-operand call guards, and
sufficient runtime allocation, indexing, and stack capacity. The earlier
resource probes exhibit reversed Boolean chains that fail normally supported
runtime stack limits; they do not refute a conditional normal-return theorem.
This review is a source argument plus executable checks, not a mechanized
semantics of the R evaluator.

## 2. The structural source invariant

Before HLA, every live binary clause is an unchanged original binary clause.
An actual target range is a singleton. Intersecting it either leaves the
singleton unchanged or empties it. Removing one literal from a binary clause
immediately takes the singleton/unit branch at lines 244-254 of the source;
that branch returns before the nonunit comparison-update callbacks. No
nonempty proper range shrink is possible.

This observation needs a classification of whole-clause deletion, because
`apply_domain_restriction` can return NULL instead of creating a unit:

1. With a unit restricting range, matching target polarity satisfies and
   deletes the clause. Opposite polarity removes the target literal and
   creates the other unit.
2. Direct subsumption between live Boolean binary clauses is possible only
   when they are duplicates: their two singleton literals must both match.
3. SSE1 restricts the donor's sole exceptional symbol. If the symbol is
   absent from the target, the helper returns FALSE immediately. If present,
   the singleton must be opposite to the donor and is removed. A same-valued
   present literal cannot be the exceptional symbol.
4. A genuine queued SSE2 call has a two-exception binary donor containing the
   restriction symbol. The inverse guards force its value, and any value of
   the other donor there, to oppose the target. Their union is consequently
   the nonempty opposite singleton. Thus this call creates a unit; it cannot
   hit the empty-restricting-range whole-clause deletion branch.

These are all physical mutations before HLA. A deleted binary clause was
therefore duplicated, unit-satisfied, or converted to one of its original
literal units. No new binary clause or binary implication edge is created.

The relevant live-operand premises come from the pair-loop guards, each
second-order handler's target/donor iteration guards, and removal from both
symbol registries before registering a newly formed unit. There is no
callback between the final handler guard checks and the raw tests in
`try_sse_2nd_order`. Initial preprocessing cannot encounter an already-created
unit at an unprocessed index: only the processed prefix is in the registry.
The live registry therefore supplies binary targets to propagation, and the
SSE helpers infer from live nonunit donors and targets.

### 2.1 Exactness required by the singleton argument

For two still-live binary clauses, all their values remain original. Their
pair comparison is initialized from those actual values, and neither clause
can later change a nonempty range while remaining binary. Consequently every
initialized such comparison and its count stays raw exact. Uninitialized
counts are NA, and the candidate lists exclude those entries. This avoids
the contextual FALSE-bit complications of wider or multivalued formulas.

The unit skip deserves its own proof: exactness between live binaries alone
would not justify using a newly converted unit's former row. The skip looks
only at its **retained symbol**. That singleton has never changed. For a
target containing the same symbol, an initialized equal pair has both
directional bits FALSE and an initialized opposite pair has both TRUE. An
uninitialized pair has both default bits TRUE. Initialization writes both
directions before any callback; this class has no nonempty range-shrink path
that could create an asymmetric bit at that retained symbol. Unit conversion
does not alter its retained value. Thus the skip's required TRUE/FALSE
asymmetry cannot occur for this target-symbol pair.

If either relevant row is not yet available, the skip is disabled by the
existing index guards. Removed-symbol columns in a former unit row are not
used in this argument. In particular, this is not a claim that such a row is
an exact representation of the whole shortened unit.

Every already registered live clause at a unit symbol is therefore visited;
any not-yet-processed clause is checked against current units when inserted.
A visited singleton is either satisfied and deleted, or falsified and removed,
creating another unit. Registrations happen immediately and pending recursive
work completes on normal unwinding. Later phases introduce no new binary
occurrences. It follows that, just before HLA:

```
no live binary clause mentions any represented unit symbol.
```

On a noncontradictory execution a unit polarity cannot change: a duplicate
merges with no range change; an opposite unit returns FALSE. The literal
constraint remains represented throughout the pre-HLA phases.

## 3. Upper bound: every produced unit belongs to K

Induct chronologically over unit registration, including a candidate that
immediately contradicts an older unit. Initial units are in I. Propagation
from literal a through an original binary clause `(bar(a) OR b)` registers b,
which follows one input edge from an earlier unit in K. Reachability closure
places b in K.

An SSE1-created unit r comes from exactly

```
(r OR p),    (r OR bar(p)).
```

These original clauses give `bar(r) -> p -> r`, so r itself belongs to S.
The absence of changing binary values is what permits referring to the
original graph here.

For SSE2, let the target be `(r OR t)`, let s be the intersection symbol, and
let t's symbol be the restriction symbol. A two-exception donor B contains
both pivots and has literal bar(t). The other donor A contains s. The helper's
off-pivot exception test permits no other symbol outside the two pivots and
the target's surviving symbol; an extra symbol would be an uncovered third
exception. Its inverse test excludes t itself from both donors.

There are exactly two cases, including the possible coincidence of s with
the surviving symbol:

* If s is distinct from r's and t's symbols, the target has no range on s.
  The raw intersection guard forces the two donor literals there to be
  opposite; call them p and bar(p). If A's other literal is r, the three
  clauses are `(p OR r)`, `(bar(p) OR bar(t))`, `(r OR t)`. They provide
  `bar(r) -> p -> bar(t) -> r`, so r is a three-edge seed. If A's other
  literal is bar(t), the two donors already give the two-edge seed bar(t),
  and the target supplies `bar(t) -> r`.
* If s is r's symbol, the two-exception donor is `(bar(r) OR bar(t))`.
  The intersection guard forces A's value on s to be r. Its only possible
  other literal is bar(t), giving `(r OR bar(t))`. Again the two donors give
  the two-edge seed bar(t), followed by the target edge to r. In fact this
  configuration also contains a direct SSE1 seed for r from A and the target,
  so it need not survive the earlier pair stage to be covered by the bound.

A repeated donor fails the intersection guard: its exceptional value on s
lies outside the target and intersects itself. No missing restriction range
case exists because B contains the restriction symbol. This completes the
case split for every actual SSE2 unit birth.

Every registration is therefore in K. In this Boolean binary class the
only nonconstant-input contradiction route is an empty intersection of
opposite unit singletons. A binary literal deletion becomes a nonempty unit
before any further propagation; the live-operand guards do not submit an
existing unit to a nonunit restriction helper. Hence an early FALSE return
implies that K contains opposite literals. The separate scalar FALSE input
case is essential to this sentence.

## 4. Original input edges remain sufficient

Let U be the consistent set of units just before HLA. The structural mutation
classification supplies a certificate for each original binary clause C:

* If C is still live, it is an unchanged binary representative.
* If it was removed as a duplicate, follow the duplicate representative that
  was live at the deletion. Repeating this step follows strictly later
  deletion events, so the finite chain ends at a live representative or one
  of the next two cases.
* If unit propagation deleted it as satisfied, that satisfying literal is
  still in U.
* If it became a unit, that unit is one of C's original literals and is
  still represented in U, possibly by an older equal unit index.

Therefore every original clause either has a literal in U or still has an
identical binary representative. In the latter case both its symbols are
unassigned by U, by the completed-propagation result.

This directly proves closure under every input edge. For `(a OR b)`, suppose
bar(a) is in U. The clause cannot be represented with both symbols unassigned.
It must instead be satisfied by U, and consistency excludes a, leaving b in U.
Thus `bar(a) -> b` is respected; exchange a and b for the other edge. This
argument accounts for deleted edges without pretending they physically remain
in the current clause graph.

It is deliberately a **pre-HLA** certificate. HLA may later remove a binary
clause without a satisfying unit; the exact unit set has already been
established at that point. No final-graph edge-preservation premise is needed.

## 5. Lower bound: every path of length at most three supplies its seed

Every input edge has its reverse complemented partner, so a path a -> b
supplies a path bar(b) -> bar(a). Suppose l is a short-path seed but l is
absent from U, and choose a path

```
bar(l) = v0 -> v1 -> ... -> vk = l,    k <= 3.
```

No variable on this path can be assigned in U. If vi belongs to U, forward
closure gives l. If bar(vi) belongs to U, contraposition of the prefix
`bar(l) -> vi` gives `bar(vi) -> l`, again yielding l. Either contradicts
the assumption. This argument includes the endpoints.

Every path edge's source clause consequently has an unchanged live binary
representative before HLA: a unit-satisfied or converted clause would assign
one of the path variables. Duplicates retain the same edge. These
representatives were also live and unchanged throughout the earlier pair
and second-order stages, because inactive binary clauses never return.

A proper binary clause has distinct symbols, so no input edge connects two
values on the same symbol. Length zero cannot join opposite literals and
length one is impossible here. At length two, the internal variable differs
from l's; the path gives exactly the SSE1 pair from Section 3. Whichever
representative is processed later, the initial pair loop fills both counts
and attempts the useful one-exception restriction. Its continued survival
as a binary pair is impossible. Thus such a seed cannot be missing.

At length three, the two internal symbols differ from one another and from
l's symbol: adjacent symbols differ, and each internal endpoint is adjacent
to an endpoint on l's symbol. Write the path's clauses as

```
A = (l OR p),    B = (bar(p) OR q),    T = (bar(q) OR l).
```

Here is a direct check of the actual queued call, beyond a general static
coverage citation:

1. B has exactly two exceptions relative to T, on p's and q's symbols. Its
   initialized B,T count is two when the manual queue is built, so that
   ordered pair is included in `sse_to_trigger` at line 640.
2. B and T are still live at the queued visit by the assumed final survival.
   The visit sets the enabled bit and calls the twoend handler. No frozen
   queue entry needs to be newly created: these fixed clauses already had
   exactly the required count at queue construction.
3. Choose p as intersection and q as restriction. Both orientations are
   tried; this one has the restriction symbol present in T. The p registry
   contains A, and its initialized A,T count is one.
4. A's exception on p is among B's two exception symbols. Its absent q
   column is ignored by the match-with-nomatch-zero subset-of-columns test.
   The count equality is therefore exactly one equals one.
5. T's q literal is bar(q), while A lacks q and B has q. Both inverse
   not-subset guards are TRUE. A and B have opposite p values, so the raw
   intersection-outside-target test passes.
6. The offered restricting union is `{q}`. Its intersection with T's
   `{bar(q)}` is empty; deleting it registers l.

Any callback that had earlier removed one of these three clauses would
contradict their supposed survival. A callback cannot merely change a range
and leave a surviving binary clause for the queued visit. Thus the dynamic
nonunit-range scheduling gaps found in broader finite-domain formulas cannot
invalidate this unchanged-triple argument.

The missing-seed assumption is impossible, giving S subset U. Initial units
also remain in U, and U is closed under G; hence K subset U. Section 3 gives
U subset K, so U = K. If K contains opposite literals, no consistent normal
execution can reach this boundary. It must have returned FALSE earlier.
Together with the upper bound this proves both directions of the verdict.

## 6. Final HLA cannot remove an established Boolean unit

Every surviving nonunit is binary and omits every unit symbol. Nonunit HLA
only removes clauses; it cannot introduce a unit-symbol occurrence or create
a unit. When unit HLA begins for a unit on s, all possible nonunit donors
have two symbols different from s. Their initial exception count is exactly
two. The repeat loop searches for count one and has no first donor to use.
It performs no extension and no unit deletion. This applies independently
to each unit, including units created late by SSE2.

Thus the pre-HLA equality U = K is also the equality of the final unit set.
The result is independent of input clause order because I, S, and K are.
No corresponding assertion of an order-independent nonunit clause set follows.

## 7. Independent executable checks

`checks.py` uses Boolean adjacency bit matrices. It computes paths of lengths
one through three by multiplication by the original adjacency relation and
computes unrestricted closure by Warshall elimination. It does not import
the sibling graph oracle, its BFS, caches, or event records.

The local enumeration instantiates every ordered pair/triple of proper
Boolean binary clauses over two, three, and four symbols and applies the
source helper's actual exception, inverse, and intersection guards. Its
productive cases have support on at most three symbols, as also proved above;
the fourth symbol checks rejection of extra-symbol arrangements. Results:

* 40 direct-subsumption instances, all duplicates;
* 80 useful SSE1 instances, all two-edge seeds, and 240 absent-pivot no-ops;
* 16,448 twoend orientations considered, with 560 productive SSE2 patterns;
* 240 of those give the direct three-edge-seed form and 320 give the
  two-edge-seed-plus-propagation form; 80 have intersection equal to the
  target's surviving symbol, and 480 use a third symbol;
* every restricting union is the nonempty opposite singleton; every
  repeated-donor and extra-symbol attempted exception is excluded.

`bridge.R` separately instruments an in-memory production function object.
At nine helper entry sites and before HLA, it checks that every live binary
is identical to its original sorted input entry and that every initialized
live-binary comparison/count is raw exact. It records each candidate unit
registration and the pre-HLA formula. The Python side verifies each birth
against K, every original-clause representative/unit certificate, every input
implication edge, all short seeds, and equality of units before/after HLA.
Each instrumented result is also compared with an uninstrumented production
call on the same input. No production source or global production binding
is altered.

Both R 3.6.3 and R 4.6.1 passed the same 3,088 executions, with exactly matching
logical and invariant counts:

| Check | Count per runtime |
| --- | ---: |
| Every sign, clause order, and per-clause symbol order of the two-/three-edge seed patterns | 416 inputs |
| Seeded random inputs on 3-9 symbols, including units and duplicate binary clauses | 2,000 inputs |
| Reversed orders of every third random input | 667 inputs |
| TRUE, FALSE, empty clause list, duplicate units, opposite units | 5 inputs |
| FALSE verdicts matching the corrected graph criterion | 1,263 |
| Unit candidates, all in K | 12,260 |
| Original binary clause representation certificates | 11,049 |
| Input-edge closure checks | 22,098 |
| Pre-HLA boundaries with every short seed present | 1,822 |
| Unit literals preserved through HLA | 4,610 |
| Observed helper/boundary snapshots | 83,884 |
| Unchanged live-binary checks | 743,884 |
| Exact initialized live-binary comparison/count checks | 2,224,998 |
| Final live binaries omitting all unit symbols | 2,355 |

The direct short-path tests all returned exactly the predicted root unit,
including all within-clause name orientations. These are scheduling checks
of the production loop, not only algebraic pattern checks.

Run from the repository root:

```sh
python3 attic/cnf_verify3/boolean_graph_review/checks.py
python3 attic/cnf_verify3/boolean_graph_review/checks.py --r46
Rscript attic/cnf_verify3/boolean_graph_review/scope_examples.R
podman exec cnf-review-r46 Rscript attic/cnf_verify3/boolean_graph_review/scope_examples.R
```

The current-R command uses the existing container. Saved `results_r36.json`,
`results_r46.json`, and matching logs retain exact counts. The scope examples
also pass on both runtimes. Their checks are described next.

## 8. Limits: Booleanity, recognition, and local saturation

**The graph criterion is not a finite-domain theorem with arbitrary
complement-range vertices.** Let x,y each have domain `{0,1,2}` and use the
three clauses `(x=i OR y=i)` for i = 0,1,2. They are contradictory: a valuation
can satisfy clauses through at most its two selected values. Production
returns FALSE. In a direct range-implication graph, every edge runs from a
two-value complement range to a singleton on the other symbol. No target
vertex is a source vertex; no path has length two, and no edge can reach its
own symbol's complementary literal. With no initial units, S and K are empty.
`scope_examples.R` checks the graph shape and the production verdict.

**A non-Boolean derived unit can disappear in HLA.** Let x be ternary and y,q
Boolean. The input is

```
(x in {0,1} OR q=0),  (x in {0,1} OR q=1),
(x=0 OR y=0),         (x=1 OR y=1).
```

The first two clauses create the unit `x in {0,1}` by SSE1. It is present at
the HLA boundary. The last two clauses retain proper nonempty x ranges inside
that unit, so their initial unit-HLA exception counts are one. The first
donor can extend the virtual unit by y=1, after which the second subsumes it;
the unit is deleted. Production returns exactly the last two clauses, which
already entail the broad x unit. Instrumented births/boundary, final output,
and all 12 finite valuations verify this. The Boolean proof prevents this
configuration precisely because a proper nonempty subset of a singleton
unit range cannot exist.

**Exact K is not the set of all entailed units.** A four-edge forcing cycle

```
(x OR a1), (!a1 OR a2), (!a2 OR a3), (!a3 OR x)
```

is satisfiable and entails x, but has no seed within three edges and no
initial unit. Production leaves all four binary clauses and no unit. The
scope script independently checks a model exists, every model has x=TRUE,
and the production clauses are unchanged.

**Exact K also does not decide satisfiability of all Boolean 2-CNF.** Oppose
that cycle by an analogous four-edge cycle forcing !x through b1,b2,b3. K is
empty, yet the eight-clause conjunction is contradictory. Production leaves
it unchanged. All 128 valuations verify contradiction, and deleting any one
clause restores a model, excluding any sound whole-clause deletion. Its
triangle-free, parallel-edge-free support excludes productive SSE1 and SSE2
under the classification above. It is therefore compatible with complete
local saturation and still fails SAT recognition.

The graph theorem characterizes unit extraction and the implementation's
FALSE verdict. The separate local-saturation theorem additionally uses
duplicate elimination and complete HLA deletion; unit-set equality alone
does not prove all deletion rules saturated. Neither result claims complete
semantic entailment. The source's existing scheduling defects outside this
immutable Boolean binary class, public constructor defects, and resource
limits remain separate issues.
