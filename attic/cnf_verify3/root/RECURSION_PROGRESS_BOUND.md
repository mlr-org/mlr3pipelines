# Recursive helper depth is charged to actual simplification progress

Independently reviewed source argument, 2026-09-06. This concerns the unchanged canonical
finite-set kernel, fixed universe, ordinary R set/copy semantics, and valid
execution of its local operations. It is a bound on active **local helper
frames**, not on bytes, elapsed time, all R evaluator frames, or an environment's
particular C/node-stack threshold. The measured growing-chain failures remain.

Let `W0` be the initial total number of original membership-fiber occurrences
as defined in `DOMAIN_INDEPENDENT_PASS_BOUND.md`. The bound is

```
active apply_domain_restriction frames <= W0 + 1
active local helper frames             <= 6 * W0 + 11.
```

The same argument can use the signature-class potential `Phi0` from
the independently reviewed `../pass_bound_review/PROOF.md`. Consequently a
coarse clause-count-only bound follows by substituting
`Phi0 <= m + m * 2^(m^2 + m)`. These constants are deliberately loose.

## 1. A nested restriction requires an earlier actual decrease

Consider an active call A to `apply_domain_restriction` which eventually
contains another still-active call B to that helper. A cannot take any of
these paths:

* Its requested symbol is absent: immediate FALSE return.
* Its local intersection covers the restricting range: the whole target is
  deleted, and the helper returns NULL. The deletion helper calls no other
  inference or restriction helper.
* Its local intersection equals its old range: immediate FALSE return.

The remaining paths both make a strict actual change before reaching B:

* A nonempty strict intersection is committed to `entries` before any
  comparison repair can call another inference helper.
* An empty intersection calls `eliminate_symbol_from_clause`, which removes
  that symbol and commits the shortened entry before it can call
  `register_unit` or a comparison callback. If the clause instead becomes
  empty, that helper returns TRUE and has no restriction descendant.

All clauses and ranges in this argument are the actual stored objects,
with canonical unique symbol names. A temporary local intersection which
leads directly to clause deletion is not counted as a committed range change.
An absent-symbol request from an older unit-propagation snapshot is legitimate
but is among the immediate-return paths, so cannot escape the argument.

The lifecycle premise is needed here: an actual target is still live at its
committed restriction/deletion. The four restriction call sites and the
guards reviewed in `../proof_state/LIFECYCLE_PROOF.md` supply that premise.
Between restriction entry and its first actual write, only leaf set evaluation
occurs; no inference callback can independently eliminate its target.

Each surviving range change removes a whole original membership fiber.
Actual clauses and ranges never grow and a deleted clause never becomes
live again. Therefore the current live fiber potential at B's entry is
strictly less than at A's entry. If another nested operation has since
deleted A's target, the decrease is only larger. Unit merges and further
restrictions cannot restore the removed occurrence.

Apply this to each pair of consecutive active restriction frames. Their
entry potentials form a strictly decreasing sequence of nonnegative
integers starting at most at `W0`; it has length at most `W0 + 1`.

The proof does **not** claim that every restriction call changes something,
or that each sibling invocation decreases the potential. A final no-op call
can be deepest on the stack. It also does not charge every cached-bit update
as an independent physical change.

## 2. Replace the fiber potential by the signature-class potential

The reviewed freezing lemma says that a symbol in an initial comparison
signature class of size at least three cannot undergo an actual range
change. Every surviving range change from Section 1 must therefore affect
one of the initially small signature classes. Its whole-fiber removal is
counted by `Phi = live clauses + fibers at small-class symbols`.

Whole-clause deletion also decreases Phi even if the clause contains only
frozen symbols. All intervening operations preserve monotonicity. The same
strictly decreasing active-frame sequence therefore works with Phi, yielding
`active restriction frames <= Phi0 + 1`. No execution-preserving replacement
of a large symbol group by a smaller group is assumed.

## 3. Other local helpers add a finite constant factor

Count only the 13 helper closures belonging to one kernel invocation, with
fixed finite input objects or ordinary promises to those objects. Arbitrary
caller-supplied argument evaluation that launches unrelated code or another
kernel invocation is outside this graph.

The source defines 13 local helpers. Every cycle in their call graph contains
`apply_domain_restriction`. Removing that vertex leaves an acyclic graph,
whose longest path contains five local helper vertices. A longest path is

```
eliminate_symbol_from_clause
  -> on_updated_subset_relations
  -> handle_sse_2nd_order_twoend
  -> try_sse_2nd_order
  -> char_union.
```

The oneend handler gives another path of the same length. The finite source
graph is extracted and checked in `recursion_progress.R` and saved as
`recursion_call_graph.json`.

R's lazy arguments need an explicit addition to a purely syntactic graph.
The `char_union(...)` restricting expression is passed by `try_sse_2nd_order`
and forced through the `y` argument of `char_intersect` inside the restriction
helper. Thus `char_intersect -> char_union` is included as a possible dynamic
leaf edge. It adds no cycle and does not increase the longest apply-free path
beyond five. The other deferred helper arguments contain only local reads,
indexing, constants and ordinary scalar/set operations; none can reenter a
nonleaf inference helper. No helper is passed as an arbitrary callback or
rebound to an inference function.

With r active restriction frames, split a helper stack immediately before
and after each such frame. There are at most `r+1` apply-free segments, each
of length at most five, plus those r frames. Therefore its length is at most
`5*(r+1)+r = 6*r+5`. Substituting `r <= W0+1` gives `6*W0+11`.
The top-level `simplify_cnf` frame can be added separately as one more frame.

## 4. What this excludes, and what it does not

The argument excludes an inference-helper recursion cycle that can deepen
without any actual clause/range progress. Combined with the signature-class
potential, it excludes unbounded local-helper depth from increasing domain
and symbol counts while holding initial clause count fixed. It is compatible
with both recorded linear-depth families, whose clause counts grow.

It does not promise that R's finite stack is sufficient for the resulting
bound, which can be enormous. It does not cover recursion in arbitrary custom
S3 methods, external mutation, printing large objects, malformed accepted
selectors, failed primitive/index operations, or constructor distribution
outside this kernel. Ordinary R evaluator and observation-hook frames are not
counted as local helpers. A complete allocation/time bound is a separate task.

This helper argument also does not by itself prove termination of every loop.
The prior finite-descent and single-use HLA-donor proofs handle those separate
loop obligations. The present result isolates the recursive part and makes
its progress requirement explicit.

## 5. Executable checks

`recursion_progress.R` creates an observation-only copy by adding entry/exit
hooks to all 13 local helper bodies. It records the true active helper path,
checks the strict fiber-potential decrease at every nested restriction entry,
checks both depth inequalities at every helper entry, and compares every
observed output identically with the original function in the same universe.
The actual `entries` and eliminated flags supply the potential; no callback
count is substituted for physical progress.

Inputs include the reduced nested-comparison control, forward/reversed unit
and common-guard chains through length 48, and 600 independently seeded
finite-domain cases, half with an explicitly planted model. Both R 3.6.3 and
R 4.6.1 completed the same **625 cases**, with **5,736 nested restriction
checks**, maximum **47 active restriction frames**, and maximum **141 local
helper frames**. All depth inequalities and exact observed/original output
comparisons passed. These checks test the source argument; they do not
establish the unbounded conclusion by sampling.

Two harness mistakes were caught before any case counts were accepted: an
oversized integer seed and a missing closing brace. Both were fixed in the
research script. No production change or production failure is attributed to
them.

The completed [independent review](../pass_bound_check/RECURSION_REVIEW.md)
audits every restriction path, target liveness, and the deferred union edge.
Its separate observer follows actual R frames in 31 cases: 2,267 helper
entries, 362 nested restriction checks, and nine observations of the deferred
`char_intersect -> char_union` edge. Its maximum depths were 12 restriction
frames and 36 local helpers. The review confirms the bounds with their
one-invocation, ordinary-promise, and finite-operation scope; it does not
assert a bound on arbitrary caller computation or all R evaluator frames.
