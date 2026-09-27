# Independent review of the local-helper recursion bound

Reviewed 2026-09-06 against `../root/RECURSION_PROGRESS_BOUND.md`,
`../root/recursion_progress.R`, and the unchanged production source
`R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
This is separate from the productive-pass review in `REVIEW.md`.

## Verdict and scope

The proposed bounds are justified under the stated finite-set/source
assumptions:

```
active apply_domain_restriction frames <= W0 + 1
active local helper frames             <= 6 * W0 + 11.
```

The original signature-class potential Phi0 can replace W0 in both formulas.
Consequently the previously reviewed bound
`Phi0 <= m + m * 2^(m^2 + m)` supplies clause-count-only dependence.

I found no substantive mathematical obstruction. Two clarifications were
sent to the author and incorporated while this review was in progress:

* Phi is an alternative potential, not uniformly a smaller one. If all
  symbols are in small classes, `Phi = W + live clause count`.
* A physical write decreases the **live** potential only if its target is
  still live. The source's call-site guards and the earlier lifecycle argument
  establish this needed premise; it should not be silently assumed.

The counted frames are executions of the 13 local helper closures belonging
to one invocation, entered through the inspected source paths. The top-level
kernel activation is one additional frame. The finite universe and input
objects are given values, or ordinary promises to such values; arbitrary
caller-supplied argument computation, custom methods, or externally induced
nested kernel calls are outside this local source graph. This does not bound
all evaluator/C frames, stack memory, or runtime.

The charging argument is valid for a finite execution prefix while the local
set/index operations behave as assumed. It does not require the entire call
to succeed before its earlier stack states can be analyzed. A resource failure
can occur below this very loose mathematical upper bound. The proof does not
assert the absence of the observed stack-limit failures.

## 1. Actual progress precedes every nested restriction activation

Let A be an active restriction-helper frame and B the next restriction
frame below it on the active helper stack. A must have entered a path which
can reach another inference helper before returning.

Direct source inspection gives the complete alternatives:

| A's path | Source location | Can have a restriction descendant? |
| --- | --- | --- |
| Requested symbol absent | 148–149 | No: immediate FALSE return. |
| Intersection covers restricting range | 152–157 | No: the clause-deletion helper changes flags/registry only, then A returns NULL. |
| Intersection has old range length | 159–160 | No: immediate FALSE return. |
| Empty intersection empties the whole clause | 161–163, 236–239 | No: return TRUE and propagate contradiction. |
| Empty intersection leaves at least one symbol | 161–163, 236–251 | Yes, after committing removal of a present nonempty range at 244. |
| Nonempty strict intersection | 165–231 | Yes, after committing the strict restriction at 167. |

The local intersection computed before a whole-clause-subsumption return
is not an actual write and is not used as the charge. Likewise the empty
clause which causes a contradiction need not be committed. Neither branch
contains a deeper restriction activation.

The surviving two cases each remove at least one actual value occurrence
before any descendant restriction can be entered. The value-range assignment
precedes comparison callbacks. Literal removal precedes both unit registration
and comparison callbacks. A range is nonempty before its literal is removed
because the input and live storage are canonical.

### The target is live when that write occurs

This is the additional premise needed to convert a physical deletion into a
decrease of a potential counting only live entries. The four call sites of
`apply_domain_restriction()` supply it:

* Recursive unit propagation at 135 explicitly skips eliminated snapshot
  entries at 128. An entry converted to a different-symbol unit can still be
  encountered, but then the requested symbol is absent and A immediately
  returns. It cannot create an unpaid descendant.
* Initial nonunit processing at 506 does not register future entries early.
  Elimination or unit conversion of the current target returns NULL and breaks
  its symbol loop; contradiction exits the kernel. A surviving nonempty
  restriction cannot recurse while its comparison matrices are absent.
* First-order restriction at 322 is reached from guarded live nonunit
  operands in `on_updated_subset_relations()`. The count-zero and count-two
  branches return before this first-order call.
* Second-order restriction at 467 has a live target from the handler guards
  and their per-candidate rechecks. Between those checks and the restriction
  there are only set/lookup operations.

The full caller audit is in `../proof_state/LIFECYCLE_PROOF.md`; I checked
these relevant paths against the actual source. Once A enters, its path to
the first committed write executes only leaf set operations or the immediate
literal-removal code. No other inference can eliminate the target in between.
The independent executable check below also asserts a live target at every
observed restriction entry.

### Charging consecutive active frames

Every current actual range is a union of original membership fibers. A strict
live restriction or literal removal therefore removes at least one whole
fiber occurrence. Actual ranges never gain values, unit merging retains an
old clause and intersects its old range, and deleted clauses never return.
Any additional work between A's write and B's entry can only lower the live
fiber potential further.

Therefore the live potential at B's entry is strictly lower than at A's
entry. This remains true if B or another nested operation later eliminates
A's target: no removed occurrence is restored. The decrease is associated
with an ancestor's own earlier committed change, rather than with a count
update or an assumed change by the deepest frame.

Apply this to consecutive restriction activations on the current stack.
Their entry potentials form a strictly decreasing sequence of nonnegative
integers whose first element is at most W0. Thus there are at most `W0 + 1`
such frames. A deepest no-op frame is allowed. No assertion is made that
sibling calls all decrease the potential or that the total number of helper
calls is bounded by this active-depth argument.

## 2. Lazy evaluation adds a leaf edge, not an unpaid inference cycle

The main R-specific risk is confusing syntactic calls with the dynamic stack
while promises are forced. The proposed proof correctly addresses the one
relevant helper-valued deferred expression:

```
try_sse_2nd_order(...)
  apply_domain_restriction(..., char_union(...), FALSE)
    char_intersect(..., restringent)
      char_union(...)
```

The restricting union is not forced merely by entering the restriction
helper. It is evaluated when the intersection needs its second argument.
At that point both the restriction and intersection frames remain active.
The dynamic adjacency `char_intersect -> char_union` must therefore supplement
the direct syntactic adjacency from the trial helper to `char_union`.

This delayed computation cannot call another restriction: `char_union`
contains only concatenation, subsetting and membership. Its x/y promises
read ranges from the trial frame; they contain no further nonleaf helper
call. All other helper arguments in the inspected source are literal values,
indices, list/environment lookups, or ordinary scalar/set expressions.
No local inference helper is passed as a callback or invoked through a
rebinding, `do.call`, or deferred arbitrary function argument. The delayed
`roe_inverse` expression later in HLA only performs a base `match`.

Consequently no new restriction frame can appear during A's pre-write set
evaluation, and adding the deferred leaf edge accounts for the relevant
extra local-helper stack depth. Merely observing a direct static call graph
without this argument would have been insufficient.

## 3. The acyclic remainder gives the claimed constant

The source defines exactly 13 local helpers. Removing the restriction helper
leaves the following conservative graph. Edges shown include both syntactic
calls and the deferred intersection-to-union edge:

| Helper | Remaining local callees |
| --- | --- |
| `char_intersect` | `char_union` |
| `char_setdiff` | none |
| `char_union` | none |
| `return_entries` | none |
| `register_unit` | none |
| `eliminate_symbol_from_clause` | `register_unit`, `on_updated_subset_relations` |
| `on_updated_subset_relations` | either second-order handler, `eliminate_clause_update_sr` |
| `on_update_range` | either second-order handler |
| `handle_sse_2nd_order_oneend` | `try_sse_2nd_order` |
| `handle_sse_2nd_order_twoend` | `try_sse_2nd_order` |
| `try_sse_2nd_order` | `char_union` |
| `eliminate_clause_update_sr` | none |

It is acyclic. Its longest path has five vertices:

```
eliminate_symbol_from_clause
 -> on_updated_subset_relations
 -> a second-order handler
 -> try_sse_2nd_order
 -> char_union.
```

Some retained syntactic edges overapproximate where deferred evaluation
actually runs; an overapproximation is safe for the upper bound. The added
`char_intersect -> char_union` path has only two vertices and cannot increase
the maximum beyond five.

With r active restriction frames, the remaining active helpers split into
at most `r + 1` segments before, between and after them. Each segment follows
the acyclic graph and contains at most five helpers. Hence

```
helper_depth <= 5 * (r + 1) + r = 6 * r + 5
             <= 6 * W0 + 11.
```

The r = 0 case is covered as well: initial unit processing, HLA's local
set calculations, and a terminal return do not create a new inference cycle.
No assertion of tightness is needed. In the independent finite control, the
longest observed segment contained four helpers, which does not establish a
universal improvement of the conservative constant five.

## 4. Substitution of the signature-class potential

The previously reviewed first-change lemma freezes every symbol in an
initial comparison-signature class of size at least three. Thus A's required
strict surviving range write must be at an initially small-class symbol.
Its whole-fiber removal is counted by

```
Phi = live clause count + original fibers at small-class symbols.
```

All other source operations preserve monotonicity of Phi. A whole-clause
deletion reduces its first term even when the deleted clause consists only
of frozen symbols. The same consecutive-entry argument gives

```
r <= Phi0 + 1
helper_depth <= 6 * Phi0 + 11.
```

This substitution relies on the source-level freezing lemma, not on any
execution-preserving replacement of groups by two symbols. Phi can be less
than W when many frozen symbols occur; it can also exceed W because of its
clause-count term. Both bounds are valid, so their minimum may be used when
both input-specific quantities are known. The clause-only upper bound follows
from the independently reviewed finite signature count.

## 5. Review of the author's experiment

`../root/recursion_progress.R` extracts the local helper definitions and a
conservative syntactic call graph, explicitly adds the deferred leaf edge,
and rejects any cycle in the apply-free remainder. Its longest-path count
is five. Each observed function body gets a balanced entry/exit observer;
the manual active stack records the entry potential of restriction frames.

The potential is computed from current `entries[!eliminated]`, using original
membership fibers. It is not approximated by recursion count or cache events.
The test checks strict decrease against the nearest already-active restriction
frame and checks the helper-segment bound at every helper entry. It also
compares the observed function's result identically with the ordinary one
using the same universe. The entry instrumentation does not force the
restriction's deferred union earlier than production needs it.

I inspected the saved results; I did not rerun this larger campaign. Both
its R 3.6.3 and R 4.6.1 reports record 625 cases, maximum helper depth 141,
maximum 47 active restrictions, and 5,736 nested-entry comparisons. The script
checks W directly. The Phi substitution comes from the theorem and receives
additional direct experimental coverage below.

## 6. Independent exact-source frame control

Run from the repository root:

```
Rscript attic/cnf_verify3/pass_bound_check/check_recursion.R \
  > attic/cnf_verify3/pass_bound_check/recursion_checks.log 2>&1
```

The control used R 3.6.3 and base R only. It reads the source into a private
environment, makes a separate observed function copy, and compares every
observed output identically to the untouched function's output. Production
files and bindings outside that process remain unchanged.

This observer maintains **no synthetic entry/exit stack**. At each source
helper entry it examines `sys.function()` and `sys.frame()` to identify the
actual active instrumented helper frames. A unique body marker distinguishes
those 13 helpers from observer and base-R functions. It checks the dynamic
adjacency of consecutive helpers against a separately written source graph,
checks each apply-free segment, and measures both active-frame bounds.
Restriction-frame entry potentials are stored in private unused bindings in
their actual execution environments. This permits a direct comparison to
the nearest active ancestor's entry W and Phi.

The 31 cases comprise terminal constants, unit merges/contradictions, no-op
and whole-clause-subsumption restrictions, proper nonempty restrictions,
literal deletion, discarded virtual HLA work, the overlapping two-symbol
second-order control, unit/common-guard chains of lengths 3, 7 and 13 in
both orders, eight fixed-clause controls with varying frozen groups and value
multiplicities, and one separately identified historical nested-comparison
fixture. The historical fixture is a calibration replay; it is not claimed
as an independently discovered input.

All checks passed:

| Check | Result |
| --- | ---: |
| Paired observed/ordinary kernel calls | 31 |
| Actual local-helper entries inspected | 2,267 |
| Helper kinds reached | all 13 |
| Restriction entries with a live target | 579 |
| Nested restriction entries with strict decreases in both W and Phi | 362 |
| Maximum active helpers observed | 36 |
| Maximum active restrictions observed | 12 |
| Maximum apply-free segment observed | 4 |
| Observed deferred `char_intersect -> char_union` calls | 9 |

Every observed dynamic edge belonged to the conservative graph, including
the deferred edge. Every checked frame satisfied both the W and Phi bounds.
With an eight-clause unit chain plus one separate clause, increasing that
clause's frozen group through 3, 5, 12 and 64 symbols and increasing concrete
fiber multiplicity from one to seven left Phi0 unchanged. Other cases had
Phi0 greater than W0, explicitly checking the direction of the earlier wording
correction.

`recursion_checks.rds` contains per-case maxima and deepest frame sequences;
`recursion_summary.R` is a readable summary, and `recursion_checks.log` records
the complete observed edge counts. No semantic truth-table oracle is claimed
for this small study: its purpose is independent dynamic-frame accounting,
dual-potential charging, and exact agreement with the unchanged function.

## 7. Established claims and remaining limits

The source path analysis establishes the per-active-frame progress charge.
The finite helper graph, including delayed union evaluation, establishes the
constant factor. The prior frozen-symbol and fiber arguments establish the
clause-only substitution. These are source-level proof arguments, with no
mechanized R operational semantics claimed.

The 31 fresh controls and the author's larger campaign challenge those
premises but do not prove an unbounded statement by sampling. Neither the
proof nor the tests establish a tight depth bound, sufficient available R
stack, total helper-call count, allocation/time bounds, arbitrary external
argument behavior, or termination of every loop. In particular the present
argument does not reinterpret a C/node-stack threshold as a number of these
13 helper frames.

No production changes or commits were made for this review.
