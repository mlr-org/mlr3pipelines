# Combining argument validity, finite execution, and semantic preservation

Reviewed composition, 2026-09-06. The canonical kernel's normal-return semantic
theorem does not by itself exclude runtime errors or infinite execution.
This note separates those obligations. The independent
[index review](../index_contract_review/REVIEW.md) and
[composition review](../index_contract_review/COMPOSITION_REVIEW.md) are now
complete and support the claims under the stated contract. It is about the six
unchanged CNF source files, specifically the local simplifier, not every
accepted public constructor representation or the full R interpreter.
The subsequent [dependency audit](../proof_dependency_review/REVIEW.md) checks
that the prefix arguments do not assume whole-call return or preservation.

## 1. The ordinary finite-operation contract

Input clauses are ordinary lists with distinct valid nonmissing symbol names.
Each actual literal is a nonempty proper finite set of distinct nonmissing
character values. Its fixed domain is also an ordinary nonempty finite vector
of distinct nonmissing character values. TRUE/FALSE are separate scalar
constants, and the empty conjunction is allowed. Symbol names resolve to one
unchanged universe. Symbol identity must agree across character membership,
list indices, environment insertion/direct lookup, and environment name
enumeration: enumerated names must remain equal to the original clause names
and retrieve the same binding, with no collisions between distinct symbols.
Equality and the set/index primitives have their ordinary
consistent semantics; no custom method, active binding or external mutation
changes the execution. All necessary arithmetic and storage indices are
representable, and sufficient memory/stack resources are available.

These premises do not assume that the particular next source indexing
expression is valid. Establishing that is the index proof's task. They also
do not assume that the whole computation terminates or returns normally.
The actual machine can violate the resource premises on the recorded long
chains; this composition does not erase those observed failures.
The later [native-name counterexample](../character_identity_review/FINDING.md)
also demonstrates why the symbol-identity premise is substantive. Under
`LC_CTYPE=C`, valid marked Unicode names can enumerate as ASCII escape text.
That input has canonical clause shapes and normally returns a wrong truth
function; syntactic normalization alone therefore does not meet this contract.

## 2. No first argument/shape failure

The reviewed `../index_contract/PROOF.md` gives an execution-prefix induction.
At the next consuming operation, stable actual/meta index spaces, allocated
matrix columns, guarded activity, exact count/bit bookkeeping at dispatch
boundaries, and lazy-row initialization establish the needed argument shape.
Finite atomic sequences between those boundaries have their own valid reads
and writes; a bit/count relation need not hold between its two assignments.

Intentional NA values in intermediate `[` selections are permitted. Their
removal before a `[[`/matrix use is proved rather than silently assumed.
Undefined helper locals, missing self inverses, stale unit-propagation snapshots
and the count variable's final matrix-to-vector transition are separate cases.
Under this proof there cannot be a first ordinary argument/shape error in a
finite execution prefix. This is a stronger conclusion than a conditional
statement about observations from successful whole calls.

## 3. Every local computation is finite

Every explicit `for` visits a finite value captured when its sequence is
evaluated. Recursive changes do not extend that R sequence. The two HLA
`repeat` loops either exit or mark the selected previously unused donor used;
there are finitely many donors. Inner HLA loops have no recursive inference
calls and finitely many registry entries. The return/helper paths perform
only finitely many primitive operations between these loops and child calls.

The reviewed recursion proof bounds active helper depth in any valid prefix.
Every active restriction ancestor commits a strict decrease in live original
fiber occurrences before a further restriction can be entered. The remaining
helper graph, including the delayed intersection-to-union edge, is acyclic.
Its conclusion is `helper_depth <= 6*W0+11`, or the alternative
`6*Phi0+11`, for the finite initial potentials defined in the cited proofs.

For termination alone, the dependency audit supplies an independent weaker
bound with fewer prerequisites. Let `Q0` count every initially stored literal
value occurrence, including slots later marked eliminated. The only actual
storage writes intersect a current range or remove a present range. Every
restriction frame with a further restriction descendant has already performed
one of those strict writes. Thus `helper_depth <= 6*Q0+11`, without needing
fiber closure, signature freezing, semantic preservation, or target liveness
as a descent premise. The [dependency graph and source argument](../proof_dependency_review/DEPENDENCIES.md)
keep this simpler proof separate from the sharper cost bounds below.

Consequently the helper invocation tree has finite height and each vertex has
finitely many children. Such a tree is finite, by induction on its height:
a leaf is finite; a vertex with finitely many finite child trees has a finite
tree. The top-level loops create only finitely many roots of these trees.
No infinite source execution remains. This argument analyzes every finite
prefix and does not require successful completion as an earlier premise.

Combining Sections 2 and 3, ordinary primitive totality and the resource
premises force a normal finite return. The separate source-level preservation
theorem then establishes equality of the input and returned truth functions.
This is a conditional source-level total-correctness composition with completed
independent review; it is not a machine formalization.

## 4. A conservative bound on all helper invocations

There is a further explicit finite bound which may help make the termination
argument independently checkable. Let `m >= 1` be the original stored clause
count. The maximum number of **direct local-helper child activations** of
one helper frame is at most

```
B(m) = 2*m + 3.
```

The bound counts helper frames, not primitive R calls or loop body iterations.
The source inventory gives:

| Helper | Bound on direct helper children |
| --- | ---: |
| register_unit | m restriction calls |
| apply_domain_restriction | one intersection, at most m pair callbacks, one range callback; other branches return through one deletion helper |
| eliminate_symbol_from_clause | one registration, or at most m pair callbacks |
| on_updated_subset_relations | at most one restriction and one second-order handler; other branches return after one child |
| on_update_range | at most m oneend plus m twoend handlers |
| handle_sse_2nd_order_oneend | at most m trials |
| handle_sse_2nd_order_twoend | two orientations, each at most m trials |
| try_sse_2nd_order | one restriction; its lazy union is additionally bounded as a child of the intersection frame |
| char_intersect | at most one deferred char_union on inspected call paths |
| remaining four leaf helpers | zero |

An original clause can become a unit only once, so the unit-domain environment
has at most m bindings. Initial nonunit scanning therefore calls the restriction
helper at most `m*m` times, regardless of the number of other symbols. The
top-level number of directly entered helper roots is at most

```
D(m) = 4*m*m + 3*m + 1.
```

The independent source recount sharpens this to `4*m*m + m + 1` by excluding
diagonal initial/manual pairs. The displayed conservative bound is retained
so the original executable child/root checks and statement stay aligned.

This deliberately loose sum includes m initial registrations, m name
intersections, m squared initial unit restrictions, at most twice m squared
pair/queued dispatches combined, at most m squared HLA domain differences,
m nonunit-HLA clause deletions and one final return helper. Early exits only
lower these counts. HLA target visits total at most m and each consumes at
most m donors, so the two HLA phases together fit the one m-squared allowance.

Set `H = 6*Phi0+11`, with
`Phi0 <= m + m*2^(m^2+m)` from the reviewed signature-class bound. Finite
outdegree and depth give the conservative total

```
local_helper_invocations <= D(m) * sum(B(m)^j, j=0,...,H-1).
```

This bound depends only on original clause count. It is enormous and is not
proposed as an operational estimate. Loops over clause symbols and primitive
value operations can still become longer when symbol/domain counts grow; no
clause-only wall-time, allocation-size or all-R-frame bound follows. The value
of the statement is the exclusion of unbounded numbers of recursive helper
activations at fixed clause count under the finite-operation contract.

Section 4's per-frame child inventory and top-level root sum have now received
the separate review linked above. They do not follow merely from the earlier
bound on simultaneous depth. No production implementation or unconditional
public-input guarantee is added by this note.

## 5. Independent child/root counting controls

`helper_call_bounds.R` adds private entry/exit observations to all 13 local
helpers. It maintains a separate invocation tree with one direct-child count
per helper activation and a separate top-level root count. Every entry checks
its exact input-dependent B or D bound, and every completed observed result
must equal the unchanged function exactly. It does not reuse the earlier
fiber-potential/depth observer to determine these counts.

Both R 3.6.3 and R 4.6.1 passed the same 1,023 cases, comprising constants,
the saved nested-comparison control, reversed unit/guarded chains, 1,000
freshly seeded cases, and fixed-three-clause inputs with increasing numbers
of symbols through 512. There were 61,029 helper activations, 26,853 top-level
helper roots, and maximum depth 141 on each version. All B/D checks passed.
Two private controls add irrelevant empty-set helper calls: one exceeds B
inside a frame and the other exceeds D at the top level. Each is rejected
by its own intended observer check. Neither changes production source.

These counts test the distinct branching/root premises. They do not evaluate
the enormous closed-form bound numerically or substitute sampling for the
finite-tree argument. Detailed per-case trees' count summaries are saved in
`helper_call_bounds_r36.rds` and `helper_call_bounds_r46.rds`.
