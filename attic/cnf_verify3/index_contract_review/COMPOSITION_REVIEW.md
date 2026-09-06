# Independent review of the total-correctness composition

Reviewed 2026-09-06 after the indexing-contract review, against unchanged
`R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
The proposed statement is `../root/TOTAL_CORRECTNESS_COMPOSITION.md`.

## Verdict

Sections 1–3 compose the source arguments without assuming successful whole
execution. Section 4's proposed helper-invocation bound is conservative and
its source counts cover the inspected call paths. I found no missing
source-count or termination premise under the ordinary finite-operation
contract. The semantic conclusion still explicitly imports the separately
reviewed normal-return preservation theorem; this document is not another
independent proof of that theorem.

The separate indexing review in `REVIEW.md` establishes the other needed
source contract: every next ordinary consumed index, shape or scalar test is
valid along a valid prefix, including the path to the deliberate internal
`stop`. It does not assume that the prefix extends to a completed call.

## 1. Why the termination composition is not circular

The source has only finite `for` snapshots and the two HLA `repeat` loops
(681 and 744). At a nonbreaking HLA iteration, the selected donor was unused
and its valid `was_used` position is set TRUE (721 or 783). There is no path
back to the next `repeat` iteration that bypasses this write. An iteration
which discovers no donor exits at 687/746; elimination exits at 700, 716,
720, 760, 778 or 782. No inference helper is called from either HLA loop;
the clause-elimination helper at 698/714 only updates flags and registries.
The finite-loop argument therefore applies to valid finite prefixes,
without first assuming that an enclosing helper returns.

I checked the active-depth argument in `../root/RECURSION_PROGRESS_BOUND.md`
against the restriction paths and its independent review in
`../pass_bound_check/RECURSION_REVIEW.md`. A restriction ancestor with a
restriction descendant has already committed a strict live range/literal
decrease at 167 or 244. The absent-symbol, unchanged-range and whole-target
elimination alternatives cannot create such a descendant. Ordinary promise
evaluation only adds the leaf `char_intersect -> char_union` edge; it cannot
perform an uncharged inference callback before the write. This gives the
finite prefix bound on active local-helper depth.

Every helper invocation has finitely many child activations, independently
of whether one initially imagines an infinite execution: its local loops
have finite snapshots or the checked finite donor-use measure. Finite height
and finite branching make the invocation tree finite, by induction on
height. Top-level loop/root creation is finite by the same local-loop
argument. Finite operations between these nodes exclude an infinite
execution within a node. Finally the no-first-error indexing result and
ordinary primitive/resource assumptions exclude abnormal finite exits.
Only at this point is the normal-return semantic theorem applied.

The alternative signature potential `Phi0` relies on the first-change
freezing lemma reviewed in `../pass_bound_check/REVIEW.md`. That source lemma
is a prefix property even though its surrounding repeated-pass theorem
assumes normally completed calls. The composition needs the prefix lemma,
not a conclusion conditional on the entire current call completing. The
fiber potential `W0` alone already suffices for finite depth on every fixed
finite input, so the clause-only substitution is unnecessary for Sections
1–3's termination conclusion.

## 2. Per-frame direct-child bound

The 13 helpers and their direct dynamic child bounds are:

| Helper | Bound | Source accounting |
| --- | ---: | --- |
| `register_unit` | `m` | One restriction per saved registry occurrence at 124–135. |
| `apply_domain_restriction` | `m+2` | Intersection at 152, at most `m` pair callbacks at 206–224, range callback at 231. Early alternatives use at most the intersection plus one deletion helper. |
| `eliminate_symbol_from_clause` | `m` | Registration at 251, or at most `m` callbacks at 277–285; these alternatives do not both occur. |
| `on_updated_subset_relations` | `2` | One restriction and one second-order handler at 322/330; alternatives at 303/310 return after one child. |
| `on_update_range` | `2m` | At most `m` targets in each of the two loops, 353–365. |
| `handle_sse_2nd_order_oneend` | `m` | One trial per candidate at 383–399. |
| `handle_sse_2nd_order_twoend` | `2m` | Two orientations, each at most `m` trials at 416–441. |
| `try_sse_2nd_order` | `1` | Restriction at 467; its deferred union is charged to the intersection frame that forces it. |
| `char_intersect` | `1` | At most one forcing of the deferred `char_union` promise. |
| `char_setdiff`, `char_union`, `return_entries`, `eliminate_clause_update_sr` | `0` | No local-helper calls. |

All snapshots have at most `m` distinct original clause indices. A saved
snapshot may contain inactive entries; this changes neither its length nor
the number of its iterations. `on_update_range` can visit an index in both
loops because the counts may change, so both `m` terms are needed. The
twoend handler similarly needs both orientations. The deferred union is
evaluated at most once because R promises memoize their values.

For `m >= 1`, all these bounds are at most `B(m)=2m+3`. The constant is loose;
its slack covers the early restriction alternatives even at `m=1`.

## 3. Top-level helper-root bound

Here a root means a local helper called without another one of the 13
helpers active in the same simplifier invocation. All root sites are:

| Root family | Bound | Source sites |
| --- | ---: | --- |
| Initial unit registrations | `m` | 485–487 |
| Name intersections during preprocessing | `m` | 499–502 |
| Initial unit restrictions | `m^2` | 505–506 |
| Two directions of initial pair work | `m(m-1)` | 551–620 |
| Manual second-order queue | `m(m-1)` | 640–646 |
| HLA set differences, both phases combined | `m^2` | 693 and 755 |
| Nonunit HLA target deletions | `m` | 698 and 714 |
| Returning the result or a constant | `1` | 42, 487, 491, 508, 614, 621, 646, 788 |

The initial restriction bound requires the lifecycle lemma: an original
clause can be registered as a unit at most once. There are therefore at
most `m` unit-domain bindings, even if the formula mentions many more than
`m` symbols. Each preprocessing clause's captured intersection with the
current binding names has at most `m` members.

The two pair directions account for `2*choose(n,2)=n(n-1) <= m(m-1)` initial
roots. The manual queue has no diagonal candidates: uninitialized/diagonal
counts remain NA and `which` discards their NA enable values. It thus has at
most `n(n-1)` rows as well. Reentrant callbacks are children, not new roots.

HLA has at most `m` actual targets in total across both phases. Each target
computes at most one set difference for each of at most `m` unused donors;
the terminal elimination iteration consumes one such donor even though it
breaks before setting its used bit. Only the current target can be deleted,
so there are at most `m` nonunit-HLA deletion roots. Every return site ends
the invocation immediately after one `return_entries` activation.

Summing gives the slightly sharper `4m^2+m+1`; replacing both `m(m-1)` terms
by `m^2` gives exactly the proposed

```
D(m) = 4m^2 + 3m + 1.
```

With helper height `H=6*Phi0+11`, the forest bound
`D(m) * sum(B(m)^j, j=0,...,H-1)` follows. No loop over arbitrary symbol or
domain size adds unboundedly many local-helper activations: such loops do
only primitive work, except the explicitly unit-domain-bounded preprocessing
restriction loop already counted. Primitive/evaluator calls, runtime,
allocation size and sufficient physical stack are not bounded by this
helper-count result. Constants and the empty conjunction terminate directly
and are handled separately from the displayed `m >= 1` formula.

No production changes or commits were made for this review.
