# Algebraic and implementation-specific exclusions

These claims concern valid finite-domain clauses and ordinary R character
membership semantics. Clause ranges are unique sets, as produced by
`CnfAtom()` and `CnfClause()`. They separate proofs of logical rules from the
question whether the implementation schedules all applicable rules. The latter
has counterexamples in this directory.

## 1. All implemented local logical schemas are sound

Write `C_s` for the set in clause C at symbol s, with absent symbols denoting
the empty set. A valuation x satisfies C exactly when some `x_s in C_s`.

**Subsumption.** If `A_s subset B_s` for all symbols, A implies B. Replacing
`A and B` by A is equivalent. This also proves removal of exact duplicates.

**Unit intersection.** Two units on the same symbol assert `x_s in U` and
`x_s in V`, which is exactly `x_s in U intersect V`. Empty intersection is
contradiction. On valuations satisfying a unit U, every range at its symbol
may be replaced by its intersection with U. If a clause's range contains U,
the unit implies that clause and it may be removed. These arguments do not
require a particular order of clause visitation.

**First-order SSE.** Suppose every range of donor A outside s is a subset of
the target T's corresponding range. Replace `T_s` by `T_s intersect A_s`.
A lost valuation would satisfy A and T but falsify the restricted T. All
non-s literals of T, and hence of A, would be false. A forces membership in
`A_s`; T forces membership in `T_s`; their intersection must then be true,
contradiction. This argument includes absent-symbol cases: restricting an
already absent target literal is a no-op.

**Second-order SSE.** Suppose donors A,B and target T satisfy

```
A_v subset T_v and B_v subset T_v, for every v outside {s,t};
A_s intersect B_s subset T_s.
```

Replace `T_t` by `T_t intersect (A_t union B_t)`. A valuation lost by this
operation falsifies all other T literals and both donors' t literals.
Therefore both donors must hold through s, putting the valuation in
`A_s intersect B_s`, hence in `T_s`, contradiction. The rule allows A or B
to omit t. It does not need the implementation's additional guard that T's
t range is not a subset of either donor's t range; that is pruning.

If `A_t union B_t subset T_t`, the same argument shows that A and B already
imply T, so the whole target clause may be removed. This justifies the
non-unit use of `apply_domain_restriction()`'s length-equality removal path.

**HLA.** If donor D is a subset of target C outside s, extend C's s range by
the complement of `D_s`. Under D, any valuation that satisfies only the added
literal falsifies `D_s`; it must satisfy D at another symbol and therefore
already satisfies C. Thus `D and C` is equivalent to `D and extended(C)`.
Repeated extensions preserve equivalence in a formula retaining every donor.
If the extension becomes tautological, or is subsumed by another retained
clause, the original C is redundant.

**Current units are part of the context.** During recursive propagation, some
raw stored ranges have not yet been intersected with current unit domains.
The schemas remain valid after intersecting *all* participating ranges with
the units, because the conjunction being transformed already asserts those
units. `contextual_audit_replay.json` demonstrates a real rewrite requiring
this contextual reading. Raw set-premise checks at every recursive entry
would be an over-strong requirement.

`rule_proofs.py` checks every pointwise Boolean pattern for these schemas.
For SSE2 its nine bits are membership in A/B/T at s and t, and truth of the
aggregate remaining literals of A/B/T. Every actual valuation induces one
of the 512 patterns; range inclusion implies the tested Boolean premises.
All 280 patterns satisfying those premises preserve truth. This is an
unbounded-domain rule proof, not an empirical bound on domain sizes.

## 2. HLA cannot need to use a donor twice

When D is first selected, exactly one of its symbols s is not a subset of
the current virtual clause C. Every other donor range is already contained
in C. Extensions only enlarge C, so those other ranges remain contained.
After adding the complement of `D_s`, repeating that same addition adds
nothing. The only possible future exceptional symbol of D is still s.
If its range eventually becomes contained too, the code checks hidden
subsumption even for previously used donors. Therefore the `was_used`
optimization cannot by itself miss a useful HLA extension. This proof does
not assume acyclic dependencies between donors.

The same argument bounds each virtual HLA loop by the number of eligible
donor clauses: an iteration either eliminates the target or marks one new
donor used. Thus HLA cycles cannot create an infinite loop.

## 3. The HTE branches cannot be reached with consistent HLA bookkeeping

A selected donor D at symbol s has `D_s` not a subset of current `C_s`.
Choose any `v in D_s minus C_s`. The extension is

```
C'_s = C_s union (domain_s minus (C_s union D_s)).
```

The chosen v is absent from both terms, so the extension is still a proper
subset of the domain. Consequently the immediate full-domain test cannot
succeed. If other donors eventually become contained, hidden subsumption
fires first. This is a proof conditional on the row/count bookkeeping
reflecting the virtual clause. The independent HLA event audit checks that
condition against actual sets at every observed selection.

Even duplicate values in the universe do not create a false-positive length
equality here. An initial virtual range is unique. Every added value is absent
from the old range and carries at most its multiplicity in the universe.
The virtual range is consequently a submultiset of the universe; omitting
any donor value makes its length strictly smaller than the universe's length.
Duplicates can make full coverage harder to recognize elsewhere, but cannot
make this selected-donor step appear full while omitting a domain value.

## 4. Virtual HLA matrices cannot contaminate later targets

The non-unit loop updates only entries `is_not_subset_of[[donor]][target, s]`
for its current target. It does not update `entries`, the original stored
clauses. A later target has a distinct target-row index in every donor matrix,
so that later target's initial comparisons remain the pairwise-phase values.
Its initial `not_subset_count_current` is copied from the unchanged global
count matrix. Removing earlier targets merely removes donors from its donor
list; it does not alter any surviving donor's raw ranges. Therefore virtual
range changes for one target cannot leak into another target's start state.

The current target itself appears in the symbol registry only for its original
symbols, but its self-comparison is FALSE and remains FALSE: its original
ranges are subsets of its monotonically extended virtual ranges. Hence that
registry iteration cannot decrement a nonexistent self-donor counter.

Unit HLA constructs a fresh local comparison list for each unit and uses only
surviving non-unit donors. Previously eliminated units are not donors, so their
removal does not invalidate an outstanding donor witness.

## 5. Large domains reduce to membership-pattern classes

For a fixed input formula, declare two values of one symbol equivalent when
they occur in exactly the same input clause ranges. Every input range is a
union of these equivalence classes. Intersection, union, and complement all
preserve this property, so every later actual and virtual range also remains
a union of whole classes. No simplification step can distinguish values
inside one such class.

More strongly, replace each domain value by an arbitrary nonempty collection
of new labels and replace every range by its full preimage. All set operations
and inclusion tests commute with this surjective refinement. Every range-
length equality test in the simplifier occurs with a known containment:
an intersection inside a restricting set, a new range inside an old range,
or a range inside the full domain. Equality of those lengths is therefore
equivalent to equality of sets, which also commutes with surjective preimages.
Clause lengths and symbol order are unchanged. By induction, the control
flow and surviving clauses match, and output ranges are the corresponding
preimages. The independent `value_refinement_naturality` experiment tests this
strong structural claim with highly unequal class multiplicities.

Thus increasing the number of indistinguishable values cannot expose a new
logical behavior. A symbol occurring in k input ranges has at most `2^k`
membership classes. This gives a parameterized small-model reduction, not a
small universal bound: more input range patterns can still produce more
classes. This result assumes unique canonical domain values. Duplicate-domain
storage is a separate API representation question.

Value order and independent renaming within each symbol are also immaterial
to the control flow. All value comparisons in this implementation are set
membership operations on ranges selected by the same symbol, or on a range
and that symbol's universe/unit domain. The initial pairwise loop obtains
those matching positions through its symbol-name map. No branch inspects a
particular value label or selects a value by its position. List/set operations
can preserve a different output value order, but cannot alter which sets or
clauses survive. This is a code-inspection premise of the quotient argument;
it is not inferred merely from equality of results on a few relabeled cases.

## 6. Algorithmic termination on finite valid input

Every recursive chain that continues through a state change removes a value,
removes a symbol, eliminates a clause, or registers/merges a unit created by
such removal. Actual stored clause ranges never grow. A call that intersects
a range without changing it returns before firing more update callbacks.
The list of original clauses is finite and no new clauses are allocated by
the simplifier. Therefore the natural-number measure consisting of surviving
clause/range content can decrease only finitely many times. The remaining
loops iterate finite fixed lists; HLA has the explicit bound proved above.
This excludes mathematical nontermination from recursive simplification on
finite well-formed inputs. It does not exclude R's practical recursion-stack,
memory, integer-index, or elapsed-time limits for large inputs.

## Limits of these claims

The implementation has confirmed event-scheduling completeness gaps. The rule
proofs do not claim a canonical form or a fixed point, and tests in this
directory actively falsify such a claim. The unit-skip matrix optimization also
needs a contextual invariant argument because snapshots can be transiently
asymmetric during recursion. Final-output soundness has been checked through
two independent oracles, but these notes do not yet supply a complete proof
of every possible R execution path.
