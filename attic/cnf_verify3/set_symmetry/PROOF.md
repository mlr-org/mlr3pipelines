# Exact value-set symmetries of the local simplifier

Reviewed 2026-09-06. The object of this proof is the unchanged
`R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
This is a source-level simulation argument, not an inference from the
experiments below and not a formalization of the R interpreter.

## 1. Statement and exact contract

Fix the ordered input clause list and each clause's ordered list of symbols.
Let `U_s` be the domain of symbol `s`. Assume:

1. Each `U_s` is a finite, nonempty, duplicate-free, ordinary **unnamed
   character vector**. Each occurring symbol names exactly one domain.
2. Each clause is a nonempty ordinary named list with distinct, nonmissing
   symbol names. Its literal ranges are nonempty proper subsets of their
   domains, stored as ordinary unnamed, duplicate-free character vectors.
   The separate inputs TRUE, FALSE, and the empty conjunction are also
   covered by their constant paths.
3. The universe does not change during execution. There are no active
   bindings, custom methods on ranges/lists, altered primitive bindings,
   concurrent mutations, or byte/encoding mixtures that make the equality
   operations disagree with one ordinary total equality relation. Ordinary
   ASCII character data is a sufficient instance of this contract.
4. Both executions have sufficient memory, stack, and supported R index and
   vector capacity. The theorem compares source behavior, not which concrete
   execution first exhausts resources. It makes no unconditional all-size
   claim about successful execution on a particular machine.

For every symbol choose a finite domain `V_s` and a **surjection**

```
p_s : V_s -> U_s.
```

Replace every occurrence of range `A` at symbol `s` by its full preimage
`p_s^-1(A)`, and replace the universe domain by `V_s`. The new domain and
each new literal range may have independent arbitrary value order. Do not
change clause order, symbol occurrence order, or symbol names.

**Theorem.** These two executions take the same source-level decisions and
the same clause/symbol iteration and callback schedule. They retain and
delete the same clause positions and symbol positions. Their returned
ordered clause lists are related by the same full preimage operation,
modulo value order inside individual ranges. TRUE/FALSE outcomes coincide.
Every intermediate actual or virtual set is a union of entire fibers of
`p_s`; no operation separates two members of one fiber.

The universe attribute is interpreted through this correspondence. Two
fresh universe environments are not `identical()` R objects; an equality
claim that ignores this distinction is false even before simplification.
Within each execution the returned universe is the unchanged input universe.

Here **source-level decision schedule** includes every explicit `if` outcome,
every evaluated scalar `&&`/`||` operand and its short-circuit choice, all
set-inclusion/disjointness decisions, loop traversal, helper dispatch, and
recursive continuation. It does **not** assert equal raw integer cardinalities,
membership-vector lengths, vector element order, number of comparisons
inside R primitives, allocation behavior, evaluation cost, or wall time.
For example `clause_symbol_length_before` can have different integer values.
Its eventual equality test has the same Boolean value.

“Canonical” in this document is the input representation contract above.
It is not a claim that the heuristic returns a unique or saturated normal
form. The existing scheduling gaps are preserved by this simulation too.

## 2. Set algebra and the cardinality lemma

Write `L_s(A) = p_s^-1(A)`. Preimages commute with union, intersection,
difference, and complement relative to the corresponding domains. Since
`p_s` is surjective,

```
A subset B       iff L_s(A) subset L_s(B)
A = B            iff L_s(A) = L_s(B)
A is empty       iff L_s(A) is empty.
```

Let `w(u) = |p_s^-1({u})|`. Every weight is positive, but the weights need
not be equal. For nested finite sets `A subset B`,

```
|L_s(A)| = |L_s(B)|
iff sum(w(u), u in B minus A) = 0
iff A = B.
```

Thus equality of cardinalities of **nested** sets and emptiness are
preserved by arbitrary unequal positive fiber sizes. Equality of sizes of
unrelated sets is not preserved. The audit in the next section establishes
that this latter, unsafe kind of decision is absent from the source.

The set helpers at lines 30–32 preserve the first argument's order for
intersection/difference; union appends previously absent values from its
second argument. These vectors represent the stated set operations because
the participating value vectors are unique. The second argument to
`char_setdiff` may contain repetitions from `c(range_old, donor_range)`;
this is harmless because that argument is only a membership table. The
first argument remains the unique universe domain.

## 3. Every value-cardinality decision

There are **eight** value-cardinality predicate occurrences. The temporary
assignment at line 151 is also included explicitly below. Source lines refer
to the inspected hash.

| Line | Source decision | Why it is invariant |
| --- | --- | --- |
| 102 | `!length(unit_isct)` | The merged intersection is empty exactly when its preimage is empty. |
| 115 | `length(unit_domains[[nu]]) == length(unit[[1L]])` | Immediately after registration/merge, the effective domain is contained in the incoming unit's saved range. New registration gives equality; merging gives its intersection with the previous unit. No callback intervenes before this guard. |
| 151, 160 | Save old range length; compare intersection length with it | The new range is contained in the saved old range. Equal lengths mean precisely that the range did not change. |
| 153 | Intersection length equals restringent length | The intersection is contained in the restringent, including when that restringent is a second-order donor union. Equal lengths mean containment of the entire restringent in the old target. This does not depend on the `is_unit_propagation` flag. |
| 161 | `!length(clause[[symbol_idx]])` | The new intersection is empty. |
| 595 | `length(range_outer) == length(range_inner)` | This expression is evaluated only after `inner_subset_of_outer` is TRUE, because it is the right operand of `&&`. Thus the compared ranges are nested. The outer `||` and its fallback `all(...)` make the same short-circuit choices under the lift. |
| 694 | Nonunit-HLA new range length equals universe length | The virtual range stays a subset of the original universe, is unique, and the appended values are outside its old range. Equality means the virtual range covers its domain. |
| 756 | Unit-HLA version of the same comparison | Exactly the same nested-set argument applies. |

For line 115, no assertion that a cached comparison is semantically current
is needed. The saved incoming unit and the new effective intersection are
concrete sets related by containment even during nested registration. The
known equality-skip defect can therefore occur in both coupled executions;
it does not invalidate the symmetry.

For HLA the implementation deliberately uses `universe[[symbol]]`, the
**original** domain, not the effective unit domain. The proof couples that
exact domain. It does not silently replace the source with a different HLA
rule. Whether the two full-domain branches are reachable is irrelevant to
this symmetry proof: their predicates are invariant whenever evaluated.
The separate branch review proves they are FALSE on canonical executions.

## 4. Every other length, ordering, and set operation

All remaining lengths count structural objects, which do not change under
a value lift:

| Lines | Counted object |
| --- | --- |
| 36, 51, 491, 499 | Number of clauses, or the initial unit queue length |
| 48, 50, 239, 245, 564, 657, 738 | Clause widths, meaning number of stored symbol occurrences |
| 396 | Number of candidate exceptional **symbols**, not domain values |
| 520 | Number of clause indices in a symbol registry entry |
| 538, 543, 548, 563 | Number of available clause positions or cache dimensions |
| 658–659 | Remaining clause count and `length(unit_domains)`, which is the number of environment bindings/symbols, not the size of their domains |
| 670, 734, 741 | Number of candidate donor clauses |
| 641 and loop bounds throughout | Matrix row counts and structural index-vector lengths |

The two sorts are `order(lengths(entries))` at line 48 and its descending
analogue for remaining entries at line 657. Their entire numeric arguments
are identical in coupled runs. Hence they produce the same permutation in
one fixed R execution model, including tie handling. The symmetry argument
does not need an additional assumption about how a sort resolves ties.
It does require identical input clause order, because that is part of the
sort's input and later tie-dependent schedule.

There is no `unique`, lexical value sort, value comparison by magnitude,
sampling, or selection of a first domain value in the kernel. All `[[1L]]`
uses involving units select the unit clause's **whole literal range**, not
its first value. Every explicit `for` iterates clause indices, cache indices,
symbol names, or the two second-order orientations. None iterates values
inside a domain or range.

The kernel's actual value operations are:

- `char_intersect`, `char_setdiff`, and `char_union` at 30–32;
- the equivalent direct unit intersection at 101;
- `all(A %in% B)` at 185, 216, 594–595, 707–708, and 771–772;
- the test at 463–464 that `(oneend intersect twoends) minus target` is
  nonempty on the **same** intersection symbol;
- restriction by the union of same-symbol donor ranges at 467;
- the two original-domain HLA complement/union constructions at 693 and 755.

All of these commute with full preimage. A membership vector may be longer
or reordered after the transformation, but filtering and `all`/`any`
produce the coupled set or the same scalar truth value. No operation
compares a value belonging to one symbol with a value belonging to another.
Fresh labels therefore need be disjoint only between fibers of the **same**
symbol; reusing labels across different symbols is harmless.

The other `match`, `%in%`, `which`, comparison, and sum operations concern
names, Boolean matrices, or structural indices. Those arguments are equal
in the coupled states. `match(TRUE, ...)` chooses the same first eligible
HLA donor; `which(..., arr.ind = TRUE)` gives the same second-order queue.
The two orientations at lines 416–418 use preserved symbol-column order.

The only registry-key enumeration that can reach propagation is

```
char_intersect(names(entries[[clause_idx]]), names(unit_domains))  # 502
```

Its result follows the **clause's** names, because the environment's names
are only the right-hand membership table. Environment enumeration/hash
order cannot choose propagation order. Registry value vectors are built
and removed using the same clause/symbol schedule. `roe_inverse` at line
730 is a delayed match on structural indices; it is forced in the same
source circumstances and returns the same vector.

## 5. Source-state induction, including stale state

Relate the two executions at corresponding source positions as follows:

- Clause list positions, symbol positions/names, eliminated/unit flags,
  registry keys and clause-index values, available indices, cached Boolean
  matrices, counters, work queues, and loop snapshots are identical.
- Every actual range, local saved range, restringent, unit-domain range,
  donor union, and HLA virtual range is related by full preimage as a set.
  Absent literals/NULL and empty intermediate character vectors retain
  their corresponding structural forms.
- A saved **range cardinality**, specifically the variable at line 151,
  denotes the weighted cardinality of the corresponding saved set. It is
  not required to equal the other run's integer value.
- The source call stack and continuations correspond. Promises that hold
  source expressions are forced at corresponding program operations;
  ordinary value evaluation and lookup introduce no user callback.

The relation holds initially. Pure set expressions preserve its range part.
Sections 3–4 show that every branch and traversal preserves its control
part. Assignments to flags, registries, and cached matrices consequently
make identical updates. A range shrink removes the same symbol if and only
if its intersection becomes empty; unit registration therefore occurs at
the same moment. Calls and returns preserve the relation even when they
reenter an ancestor's unfinished work. Locally cached ranges and queues are
coupled snapshots, including snapshots that are stale in both runs.

Crucially, this induction does **not** require every cached Boolean to mean
the currently true mathematical subset relation. It requires the two
executions to store the same Boolean and make the same later updates. Thus
the proof is independent of semantic-preservation and scheduler-completeness
proofs. It preserves an implementation's deterministic mistakes as well as
its successful simplifications.

At return, `entries[!eliminated]` selects identical clause positions in
identical order. The remaining range correspondence proves the theorem.

## 6. Consequences and stronger ordered statements

**Injective per-symbol renaming.** Set `V_s` to exactly the image of the
injection; inverse renaming supplies `p_s`. This proves the requested
renaming property. Adding unrelated extra domain values is not part of a
renaming. Such values must belong to a specified nonempty fiber for the
lifting theorem to apply.

**Unequal splitting.** Replace each old value with a nonempty, pairwise
disjoint set of new labels. Mapping these labels back to the old value is
the required surjection. Uniform fiber size is unnecessary. Empty fibers
are forbidden: deleting a value can destroy an emptiness/inclusion witness.

**Independent within-range/domain permutations.** Use singleton identity
fibers and choose arbitrary new vector orders. The source schedule and
ordered output clauses/symbols remain unchanged modulo value order.

There are two useful stronger conclusions:

1. If each value is replaced consistently by one fixed **ordered block**,
   and every input vector is expanded by concatenating its blocks, all
   ordered set helpers commute exactly with this block substitution.
   Thus the returned literal **vectors** equal the block expansion of the
   original vectors, not just their corresponding sets. Elementwise
   injective renaming is the one-element-block special case.
2. If only universe-domain value order changes, actual returned literal
   vectors are exactly unchanged. Actual stored ranges only shrink by
   intersections, preserving their old order; unions and domain complements
   affect a restringent or HLA's temporary virtual clause. HLA never writes
   its expanded range back into the returned `entries`.

**Equal-membership-cell quotient.** For a symbol with initial occurrences
`A_1, ..., A_m`, identify values having the same membership vector

```
(1[v in A_1], ..., 1[v in A_m]).
```

Every initial range is a union of cells. Keep one label for **each nonempty
cell**, including the all-zero cell whenever it exists. There are at most
`2^m` quotient values; an unused symbol has one cell. Mapping concrete values
to their cells is a surjection whose full preimages recover the original
sets. The theorem applies in the reverse direction and proves equality of
the complete source decision schedule and output representation modulo the
quotient. Equal input profiles may have arbitrary different cell sizes.

The partition may be any refinement of this membership partition as well;
all that is required is that every initial range is a union of whole cells.
The maximal membership quotient is the smallest such quotient. This bound
depends on the number of occurrences; there is no single fixed domain-size
bound independent of formula structure.

Every output range stays in the Boolean algebra generated by the original
ranges and the domain. Therefore the same fixed quotient also couples every
subsequent simplification pass, including its clause sorting and all known
multi-pass scheduling effects. This does not prove a pass bound by itself.

## 7. Duplicate symbol occurrences: separate extension and its limits

The primary contract deliberately excludes duplicate symbol names. The
related argument in `../small_selectors/NOTES.md` instead preserves them
positionally, including stale copies and errors. Its set-level reasoning
is sound, but a source argument must also address an R-specific issue:
`clause[[c(i,j)]]` could recursively select a **value** from a literal if a
supposed scalar clause selector became a vector. Such indexing would not
be covered merely by saying that every operation is a set operation.

For the occurrence-preserving version, retain all occurrence positions and
assume each individual range and domain still meets the ordinary unique-value
contract. The following structural audit closes that issue:

- `register_unit` is called only at physical width one, so `names(unit)`
  has length one even if earlier clauses had duplicate names.
- Before HLA, initialized nonmissing counters equal sums of their
  **stored Boolean rows** whenever a handler selects a mask. This is an
  arithmetic invariant, not a claim
  that duplicate-name rows mean the right mathematical comparisons. Every
  scalar-column toggle updates its counter once; duplicate registry indices
  revisit an already toggled bit and do not decrement it twice. Initialization
  takes the actual row sum. Each bit/count update finishes before another
  handler is called. During nonunit HLA the separate current-target count
  maintains this property for the row being extended; the original count
  matrix is intentionally not updated for already processed virtual targets.
- A first-order count-one mask therefore selects exactly one symbol name.
  A count-two mask supplies exactly two stored columns; the two-end handler
  indexes them one at a time. The one-end handler filters its mask and
  explicitly requires `length(symbol_target) == 1` before calling the
  second-order restriction.
- Unit HLA uses a different initial count: occurrence width minus a Boolean
  registry-membership indicator. The registry has no false positive symbol
  membership, although deletion can leave a duplicate occurrence unregistered.
  Therefore the initial count is at least the sum of the lazily initialized
  `names(clause) != unitsymbol` mask. Materializing/updating the mask clears
  one stored bit and decrements the count together. A selected count of one
  consequently has a mask of size **zero or one**, never more than one.
  Size zero causes the documented `[[character(0)]]` error before inspecting
  any literal value. Its cause is structural and is unchanged by a value lift.

Thus scalar/empty symbol selectors do not turn into arbitrary value-level
recursive indexing under these occurrence inputs. All remaining operations
and length comparisons have the same set/structural classification as above,
separately for each stored occurrence. The simulation extends to their
ordered positional output, or to the same structural error and preceding
source decision schedule. Cached mathematical inaccuracies and stale
trailing occurrences remain coupled; this asserts **no semantic correctness**
for such inputs. The already known accepted selector errors and wrong outputs
remain real defects.

This extension does not include duplicate **value storage**, duplicate
universe values, NA selectors, arbitrary attributes/classes, byte-marked
equality failures, active bindings, or external mutation. In particular,
the current `CnfSymbol` constructor does not reject repeated domain values;
the representation contract is stronger than “the constructor accepted it.”
The separate finite selector enumerations retain all of their explicit
symbol/clause/occurrence bounds. The symmetry theorem removes domain-size
restrictions from a correctly enumerated shape, not those structural bounds.

## 8. Relation to earlier work and evidence

`../quotient_review/QUOTIENT_REVIEW.md` already supplied the essential
preimage and nested-cardinality argument for the `(3,3,2)` profile campaign.
The present contribution states the arbitrary-shape source simulation,
audits every length/ordering dependence, includes short-circuit operands,
states stronger vector-order corollaries, and separately checks the positional
selector extension. It does not repeat either exhaustive quotient campaign.

The new checks are described in `README.md`. Their independently generated
inputs and deliberately changed source-event controls support the audit;
no unbounded statement here follows from their finite successful counts.
