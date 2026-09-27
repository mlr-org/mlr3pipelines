# Independent review of the membership-pattern reduction

Reviewed 2026-09-06 by the representation stream. This is a review of
`../independent_solver/PROOFS.md`, `exhaustive_two_symbol.py`, the persistent R
bridge, and the relevant production source. It does not reuse the sibling's
instrumentation in the additional branch-trace experiment below.

## Verdict and exact scope

The refinement argument is valid for canonical, finite, set-valued inputs to
`simplify_cnf`. The 520,200-case enumeration consequently excludes a semantic
simplifier error in every formula with at most three clauses and at most two
occurring symbols, with arbitrary finite domain cardinalities, subject to the
explicit assumptions below. It is substantially stronger than enumeration of
domains of size at most eight: every larger domain is a refinement of one of
the enumerated membership-pattern domains.

This does **not** prove every public constructor arrangement correct. In
particular, the known errors involving universe-free TRUE/FALSE objects are
outside the bridge's normalization path. It does not prove completeness of
simplification: the enumeration itself records 32 cases with a residual
subsumed clause. It is a statement about preservation of Boolean semantics.

## The quotient and its lifting map

Fix an ordered triple of clauses C1,C2,C3 and a symbol s. For each domain value
v define its membership vector

```
q_s(v) = (v in C1[s], v in C2[s], v in C3[s]).
```

An absent symbol range is empty. The quotient domain P_s is the nonempty image
of q_s in `{0,1}^3`. Each quotient input range consists exactly of the vectors
whose corresponding coordinate is one. Hence every original range is the
full preimage of a quotient range under the surjection q_s.

Surjective preimages preserve union, intersection, complement relative to the
domain, emptiness, equality, and proper/nonproper inclusion. This covers all
set-valued calculations in the simplifier. There is no arbitrary choice of a
concrete value, or branching on an individual value's label or position.

There is a useful state-level strengthening. Couple the quotient and original
executions by retaining the same clause ids, symbol order, unit ids, registry
indices, matrices, and callback stack, and replace every stored or virtual
range by its full preimage. Every transition preserves this coupling. Thus
both runs choose the same rules, remove the same clause ids, and terminate
with corresponding output sets. The original output value *order* can differ
because intersections and unions preserve different source orders; it is the
sets and clause/symbol order that correspond.

## Every cardinality-sensitive production decision

The proof must check the length shortcuts; saying only that set operations
commute would be insufficient. The following is the complete list of
comparisons involving the cardinality of a value range in
`R/CnfFormula_simplify.R` at the reviewed source hash:

| Location | Length decision | Required containment and resulting set predicate |
| --- | --- | --- |
| `register_unit`, line 102 | nonempty intersection | emptiness is preserved by a surjective preimage |
| `register_unit`, line 115 | effective unit length equals the registering unit's range length | the effective range is a subset of the registering range; equivalent to equality |
| `apply_domain_restriction`, line 153 | restricted target length equals restricting range length | the restricted target is the intersection and is contained in the restricting range; equivalent to equality |
| `apply_domain_restriction`, line 160 | new target length equals old target length | new is contained in old; equivalent to no set change |
| `apply_domain_restriction`, line 161 | restricted range is empty | emptiness is preserved |
| initial pairwise loop, line 595 | inner subset of outer and equal lengths | under the first conjunct, equal lengths means equality, and therefore reciprocal inclusion |
| non-unit HLA, line 694 | virtual range length equals domain length | virtual range is contained in the domain; equivalent to coverage |
| unit HLA, line 756 | virtual range length equals domain length | same argument |

All other `length`, `lengths`, `order`, `seq_*`, `which`, or positional
operations concern clauses, their number of symbols, registry indices, or
Boolean relation matrices. Value refinements preserve those quantities and
the order of symbols. In particular, `length(unit_domains)` counts entries in
an environment, not the size of a symbol's domain.

The constructor decisions are also preserved: `all(domain %in% values)` is
coverage; empty `values` is emptiness; `unique` removes repeated labels; clause
merges are unions. Consequently empty/full input ranges normalize the same
way before entering the coupled simplifier runs.

## Why 255 x 255 x 8 covers the intended input space

1. Each symbol's quotient domain is an arbitrary nonempty subset of eight
   vectors. `domain(mask)` enumerates every one once for masks 1 through 255.
   The `p000` vector is present exactly when some values occur in none of the
   three ranges. Such unused values are essential for complements and are
   explicitly included, rather than silently discarded.
2. `range[i]` is built from coordinate i of those vectors. Therefore every
   ordered triple of ranges over any finite domain has an enumerated quotient.
   Equal coordinates cover duplicate clause ranges; equal coordinates for
   both symbols cover duplicate whole clauses. No distinctness condition
   silently excludes these cases.
3. Swapping clauses means permuting the three coordinates in *both* symbols'
   pattern vectors. That permutation induces a bijection on the eight-vector
   cube and hence on its nonempty subsets. All resulting mask pairs already
   occur. An additional six-way clause-permutation loop would be redundant.
4. A clause with two present symbols has exactly two symbol orders. The three
   independent binary orientation choices cover all eight combinations. Empty
   ranges may disappear, and full ranges may make the clause constant, in
   which case some orientations are duplicates rather than missing cases.
5. Every original formula with fewer than three clauses can be padded with
   tautological clauses. The bridge builds these from full-domain atoms in the
   same universe, and `CnfFormula` removes them before calling `simplify_cnf`.
   A one-symbol formula can be embedded by giving the other symbol a singleton
   domain with only pattern `000`. No literal on that symbol survives. A
   zero-symbol formula is constant and is covered semantically by the
   constant cases. A direct empty-clause-list constructor may have different
   universe attributes; this does not change its Boolean value.
6. An empty clause is generated by zero ranges at both symbols. It normalizes
   to FALSE and forces the formula to FALSE. A TRUE clause arises from a full
   range at either symbol. Both constant cases are represented by ordinary
   same-universe atoms, so constructor normalization succeeds consistently.
7. Symbol renaming is harmless here as well: all environment lookups use the
   actual symbol name as a dictionary key. The only use of
   `names(unit_domains)` is as the right-hand argument of `char_intersect`;
   the iteration order follows the current clause's names, not the dictionary
   enumeration order. Thus a change in dictionary hashing or name ordering
   cannot affect the order of propagated symbols.

The source loops have no adaptive exclusions. The stored report has exactly
all 255 outer masks, 520,200 formula checks, and no errors. Both the one-hot SAT
oracle and independent multivalued decision-diagram oracle were applied to
every returned result. This review inspected those facts in the report; it
has not duplicated the full 520,200 production runs. The count is the expected
`255 * 255 * 2^3`.

## Independent executable check of the stronger control-flow claim

`trace_refinement.R` independently transforms the abstract syntax trees of
CnfAtom, CnfClause, CnfFormula, and simplify_cnf, wrapping every `if` predicate
in a recorder that forces and returns its original value at the original
decision point. It changes no production files. It compares the full resulting
branch-decision sequence for an original formula and a refinement in which
individual domain values are replaced by between one and nine fresh labels.
The class multiplicities are unequal, and both domain/range orders are
shuffled. Clause ids and symbol order are preserved and checked.

Result on R 3.6.3: **3,000 refinements, 852,862 matched branch decisions, 125
instrumented condition sites, zero trace or output-set discrepancies.**
Four-fifths of the cases use proper nonempty ranges over domains of sizes two
through eight; the other fifth includes empty/full ranges and singleton
domains. Formulas use up to five symbols and twelve clauses, so this extra
check also exercises coupling beyond the exhaustive three-clause bound.

This experiment checks the implementation-specific premise independently; the
unbounded domain reduction is the induction above, not an extrapolation from
these random examples.

## Assumptions and boundaries that should accompany the theorem

- Domains are finite and nonempty, with unique values, and ranges contain only
  domain values. The equality/membership relation is ordinary total string
  equality and is consistent across all set operations.
- Symbol names identify distinct symbols under R's exact name lookup and
  character matching. Objects are canonical constructor outputs, without
  adversarial S3 overrides or manual internal mutations.
- Domain environments are fixed during each call. Public documentation
  already states that changing/deleting symbols is not intended.
- The execution has sufficient R stack, memory, and indexing capacity. The
  refinement result is mathematical; adding many duplicate class members can
  hit a resource limit earlier without contradicting semantic coupling.
- The theorem concerns the simplify_cnf source reviewed here, including its
  existing scheduling gaps. It does not repair those gaps or prove a unique
  normal form.
- Mixed byte-marked and encoded Unicode strings can cause ordinary R
  membership primitives to error. Such representations do not satisfy the
  total consistent membership assumption. They are a separate accepted-input
  validation/display question, not covered by the ASCII representative run.

Reviewed simplifier SHA-256:
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
