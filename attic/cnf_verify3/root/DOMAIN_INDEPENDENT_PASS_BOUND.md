# A pass bound independent of finite domain cardinalities

The reviewed repeated-pass proof bounds productive passes by the initial
number of stored value occurrences. That bound can be sharpened when many
distinct domain values have the same input membership pattern. This note
combines its descent argument with the independently reviewed value-refinement
theorem. It concerns canonical finite inputs, a fixed universe, and normal
completion of each invocation of the unchanged simplifier.

## 1. The smaller potential

Give each original clause a persistent ghost identity. For an occurring symbol
`s`, let `m_s` be the number of original clauses containing it. Partition its
domain by the vector of membership bits in those `m_s` original ranges.
Write `P_s` for the set of bit patterns that actually occur. A concrete pattern
may have arbitrarily many distinct domain values in its fiber.

Every actual range starts as a union of whole fibers. Every operation that
can influence an actual or virtual range is union, intersection or complement
within this fixed domain, and these operations preserve unions of fibers.
Actual clauses never acquire a new symbol or value. A unit merge retains an
old clause identity and intersects its range; it does not create a new actual
clause. Thus a surviving actual clause range always contains either an entire
original fiber or none of it, throughout every pass.

Define `W(F)` to count **fiber occurrences** across actual stored clauses and
symbols: count a fiber once for each clause range containing it, irrespective
of how many concrete values that fiber has. Set `W(TRUE)=W(FALSE)=0`.

A productive pass deletes at least one actual value occurrence or a whole
clause. Because whole fibers move together and every present range is
nonempty, it deletes at least one fiber occurrence. Therefore `W` strictly
decreases on every productive pass. Initially,

```
W(F_initial) = sum_s sum_{p in P_s} popcount(p).
```

This gives a finite upper bound on productive passes that depends only on
the initially present membership classes. It can be far smaller than the
total number of concrete stored values.

## 2. A bound using only clause/symbol incidence

Among all `2^m` Boolean patterns of length `m`, each of the `m` coordinates is
one in exactly `2^(m-1)` patterns. Hence

```
W(F_initial) <= sum_s m_s * 2^(m_s - 1).
```

If the original formula has `m >= 1` clauses and `q` occurring symbols,
then `m_s <= m` and the coarser bound is

```
productive_passes <= q * m * 2^(m - 1).
```

There is no term involving the cardinality of any domain. In particular,
with fixed clause and occurring-symbol counts, increasing domain sizes alone
cannot force arbitrarily many productive passes. Unequal positive fiber
sizes do not matter. Values belonging to no original range have all-zero
membership and contribute zero to this potential.

The exact storage fixed point may require the final sort-only pass described
in `../repeated_passes/PROOF.md`. One additional call may be used to observe
equality of consecutive stored results. The present result bounds productive
passes and inherits that same distinction; it is not a tight bound.

## 3. Stronger follow-up and limits

This does not conflict with the family requiring exactly `n` productive
passes: its clause and symbol counts both increase with `n`. It also does
not by itself prove a domain- and symbol-independent bound from clause count
alone. The separate follow-up in
[pass_bound_review/PROOF.md](../pass_bound_review/PROOF.md) addresses that
question: group symbols by their initial support and pairwise inclusion
signature. A group of three or more cannot undergo a first actual range
change, because any exceptional donor would have at least three exceptions.
Counting whole-clause deletions and fibers in the remaining small groups
gives the coarse bound `m + m * 2^(m^2 + m)`. The completed
[independent review](../pass_bound_check/REVIEW.md) verifies the source-level
first-change and transient-count obligations. Its separate observer checked
2,033 cases and 4,686 paired calls, deliberately reaching inaccurate raw
comparison bits while checking the narrower sufficient invariants. The argument is
not a claim that replacing a large symbol group by two preserves behavior.

The statement excludes malformed duplicate-name clauses and inconsistent
character equality, as does the membership-pattern theorem. It is about
distinct concrete values in a fiber, not arbitrary duplicated R vector
storage. No production source is changed by this argument.
