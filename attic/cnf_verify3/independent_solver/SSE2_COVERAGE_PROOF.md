# Static second-order candidate coverage

This is a completeness argument about **fixed** current clause ranges and
exact pairwise comparisons, not the event-driven update schedule. The dynamic
counterexamples already demonstrate that a candidate becoming useful later
can miss a revisit. The theorem concerns canonical finite sets; it is separate
from the previously proved soundness of the second-order rule.

## Premises of a useful rule

For donors A,B and target T, choose distinct symbols s,t with

```
A_v subset T_v and B_v subset T_v for all v outside {s,t};
A_s intersect B_s subset T_s;
T_t minus (A_t union B_t) is nonempty.
```

The proposed rule restricts T_t to its intersection with A_t union B_t. Let
E_A and E_B be the symbols where each donor is not a subset of T. The first
premise gives `E_A, E_B subset {s,t}`. The final premise immediately gives
`T_t not-subset A_t` and `T_t not-subset B_t`, exactly the two inverse guards
in `try_sse_2nd_order()`. First consider live non-unit donors and target, for
which all matrix/registry indices are present. Unit cases are handled below.

## Exhaust the exceptional-set arrangements

1. If either E is empty, that donor directly subsumes T.

2. If either E is {t}, that donor already supplies an SSE1 restriction of T_t
   to A_t or B_t. This is at least as strong as intersection with their union,
   and is useful because the final premise includes a value outside both.
   Therefore this arrangement cannot be an independent SSE2 opportunity once
   direct subsumption and SSE1 are saturated. This case includes a donor
   omitting the intersection symbol s, which can never belong to E.

3. In every other arrangement, both E sets contain s, so both donors actually
   have a nonempty range at s. If both E sets equal {s}, all donor ranges
   outside s are contained in T. Extend T_s by the complement of A_s. For any
   value of B_s missing from the old T_s, the intersection premise says it is
   outside A_s, so it is added by that extension. B is consequently a subset
   of the extended target at every symbol. HLA eliminates the entire target;
   a separately enumerated oneend/oneend SSE2 pair is unnecessary for clause
   deletion. A complete HLA pass therefore covers this arrangement.

4. The remaining arrangements have E_A={s,t} or E_B={s,t}; select such a donor
   as the twoend. The other donor's E is {s} or {s,t}. The manual second-order
   queue includes the twoend/target pair whenever this fixed state is the
   state from which the queue is built. The twoend handler considers s as
   intersection symbol and t as restriction symbol in one of its two outer
   orientations. T contains t because it has a value to remove. The other
   donor contains s, so it is in that symbol's registry, and its count is
   either one or two. Its exceptional columns are exactly among {s,t}, so the
   handler's column/count check passes. Both inverse guards pass as shown
   above, and the direct intersection guard passes by hypothesis. Thus the
   helper attempts the desired restriction.

These cases exhaust all subsets of {s,t}. There is no omitted static
arrangement of missing symbols, oneend donors, twoend donors, or the choice
of which symbol is used for intersection. The restrictions on registry
membership and inverse subset bits cannot independently discard a useful
rule outside the earlier-rule cases just described.

## Consequences and limits

If a final result has a useful SSE2 operation, at least one of the following
must hold: a direct subsumption/SSE1 operation was itself left pending; a
oneend/oneend HLA deletion was left pending; or a genuinely twoend operation
was enabled after its scheduled visit and was not revisited. Existing traces
exhibit the third case. This argument does not prove that applying a whole
batch of candidate operations once reaches a fixed point: those operations
change the ranges on which the original enumeration was based.

`sse2_static_coverage.py` checks the case split on all 255 squared nonempty
membership-pattern domain subsets for the two distinguished symbols. Off-
pivot literals can be any ranges satisfying the first premise and do not
affect the classification. This finite verification checks the exhaustive
set algebra; the proof above supplies arbitrary domains and other symbols.

## Unit clauses do not create an extra independent arrangement

At a quiescent boundary, all actual non-unit ranges are physically contained
in each applicable unit. If A is a unit on t, a target value outside A_t
cannot remain, contradicting usefulness. If A is a unit on s, B_s is contained
in A_s, so the intersection premise reduces to `B_s subset T_s`; B then has no
s exception and supplies an earlier subsumption/SSE1 case. If A is a unit on
another symbol, the off-pivot premise means it directly subsumes T. The same
reasoning applies with A,B exchanged.

If T is a unit, its symbol must be t. Every donor t range is physically
contained in T_t and every off-{s,t} donor range is absent by premise. Thus
both donors can have only the s exception, and their resolution is covered by
the unit-HLA case of the oneend/oneend argument. These reductions explain why
the actual second-order helper only needs live non-unit registry entries.
They depend on completed physical unit propagation, proved separately in
`UNIT_CONTAINMENT_ARGUMENT.md` and the independent lifecycle review.
