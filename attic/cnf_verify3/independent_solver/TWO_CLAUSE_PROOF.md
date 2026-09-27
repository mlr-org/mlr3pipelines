# At most two clauses: soundness and first-order saturation

This argument covers normalized finite-domain input with at most two clauses,
arbitrarily many symbols, and arbitrary nonempty finite domains. Constant
constructor arrangements and unusual character representations are separate
API questions. "Saturated" means no direct subsumption or useful first-order
SSE remains. It does not mean the formula is a canonical representation of
its truth function, or that these rules decide satisfiability in general.

## Initial constants and units

A constant FALSE clause makes the conjunction FALSE; TRUE clauses disappear.
If all remaining clauses are units, their constraints either concern distinct
symbols, or merge into their intersection. This is complete for unit
intersection and direct subsumption.

If there is one unit U on s and one non-unit C, preprocessing handles C before
matrices are built. If C contains all U values at s, the unit subsumes it.
Otherwise its s range is replaced by its intersection with U. It then is a
proper subset of U or is absent. If removing s makes C another unit, that unit
must concern a different symbol: the only symbol removed was s. Otherwise C
remains non-unit. No additional unit or non-unit clause exists which could
change U, so this propagation is already saturated. The cached unit-skip
optimization cannot run on this path because the comparison matrix is NULL.

## Two non-unit clauses

Write E(A,B) for the symbols at which A's range is not a subset of B's range.
The pairwise loop initializes both comparison directions from the actual
ranges before either callback. With no third clause there is no pending row
to another donor and no unrelated callback which can modify either range.

If E(A,B) is empty, A subsumes B and one clause remains. If both E sets have
at least two symbols, neither first-order operation applies. A useful
second-order operation needs two donor clauses other than its target. The
twoend handler can technically submit the same donor in both roles; with exact
comparisons this attempt fails its intersection guard, because
`A_p intersect A_p = A_p` is not a subset of the selected target range. Thus
the lack of a second distinct donor prevents any second-order change here.

Consider E(A,B)={p}. Every other A range is already a subset of B's range.
The only offered first-order operation is

```
B_p := B_p intersect A_p.
```

If this is a no-op, B_p was already a subset of A_p. If the reverse direction
then makes a useful change, apply the following argument with A,B exchanged.
If it is a proper restriction and does not eliminate a symbol/clause, then:

* A still is a non-subset of B at p, since restricting B cannot restore that
  inclusion. A's other ranges and all B ranges outside p are unchanged.
  Therefore E(A,B) remains {p}; repeating its operation is now a no-op.
* B becomes a subset of A at p. E(B,A) can only lose p; it cannot acquire a
  different exceptional symbol because only B_p shrank. The outgoing update
  immediately processes this TRUE-to-FALSE flip.
* If the new E(B,A) is empty, B subsumes A. If it equals {q}, q is different
  from p. But A_q was already a subset of B_q, so the newly offered reverse
  intersection of A_q with B_q is a no-op. A larger exceptional set offers no
  reverse first-order operation.

The same reasoning covers a pivot range becoming empty while B remains a
non-unit: symbol deletion is the empty-set instance of the above relations.
If instead B becomes a unit, B originally had just p and one other symbol q.
Since A was a subset of B outside p, A cannot contain any symbol other than
p,q. Its q range is already a subset of B_q. Registering B therefore either
eliminates A when these q ranges are equal or leaves A's already-proper q
range unchanged. This only has one unit and one non-unit, so no third-clause
recursion or stale strictness witness can arise.

Thus at most one useful non-unit range intersection is possible before the
pair reaches first-order saturation or reduces to the unit cases above.

## HLA cannot change the stored result

With two surviving non-units, each target has at most one other donor. If the
donor has more than one exceptional symbol, HLA does nothing. If it has one,
the extension adds the complement of that donor's pivot range. A donor value
which was absent from the target remains absent after this extension, so the
virtual target is neither tautological nor subsumed by the same donor. There
is no second donor to enable a later extension or hidden subsumption. The
single-use argument in PROOFS.md prevents repetitions from adding anything.

With one unit and one non-unit, non-unit HLA has no donor, and unit HLA has at
most that single non-unit donor. The same missing-donor-value argument rules
out eliminating the unit. With only units, HLA is not needed.

Consequently a second simplification pass has no rule left which can change
the stored clause/range sets. This proves idempotence modulo representation
ordering for all such normalized inputs, independently of domain sizes and
the number of common symbols. It also shows that every confirmed residual
SSE/subsumption mechanism in this investigation necessarily needs at least
three input clauses.

## Implementation and validation boundaries

The source inspection used the pair initialization order, its checks of both
directions, and the callback immediately after an outgoing TRUE-to-FALSE
change. It does not assume that arbitrary larger formulas reach saturation.
The general local logical rule proofs establish truth preservation on each
of the limited paths above. `two_clause_probe.py` tries to falsify the claim
over all 15 nonempty two-bit domain-pattern masks on one through four symbols,
with two clause orders and varied within-clause orders. It checks both semantic
oracles, direct residual-rule detection, and a second simplification pass.
The script alone is not an exhaustive proof for unbounded symbols; the
argument above supplies that extension.
