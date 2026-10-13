# Independent review of the repeated-pass proof

Reviewed 2026-09-06 against `repeated_passes/PROOF.md`, the family generator,
the static rule checker, and the unchanged `R/CnfFormula_simplify.R` whose
SHA-256 is `7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
This review makes no production edits and creates no commits.

## Verdict

The induction and the finite convergence argument are sound under their
stated canonical finite-set assumptions and normal execution. The reverse
family has exactly `n` productive passes, while its forward-order control
has one. No callback or manual-queue ordering case found in the source
invalidates the claimed delay. A nonproductive call cannot be followed by
a productive call when the same returned representation and universe are
passed back without external mutation.

The proof distinguishes clause/value stability from exact storage stability
correctly. The minimized three-pass member does need an additional sort-only
call; that call does not contradict the statement that a nonproductive
call's *output* is already a fixed point.

## Independent check of the symbolic argument

The family is defined with binary `T,S0,...,S(n+1)`, guard domain
`G={g0,...,gn}`, and clauses

```
A0 = (S0=0 | S1=0)
Bi = (Si=1 | T=1 | G=gi),                    0 <= i < n
Ai = (Si=0 | S(i+1)=0 | T=0 | G in P_i),     1 <= i <= n
P_i = {g0,...,g(i-1)}.
```

All literals are nonempty proper subsets, including the boundary `n=1`.
Every shortened target still has three symbols, so the family has no units.
The given model `T=1`, all selectors `0` satisfies every frontier state.

The clause-deletion witnesses are valid at every frontier, not just at the
initial and final states. They falsify exactly the selected clause. In
particular, the `B0` witness uses `g1`, which exists for `n=1`; the general
`Bi` witness uses `g0`, which every target guard contains. The literal-value
witnesses protect each selector and each individual target guard value, plus
the blocker's `T` and selector values and the guards of `B0` and `B1`.
Thus it is legitimate to reduce the remaining syntactic analysis to target
`T=0` deletions and the unprotected guards of blockers with index at least 2.

For a blocker's guard restriction, any `A` donor has two selector exceptions
outside the proposed guard pivot. A different blocker `Bj` has exceptions
`{Sj,G}`. Two blocker donors can fit a common intersection selector only by
being the same blocker, whose nonempty self-intersection on the absent `Sj`
fails the necessary containment. This rules out the remaining blocker
changes; the proof does not incorrectly claim semantic essentiality for all
blocker guards.

For a target `Ai` at pivot `T`, a blocker must be `Bj` with `j<i`, using
`Sj` as the intersection symbol. Among `A` donors, only adjacent clauses
can leave one selector outside the target. The right neighbor requires
`j=i+2`, incompatible with `j<i`; the left neighbor gives exactly
`A(i-1),B(i-1)`. Two different blockers cannot share their required outside
selector. Two adjacent `A` donors have different outside selectors. Repeated
donors fail the self-intersection premise on the absent outside selector.
Self-donation by the target cannot shrink its own pivot because the union
already contains its range. The mixed predecessor pair is therefore the
only useful local restriction, and it becomes useful exactly when its
predecessor has lost `T=0`.

The whole-clause witnesses exclude HLA deletions provided the separately
established soundness of virtual HLA additions is used. This is a justified
dependency on local semantic preservation, not on scheduler completeness:
under the other clauses, each virtual extension remains equivalent to the
starting target. A witness falsifying only that target persists throughout
its HLA exploration. It prevents both virtual tautology and subsumption by
another retained clause.

## Source scheduling and recursive callbacks

On the call starting from frontier `F_k`, the initial stable sort gives

```
A0, B0,...,B(n-1), A1,...,Ak, An,...,A(k+1).
```

The earlier shortened targets appear in increasing order because each new
short target is moved behind the previously sorted length-three prefix on
the next call. The remaining length-four targets keep decreasing order.
One should not assume every just-returned intermediate `F_k` is already
sorted: its newest short target can still be at the end. The next call's
initial sort is what establishes the displayed induction order.

The queue at source lines 628–646 is a snapshot of count-two pairs. Its
`which(..., arr.ind=TRUE)` result is enumerated by columns, which are target
indices. Every `Bi -> A(i+1)` has exactly `{Si,T}` as its exceptional set and
is in that queue. Consequently the unshortened targets are visited in
decreasing index order. The inverse guard in `try_sse_2nd_order()` rejects
the predecessor pair while its `T` range still equals the target's `{0}`.
At the unique frontier target, the predecessor lacks `T`, the inverse guard
passes, the selector intersection is empty, and union of the pivot ranges
is `{1}`. Exactly the target's `T=0` is removed.

The absence of a recursive successor wakeup is established by the actual
comparison direction. `is_not_subset_of[[new_donor]][target,T]` was FALSE
for every still-unshortened target: `{0}` was already contained in `{0}`.
Deleting the donor's `T` leaves those bits FALSE. At lines 264–285,
`eliminate_symbol_from_clause()` dispatches only outgoing rows that changed
from TRUE to FALSE. It does not dispatch these targets and has no final
`on_update_range()` call. The incoming bits are updated before dispatched
callbacks, but count increases do not enqueue fresh work.

The dispatched callbacks retain their specified target through the SSE
handlers. Their targets are blockers, the seed, or already shortened
targets, none of which admits a productive rewrite by the classification
above. Hence they cannot initiate a mutation cascade that eventually reaches
the missed successor. That successor's manual target column has already
been visited, and the fixed snapshot does not requeue it.

In the forward control, the successor's column is still pending. Updated
incoming comparisons are already available when that column runs, so the
same rule fires there in the same invocation. This accounts for the full
forward cascade without requiring a missing callback.

## General bound and the last sort

The value-occurrence potential tracks *actual* stored clauses, with clause
multiplicity retained. Intersections remove values, symbol deletion removes
a nonempty range, unit merge deletes one old clause and only intersects the
retained clause, and HLA modifies virtual storage before deleting a target.
No source operation increases any surviving actual range. Each productive
pass therefore decreases the nonnegative integer potential by at least one.
Terminal logical values can be assigned potential zero.

If a call does not decrease that potential, no actual clause/range mutation
occurred. Its possible storage change is the stable initial length sort.
The next call begins with exactly that sorted storage; local registries,
comparison matrices, and HLA virtual state are reconstructed. There is no
persisting hidden queue or cache that could create later work. The same
nonmutating execution repeats, establishing exact storage equality.

Thus `V_initial` bounds productive passes and `V_initial+1` calls suffice
to *produce* a fixed representation. One additional call can be necessary
for an equality-based loop to *observe* it. The argument does not depend on
logical strengthening: all productive outputs are logically equivalent.

The constructed family has `2n+1` clauses and initial potential
`2+6n+n(n+1)/2`. It gives a linear lower bound in its clause count and proves
the absence of a constant pass bound. It does not establish tightness of
the more general value-occurrence bound.

## Independent executable checks

`check_repeated.R` independently builds the family from the displayed
mathematics and uses ordinary exported constructors from the installed
development package. It does not call `repeated.py` or copy its bridge
traces. It ran in R 4.6.1 (2026-06-24), package version `0.11.0.9000`, using
the installation and provenance recorded in `execution_modes/NOTES.md`.

For `n=1,...,6`, the check covers:

- All 27 prefix states, 251 clause-deletion witnesses, and 876 protected
  literal-value witnesses.
- 27 reverse-order calls, including a final exact-equality check for each
  member, and 12 forward-order calls including equality checks.
- Complete finite-domain truth tables, with 51,168 output assignment-row
  comparisons. No SAT/MDD implementation or bit-vector evaluator is used.
- Exact expected remaining ranges, value-occurrence mass, and the actual
  stable-sort clause order after every reverse-order call.
- R's column-major matrix-index enumeration in 225 matrix shapes.

Observed masses were:

```
n=1:  9 ->  8 ->  8
n=2: 17 -> 16 -> 15 -> 15
n=3: 26 -> 25 -> 24 -> 23 -> 23
n=4: 36 -> 35 -> 34 -> 33 -> 32 -> 32
n=5: 47 -> 46 -> 45 -> 44 -> 43 -> 42 -> 42
n=6: 59 -> 58 -> 57 -> 56 -> 55 -> 54 -> 53 -> 53
```

The saved minimized member was independently replayed for five public calls
on all 64 assignments, adding 320 output assignment-row comparisons. Its
mass is `22 -> 21 -> 20 -> 19 -> 19 -> 19`, and raw storage changes on calls
1–4 but is identical on call 5. Every truth vector equals the original.
This independently confirms the final sort-only phenomenon as well as the
three productive calls.

All checks passed; detailed output is in `repeated_checks.log`, and machine
records are in `repeated_checks.rds`. Reproduce from the repository root:

```
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/quotient_review/check_repeated.R
```

## Scope

The statements concern canonical finite-domain objects: each clause has one
entry per symbol, each range is a nonempty unique character subset of a
fixed domain, and the simplifier returns normally. They do not cover the
accepted malformed matrix/NA selector objects studied in `r_values/NOTES.md`,
or external mutation between passes. Arbitrary `n` is a mathematical family
and source induction, subject to the stated ordinary runtime/resource limits;
the finite executions are supporting checks, not the unbounded proof. No
claim is made that the final result is a complete SAT decision procedure,
a unique logical normal form, or independent of input ordering.
