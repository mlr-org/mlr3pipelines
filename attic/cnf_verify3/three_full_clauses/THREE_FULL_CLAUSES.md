# Three full clauses: independent incidence analysis

2026-09-06. Production source remains unchanged at SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
This directory contains only analysis and controls for the precise input
shape of three canonical proper clauses, each with exactly the same three
distinct symbols. Every original literal range is nonempty and proper in an
ordinary finite domain. Clause and within-clause symbol orders are arbitrary.

The root's completed aligned-order membership-profile enumeration is separate
evidence. None of the checks here repeats or modifies it, and no all-order
claim below follows from treating its aligned cases as all-order coverage.

## Status

The independent analysis gives a proposed source-level **first-call local
saturation theorem for arbitrary within-clause orders**, under the existing
reviewed soundness, lifecycle, and comparison-owner lemmas. The new algebraic
core is the incidence of one value among the three persistent clauses. It is
not a restatement of the profile enumeration.

The unit part is a short complete argument. The SSE1 and SSE2 scheduling
parts below close the specific structural obligations encountered during this
investigation, but require the same careful source review as other proposed
campaign proofs. In particular Sections 5 and 7 expose the exact pending-visit
cases rather than assuming that exact final comparison counts imply complete
scheduling. No mechanized execution model of R is claimed. The local SMT
checks verify set-transition implications, not callback-stack behavior.

No concrete ordering counterexample was found. The 36,648-arrangement control
also found no set-level output dependence on symbol order, but **output
confluence is not claimed or proved** here.

## 1. Matching projections and units

Write the original ranges as `C[i,s]`, and retain the ghost identity of each
clause while its actual ranges shrink. Fix a symbol `s` and a value in any
`C[i,s]`. Satisfy clause `i` with that value. Assign the other two symbols to
arbitrary values in the corresponding ranges of the other two clauses. Those
two assignments are independent. Thus every value in

```
W_s = C[0,s] union C[1,s] union C[2,s]
```

extends to a model of the original formula. This also proves that every
input of the stipulated shape is satisfiable.

If a sound intermediate formula has a unit `s in U` descended from clause
`i`, entailment requires `W_s subset U`, while monotone ghost storage gives
`U subset C[i,s] subset W_s`. Therefore

```
U = C[i,s] = W_s.
```

The retained symbol of a newly born unit was never narrowed. Every other
current `s` range was contained in the unit's range from the beginning.
Consequently actual unit propagation in this input class **never narrows a
range or removes a symbol**. It can only eliminate a clause whose `s` range
equals `W_s`; an initially strict subset can never grow to equality.

There is no first unit merge that could create a smaller effective domain:
every unit on `s` would have the same `W_s`. The equality-skip optimization
therefore cannot exploit contextual containment debt in this shape. Before
unit birth, ordinary source/target updates supply exact FALSE containment
certificates; no unit propagation thereafter changes a raw comparison.

Units nevertheless can occur. The directed control over domains `{0,1,2,3}`
is

```
C0: X in {0,1,2} | Y=0 | Z=0
C1: X in {0,1}   | Y=1 | Z=1
C2: X=2          | Y=2 | Z=2.
```

Every one of its 216 within-clause order arrangements produces `X in
{0,1,2}` with C1 and C2 unchanged. A second call is nonproductive.

The projection also characterizes possible units more closely. Fixing `s`
outside `W_s` leaves three clauses on the other two symbols. They have a
model exactly when two ranges on at least one of those symbols overlap:
choose a value in that overlap and satisfy the third clause on the remaining
symbol. Conversely, if each of the two range families is pairwise disjoint,
one value per symbol can satisfy at most two clauses. Thus a proper entailed
unit on `s` requires pairwise disjoint original ranges on each other symbol.
An actually represented unit additionally requires one original `s` range
to contain the other two. These conditions show that at most one symbol can
have a proper represented unit in this full-three shape.

## 2. Incidence of one value

Use three live nonunit ghosts A, B, T and fix a symbol `t` and a value `v`.
All actual literal ranges are monotone, and Section 1 removes unit-induced
range changes from consideration.

**All-three incidence.** If all three `t` ranges contain `v`, none can be
the first to lose it. An SSE1 bound contains its donor's `v`; an SSE2 bound
contains the union of the other two donors' `t` ranges. A deleted ghost is
irrelevant to a later three-live-clause witness.

**A single absence freezes other coordinates.** Suppose A and T contain
`v` at `t`, while B does not. No range of B at a different symbol `q` can
change. SSE1 from A or T at `q` needs containment at `t`, contradicted by
`v`. SSE2 from A,T at `q` either needs that same containment, or uses `t` as
its intersection pivot, in which case `v` violates the intersection premise.
Since the original B is full, both of its other literal ranges remain
nonempty throughout this interval.

**A second absence leaves a lasting dominance.** If A is the next clause to
lose `v`, its bound cannot come from T and cannot be the SSE2 union of B,T.
The operation is SSE1 from B at `t`, establishing

```
B_q subset A_q  for every q != t.
```

This off-`t` dominance persists for as long as A,B,T all survive and T keeps
`v`. A later restriction of A at `q != t` cannot come from T alone, because
of `v`. Every other possible bound on A_q includes B_q. Shrinking B itself
preserves its containment in A. Actual units cannot change this reasoning.

These are local set facts independent of initialization, pivot order, or
the cardinality of any ordinary finite domain.

## 3. Why new union exclusions cannot explain a final independent SSE2 gap

Take a proposed final useful SSE2 rule on T, with intersection symbol `s`,
restriction symbol `t`, donors A,B, and a retained witness

```
v in T_t minus (A_t union B_t).
```

After earlier direct/SSE1 cases are excluded, the static coverage theorem
requires both donors to have an `s` exception against T; their exception
sets are `{s}` or `{s,t}`, with at least one `{s,t}` for an independent
twoend case.

The value `v` cannot have occurred in all three original `t` ranges, by
Section 2. If it occurred in A and T but not B, its removal from A leaves
`B_s subset A_s` permanently. The SSE2 intersection premise then says

```
B_s = A_s intersect B_s subset T_s,
```

contradicting B's required `s` exception. Exchange A,B for the other
two-occurrence case. Hence **every final independent SSE2 witness `v` was
present only in T initially**.

It follows that T_t is unchanged throughout the execution: any SSE1 or
SSE2 restriction at `t` would remove this `v`, which never belonged to either
donor. Consequently a donor's `t` exception can only disappear as its own
range shrinks; target restriction cannot create a new one. In particular,
the missing notification for a donor shrinking an already-contained `t`
range cannot independently explain a final rule in this three-clause shape.

## 4. The only first-order scheduling risk

Use the reviewed ordinary-visit classification: initialization and a source
comparison decrease to count zero or one own a visit; a zero-to-one target
transition retains the pending zero visit. A surviving count-one rule can
become useful without such a visit only when its donor loses a value at its
existing exceptional symbol while the exceptional bit remains TRUE.

Suppose A-to-T has a final omitted SSE1 restriction at symbol `t`, witnessed
by `v in T_t minus A_t`. Consider the first removal of `v` from A on a path
where A-to-T's count-one role was already visited. If the role is instead
first formed later, the ordinary comparison-decrease visit handles it.
Section 2 shows that B lacked `v` originally, that B's other ranges have
been frozen, and that the removal from A must be SSE1 from B at `t`.
At that moment

```
B_q subset A_q subset T_q  for q != t.
```

Since B's off-`t` ranges were frozen and T only shrinks, B-to-T was already
a zero/one-exception rule at its pair initialization. If that pair was
previously processed with B,T surviving, it would have removed T or `v`.
Thus the putative gap must occur before B-to-T is initialized.

There are only three clauses, all originally of length three. A-to-T has
already been visited, so A,T are the first two clauses and B is the current
third outer clause. If their order is T,A,B, the outer B visits T first, which
already resolves the witness. The only potentially troublesome order is
therefore A,T,B, with B currently comparing to A.

## 5. The A,T,B relay cannot lose its repair donor

At that A/B initialization, production processes inner A-to-outer B first
(lines 608-623). If A's other ranges equal B's, A-to-B has only the `t`
exception: `v` belongs to A_t but not B_t. Its restriction makes B a subset
of A, and B removes A, either by its source update or the reverse initialized
visit. A cannot be a final survivor.

Otherwise some off-`t` A range strictly contains B's. When B restricts A_t
and both A,T survive, the resulting relay state is

```
A_t subset B_t;   v in T_t minus B_t;   A_t not-subset T_t;
B_q subset A_q subset T_q for both q != t;
at least one A_q not-subset B_q; both B_q are nonempty.
```

The last line is what rules out the intuitive but incorrect example where
A immediately deletes B before B's pending T comparison. In that equality
case the earlier A-to-B initialization direction already deletes A instead.

Every possible primitive restriction preserves this relay or immediately
removes A, T, or `v`:

* A and B cannot usefully narrow their off-`t` ranges. T alone fails the
  `t` containment premise for an SSE1 restriction of either one. A relevant
  two-donor bound contains an already covering counterpart range.
* A_t is already inside B_t. A restriction using B, or the B/T union, does
  nothing. A restriction using T makes A a subset of T. Only A-to-T can
  acquire a new outgoing subset bit at that step, so the ordinary source
  update immediately deletes T. Complete removal of A_t leaves two
  nonempty other literals and has the same single changed comparison.
* Narrowing B_t with A/T's union preserves A_t and the relay. If it makes
  B_t equal to A_t, B immediately subsumes A. Narrowing B_t with T makes B a
  subset of T; the source update visits A before T, and either immediately
  deletes A or immediately deletes T. No unrelated callback is available
  between those two zero-count possibilities.
* A first-order restriction of T_t removes `v`; its two-donor restriction
  does likewise, because A_t is inside B_t. Restricting an off-`t` target
  range with the A/B union supplies a bound already contained in that target
  range, so production's range helper deletes T directly.
* Neither A nor T can directly subsume B: the former fails the strict
  off-`t` comparison and the latter fails at `v`.

The exact local SMT control exhausts the SSE1/SSE2 set implications in this
list. The source observation about immediate zero-count callbacks is an
additional manual obligation, not a consequence of that SMT result.

Thus B survives to its B/T initialization unless A,T or `v` has already
disappeared. That pair removes T or `v`, contradicting the proposed final
SSE1 remainder. This closes the unchanged-bit SSE1 route without adding a
fourth donor or a narrowing unit. The known four-clause first-order example
escapes precisely by using its fourth clause to create a unit that changes
an off-pivot range and deletes the repair donor; Section 1 and the three-clause
count prohibit that escape here.

## 6. Remaining second-order notifications

Assume a final independent SSE2 witness as in Section 3. Target shrinking
cannot newly enable the abstract set premises: donor containments and the
intersection containment only become harder, and shrinking T_t cannot create
a value outside the donor union. Section 3 moreover makes T_t constant for
this final witness and removes later union exclusion as its cause.

Only donor changes can newly establish an otherwise missing set premise:

* A donor shrinks its intersection range. If its pivot exception remains,
  production's `on_update_range()` visits this target and tries both
  orientations of a twoend. If the exception becomes a subset, the ordinary
  source comparison-decrease visit provides direct/SSE1 coverage.
* A donor shrinks another range and newly satisfies an outside containment.
  Its TRUE-to-FALSE comparison decreases the row to at most two and supplies
  the ordinary visit.
* A change to an already-contained unrelated range cannot change any of the
  rule's relevant set premises. A change to an already-contained union range
  cannot supply the final independent witness, by Section 3.

At the initial second-order boundary, every live twoend pair is put in the
manual queue. A disabled pair retains that later queue visit. Conservative
extra TRUE comparisons have their usual pending source-update owners; the
last relevant decrease supplies a visit once both donor roles are visible.

The special issue that still needs an explicit argument is a new twoend
role caused by a reverse comparison increase while an earlier oneend role
has not been acted upon. That argument is next.

## 7. A pending oneend cannot be hidden by a new pivot exception

Choose a final donor A with the permanent `t` exception of Section 3.
A new `s` exception of A relative to T must come from shrinking T_s.
An SSE2 bound `A_s union B_s` preserves any earlier `A_s subset T_s`, so
only an SSE1 restriction of T_s by B could create it.

Immediately before such a transition, if A's off-{s,t} range is already
contained in T, A is a useful sole-`t` donor and its ordinary visit would
remove `v`. Thus its visit must be uninitialized or pending. If its third
range is not yet contained, its later containment supplies a fresh count-two
visit and there is no missing notification.

Uninitialized pairs and pending initialization visits occur before the
second-order queue is built. A resulting count-two pair is therefore caught
by that queue. After enablement, a nonempty source comparison decrease
installs its new count and invokes its ordinary handler without an intervening
recursive call. The only ordinary count-one visit that can remain pending
while another callback runs is the batched visit after a whole symbol is
removed.

Here that removed symbol must be A's third symbol `r`, distinct from s,t:
t remains exceptional, and an s range that has been removed cannot later
become an s exception. Any rule that removes A_r while T retains `v` forces
`B_s subset A_s`:

* SSE1 from T at r is blocked by `v in T_t minus A_t`.
* SSE1 from B requires `B_s subset A_s` directly.
* SSE2 from B,T must use t as its intersection pivot; using s would require
  the impossible `T_t subset A_t`. Its off-pivot premise again requires
  `B_s subset A_s`.

When the pending A-to-T row has only the t exception, A_s is contained in
T_s. Consequently B_s is contained in T_s too. B cannot have the s exception
needed for the intervening sole-s SSE1 restriction. A two-donor restriction
of T_s preserves A_s, and a bound already covering/subsuming a target only
removes a purported survivor. Therefore the pending visit cannot lose its
useful sole-t role through the proposed reverse transition.

If an outer source-update loop has not yet cleared A's actual s containment,
the cache still has the extra TRUE. A target change before that later repair
does not erase all notifications: its source-update tail checks the changed
exceptional range, or the later outside-comparison decrease dispatches the
current twoend. The exceptional bit at t is never falsely FALSE, because
its raw noncontainment is permanent.

This rules out the reverse oneend-to-twoend escape for this final witness.
Together with Section 6's enabled-premise visits and both pivot orientations
in the actual twoend handler, it excludes a final independent SSE2 remainder.

### Exact source reasons for ordinary-visit deferral

All line numbers below refer to the unchanged source hash at the top of this
document. This is the branch audit behind the pending-visit split, including
paths that do not themselves defer an ordinary visit.

| Source lines | Owner or branch | Consequence for the proposed witness |
|---|---|---|
| 608-620 | Initialization installs both directional counts before invoking the inner-to-outer ordinary handler. The reverse direction reads its count only after that handler returns. | The reverse ordinary visit can be pending during recursion, but all such initialization happens before line 628 enables second order. Its final twoend state is included in the queue made at lines 629-640. |
| 613, 615, 622-623 | Initialization breaks/skips after NULL or an inactive inner/outer clause. | The reviewed caller lifecycle checks are needed here. A final A/B/T witness has three active nonunit operands, so its operand's permanent retirement cannot preserve the proposed witness. These guards are not new first-order owners. |
| 221-224 | A nonempty source update clears one TRUE bit, decrements the count, checks the threshold, and invokes the ordinary handler. | No recursive call occurs between the count-one write and this invocation. There is no newly pending count-one window at this site. |
| 203-216, 225-226 | Earlier rows in that source-update loop can recurse before a later actual containment has been written to the cache; an inactive source returns. | This is conservative TRUE comparison debt, not a consumed count-one visit. If the source survives, the later repair or line 231's source-range notification remains. If it retires, it cannot be a final operand. |
| 264-266, 277-288 | Complete literal removal clears all affected source-column TRUEs and decrements their counts as a batch, then invokes their ordinary handlers in clause-index order. Earlier rows can recurse first. | This is the only enabled-phase site that can leave a newly installed ordinary count-one visit pending during another callback. Section 7 analyzes the deleted third symbol; Section 5 analyzes complete pivot removal in the relay. |
| 245-252, 255, 261 | Literal deletion returns early for a new unit, a missing comparison matrix, or an uninitialized source matrix. | A final independent SSE2 operand is nonunit. The two matrix-init exits occur before this operand participates in the enabled second-order stage. For the specific relay and third-symbol cases, the two remaining literals are proved nonempty, so unit birth is unavailable. |
| 284, 298-303 | A pending visit reads its current count; count greater than the threshold skips, and enabled count two dispatches SSE2. | The visit carries a pair rather than a frozen operation. A count-one-to-two transition is precisely why Section 7 needs an argument; it must not be dismissed merely by citing ownership. |
| 317-325 | An ordinary count-one visit applies the first-order restriction before checking whether second order is enabled. | Neither phase disablement nor the matrix flag can suppress this ordinary SSE1 operation. A retained witness outside the bound cannot survive it as a no-op. |
| 317, 327, 646 | A manual-queue visit passes `second_order_only=TRUE`; a pair's matrix flag can also defer second-order handling. | These visits do not replace ordinary ownership. A queued pair was count two when the queue was made. If it later becomes count one, the source transition that created that count owns the ordinary visit described above. The flag check is after ordinary SSE1 for every FALSE-flagged ordinary call. |
| 309-311 | A handler entered with count zero deletes its target before any recursive call. | Section 5 additionally identifies the exact source-row order that reaches this branch on a relay exit, instead of treating every transient zero as an irrevocable deletion decision. |

The only helper that can register a new unit on this input class subsequently
does no range narrowing (Section 1). It therefore adds no omitted enabled-phase
ordinary-visit owner or nested comparison-update case to this table.

## 8. Units, deletions, and arbitrary symbol order

The previous sections concern final surviving nonunit operands. A unit
participant reduces to the earlier unit/direct/SSE1 cases or unit HLA under
the static SSE2 classification. Unit propagation has already been shown to
leave no equality remainder in this shape.

The reviewed HLA saturation argument then excludes a remaining refutable
target: the virtual target procedure is complete with its current donors,
and removing donor clauses later cannot create a refutation that did not
exist at the target's earlier visit. Actual clause ranges do not change in
the HLA phase, so these deletions cannot create a fresh range-restriction
obligation among final survivors. Constants are separate, and the matching
model excludes a sound FALSE result for the input shape here.

None of these arguments fixes the stored order of X,Y,Z. Pair initialization
computes all coordinate comparisons before its ordered clause callbacks.
The source-update loops use clause indices. The twoend handler's two pivot
orientations may occur in either order; the arguments above require both
scheduled orientations and the resulting pending-visit ownership, not a
particular first orientation. HLA uses the unique currently exceptional
symbol of a donor. Thus the proposed theorem is an all-order saturation
claim, **not** an all-order reduction of the root's aligned enumeration.

## 9. Exact independent controls

`ordered_controls.py` builds clauses by explicit coordinate ranges and a
seeded generator independent of the root membership-profile generator. Its
scalar Cartesian-product evaluator is independent of the root positional
bit-mask evaluator. Each arrangement runs production twice and compares
clause/range sets while retaining clause multiplicity.

Results in `ordered_controls_results.json`:

* Three directed inputs, each under all `6^3 = 216` symbol-order arrangements.
* 1,000 seeded full-three inputs, each under 36 arrangements fixing only
  the first clause's X,Y,Z order. This is an explicit bounded control, not
  a claim to cover every profile or every clause order.
* 36,648 arrangements and 73,296 production calls; zero second-call changes.
* 4,176 complete input/output model-set comparisons; zero differences.
* Zero observed set-level first-output differences between the orders of
  any one tested input. This observation does not prove confluence.

`local_transition_checks.py` uses independent bit-vector set operations and
Z3 to seek countermodels to 87 local implications. All 87 are UNSAT in
`local_transition_results.json`. Each local step has only three pre-state
sets per coordinate and deterministic intersection/union output, so any
ordinary finite-domain countermodel has at most eight membership atoms per
coordinate and is represented by the width-eight check. This exact local
quotient justifies these **algebraic implications**, not a scheduler theorem.

`trace_controls.py` saves an audited production replay and checks its result
against an unaudited call for each directed input. `directed_traces.json`
includes the known two-symbol boundary control with the exact symbol order

```
domain(X) = {1,2,4,5}; domain(Y) = {2,3,5}
C0: X in {2,4} | Y=3
C1: Y=5        | X in {2,1}
C2: Y=2        | X in {2,5}.
```

Its first output is `X=2` together with `Y=2 | X=2`; the second call deletes
the latter clause. Adding the proper common literal `Z=0` over `{0,1}` to
all three clauses instead produces only `X=2 | Z=0` in one call, under all
216 symbol arrangements. This shows why simply appending a common third
literal is not a counterexample to the proposed full-three theorem.

Commands from the repository root:

```
attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/three_full_clauses/ordered_controls.py
attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/three_full_clauses/local_transition_checks.py
attic/cnf_verify3/independent_solver/.venv/bin/python attic/cnf_verify3/three_full_clauses/trace_controls.py
```

## 10. Smallest remaining review obligations

The remaining work is independent source review of the proof's scheduling
translation, not another large aligned-order enumeration:

1. In Section 5, verify the stated immediate zero-count paths from a relay
   exit, especially that complete pivot deletion leaves the source nonunit
   and that no earlier changed row can destroy the proposed direct deletion.
2. In Section 7, verify the exhaustiveness of ordinary pending count-one
   owners after second-order enablement and the handling of a source range
   whose TRUE-to-FALSE repair is still pending when another callback runs.
3. Confirm that combining these arguments with the existing HLA/static SSE2
   reductions needs only physical unit containment plus absence of equality
   leftovers, both supplied by Section 1, and introduces no stronger
   unproved transient-cache premise.

These are explicit obligations for review of the proposed theorem. The
controls passed, and no ordering witness has been found, but those facts
should not be substituted for checking these source-level steps.
