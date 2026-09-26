# Independent review of the three-full-clause theorem

2026-09-06. Reviewed production SHA-256:
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
No production source was edited.

## Verdict and exact scope

The proposed first-call local-saturation theorem is supportable, subject to
the finite ordinary-set, lifecycle, soundness, and normal-return premises
listed below. Two scheduling arguments need to be made more explicit than
they are in `../three_full_clauses/THREE_FULL_CLAUSES.md`:

* The first-order relay is resolved inside initialization, with second order
  disabled. Its actual row order gives a shorter argument than invariance
  under arbitrary later SSE2 steps. A pending reverse initialization visit
  also has to be distinguished from an uninitialized pair.
* For second order, a last-change argument should stabilize the **raw donor
  roles**, while an additional set invariant keeps the intersection premise
  valid. The owner of a conservative pivot TRUE must still have its target
  row and source-range notification ahead of it. Mere quiescent exactness,
  or the existence of some active update frame, is insufficient.

Sections 3–6 supply these details. In particular, the proof below does not
infer callback completion from the proposal's 87 local SMT implications or
from the completed aligned-order quotient. It is a source-level proof, not
a mechanized semantics of R.

The conclusion concerns exactly three input clauses, each originally
containing the same three distinct symbols, with every range nonempty and
proper in its fixed finite domain. Clause order and the three independent
within-clause symbol orders are unrestricted. Missing symbols produced by
the simplifier are permitted. Local saturation includes unit propagation,
direct subsumption, SSE1 restriction, two-donor SSE2 restriction, and the
domain-propagation/HLA clause-deletion predicate of the reviewed HLA theorem.
It does not mean complete entailment, canonical output, output confluence,
or exact ordered-object idempotence. A later sort-only pass is compatible
with this conclusion.

## 1. Premises used, and premises not used

Use the ordinary representation contract from
`../review_semantics/REVIEW.md`: unique nonmissing symbol names within each
clause; finite duplicate-free ordinary character sets as ranges and domains;
ordinary list, indexing, `%in%`, length, and copy semantics; no external domain
mutation; no count/index overflow. The statement is conditional on normal
return, and does not promise that a resource-limited R session avoids stack
or memory failure.

I use the reviewed local soundness and permanent-retirement facts, complete
registries for live operands, consistency of initialized counts with stored
bits, and eventual repair of live TRUE debt from
`../proof_state/LIFECYCLE_PROOF.md`,
`../proof_state/QUIESCENT_MATRIX_PROOF.md`, and their independent review in
`../review_semantics/REVIEW.md`. I use the ordinary-visit classification in
`../scheduler_review/REVIEW.md:79–185`, **without** importing its diagnostic
added rescans into production. Removing those added scans leaves precisely
the unchanged-exception source-range obligation discussed in Section 3 here.

The static reduction in `../independent_solver/SSE2_COVERAGE_PROOF.md` and
the domain-propagation deletion theorem in
`../independent_solver/HLA_SATURATION_PROOF.md` are used only at quiescent
boundaries. In particular, neither supplies a dynamic SSE2 visit.

I do not assume that every transient TRUE is an actual exception, that an
installed zero count irreversibly selects deletion, that every pending visit
retains its old count or pivot, or that a manual queue visit performs SSE1.
Those stronger claims are false for the inspected scheduler. A source update
can have another live row still in its finite scan; the pending handler at
line 285 reads the current count at line 298; and line 317 expressly suppresses
SSE1 for `second_order_only=TRUE`.

## 2. Unit behavior and the all-three reduction

Retain each original clause's identity while its stored ranges shrink. For
any symbol q and any original value in clause i's q range, satisfy i using
q. The other two original clauses can be satisfied using distinct remaining
symbols and their nonempty original ranges. Thus every value in the union
W_q of the three original q ranges extends to a model. In particular, every
input in scope is satisfiable.

A represented unit q in U descended from clause i is entailed by the input,
so W_q is contained in U. Actual range monotonicity gives
U contained in original C_i,q contained in W_q. Consequently all three are
equal. This proves that the unit's retained range was never narrowed, all
other q ranges were already inside it, and same-symbol unit merges cannot
narrow it. Unit propagation therefore causes no actual range shrink and no
symbol deletion in this input shape.

Equality elimination is also covered; physical containment alone would not
suffice. A live clause equal to U at q was equal from the beginning, and its
q range has never changed. If the pair is initialized, the unit donor's q
bit against that clause was FALSE on direct comparison and has no possible
FALSE-to-TRUE transition. If the pair is not initialized, its reverse
comparison is still the default TRUE, since q has never been removed or
changed. Either way the conjunction required by the skip at production line
133 is false. The range helper at lines 151–157 deletes the equal clause.
If the two matrices are unavailable, the shortcut is disabled at lines
115–123. No narrowing unit merge or transient raw-containment debt is being
assumed away here; neither can arise for this retained unit range.

At the end of propagation, therefore, surviving nonunit ranges are physically
contained in the units and no direct unit-equality remainder survives. HLA
later only removes actual clauses, so it cannot create a new equality.

For an independent SSE2 remainder there must be three distinct surviving
nonunit operands A, B, T after the earlier direct/SSE1/unit/HLA reductions.
These are the three original ghosts. No other ghost has existed, and none
can have retired, become a unit, and returned. This removes unit handling
and fourth-donor callbacks from all later witness arguments.

## 3. First-order relay: actual initialization closes obligation 1

For completeness, the ordinary-visit bookkeeping is as follows. Initial
counts zero/one own a visit (608–620); nonempty source decreases invoke it
immediately (221–224); complete literal deletion batches then visits affected
pairs (264–288). A zero-to-one reverse increase retains the outstanding zero
visit; an excursion above one followed by a decrease to one obtains a new
visit. A count-one handler uses the current pivot and donor range (298–322).
Target narrowing preserves a previously performed SSE1 restriction at the
same pivot. Source narrowing at another symbol either preserves it or
supplies one of these ordinary count-decrease visits.

It follows that a surviving omitted SSE1 restriction A→T at t requires a
loss from A_t while its already established exceptional bit at t remains
TRUE. Let v be a retained target value lost in that operation. Before this
loss v belongs to A and T. It cannot originally have belonged to all three:
no first loss from all-three membership is possible using an SSE1 donor or
an SSE2 union of the other donors. The third clause B has lacked v throughout.

While A and T still contain v, no off-t range of B can change: an SSE1 donor
A or T fails containment at t; an SSE2 operation either has that same
obstruction or uses t as intersection pivot, where the common value v
violates its premise. Thus B's two off-t ranges are still their original
nonempty sets. The loss of v from A must be SSE1 from B, and at that point

```
B_q ⊆ A_q ⊆ T_q   (both q != t).
```

B→T would have been a zero/one pair already at its initialization: its
off-t ranges were frozen and T could only shrink. Any completed ordinary
B→T visit would have removed T or v, since B_t has never contained v.

There is a small initialization qualification to the proposal's Section 4.
"Not already processed" does not alone mean "uninitialized": the reverse
direction can be pending while the first direction recurses. That pending
case cannot produce the required loss from A here. If B→T is the first
direction, it removes T or v before recursing. Otherwise the first direction
is T→B. Its only possible sole exception is t, because T has v and B does
not. Its update changes only B_t. Any ordinary nested B→A call caused by
that update has just **cleared** B's t comparison, so cannot be an SSE1
restriction of A at t. A resulting zero deletes A. A resulting one at an
off-t symbol q means B_q is not contained in A_q. This exception is exact:
B_q is frozen and target A_q only shrinks. The attempted restriction of A_q
leaves that same B_q noncontainment permanent until the supposed first loss
of v from A. Hence B cannot later be a sole-t donor for that loss. The
other possibility, a count above one, is skipped while second order is
disabled. This finite scan cannot manufacture the required loss from A_t.
Thus it cannot be hidden inside the pending reverse initialization visit.

Accordingly the unresolved B/T pair is uninitialized when the relevant
A_t loss occurs. A and T are the first two initialized clauses and B the
third outer clause. If their order is T,A,B, the outer B processes T first
and removes T or v. The only remaining order is A,T,B with B comparing to A.
At this point the only initialized off-diagonal pairs are A/T and A/B;
second order is FALSE (547, 628), and old initialization callbacks have
finished before the outer loop advances (571–625).

The inner A→outer B direction is considered first (608–615). A_t contains v
absent from B_t, so A→B can only be an ordinary count-one rule at t. If the
off-t ranges of A and B are equal, that direction restricts B_t into A_t;
the subsequent B→A comparison is zero and deletes A. Nonempty update and
complete pivot removal both have only the initialized A row to process for
B, because B/T is still NA. B retains its two nonempty off-t literals, so
the early unit-return at 245–252 is unavailable. If A's off-t ranges are
strict subsets instead, any newly available reverse off-t restriction has
a bound already strictly containing its target range and cannot prepare
the required loss from A_t. Therefore a relevant surviving loss has

```
B_q ⊆ A_q ⊆ T_q;
at least one A_q strictly contains B_q;
both B_q are nonempty                    (q != t).
```

The strict comparison makes the earlier A→B count at least two. Production
skips it in this phase. The reverse B→A visit at t can now remove v from A.
If B_t is contained in old A_t, the range helper deletes A immediately
(153–157). Otherwise write A'_t = A_t intersect B_t:

* If A'_t is contained in T_t, the A→T count becomes zero. The source scan
  visits T (index 2) **before** B (index 3), so line 224 reaches the deletion
  at 309–311 without an earlier distinct-row callback. If A'_t is empty,
  A still has its two nonempty off-t literals; the batched visit has the
  same T-before-B order (264–285). Neither unit early return applies.
* Otherwise A→T retains its t exception. A'_t is contained in B_t, so the
  only new forward repair is A→B at t. Its remaining count is the number
  of strict off-t A/B comparisons. Count two is skipped. Count one invokes
  an off-t restriction of B whose bound strictly contains B's current
  range, hence the helper returns unchanged at line 160. It creates no
  recursive update, unit, or deletion.

Thus either a final operand/v has disappeared, or the B→A handler returns
and initialization reaches B/T. The strict off-t comparison also implies a
strict off-t T/B comparison. Hence the first T→B direction now has at least
two exceptions and is skipped; the reverse B→T zero/one visit removes T or v.
This is the actual immediate-zero/path argument required by obligation 1.
It does not require extending the relay through hypothetical enabled-phase
SSE2 callbacks. No final SSE1 remainder survives.

## 4. A stronger set invariant for the second-order argument

Take a final useful SSE2 restriction on T_t with donors A,B and intersection
pivot s. Choose v in T_t outside A_t union B_t. The incidence argument in the
proposal's Sections 2–3 is valid: if v originally belonged to both donors,
there is no first loss; if it originally belonged to just A and T, A can
lose it only by an SSE1 bound from B at t. That operation establishes
B_q contained in A_q for q != t. Every later allowed bound on A_q contains
B_q, while shrinking B preserves the dominance. The final intersection
premise at s would then imply B_s contained in T_s, contrary to B's required
s exception in an independent SSE2 pattern. Exchange A and B for the other
case. Thus v was originally exclusive to T.

Two consequences hold throughout the life of the final triple:

1. T_t never changes. Every possible productive restriction at t uses a
   donor or donor union that excludes v and would remove it.
2. **For every q != t, A_q intersect B_q is invariant.** A useful SSE1
   restriction of A_q cannot use T, since T_t contains v absent from A_t.
   A two-donor restriction using B,T must have intersection pivot t;
   otherwise it also requires the impossible T_t containment. Its bound
   at q is B_q union T_q. The only possible bounds are therefore B_q and
   B_q union T_q, both containing B_q. Intersecting A_q with such a bound
   preserves A_q intersect B_q. The argument is symmetric for B. A repeated
   donor in the implementation is a degenerate SSE1 case and changes none
   of this. Actual unit narrowing is already excluded.

Consequently the final SSE2 intersection premise held from the beginning:
the donor intersection is constant and every earlier T_s contained its
final range. The retained union-exclusion witness also held from the
beginning. Neither premise needs a new value-change notification.

A useful related persistence fact follows. Once B_s is contained in A_s,
it remains so while the triple and v survive: every later bound on A_s
contains the current B_s. Equivalently, the constant-intersection argument
shows that B_s is then frozen as their intersection. This supplies the
persistence step needed after third-symbol deletion in Section 6.

## 5. Stabilize the final raw roles, then discharge remaining cache debt

After Section 3 and the static SSE2/HLA/unit reductions, a final independent
pattern has donor exception sets `{s,t}` and either `{s}` or `{s,t}`.
Write r for the third original symbol. Both donor r ranges are contained
in T_r, both s ranges are actual exceptions, and at least one donor, call
it A, has a t exception. Since T_t is constant and A_t only shrinks, A's
final t noncontainment has held throughout.

Choose the last actual transition of the six raw donor→T comparisons at
s,t,r, or the boundary immediately before second-order enablement if all
those comparisons already have their final values there. There are finitely
many changes of actual ranges. After this boundary every relevant raw role
is final. Section 4 ensures that the intersection and excluded-value
premises hold too, even though individual ranges may still shrink.

A last transition into one of these final roles has only two forms:

* A donor shrinks at r, or the eventual oneend donor shrinks at t, and
  changes a raw exception into containment.
* T_s shrinks and creates a donor's final s exception.

Target shrinking cannot create a donor containment. A donor's final s
exception cannot be created by shrinking that donor. T_t is constant. These
facts exhaust all coordinates and directions. If T_s newly makes a final
donor D exceptional, the responsible operation must be SSE1 from its
counterpart E; an SSE2 bound D_s union E_s preserves D_s containment. E_t
must be contained in T_t then and forever after. Thus D is the twoend A
with the permanent t exception. This reduces every unsignalled target
transition to the case analyzed in Section 6.

After the chosen boundary, every extra TRUE in a final donor row is
conservative containment debt. Its live nonempty source update owns a still
pending scan, or a whole-symbol deletion clears it before batching visits.
Clearing the last extra TRUE in one row decreases its count to the final
one or two and invokes the ordinary handler at 224 or 285. If the partner
still has extra TRUEs, that partner's later last repair supplies another
visit. This is an argument about the **last repairs after stabilization**,
not a claim that an arbitrary earlier repair saw the final pattern.

The inverse guard at t (453) cannot be falsely FALSE: v belongs to T_t and
neither donor at every such time. Sound raw FALSE comparisons are enough;
there is no requirement that transient TRUEs be exact.

At a valid stable-role visit, the handler has the source and partner in the
symbol registry, the needed one/two counts, both s exceptions, no outside
exception, the true inverse comparisons, and the actual intersection
premise (382–441, 453–467). It checks both orientations in the twoend
handler (416–445). Nested callbacks cannot retire these final operands or
invalidate the stabilized roles/premises. The desired call therefore reaches
the useful restriction and removes v, a contradiction.

If there is no post-initialization role transition, exact live comparisons
at 628–640 put the twoend pair in the initial queue. The same stable-role
argument applies to that visit. If a source-range notification is skipped
because its matrix flag is FALSE (354, 361), the already recorded manual
queue entry remains ahead of it and sets that flag TRUE before dispatch
(640–646). An ordinary count-two handler actually bypasses this flag check
(298–303); it must not be described as suppressed by line 327.

## 6. New pivot exception: pending visits and TRUE debt close obligation 2

Consider the remaining last transition: T_s is restricted by the sole-s
donor B, and A_s changes from contained to exceptional. Just beforehand,
A's raw role is sole-t: A_r is contained in T_r, A_s is contained in T_s,
and the t exception is permanent. A useful ordinary A→T visit would already
have removed v. The source contains s and t (both survive finally), so it
is not a unit.

If A's r bit is an extra TRUE, its later last repair after stabilization
is already the visit of Section 5. Assume it is FALSE. There are then two
cases for the cached s bit immediately before the target change.

### Cached s is FALSE: an exact count-one visit is outstanding

An uninitialized or reverse-initialization pending visit is necessarily
before second-order enablement; a resulting twoend pair is caught by the
queue. In the enabled phase, examine every ordinary invocation site:

| Source | Can a newly installed count one wait during another callback? |
| --- | --- |
| 608–620 | Yes, for the reverse initialization direction; only before enablement. |
| 221–224 | No: clear, decrement, threshold test, and invocation have no recursive call between them. |
| 264–285 | Yes: whole-symbol deletion installs all decreased counts, then visits them in index order. |
| 298–322 | Entry reads the current count and performs ordinary SSE1 before any second-order flag check. |
| 640–646 | This is a manual second-order visit, not an origin of ordinary count-one ownership. |

A pending zero-to-one visit is unavailable for A: its t exception has
never been FALSE. A newly installed count one can therefore be pending only
in the whole-symbol deletion batch. The removed symbol must be r. It cannot
be t, whose exception survives, or s, whose nonempty range survives.

Removing A_r forces B_s contained in A_s. SSE1 from T is blocked at t by v;
SSE1 from B requires this containment. SSE2 from B,T must have pivot t,
and then its off-pivot s premise again requires it. Section 4 proves that
B_s contained in A_s persists through any intervening callbacks. At the
proposed target transition A_s is still contained in T_s, so B_s is contained
in T_s as well. B cannot be the productive sole-s donor that creates A's
new s exception. If an overly conservative sole-s call nevertheless uses
the contained B_s as a bound, line 153 deletes T instead. Either alternative
contradicts the surviving witness.

This covers whole-symbol deletion without assuming that the pending visit
retains its old count. A and T's continuing s and t ranges are nonempty, so
the two-literal source is explicitly excluded from the unit early return.

### Cached s is TRUE: its nonempty source repair and tail are still ahead

Now the s bit is conservative debt before the target change. It was made
inaccurate by a strict nonempty shrink of A_s. Choose the latest such shrink
whose update has not repaired this bit. At a recursive boundary that update
has already written reverse increases and formed its forward target list
(178–205). Since T contains the still-nonempty A_s at this time and remains
active/initialized, T belongs to that list. Its row has not yet been passed:
if the current inclusion held when the row was examined, lines 216–222
would have cleared it; if a later source shrink established inclusion,
that later shrink owns a newer update instead. This is the needed sharper
debt-owner statement.

The target restriction now makes the existing TRUE at s raw-correct, so
that pending row may legitimately perform no TRUE-to-FALSE repair. This
is exactly why "eventual bit repair" alone would be an invalid argument.
Nevertheless the owning update's **tail is still pending**, after its row
list. Its surviving source reaches `on_update_range(A,s)` at line 231.
After the chosen last-role transition s is a permanent TRUE and s remains
present in A. Thus T is in `potential_targets` (349). If A's count is still
too large, the later last r repair supplies the ordinary visit of Section 5.
Otherwise A is a twoend and the notification tries both pivot orientations
(359–364, 416–445). If the other donor is still hidden by debt, its last
repair supplies the visit; if the notification's flag is disabled, its
manual queue entry remains pending. These alternatives all occur after raw
role stabilization and so have the valid-handler guarantees of Section 5.

An older notification snapshot is not being assumed to contain a target
that becomes eligible later: the specific debt owner here has **not yet
reached its source tail**. Nor is a changed already-contained union range
being treated as notified; Section 4 removed that as a possible cause of
this final useful independent witness.

These cases discharge obligation 2 and exclude a final independent SSE2
restriction for arbitrary within-clause orders.

## 7. HLA and unit reductions close obligation 3

The static SSE2 classification uses only exact final comparisons and
physical unit containment. An actual unit donor at t contradicts the
retained excluded value; at s it makes the other donor's intersection
premise a first-order containment; at another symbol it directly subsumes
the target. A unit target reduces to the oneend/oneend unit-HLA case. No
transient-cache premise is introduced by these reductions.

For nonunit operands, the direct and sole-t cases are covered by Section 3.
The oneend/oneend case is a two-donor domain-propagation refutation and is
covered by HLA. A twoend case is covered by Sections 4–6.

The reviewed HLA proof needs complete initial live rows/counts, physical
unit containment, complete registries, and no live zero count at HLA entry.
Those are quiescent facts. Section 2 additionally rules out the unit-equality
exception explicitly allowed by that theorem. During HLA actual ranges
never change; a later donor deletion cannot create a refutation absent with
the earlier larger donor collection. HLA therefore leaves no additional
local deletion or restriction opportunity. Nothing in this composition
requires exact raw TRUE comparisons during a pending source-update loop.

## 8. Independent executable controls

`checks.py` uses Python's standard library only. Its rule checker enumerates
raw finite-set direct/SSE1/SSE2 premises and independently performs domain
propagation with the target false. It does not use production counts or
registries to decide whether an output has a residual rule. Its scalar
Cartesian evaluator checks first and second outputs against the original.

Before any production control, it exhausts 512 ordered triples of nonempty
proper Boolean clauses on two symbols. It compares its domain propagation
with a separate virtual-complement closure for all 1,536 targets, and checks
every reported rule application against the scalar truth table: 768 direct,
1,344 SSE1, 708 SSE2, and 744 HLA applications. Every rule family is exercised.

Two deliberately outside-scope unchanged-source controls must be rejected:
the three-binary-clause unit-equality example and the saved five-clause
missing-SSE2 example. Each must have an independently detected residual rule
and a productive second call. These prevent the output checks from being
vacuously insensitive to the known scheduling failures.

`bridge.R` sources the unchanged kernel directly on plain clauses and a
plain domain map. Selected line checkpoints record state without modifying
it. Every observed execution is required to return an object `identical()`
to a separate unaudited call on the same input. At checkpoints, the Python
observer independently checks count sums, raw FALSE soundness, private
target-range constancy, and the new donor-intersection invariant. It records
conservative TRUE debt and pending enabled-phase count-one batches when
observed. These finite executions calibrate the observer and challenge the
proof; they do not prove universal pending-callback coverage.

The completed counts are:

| Check | R 3.6.3 | R 4.6.1 |
| --- | ---: | ---: |
| Full-shape arrangements, all saturated | 22,000 | 2,392 |
| Outside-scope known-gap controls, rejected | 2 | 2 |
| Plain unchanged-kernel calls | 44,004 | 4,788 |
| Additional observed calls equal to plain calls | 4,080 | 2,392 |
| Output valuation rows compared with input | 986,924 | 434,490 |
| Constant donor-intersection checks | 752,588 | 496,158 |
| Private target-range checks | 376,294 | 248,079 |
| Elapsed seconds | 267.01 | 215.06 |

These are execution counts, not disjoint quotient classes; the R 4.6.1
controls deliberately overlap the native-R run. Native R observed 150
enabled-phase conservative-TRUE occurrences and nine count-one pair entries
pending in enabled symbol-deletion batches. Zero residual rules, productive
second calls, semantic differences, or observation/plain-output differences
occurred within the stipulated full-shape controls.

`boundary_replays.py` separately replays a discovered enabled-debt input
and an enabled pending-batch input on both R versions. It sees six actual
callback-entry observations with raw-inaccurate TRUEs and two pending
count-one pairs per version, and checks the new intersection invariant 924
times per version. The outputs agree across R versions. It also requires
the saved four-clause production SSE1 gap to be detected on both versions.
These focused replays add six plain and two observed calls per version;
they are recorded separately in `boundary_replays.json`, not added to the
table above. They concretely refute an attempted stronger premise of raw
matrix exactness at every recursive callback, even for three full clauses.

The complete machine-readable counts and saved concrete transient states are in
`results_r36.json` and `results_r46.json`; the corresponding logs contain the
commands' completion output. The Boolean control fixes the first clause's
order and tests all 36 remaining independent orders for every ordered Boolean
input. Its use of global symbol renaming is confined to that Boolean test;
the proof above does not rely on this finite-order reduction. Directed
relay, complete-pivot-deletion, and unit-birth inputs run all 216 independent
orders. Seeded membership-profile controls also include varying finite
domains and nonnested ranges.

Reproduction from the repository root:

```sh
python3 attic/cnf_verify3/three_full_review/checks.py --random 1200 --trace 1200 --boolean
python3 attic/cnf_verify3/three_full_review/checks.py --current-r --random 24 --trace 24
python3 attic/cnf_verify3/three_full_review/boundary_replays.py
```

The current-R bridge first starts/checks the existing container through
`review_semantics/run_r46.sh`, then uses `podman exec -i` for its streaming
protocol; the launcher itself omits stdin forwarding. During checker
development, jsonlite's default matrix-NA encoding was initially read as
the string `"NA"`; explicitly writing `na="null"` fixed the observer's
uninitialized-pair decoding. This was a harness error, not a production
comparison failure.
