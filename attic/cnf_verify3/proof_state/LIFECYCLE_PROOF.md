# Independent lifecycle and unit-containment audit

Written 2026-09-06, after independent inspection of the production callbacks
and a review of `independent_solver/PROOFS.md` and
`independent_solver/UNIT_CONTAINMENT_ARGUMENT.md`.

This is a source-level proof argument, not a mechanization of R semantics.
The claims below assume ordinary finite, valid CNF inputs: each clause is a
nonempty named list, symbol names are unique, ranges are nonempty unique
character sets inside fixed finite domains, and the universe is not mutated
by external callbacks. They concern executions that reach the relevant
boundary normally. A signalled contradiction may leave intermediate caches
unfinished because the whole simplifier immediately returns `FALSE`.

## 1. Registered clause indices are not registered a second time

There are exactly two call sites for `register_unit()` in production.

1. Lines 485–488 visit the original `unit_queue`, whose indices are unique.
   The symbol registry is still empty throughout this loop. Consequently an
   initial registration cannot recursively reach another clause, and cannot
   cause a later original unit's index to be visited early. Duplicate-symbol
   units merge, but each *index* is registered only once.
2. Line 251 is reached after `eliminate_symbol_from_clause()` has changed a
   non-unit clause into a singleton. Both the removed symbol's registry entry
   and the remaining symbol's registry entry have been cleaned before the
   call (lines 240–250). Before registration calls any recursive operation,
   it either marks the candidate a unit (line 95) or marks it eliminated
   after merging into a different older representative (line 104).

The potentially dangerous entry into symbol elimination is therefore
`apply_domain_restriction()` at line 163. Its caller lifecycle is as follows.

| Caller | Why an already registered same-symbol unit cannot be changed |
|---|---|
| `register_unit`, line 135 | Its loop snapshot can contain a clause that has subsequently become a unit. If that unit is on the propagating symbol, it was merged into the already existing representative and marked eliminated; line 128 skips it. If it is on a different symbol, the propagating symbol has already been removed; line 149 returns before changing anything. The retained representative itself was removed from the symbol registry before the loop snapshot was obtained. |
| Initial non-unit preprocessing, line 506 | Future clauses are not registered yet, so earlier propagation cannot touch them. Within the current clause, a callback that creates a unit is possible only when that clause itself becomes a unit; symbol elimination then returns `NULL`, and line 507 breaks its symbol loop. A still-non-unit range change cannot recurse while the matrices are `NULL`. |
| First-order SSE, line 322 | Every actual invocation of `on_updated_subset_relations()` has live non-unit operands, as established below. Its target remains active until `apply_domain_restriction()` begins. |
| Second-order SSE, line 467 | Both handler loops maintain a live non-unit target and active donors through every call to `try_sse_2nd_order()`, as established below. |

Thus a clause first registered as a unit can never later re-enter line 251.
An older unit's *stored range* can change through a merge at line 103, but
the old index is not passed to registration again. The new registration uses
the new singleton's index, including when that singleton is immediately
eliminated in favour of the old representative.

This closes the principal lifecycle obligation in the birth-order argument.
It also explains why `available_inverse[[unit_idx]]` is a valid index whenever
the matrix optimization is attempted: initial/preprocessing units are never
registered again after `available` is constructed; every later singleton was
an available non-unit when that fixed index map was created.

## 2. The callback guards preserve their callers' live operands

### `on_updated_subset_relations()`

Its callers are limited to these locations:

- Range updates, line 224: the target is checked at line 210 on every loop
  iteration. If the callback indirectly makes the source inactive, line 226
  immediately returns from the outer range update before another iteration.
- Symbol deletion, line 285: the source has at least two symbols in this
  branch; the target is checked at line 283. The source is checked immediately
  after the callback at line 288.
- Pair construction, lines 612 and 620: the initial outer and inner guards are
  lines 553 and 575. After the first callback, `NULL` breaks the outer clause's
  inner loop and line 615 skips an inactive inner source. After the second,
  line 622 breaks if the outer source became inactive. No intervening operation
  changes clauses without one of these callback returns.
- The manual second-order queue, line 646: both operands are checked at line
  644 immediately before the call. The next queue iteration checks afresh.

No self-pair is submitted. Pair construction uses distinct inner/outer indices;
the dynamic update lists require a non-`NA` count; diagonal counts remain `NA`
throughout the pairwise phases. The matrix diagonal's `FALSE` row is therefore
never interpreted as an instruction to delete a clause using itself.

`on_updated_subset_relations()` does *not* promise to return `NULL` whenever
its **source** becomes inactive. Its documented `NULL` status concerns its
**target**. For example, the second-order handler can return `NULL` for a dead
source, which lines 304–307 intentionally do not forward. Every caller above
either checks its source explicitly or discards both operands before its next
fresh guarded queue iteration. Treating this status as a generic "some clause
became inactive" status would be an incorrect audit assumption.

### Second-order handlers and `try_sse_2nd_order()`

`handle_sse_2nd_order_oneend()` checks its target at line 381 and each candidate
twoend donor at lines 390–393. After a trial rewrite, it checks the main source
at line 401 before handling a `NULL` target result at line 402. Consequently
an inactive source is never masked by a simultaneously eliminated target.
The next iteration rechecks the source's required count and symbol at line
386. The oneend handler's callers either have live operands from the preceding
proof or are `on_update_range()`, whose previous handler return is checked
before advancing (lines 355–357).

`handle_sse_2nd_order_twoend()` checks its target and count at line 412. It
checks each oneend donor at line 431, the current source's two exceptional
columns at line 427, and both source/target after each rewrite at lines
443–444. These checks also protect the second orientation of its outer loop.
Its `on_update_range()` caller stops on the source-inactive `NULL` return
(lines 362–364).

Thus the commented-out active-operand guard at line 457 is redundant under
these call paths. The decision-boundary instrumentation independently tested
this claim for 9,855 actual `try_sse_2nd_order()` entries in the 20,000-case run,
plus directed replays, without an inactive operand.

One narrower comment is stronger than necessary: a candidate from an old
symbol-registry snapshot may lose the intersection symbol while staying a
non-unit. The twoend handler can still pass it if its remaining exceptional
columns meet the rule. This is safe: an absent symbol denotes the empty set,
so the disjointness test is automatically satisfied and the union restriction
is still a valid instance of the second-order schema. The code tolerates this
case; the mathematical rule does not require nonempty intersection ranges.

## 3. Contextually sound FALSE comparisons

Let `U_s` be the intersection of current registered unit constraints on symbol
`s`, or the original domain if there is no unit. At an actual pairwise rewrite
boundary, every consumed `FALSE` matrix comparison certifies

```
A_s intersect U_s subset B_s intersect U_s.
```

The source-level preservation argument is:

1. Pair construction establishes raw inclusion by direct membership tests
   (lines 587–605). Raw inclusion implies contextual inclusion.
2. Shrinking a source preserves its previous FALSE certificates. New FALSE
   bits are installed only after direct current-range inclusion tests
   (lines 215–221).
3. A non-unit restriction that shrinks a target refreshes every initialized
   reverse FALSE comparison for registered sources before invoking callbacks
   (lines 178–192). A source absent from that symbol registry is inactive or
   has an empty range at the symbol, so it needs no active comparison update.
4. A unit restriction that shrinks a target does not need reverse updates:
   the same new unit restricts the interpretation of *both* sides. Taking
   intersection with a common set preserves a prior inclusion. This remains
   true while some raw ranges have not yet been physically restricted.
5. Removing a symbol from a remaining non-unit sets its whole source column
   FALSE and repairs reverse comparisons before callbacks (lines 263–285).
   The first operation is justified by the source range becoming empty.
6. Removing a clause, or converting it into a unit, removes it from the active
   non-unit comparison obligations before recursive pairwise work starts.

The qualification "consumed" matters. When a clause becomes a unit, its old
matrix is deliberately not repaired for all its deleted symbols. Those old
comparisons may be false even contextually. Registration reads only the
retained unit-symbol column, and the ordinary pairwise callbacks exclude the
inactive unit. An initial version of `exp02_decision_invariants.R` wrongly
checked every old column at a unit skip; trial 1747 exposed this *audit false
positive*, not a production bug. The corrected checker tests the actual
retained symbol only.

## 4. Birth-order certificates establish physical containment before HLA

The contextual invariant alone is insufficient for unit-HLA: using a unit as
an assumption while proving that same unit redundant would be circular.
The stronger certificate concerns the registering candidate's fixed birth
range `R_b`, rather than an old representative's mutable range.

For a unit candidate D born at event b, a retained FALSE comparison
`C -> D` at its unit symbol s can be justified as

```
C_s intersect E_b subset R_b,
```

where `E_b` is the intersection of constraints on s born *strictly before* b.
Before birth, raw comparisons establish this immediately. A unit restriction
of D can defer physical restriction of C, but its unit already exists before
D's birth, so it is a factor of `E_b`. A non-unit restriction of D repairs the
reverse comparisons before callbacks and cannot introduce an unrecorded
restriction. Later shrinking C preserves the certificate.

After D's birth, ordinary range-update loops exclude D as a non-unit target.
Their incoming FALSE comparisons cannot acquire a circular justification from
D's new unit. Clearing a removed source-symbol column is harmless because C
then has an empty range there. Merging changes an older representative's stored
range but does not reuse its index for a new registration, by Section 1. An
already-running registration retains its local `unit` snapshot and therefore
continues to refer to its original `R_b`, even if a child merge shrinks the
representative's actual range.

Now consider a live non-unit clause after all recursive propagation returns.
For each introduced constraint `R_b`, one of three things happened:

- It was explicitly restricted by that registration to the current effective
  domain, which is a subset of `R_b`.
- It was skipped using the birth certificate above. By induction on b, its
  final physical range is already inside `E_b`, so the certificate implies
  physical containment in `R_b`.
- It was not yet registered during initial preprocessing. When its own
  preprocessing iteration starts, that unit is included in the initial unit
  symbol list and explicitly applied. A new unit cannot appear midway through
  processing a still-non-unit clause, by the preprocessing lifecycle argument
  in Section 1.

Ranges never grow, and no later phase adds actual symbols. A clause that was
removed or lost s needs no further restriction on s. The induction therefore
establishes physical containment in every final registered unit range before
HLA starts.

The optimization's effective-range guard is consistent with this argument.
If it disables skipping, the actual propagated intersection is inside `R_b`.
If it enables skipping and a recursive child later tightens the effective
domain, the parent's certificate still proves containment in its own `R_b`;
the child's separate birth event supplies the additional tighter constraint.

This proves **containment**, including equality. It does not prove **proper**
containment. The confirmed equality-skip defect is possible precisely because
the reverse `TRUE` bit is only a conservative, possibly stale cache entry.
There is no analogous birth certificate asserting strictness.

## 5. Concrete deferred-restriction witness

Replaying `independent_solver/outside_skip_satisfiable_dense.json` under the
independent entry/exit hooks produces this temporary skip:

```
active registration stack: X1 -> X0 -> X1
skipped clause 8: X1={3} | X0={5,2}
current domains: X1={6}, X0={5}
```

The inner X1 frame skips a raw range `{3}` disjoint from the current unit
`{6}`. Its comparison is nevertheless contextually sound: the projected
range is empty. The older X1 frame subsequently repairs the deferred work.
All 188 helper entry/exit equivalence checks and all three outermost-unit
postconditions pass. The result has exactly the units `X0=5` and `X1=6` and
is satisfiable. This excludes any proof based on claiming that *every*
individual unit skip is already a raw subset, even for satisfiable inputs.

Evidence is saved in
`results/decision_success_seed801_replay.rds`. The example was discovered and
independently SAT/MDD-verified by `independent_solver`; the recursive hook
checks and stack witness here are independent.

## 6. Algebraic review verdict

I independently checked the finite-set arguments in `PROOFS.md` for ordinary
subsumption, unit intersection, first-order SSE, second-order SSE, and HLA.
They are correct under their stated premises and preserve logical equivalence,
not merely satisfiability. The second-order deletion case uses
`A_t union B_t subset T_t`, which matches the simplifier's intersection-length
equality branch. Absent-symbol ranges are empty and do not invalidate it.

The HLA donor-single-use and per-target matrix-isolation arguments are also
correct: virtual extension is monotone, a donor cannot acquire a different
exceptional symbol, and matrix writes are confined to the current target's
row within each donor matrix. The current target's own row is FALSE, so it
cannot be mistaken for a donor counter requiring an absent self index.

The HTE-unreachability proof is sound provided donor selection counts and
bits actually describe the current virtual clause. This remains a conditional
implementation claim; the proof of the algebra alone does not establish that
premise. The duplicate-domain submultiset observation is correct and closes
the earlier notes' speculative false-positive length-equality concern for
that specific HTE test.

The membership-class quotient and nonuniform refinement arguments are valid
for unique canonical values: every length comparison used for control flow
is made between sets known to be nested, so it tests set equality, which a
surjective refinement preserves. This does not establish a universal finite
test bound on clause/symbol counts.

No statement here asserts completeness, idempotence, or a canonical normal
form. The four independently confirmed scheduling defects falsify stronger
claims of that kind. No full mechanized proof of all R executions is claimed.
