# Independent review of the arbitrarily wide four-clause boundary

Reviewed 2026-09-06 against unchanged `R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.

The family theorem in `../wide_four_boundary/PROOF.md` is supportable. I found
no substantive gap in its one-pad trajectory or its simulation from one pad
to an arbitrary positive finite number. The distinction from zero padding
is necessary and is respected: zero padding takes unit branches in both
passes, whereas every positive-padding execution in this coupling uses the
nonunit paths. The proof establishes an ordered existence family, not a
claim about every ordering of the four clauses.

The conclusion remains conditional on ordinary finite-set representation,
fresh distinct padding names, supported indexing/count arithmetic, and normal
return with sufficient resources. No production changes or commits were made.

## 1. Independently reconstructed one-pad initialization

Use the proof's core names and order:

```
A: Y={0,5}, X={0,5}
B: X={1},   Y={0,1,4}
C: X={0},   Y={4,0,5}
D: Y={0},   X={4,5}
```

Every clause also has `P1={on}`. Both core domains have the fixed ordered
values `{0,1,4,5}`. All initial widths are three, so line 48 preserves the
listed clause order. No preprocessing unit exists.

The pair visits before the first actual mutation are completely determined:

* `A/B` has two exceptions in both directions; no first-order action runs.
* `A -> C` has sole exception `X`, but `C_X={0}` is already contained in
  `A_X={0,5}`, so its intersection is unchanged. `C -> A` has sole exception
  `Y`, and its bound `{4,0,5}` likewise leaves `A_Y={0,5}` unchanged.
* `B/C` has two exceptions in each direction.
* At `A/D`, the first direction `A -> D` has two exceptions. Reverse
  `D -> A` restricts `A_X` to `{5}`. The resulting source scan leaves
  `A -> C`'s exceptional `X` bit TRUE. It clears `A -> D`'s `X` bit and
  visits its remaining `Y` exception, but the bound `{0,5}` does not change
  `D_Y={0}`. There is no second-order range notification while disabled.

At `B/D`, reverse `D -> B` deletes `B_X={1}`. The target retains `Y` and
the pad, so lines 245–252 do not register a unit. The deleted source `X`
column clears `B`'s comparison bits against `A,C,D`, installing sole-`Y`
counts before batching those visits in clause-index order.

The `B -> A` visit narrows `A_Y` from `{0,5}` to `{0}`. Its forward repair
against `B` leaves a sole-`X` visit whose target `B_X` is absent, hence the
helper returns at line 149. Its next relevant repair against `D` gives
count zero and deletes `D`. `A -> C`'s already-FALSE `Y` bit is not repaired
again, so its missed `X` restriction receives no new ordinary visit.

The pending `B -> C` visit narrows `C_Y` from `{4,0,5}` to `{4,0}`. Its new
`C -> B` sole-`X` visit also has an absent target literal and changes nothing.
The last batch target `D` is skipped because it is eliminated. The outer
initializer subsequently observes that `D` is eliminated and does not
initialize the `C/D` pair.

This reconstructs every actual first-phase change in the proposal's table.
In particular, all live clauses keep a core literal: only `B_X` disappears,
leaving `B_Y`; `A` and `C` retain both core symbols, and `D` is retired with
its core still present.

## 2. The remaining SSE2 and HLA work is also finite and determined

After these mutations the active ordered exception sets are:

| Source | Target | Exception set |
| --- | --- | --- |
| A | B | X |
| A | C | X |
| B | A | Y |
| B | C | Y |
| C | A | X,Y |
| C | B | X |

Only `C -> A` enters the initial manual twoend queue. With intersection
pivot `X` and restricted symbol `Y`, the only eligible non-diagonal partner
is `C` itself; the inverse guard fails because `A_Y={0}` is contained in
`C_Y={4,0}`. With intersection pivot `Y` and restricted symbol `X`, partners
`B` and `C` are examined. In both cases their intersection contains `Y=4`,
which is absent from `A_Y`. The raw intersection check rejects both calls.
No enabled actual range change occurs, so no additional range notifications
can be created in this phase.

Physical HLA order is `A,C,B`, with widths `3,3,2`. The virtual calculations
then agree with the proposal:

* On target `A`, donor `B` adds `Y=5` virtually. `B` still has values `1,4`
  absent there, and `C` still has its `X` and `Y` exceptions. No deletion.
* On target `C`, donor `A` expands virtual `X` to `{0,1,4}`. `A_X=5` stays
  outside. Donor `B` then expands virtual `Y` to `{4,0,5}`, still missing
  `B_Y=1`. No deletion.
* On target `B`, donor `A` supplies the candidate virtual `X={0,1,4}`.
  This contains `C_X=0`; `C_Y={4,0}` is already contained in `B_Y={0,1,4}`.
  The current `C -> B` hidden count becomes zero and HLA deletes `B`.

None of the constructed virtual ranges is full-domain. The last virtual
range is used for the hidden-subsumption test before the target is deleted;
it need not be assigned into even the local virtual clause afterward.
Actual clause ranges are never replaced by these virtual ranges.

The returned actual clauses are therefore exactly

```
(Y={0}, X={5}, P1={on})
(X={0}, Y={4,0}, P1={on}).
```

On the second call, the initial `A -> C` ordinary visit now removes `C_X`.
Its retained `Y` plus pad again preclude the unit branch. The resulting
reverse sole-`Y` restriction leaves `A_Y={0}` unchanged, and there is no
count-two pair for the manual queue. HLA constructs virtual `A_Y={0,1,5}`
and virtual `C_X={0,1,4}` without deleting either target. This gives the
second displayed output, with only `Y={4,0}` and the pad in its second clause.

## 3. The source coupling is valid for every positive padding count

Compare the one-pad execution with the execution containing `k >= 1`
common pads. The related states have identical ordered actual cores, core
domains, active flags, clause/index maps, core registries, completed-pair
counts, enable flags, and pending core operations. For each clause, the
single pad occurrence is replaced by the same ordered block of `k` fresh
proper occurrences. Every padding registry is a copy of the one-pad registry.
The same relation applies to local virtual HLA clauses after removing pads.

This is an induction over core operations with blocks of padding-only work
between corresponding boundaries. It does not assert that instruction counts,
raw loop indices, or every intermediate padding-column write are identical.

**Widths and nonunit branches.** A related physical width changes from
`core_width+1` to `core_width+k`, a uniform shift of `k-1`. Both physical
width sorts therefore preserve their comparisons and ties. Initialization
order stays `A,B,C,D`; HLA order stays `A,C,B` in the first pass and `A,C`
in the second. Source comparisons of literal-range cardinalities do not
change at core symbols.

The retained-core condition is not assumed from the desired arbitrary-width
conclusion. It is established at each successor of a core deletion in the
finite one-pad trajectory above. If the two executions have followed the
same core operations up to a putative first divergence, their next core
deletion removes the same symbol from the same core. Its one-pad successor
retains `Y`, and therefore so does the many-pad successor. Each has width
at least two. The emptiness and unit branches consequently agree. The
source's unit flags remain FALSE, and its unit queue and domains remain empty.

This family-specific fact is essential. For an arbitrary different core
example, reaching a pad-only clause would produce a unit at one pad but a
nonunit at several pads; a general unconditional padding theorem would fail.

**Comparison initialization.** New source matrices initially contain TRUE
for future rows, including every pad column. Their diagonal row is set FALSE.
For an actual pair, both clauses still have the identical `{on}` pad ranges,
so each pad comparison clears both directional bits. Lines 608–610 compute
both row sums only after the entire common-symbol loop has finished; no
callback intervenes in that loop. Completed pairs thus gain only FALSE
columns and retain the exact same count and core exception mask.

Uninitialized pairs are deliberately not asserted to have FALSE pad bits.
Their counts remain NA. The source-update scans at 182/205, the deletion
batch at 264, the manual `which()` queue, and the oneend/twoend candidate
filters all exclude those NA pairs before inference. An SSE2 candidate
obtained from a completed donor-to-target count also has the reverse pair
completed, since the two directions are initialized together. Therefore
uninitialized TRUE pad columns never become a pivot or a counted exception.

**Actual updates and candidate order.** At every ordinary or SSE2 callback,
padding bits for its completed pairs are FALSE, so the selected exception
symbols are core symbols. The source reads the same core ranges and performs
the same set tests and intersections. Nonempty source changes repair the
same core column against the same core-registry index vectors. A source core
deletion clears the same column and batches the same clause-index rows;
padding columns are not touched. The nonunit successor argument above then
gives the same recursive path and return values.

The twoend handler's pivot order comes from the TRUE core columns, preserving
their order when padding is appended. Its partner lists come from core
registries and the unchanged counts. The manual queue is built from the
same count matrix and has the same order. This includes the actual stale
and unchanged comparison bits responsible for the missed SSE1 notification;
it imports no hypothetical saturation scan.

Clause elimination removes the same index from core registries and then
from each pad registry. The extra pad removals have no callback and no
effect on core registry order. The kernel uses direct core-symbol lookup
for candidate registries; adding pad bindings does not introduce an
environment-enumeration ordering dependence here.

**HLA virtual targets.** Each target starts with the complete unchanged
padding block. Completed donor rows have no pad exception. A unique HLA
exception is therefore a core symbol, whose domain and donor values are
identical in the two executions. The same virtual range is constructed;
the same core-registry loop updates the same target-local counts and used
flags. Padding neither expands nor disappears, and cannot become a later
exception through virtual expansion of a core range.

When a previously absent core symbol is added virtually, it can occur
*after* the padding block in the local list. This does not break the
coupling: removing pads preserves the ordered core, and HLA accesses the
virtual range by its core name. It does not use that changed physical
position as an exception index or use virtual width for the target sort.
The same target is eventually kept or eliminated, and virtual ranges are
discarded rather than written into actual entries.

These cases cover every branch by which the new padding could affect core
execution. They prove the simulation for arbitrary finite supported `k`,
not merely the tested padding counts. Applying it separately to the two
one-pad calls gives the exact ordered outputs in the proposed theorem.

## 4. Truth, remaining rule, and the zero-pad limit

The first output has exactly the productive ordered SSE1 pair `A -> C` at
`X`: its `Y={0}` is contained in target `{4,0}`, all pads agree, and the
target `{0}` intersects donor `{5}` to the empty set. This removes a literal
on the second call while leaving the ordered `{4,0}` range unchanged.

The core formula has the four models with `Y=0` and the additional model
`X=5,Y=4`. Both stripped outputs have precisely those five models. Since
each clause retains the whole same padding disjunction `Q_k`, distributivity
gives `AND_i(C_i OR Q_k) = F OR Q_k`. There are therefore exactly
`16 * 2^k - 11` models in the padded family. This algebra justifies all
padding assignments; testing large width is unnecessary for that conclusion.

The zero-pad case has the same returned stripped cores but a different
execution. Removing `B_X` on the first pass and `C_X` on the second creates
a `Y` unit and invokes registration/propagation. Those calls cannot be
identified with the positive-padding nonunit callbacks. Neither the source
induction above nor the proposed proof uses such an identification.

## 5. Small fresh executable controls

`checks.R` independently constructs the public family with padding counts
`1,4,11`, repeats `11` with the shared pad order reversed, and uses `0` only
as a unit-path control. This is a small directed suite, not another broad
padding bank. It checks exact ordered public outputs, the surviving direct
SSE1 premise, and complete scalar truth tables for those small counts.

A separate private source copy observes actual writes, clause deletion,
matrix/pair initialization, HLA entry, virtual range construction, and unit
registration. The original production method remains unchanged, and every
observed output must be identical to its plain public-constructor output.
At observation boundaries, assertions check retained core literals, untouched
ordered pads, uniform pad registry vectors, identical pad columns, FALSE
pad bits for initialized pairs, and the absence of positive-padding units.
The observations also require the presence of default TRUE pad columns
somewhere, so the check does not silently assume they are always FALSE.

All four positive configurations produce identical projected traces on each
runtime. Zero padding records two unit registrations across its two passes;
positive padding records none. Full first/second output and truth checks
pass on both R 3.6.3 and R 4.6.1, with 65,840 scalar assignment rows per
runtime. Results and lossless projected traces are
in `results_r36.{json,rds}` and `results_r46.{json,rds}`.

Reproduce from the repository root:

```sh
Rscript attic/cnf_verify3/wide_four_review/checks.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/wide_four_review/checks.R
```

Only this review directory was written. Earlier completed review directories
were left stable. The controls corroborate the finite base trajectory and
source premises; the arbitrary-width conclusion rests on the coupling.
