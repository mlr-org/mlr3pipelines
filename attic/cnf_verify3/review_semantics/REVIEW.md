# Independent review of normal-return semantic preservation

Reviewed 2026-09-06. The production source reviewed is
`R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.

This review independently follows the source rather than treating the earlier
finite checks as a proof. Its main references are
`independent_solver/SEMANTIC_PRESERVATION_MAP.md`,
`independent_solver/UNIT_CONTAINMENT_ARGUMENT.md`, and
`proof_state/LIFECYCLE_PROOF.md`; I also read the local-rule and quiescent-matrix
arguments to check the composition.

**Verdict:** I found no semantic counterexample or circular implication in the
normal-return theorem under the canonical representation and ordinary-R
assumptions. The unit-birth argument is valid, and it has a shorter formulation
below. I recommend adding an explicit lazy-row lemma for unit HLA and making
the phase boundaries of the ambient-unit invariant conspicuous. These are
presentation obligations that can be discharged from the current code, not
requests for a production repair. This is still a source-level proof, not a
mechanization of R execution semantics.

## 1. State interpretation and exact hypotheses

Let the fixed domain of symbol `s` be `Omega_s`. A valuation chooses one value
of every symbol. An absent literal denotes the empty set, and a clause `C`
asserts `OR_s [x_s in C_s]`. At a semantic boundary, interpret exactly

```
F = AND_{i: !eliminated[i]} entries[i].
```

The sufficient representation contract should say explicitly that the logical
input is `TRUE` or `FALSE`, and that ordinary clause/list indexing and
membership denote these mathematical sets. In particular, a scalar `NA` must
not be left to an ambiguous reading of "logical constant." The simplest
contract uses a well-defined symbol-to-domain map, ordinary unique character
values, unique nonmissing symbol names in each clause, nonempty domains, and
nonempty literal ranges contained in their domains. An empty *formula list*
is harmless: it denotes `TRUE`. Full-domain ranges are also harmless; their
exclusion is not needed for this semantic theorem.

I read "ordinary R list/copy semantics" as excluding overloaded indexing that
changes the meaning of the source operations, and "ordinary total character
membership" as requiring consistency between membership, symbol lookup, and
the proposed valuation semantics. These assumptions should be explicit rather
than inferred from the presence of a `Cnf*` class. The public-API malformed
selector examples therefore do not refute this kernel theorem.

The fixed-domain map is read but never mutated by this kernel. The usual
finite indexing/count bounds are necessary to interpret integer counts as
natural numbers. Successful normal return excludes exceptions and allocation
or stack failure, but includes the intentionally returned logical `FALSE`.

One small wording issue in the composition table: the kernel does not contain
a general initial-TRUE-clause removal pass. It returns scalar logical input,
returns `TRUE` for an empty conjunction, and can remove a clause through its
specific redundancy rules. Replacing that table entry by these exact cases
would avoid importing a constructor's behavior into the kernel proof.

## 2. Why the unit context is legitimate during recursion

Before HLA, every registered symbol has exactly one non-eliminated unit
representative. A fresh registration either creates that representative or
intersects a fresh candidate into the older representative and eliminates the
candidate. `unit_domains[[s]]` is the representative's current range.

The only two registration call sites are the unique initial `unit_queue`
indices and the transition from a non-unit to a singleton. In the latter case,
both registry removals precede registration, and registration marks the
candidate either unit or eliminated before propagating recursively. I checked
the stale-snapshot paths identified by the lifecycle audit. A later callback
cannot register that same index again: a same-symbol singleton has already
merged and been eliminated, and a singleton on another symbol has lost the
symbol the old propagation frame is trying to restrict. The current initial
preprocessing clause cannot recurse while remaining non-unit when the
comparison matrices are `NULL`.

Thus, until HLA begins, the ambient constraint

```
U = AND_s [x_s in unit_domains[[s]]]
```

is an actually retained part of `F`, not merely an inference the code hopes
will become true after propagation. It is safe to reason under `U` even while
some other stored range is still physically outside its current unit domain.
Unit propagation does not need to finish before the unit becomes an asserted
constraint: the singleton clause already exists in `entries`.

The merge statement at line 103 writes the intersection into the older unit
before line 104 removes the candidate. The intermediate conjunction is
`(old intersect new) AND new`, which equals `old AND new`; the subsequent
candidate removal also preserves it. There is no recursive callback between
the domain update and the representative update inside line 103.

No pairwise operation removes a retained unit. `eliminate_clause_update_sr()`
rejects units; ordinary SSE call paths exclude them. The first unit deletions
without replacement occur in the final unit-HLA loop. At that point the code
must stop treating `unit_domains` as the conjunction of live assumptions:
that registry is deliberately stale after a unit is removed. It does stop:
subsequent unit-HLA proofs use only raw non-unit donors and fresh local rows.
The phase qualification in the proposed theorem is therefore essential and
is respected by the implementation.

## 3. Contextual FALSE comparisons support every pairwise rewrite

For a live initialized non-unit pair, a consumed FALSE bit is the assertion

```
A_s intersect U_s subset B_s intersect U_s.                 (1)
```

The source preserves it by direct initial membership comparisons, source
shrinkage, explicit reverse refresh on non-unit target shrinkage, and shared
intersection with a new ambient unit on a unit target restriction. Deleting a
source symbol clears its column, representing an empty source range. Deleting
a target symbol repairs reverse FALSE bits before callbacks. A conversion to
a unit withdraws the old pair from live non-unit obligations before further
pairwise inference.

I checked that the non-unit reverse refresh covers every relevant initialized
source: a still-live source with a nonempty occurrence is in the symbol
registry. An absent occurrence is empty and cannot invalidate inclusion. A
future pair has no count yet and will compare the then-current ranges when it
is constructed. No callback intervenes in the reverse-refresh loop.

Count consistency is a separate obligation. Each initialized count is the sum
of its source matrix row; each guarded bit flip changes the count once, and a
symbol removal clears precisely the bits whose counts it decrements. The
diagonal counts stay `NA`, so the FALSE self rows cannot cause self deletion.

The TRUE bits may overstate non-inclusion during nested callbacks. This can
change scheduling but does not invalidate an inference: count zero supplies
all FALSE premises, count one supplies every off-pivot FALSE premise, and the
second-order handlers likewise identify an allowed set of at most two
exceptions. The second-order raw intersection test is stronger than the
corresponding contextual test. Its inverse TRUE tests merely reject attempts.

There is an easy-to-miss deletion case in `apply_domain_restriction()`: it
removes the *whole target* when the restricting range is contained in that
target's pivot range, even for a non-unit restriction. This is justified:

* For first-order SSE, the donor is then contained in the target at the pivot
  as well as at the other symbols, under the retained units.
* For second-order SSE, if the union of the donors' target-pivot ranges is
  contained in that target range, the same two-donor argument proves that the
  donors imply the target.

This remains true if the restricting range is empty. The containment premise
and the off-pivot donor premises then already imply the whole target; this
branch must not be described only as ordinary unit subsumption.

Clause deletion removes exactly its target and its registry occurrences. The
donor clauses are retained at that semantic operation. They may later be
rewritten or removed by another valid operation; sequential equivalence is
enough, and no permanently retained original witness is required.

## 4. A shorter derivation of the frozen birth certificate

Number unit registration events by their actual chronological entry order,
including candidates immediately merged into older representatives. For an
event `b` on symbol `s`, define:

* `R_b`: the candidate's own range saved in local `unit[[1L]]`;
* `E_b`: the intersection of constraints on `s` registered strictly before
  `b`, with `Omega_s` if there are none.

Just before a non-unit candidate `D` becomes a singleton on `s`, its last
symbol deletion removes a *different* symbol. Therefore every retained-symbol
comparison `C -> D` that is FALSE inherits (1), with `U_s = E_b`, unchanged:

```
C_s intersect E_b subset R_b intersect E_b,
```

and hence

```
C_s intersect E_b subset R_b.                             (2)
```

This freezes the required certificate directly from the contextual invariant
at the birth boundary. It avoids reconstructing which earlier comparison or
unit restriction first justified a particular FALSE bit. Initial and
preprocessing units have no usable matrices and need no such certificate.

After this birth, the ordinary update loops exclude `D` as a non-unit target.
Pair construction cannot revisit it as a live target either. Consequently an
incoming retained-symbol bit used by this registration cannot be replaced by
a circular certificate based on the new `R_b`. Source shrinkage preserves
(2). The remaining operation that can clear an entire source-symbol column
only does so after that source range has become empty, so (2) is trivial there.

The registering index is crucial. A merge changes an *older representative's*
stored range without updating its inactive matrix. The code reads the fresh
candidate's matrix, whose retained-symbol certificate was frozen for `R_b`.
The local `unit` value also remains `R_b` if a recursive child later tightens
the representative. The proof must not identify this saved value with the
mutable current representative range.

Only births on the same symbol matter to (2). One can induct separately on
each symbol's birth sequence; using a global chronological sequence is also
valid. Recursive call-stack ancestry is not the order being used. In a stack
`X1 -> X0 -> X1`, the last X1 certificate can depend on the first X1 constraint
without depending on itself, even though work for the first is still pending.

## 5. Discharging the certificate at the HLA boundary

Fix any final live non-unit clause `C` and a symbol `s` it retains. Actual
clauses never gain a symbol or regain a deleted value. No suspended callback
later writes its old `clause` snapshot back: the actual clause writes in range
restriction and symbol removal precede callbacks, and the resumed code uses
current `entries` for comparisons. Thus all later changes to `C_s` are
shrinkages.

For each birth `b` on `s`, one of these exhaustive cases applies:

1. The clause is already registered. Its index is in the propagation snapshot.
   Explicit propagation intersects its range with the current effective unit
   domain, which is a subset of `R_b`; or the matrix skip supplies (2).
2. The clause has not reached its initial preprocessing iteration. Its own
   preprocessing later applies the registered unit. While it remains non-unit,
   preprocessing cannot recursively introduce a new unit halfway through its
   saved symbol list: those callbacks are disabled while matrices are `NULL`.
3. The clause loses `s` or disappears. This cannot be our fixed final clause
   with a nonempty `s` range, and otherwise its containment is vacuous.

In the explicit case, the final range is contained in `R_b`. In the skip case,
induction gives `C_s subset E_b` at the final boundary, and (2) yields
`C_s subset R_b`. For the first birth on `s`, `E_b = Omega_s`, which is the
base case. Intersecting all these birth constraints gives physical containment
in the final representative range.

This proof does **not** claim that the range is already contained in `R_b`, or
in the effective domain, at the instant of a nested skip. The supplied deferred
example has a raw range disjoint from its current unit and is still handled
correctly. The ancestor's pending work supplies the older obligations used in
the final induction.

The proof also does not require the cached reverse TRUE bit to establish
strictness. That bit and the effective-range length guard control whether a
skip is attempted. Once a skip happens, the forward FALSE certificate and
birth induction establish containment, including equality. In fact the
effective-range guard is unnecessary for this containment proof itself; it
matters to the intended equality/subsumption behavior. A later child
registration provides its own stronger birth constraint and cannot be undone
by an older frame.

## 6. Unit HLA: the additional lazy-initialization lemma

The physical result of Section 5 justifies the unit-HLA starting row for every
surviving non-unit donor `D`: its range on the target unit symbol is contained
in the unit range; every other nonempty donor range faces an absent target
literal. Thus the count at line 738 is exact even when the same-symbol
containment is equality. The comments claiming *proper* containment are
stronger than this phase needs.

There is a further source detail that the current documents compress too much.
Lines 750 and 769 lazily initialize a row using the *initial* rule
`names(donor) != unitsymbol`, possibly after the virtual target has expanded.
Why is this not stale initialization?

**Lazy-row lemma.** A donor with an unallocated local row has no symbol that
has already been processed as a virtual HLA pivot. Each previous pivot loop
visited every live non-unit donor containing that symbol through the symbol
registry, allocating its row even when it did not clear the pivot bit. The
donors and their raw ranges do not change during unit HLA. Hence every
comparison of an unallocated donor is still its initial comparison. Constructing
its row by the initial rule is exact whenever the code does so.

Once allocated, each row is updated on every changed symbol it contains.
Virtual extension is monotone, so a FALSE stays FALSE, and each TRUE cleared
by the direct subset test decrements the count once. This proves exactness
before every selection, including later iterations with newly allocated rows.

The non-unit HLA argument has no analogous lazy-initialization issue: its rows
are already present. I independently checked the quiescent TRUE-debt argument:
only source shrink can make a TRUE bit inaccurate, and every such change is
owned by a finite active source-update loop until repaired or until that source
becomes inactive. Once pairwise calls unwind there is no owner left. Combining
this with (1), physical containment, and count consistency gives exact raw
rows at HLA entry. Writes for an earlier HLA target affect only that target's
row in each donor matrix, so later targets still start from their own exact
raw rows.

HLA retains each donor during the current target's virtual proof. A hidden
subsumption deletion therefore satisfies

```
G AND original_target = G AND virtual_target = G,
```

where `G` is the conjunction of all other current clauses. Later donor deletion
must prove its own equivalence with the already-deleted target absent, so it
cannot create a cyclic deletion justification.

It is not circular that donor ranges may previously have been strengthened
using the unit now being tested. Those are actual current clauses, and the
earlier transformation already established `U AND D = U AND D_restricted`.
If the restricted donors imply `U`, removing `U` now is a valid second
equivalence. It would be circular to use *only contextual containment* to
initialize an unpropagated raw donor row; the countermodel below shows the
difference.

## 7. Executable checks and countermodels

`check_lemmas.py` imports no production code or earlier harness. It exhausts
the pointwise freeze implication, all backward dependency graphs with four
births, the merge intermediate states, local HLA equivalence, and the lazy-row
update obligation. All 1,167 obligations pass. The pointwise checks are finite
proofs of the listed Boolean implications, which apply to each value of an
arbitrary domain. They do not prove that R executes the intended transition.

The script also verifies three deliberately invalid alternatives:

* With domains `x={0,1,2}`, `y={0,1}`, let the target unit be `x in {0,1}`
  and the two donors be `x in {1,2} OR y=0` and `x in {1,2} OR y=1`.
  Assuming the unit makes both same-symbol comparisons contextually contained.
  If those are used as raw unit-HLA FALSE bits, the first donor extends `y` and
  the second appears to subsume the virtual target. Removing the unit enlarges
  the models from `x=1` to `x in {1,2}`. With actual unit restriction first,
  the donors contain only `x=1`, and their removal proof is valid.
* A source range `{0,1}` is contained in an older representative's birth
  range `{0,1}` but not in its merged range `{1}` after a `{1,2}` candidate.
  Reinterpreting the older matrix as the merged candidate's matrix is invalid.
* Cyclic certificates `C intersect R1 subset R0` and
  `C intersect R0 subset R1` can both hold with `C={1}` and `R0=R1={0}`.
  They do not establish either physical containment. Strictly older birth
  dependencies exclude exactly this kind of ungrounded cycle.

`check_source_births.R` sources only the actual kernel and base R. It creates
an in-memory source copy with hooks, while running the unchanged source too to
check that instrumentation has not altered returned storage. An independent
truth-table evaluator checks every committed clause/range/elimination change
against the original model set, including the intermediate unit-merge write.
Additional hooks check incoming FALSE certificates at birth, skipped
propagations against each frozen `E_b` and `R_b`, physical containment at HLA
entry, and exact allocated/unallocated rows before every unit-HLA selection.

The completed run used R 3.6.3, seed 6137, 5,000 independently generated cases
plus two directed cases. It passed:

| Obligation | Count |
|---|---:|
| Output/storage comparisons and truth tables | 5,002 |
| Satisfiable input cases | 4,862 |
| Committed semantic mutation checks | 41,694 |
| Unit births, with unique-index and prior-domain checks | 6,492 |
| Incoming FALSE comparisons frozen at birth | 4,275 |
| Matrix skips checked against frozen birth certificates | 3,063 |
| Physical birth-containment checks at HLA entry | 7,934 |
| Exact unit-HLA donor row/count checks | 15,141 |
| Of these, still-unallocated row checks | 7,432 |

The directed deferred example supplies one physically outside skip and passes
its frozen certificate check. Random cases have 2–5 symbols, 2–4 values per
domain, and 3–16 clauses; half are generated with a planted satisfying
valuation. These are additional finite source checks, not a bound on formulas
or a substitute for the preceding lifecycle arguments. Results are in
`source_seed6137.log`, `source_results_seed6137.rds`, and `lemma_results.json`.

No production files were changed and no commits were created for this review.
