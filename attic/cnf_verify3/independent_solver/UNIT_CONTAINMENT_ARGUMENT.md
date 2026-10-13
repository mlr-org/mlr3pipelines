# Why stale unit-skip comparisons appear to miss equality only

This is a program-level proof argument to be challenged independently. It
explains the observed distinction between missed subsumption and preserved
physical unit containment. It is not a mechanically verified R semantics proof.

## The easy invariant: FALSE bits are contextually sound

At a rewrite boundary, let U be the intersection of all registered unit
constraints. A FALSE comparison bit for source A, target B, symbol s means

```
A_s intersect U_s subset B_s intersect U_s.
```

Fresh comparisons establish raw inclusion, which implies this. Shrinking a
source preserves inclusion. Shrinking a target by a non-unit restriction
updates every initialized reverse FALSE bit before invoking recursive
callbacks. Shrinking a target by a unit is a no-op under U and needs no reverse
update. Deleting a symbol from a remaining non-unit updates reverse bits before
callbacks; deleting a symbol to create a unit makes the clause inactive as a
non-unit before `register_unit()` propagates recursively.

This invariant justifies all count-based logical rewrite premises, but does
not alone justify unit-HLA initialization: using the unit under consideration
as an assumption while proving its own redundancy would be circular. At the
start of HLA, actual stored ranges must be physically contained in their units.

## Refine the invariant with the birth time of a unit clause

Consider the matrix column for a clause D which later becomes a unit on s.
Before D becomes a unit, every FALSE `C subset D` bit is justified by either:

1. an actual raw inclusion at comparison/update time; or
2. raw inclusion after intersecting with unit constraints which were already
   registered *before D's birth as a unit*.

The second case occurs when D was shrunk by a still-running unit propagation
P. If C was a subset of D's old range, then

```
C_s intersect P_s subset old(D_s) intersect P_s = new(D_s).
```

Shrinking C later preserves this certificate. Non-unit restrictions of D
refresh reverse comparisons before callbacks, so they cannot introduce an
unrecorded stronger range into this certificate.

After D becomes a unit, ordinary update loops exclude it as a non-unit target.
The relevant FALSE bit is consequently not replaced by a justification that
depends circularly on D's own newly registered constraint. The exceptional
operation that sets an entire removed-symbol column FALSE is trivially sound
because the source then has an empty range for that symbol; such a clause no
longer needs propagation through that symbol.

`register_unit()` reads the column of the *registering clause's index*, not
the column of an older unit retained after merging. The registering clause's
range is saved in its local `unit` value. This distinction matters: an older
unit's stored range may later shrink without updating its inactive matrix
column, but that old column is not reused as the registering new unit's column.

## Induction after all propagation has returned

Order introduced unit constraints by birth. For a live non-unit C and each
introduced unit constraint R on s, registration either explicitly intersects
C's range with R or skips it using the above certificate. In the skipped case,
the certificate depends only on units born earlier than R. After propagation
has completed, assume by induction that C is physically contained in all those
earlier units. Its certificate then gives actual containment in R as well.
The first introduced unit has no earlier units to defer to, giving the base
case. Clause ranges only shrink afterward, so these containments persist.

Merging a unit also requires containment in previously registered units. The
existing effective-range guard disables the matrix skip when the effective
intersection differs from the registering clause's own range. If a recursive
child tightens the effective domain after an outer call computed that guard,
the child has itself introduced and propagated that stronger constraint; the
induction handles that child constraint independently. An outer skip does not
undo any physical restriction.

During initial preprocessing, clauses not yet in the symbol registry are
handled when their own preprocessing iteration starts. A still-non-unit
preprocessing clause cannot trigger unrelated recursive changes while the
comparison matrix is NULL: recursive registration only happens if that very
clause becomes a unit, after which it never becomes a non-unit again. Thus a
new unit cannot silently evade a future live non-unit's initial restriction.
Once matrices exist, all live non-units are registered. Actual clauses never
acquire a new symbol, so a clause absent from a symbol registry cannot later
start needing propagation at that symbol.

If these lifecycle facts hold on every caller path, every live non-unit range
is physically contained in every applicable final unit before HLA. This is
the load-bearing property unit-HLA needs. The induction says nothing about
strictness or removal of a clause equal to a unit.

## Why equality still slips through

The skip also requires the reverse comparison `D not-subset C` to be TRUE.
TRUE is only a conservative cache entry: when D shrinks, an outer update may
not yet have refreshed that row. It can remain TRUE even when D and C have
become equal. The FALSE `C subset D` certificate is still sound, preserving
containment, but treating it as *strict* containment is incorrect. This is
exactly the minimized three-clause trace.

A diagnostic source-copy variant additionally requires
`length(current C_s) < length(current unit domain)` before skipping. It only
prevents skips, so it cannot weaken propagation. It repairs all 96 order
variants of the common-plus-three-private selector family checked in
`unit_skip_candidate.py`; the unrelated SSE scheduling gaps remain.

## Remaining proof obligations

* Independently inspect every caller's lifecycle guards to exclude a clause
  already registered as a unit from being registered again through a stale
  non-unit index. If this could occur, the birth-time certificate argument
  would need amendment.
* Formalize the comparison-certificate updates rather than relying on the
  informal contextual invariant.
* Retain the finite-input/canonical-set assumptions. The argument does not
  cover malformed hand-built lists, unsupported R attributes/coercions, or
  practical stack/memory limits.

The first two obligations have now received an independent source-level
caller audit and a pointwise certificate check in
`../proof_state/LIFECYCLE_PROOF.md` and `certificate_obligations.py`.
The 5,000-case lifecycle probe added here found no repeated registration or
inactive SSE operand. The five-clause satisfiable deferred-skip example in
`minimized_deferred_skip.json` confirms why raw containment is required only
after the relevant ancestor work completes, rather than at every nested entry.
These are supporting reviews and finite checks, not mechanized R execution
semantics. The exactness of quiescent matrices before HLA is a separate global
invariant which the proof-state agent is investigating.
