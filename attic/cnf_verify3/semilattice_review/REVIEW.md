# Independent review: initial propagation and finite semilattices

Reviewer: `semilattice_review`, 2026-09-06.

## Verdict

I accept all three claims in
[`../root/SEMILATTICE_COMPLETENESS.md`](../root/SEMILATTICE_COMPLETENESS.md)
under its canonical finite-domain contract and its explicit runtime and
representability qualifications. I found no canonical counterexample or
missing source case in the initial propagation prefix. This review does
not establish any later SSE, matrix, HLA, public-selector, or whole-program
totality theorem.

The source correspondence needs an invariant about *all GAC sub-boxes*, not
only preservation of complete models, to prove greatestness. The argument
below makes that distinction explicit. It also discharges the captured
preprocessing symbol list, nested registrations, stale registry iterations,
and duplicate original units.

The checked source is `R/CnfFormula_simplify.R`, MD5
`376aac6eb81334e751e9351f9eddc02f`. The boundary is after line 523 and before
the `available` assignment at line 530. No production file was edited.

## 1. Exact characterization of one proper clause

For each distinct coordinate `s`, let `D_s` have an associative,
commutative, idempotent meet `m_s`; order it by `a <= b` iff `m_s(a,b)=a`.
For a clause `C = OR_s [x_s in R_s]`, every `R_s` is a proper subset of its
domain. Here an ideal means a **downset**, without a directedness condition.

The clause relation is closed under the product meet exactly when every
range is meet closed and at most one is not a downset.

* If `a,b` lie in `R_s` but their meet does not, keep each other coordinate
  fixed outside its own range. The two assignments with `s=a` and `s=b`
  satisfy the clause, but their meet fails it. Idempotence keeps the other
  coordinates at their chosen outside values. This establishes necessity
  of each range's closure independently of the other ranges.
* If two ranges are not downsets, choose `a<=b` with `b` inside and `a`
  outside the first, and `c<=d` with `d` inside and `c` outside the second.
  The two assignments `(b,c)` and `(a,d)` satisfy the clause; their meet
  `(a,c)` fails it. Again fix all other coordinates outside their ranges.
* Conversely, if one of two satisfying assignments satisfies a downset
  literal, their meet still satisfies that literal: the corresponding
  meet value is below that assignment's value. Otherwise both assignments
  must satisfy the unique remaining literal, whose range is meet closed.

This is an exact equivalence for the original *individual constraints*.
A conjunction can have a meet-closed model set even when its individual
clauses fail the test. A full-domain literal also invalidates necessity:
it makes the clause a tautology and can hide an arbitrary other range.
Distinct coordinates matter; repeated stored symbol names do not fit this
Cartesian-product argument.

## 2. A relation-based definition of the greatest GAC box

Write `E = product_s E_s` for a box of nonempty current domains. For a
constraint relation `Q_C`, restrict its tuples to `E`, project the result
onto each coordinate in its scope, and replace those domains by their
projections. Keep coordinates outside the scope unchanged. If the
restricted relation is empty, return a single failure state `bottom`.

Call this operator `P_C`. It is contracting and monotone under
coordinatewise set inclusion, with `bottom` below every nonempty box.
Monotonicity follows directly from inclusion of restricted tuple sets and
their projections. Starting at the original domain box and applying these
operators fairly until none changes therefore gives the greatest common
fixed point: for every common fixed point `B`, induction gives

```
B <= E  implies  B = P_C(B) <= P_C(E).
```

Finite decreasing domains give termination for a fair sweep implementation.
At a nonfailure result, the last state is itself a common fixed point and
contains every other one. If a sweep reaches failure, there was no nonempty
common fixed point. This avoids treating different partially empty domain
vectors as distinct failure answers.

For a unary-range disjunction this generic relational operator has the
special shape used in the proposed proof. If two literal coordinates are
possible, any value at either coordinate has support via the other; values
at all further coordinates have support via either one. With exactly one
possible literal, that coordinate is restricted to its range and all
others retain support. With none, the relation has no tuple in the box.
An entire domain contained in one literal makes the relation universal on
the current box and on all later sub-boxes.

This specification describes **GAC**, not the projections of all full
models. It can have a nonempty fixed point for an unsatisfiable formula.

## 3. Source correspondence, including the nested cases

The following assertions concern only this prefix. Both early exits at
lines 170 and 255 see `is_not_subset_of == NULL`; no matrix callback runs,
and the shortcut at lines 115–133 cannot skip a unit application.

### Effective-domain and original-clause certificates

Let `E_s` be `unit_domains[[s]]` when present and `D_s` otherwise. Every
update to `E_s` is a restriction. A retained unit representative agrees
exactly with that effective domain.

For each original clause, its stored ranges before unit promotion are
intersections of its original ranges with domains seen earlier. Thus
intersecting a stored range with the *current* effective domain gives the
same set as intersecting its original range with that domain. A removed
literal was already impossible under an earlier, larger domain and stays
impossible afterward.

When a clause becomes a unit, all its other original literals are
impossible under the then-live domains. This is its unit birth certificate.
If a unit on that symbol already exists, lines 100–103 intersect the new
range with its current effective domain immediately. Consequently it is
safe that preprocessing stops as soon as the clause becomes a unit, even
if its surviving range has not yet been processed in the captured symbol
loop.

When a clause is deleted at lines 153–157, the restricting domain is
contained in one stored range, hence in its original range. That is a
persistent satisfaction certificate because the effective domain only
shrinks. A duplicate unit eliminated at line 104 has the analogous
certificate from its merged effective domain. Its eliminated entry may
have a stale range or `is_unit == FALSE`; neither invalidates the live-state
invariant.

### Original duplicate units

The original units are all handled before nonunits enter any registry
(lines 485–488). For each symbol the first unit is its representative;
later units update that same representative to the accumulated
intersection. No nested propagation happens in this phase because all
symbol registries are empty. If the intersection becomes empty, returning
failure is the corresponding GAC failure. Otherwise the all-unit early
return at line 491 already has the required fixed point.

### The captured preprocessing symbol list

Consider the unregistered clause currently processed at lines 499–523.
Before its registration, every application at line 506 has four outcomes:

1. it does not change the literal;
2. it intersects or removes a literal and leaves at least two literals;
3. it deletes the clause as satisfied; or
4. it promotes the clause to a unit and runs recursive propagation.

The first two outcomes cannot invoke another callback in this phase. The
third ends the clause's obligations with a satisfaction certificate. The
fourth ends its obligations as an unregistered nonunit: the new unit is
immediately merged with any existing effective domain, propagated, and
kept out of the nonunit registries. Contradiction is forwarded.

Therefore a clause cannot remain an unregistered nonunit while a callback
creates an uncaptured restricting symbol. If it survives as a nonunit,
the effective domains have not changed during its local restriction loop,
so its captured symbol list was sufficient. It is then registered under
its updated symbol list at lines 515–520. A future unit will find it there.

### Saved registry iteration and nested registration of the same symbol

Only the outer preprocessing loop adds nonunit registry memberships.
During a unit-propagation callback, registry memberships can only disappear.
Its saved iteration therefore contains every existing live nonunit that
requires the propagating symbol. It cannot miss a newly registered
nonunit during recursion.

If a saved entry was eliminated during recursion, line 128 skips it. If
it lost the propagating symbol, the symbol lookup at line 149 returns
without action. If it became a unit on the propagating symbol, the already
present representative forces it through the duplicate-unit branch, so
it is eliminated and skipped. A surviving new unit on another symbol has
lost the propagating symbol. This also explains why the deletion helper's
unit guard is not reached through a stale prefix registry entry.

A nested registration may shrink the same effective domain while an outer
registration is suspended. The new intersection is written before nested
propagation begins, and nested propagation visits the current registry.
The outer call reads `unit_domains[[nu]]` afresh at line 135 for each later
application. Its local `unit` can be stale, but the matrix shortcut that
would consult that local range is disabled throughout this prefix.

Every clause index is registered as a unit at most once in the prefix:
original units occur once in the initial queue, and each other clause can
be promoted only once before it permanently leaves the nonunit registries.
Thus even redundant, nonshrinking duplicate merges do not create an
unbounded callback process. This is a finite-call argument, subject to the
usual actual-R stack and resource qualifications.

### Quiescence and greatestness

At the completed boundary, every surviving nonunit is represented in
exactly its symbol registries. Each of its ranges is nonempty and a strict
subset of the corresponding effective domain, and it has at least two
literals. Every surviving unit equals its effective domain. Eliminated
clauses retain their satisfaction certificates. These facts establish GAC
for every *original* clause.

For greatestness, preserve an arbitrary common GAC sub-box `B`, not merely
the set of full models. If a source unit is created while `B <= E`, its
birth certificate says all other literals in its original clause are
impossible even under `E`. They remain impossible under `B`. GAC of `B`
therefore forces its remaining coordinate into that unit's range, so
`B` survives the source restriction. Original units satisfy the same
argument directly. A contradiction rules out every such nonempty `B`.
Intersections of stored literals and certified clause deletions do not
change the effective box. Induction preserves all common GAC sub-boxes,
and boundary quiescence makes the returned one the greatest.

## 4. Independent least-model proof

There is also a purely relational route to the constructive conclusion.
The exact clause characterization establishes that every original clause
relation is meet closed. A product of meet-closed domains is meet closed;
intersecting it with a meet-closed relation and projecting to coordinates
preserves meet closure. Thus every nonempty domain produced by relational
GAC is meet closed. Equivalently, the source's effective domains are
intersections of meet-closed original ranges and original domains.

Let `e_s` be the meet of the final `E_s`. Finiteness and closure imply that
`e_s` belongs to `E_s` and is its least member. At a GAC fixed point, each
`e_s` in the scope of a clause has a supporting tuple in that clause's
relation restricted to `E`. Meet those support tuples, one per coordinate.
In coordinate `s`, one tuple has value exactly `e_s` and all others have
values above it. Their meet is consequently `e_s`. Closure of the relation
places the resulting tuple `e` in that relation. This holds for every
clause, so `e` satisfies the original formula.

Every original model survives the GAC restrictions and hence lies above
`e` coordinatewise. Therefore `e` is the least original model, including
coordinates not occurring in any constraint. A successful prefix is
therefore a satisfiability certificate on this class; a failed prefix is
an unsatisfiability certificate without any class restriction.

The implementation does not receive a meet table or return an assignment.
The result is an external extraction theorem from the initial boundary's
effective domains. It is not a claim that effective domains can be read
unchanged from the final public result after later simplification phases.

## 5. Independent executable evidence

[`checks.py`](checks.py) uses no earlier campaign solver, domain-propagation
routine, observer, or saved corpus. Its GAC oracle materializes each
clause's satisfying tuples, filters them against current domains, and
projects those tuples. For the exhaustive small cases it separately
enumerates **every nonempty domain box**, tests the fixed-point property,
and identifies the greatest such box. Forward and reversed relational
sweep schedules must agree.

[`bridge.R`](bridge.R) sources the unchanged kernel directly, without CNF
constructors or package helpers. It uses the parsed function body to cut
execution before `available` is allocated. A private observed copy captures
the returned effective domains and registry state; on every request its
result must be identical to an unobserved private prefix. Selected cases
also run the unchanged full kernel. Complete truth tables check semantic
equivalence of each prefix and every requested full output.

The same corpus passed on R 3.6.3 and R 4.6.1:

| Check | Coverage |
| --- | ---: |
| Direct clause-relation closure versus the syntactic criterion | 34,210 |
| All labeled semilattices on 1, 2, 3, 4 values | 1, 2, 9, 76 |
| All isomorphism types on 1, 2, 3, 4 values | 1, 1, 2, 5 |
| Source-prefix cases per R version | 3,791 |
| Explicit candidate domain boxes checked | 62,329 |
| Common GAC fixed boxes found in that enumeration | 21,423 |
| All sets of canonical clauses on two Boolean symbols | 256 |
| All multisets of at most two clauses on two three-value symbols | 1,225 |
| General random formulas | 750 |
| Semilattice SAT cases with least-model checks | 1,090 |
| Semilattice UNSAT cases | 469 |

The closure checks enumerate all unary and binary proper clauses for
every labeled semilattice through four values, all ternary proper clauses
for every isomorphism type through four values, all binary and 300 sampled
ternary clauses for each of the nondistributive five-element lattices
`M3` and `N5`, and 1,300 sampled clauses for the eight-element powerset
semilattice. Formula cases also cover flat semilattices without a top,
unused singleton domains, duplicate units, and 180 formulas whose
coordinates use different semilattices. This is finite evidence for the
general proofs, not an exhaustive claim at arbitrary sizes.

The directed nested example has three-value domains and clauses

```
X in {2}
Y in {0}   or Z in {1,2}
Z in {0}   or Y in {2}
Y in {0,1} or W in {1,2}
X in {0}   or Y in {1,2}
```

The last clause captures only `X` before it is restricted. It then becomes
`Y in {1,2}`, creates `Z in {1,2}`, and recursively registers `Y in {2}`.
That narrows the still-active outer unit representative to `{2}` and
creates `W in {1,2}` before the outer `Y` registration returns. The final
domains are `X={2}`, `Y={2}`, `Z=W={1,2}`. All 24 orders of the four binary
clauses and both literal-order directions were also checked. The full
observations are in [`directed_r36.json`](directed_r36.json) and
[`directed_r46.json`](directed_r46.json).

A nonchain example uses `D={0,1,2,3}` with bitwise AND and downsets
`A={0,1}`, `B={0,2}`. Include all four clauses

```
(X in A or Y in A), (X in A or Y in B),
(X in B or Y in A), (X in B or Y in B)
```

and the unit `X in {3}`. Propagation produces `Y in {0}`, yielding least
model `(3,0)`. These original four binary constraints cannot all satisfy
the ordered-Horn criterion under any independent total orders on `X` and
`Y`: `A` and `B` are incomparable by inclusion, so at least one is not a
prefix in each order, and the corresponding two-nonprefix clause is
present. All `24 * 24 = 576` order pairs were checked. This concerns the
given constraint presentation; an equivalent rewritten formula can admit
an ordered-Horn presentation.

## 6. Limits and reproduction

The three-valued Horn example

```
(X in {0} or Y in {0}) and (X in {0} or Y in {1})
```

leaves both domains full under initial propagation, although every full
model has `X=0`. The least tuple `(0,0)` remains a model. This confirms that
greatest GAC is not complete pruning of unsupported full-model values.

The four binary Boolean clauses with all sign combinations remain
nonempty under initial GAC although jointly unsatisfiable; the unchanged
full simplifier returns FALSE. The review reproduces this boundary rather
than extending semilattice completeness to arbitrary CNF.

Meet closure must hold for each original clause relation, under one fixed
meet per symbol shared across its occurrences. Merely knowing that the
whole conjunction is meet closed is insufficient. For example, the three
Boolean clauses excluding `00`, `01`, and `10` have the singleton model
set `{11}`, which is meet closed, but initial propagation leaves both
domains full and their minima `00` do not satisfy the formula.

[`limits.py`](limits.py) reproduces that distinction on both R versions,
along with a full-domain literal hiding a nonclosed range and a diamond
semilattice unit `{1,2}` whose meet `0` is outside the unit. The latter
shows why closure of surviving domains is necessary for the extraction
argument. Its outputs are [`limits_r36.json`](limits_r36.json),
[`limits_r46.json`](limits_r46.json), and [`limits.log`](limits.log).

The usual canonical storage requirements remain essential, including
distinct symbol names within each clause and unique ordinary character
values. This review does not cover the known duplicate-symbol matrix
selector output or other malformed public representations.

Run from the repository root:

```sh
python3 attic/cnf_verify3/semilattice_review/checks.py \
  > attic/cnf_verify3/semilattice_review/checks_r36.log 2>&1
python3 attic/cnf_verify3/semilattice_review/checks.py --r46 --skip-algebra \
  > attic/cnf_verify3/semilattice_review/checks_r46.log 2>&1
python3 attic/cnf_verify3/semilattice_review/limits.py \
  > attic/cnf_verify3/semilattice_review/limits.log 2>&1
```

The second command reuses the algebra result from `results_r36.json` and
replays the identical source corpus through the existing
`cnf-review-r46` container with stdin kept open. It needs that container
running. Summaries and the actual meet tables are in
[`results_r36.json`](results_r36.json) and
[`results_r46.json`](results_r46.json). No full-package test run, production
change, or commit was made in this review stream.
