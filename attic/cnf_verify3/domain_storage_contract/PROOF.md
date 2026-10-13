# Ordinary domain storage normalizes exactly at the actual-range boundary

Source audit dated 2026-09-06, against unchanged
`R/CnfFormula_simplify.R`, SHA-256
`7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc`.
This is a source-level proof argument with explicit R representation
assumptions, not a mechanization of the R interpreter.

## 1. The theorem and the representation contract

For each symbol `s`, let `S_s` be its stored domain object. It is a finite,
nonempty character object with no missing **values**. Its scalar value
sequence may contain repetitions, names (including repeated, empty, or NA
names), matrix/array dimensions and dimnames, and ordinary inert attributes.
Let

```
U_s = unique(flatten_and_remove_attributes(S_s)).
```

Here flattening means the sequence obtained by ordinary one-index R vector
access, in R's stored element order; `unique` means value deduplication,
not the `unique.matrix` or `unique.array` row/margin operation. Any other
ordering of these distinct domain values gives the same theorem below.

Each input clause is an ordinary nonempty named list. Its symbol names are
distinct, nonmissing, registered names, and each actual range is a flat,
unnamed, unique, nonempty proper character subset of `U_s`. Keep the input
clause order, symbol order, and **literal vector order exactly fixed** in
the compared executions. Plain TRUE/FALSE and the empty conjunction are
covered by their corresponding constant paths.

The scope explicitly excludes:

* Active bindings, changing universes, concurrent mutation, custom
  dispatch affecting the relevant operations, altered primitive bindings,
  and callbacks capable of observing or changing the running computation.
* Attributes that change indexing, concatenation, length, or equality
  semantics. Ordinary names/dim/dimnames and metadata are included. An
  otherwise unused class marker with no applicable methods is included;
  assigning a semantically active class to a character vector is not an
  inert-attribute example. The proof needs the base-operation behavior
  stated below, not a claim about every arbitrary class accepted by an
  assertion.
* Encoding/byte mixtures for which `%in%`, `match`, `unique`, and the
  logical value equality do not agree with one fixed ordinary total
  equality relation. ASCII values, including `""`, are sufficient.
* Resource failures, stack exhaustion, and unsupported vector/index sizes
  or arithmetic. Both executions have the supported capacity to complete
  the corresponding operations. This is not equality of allocations,
  stack cost, primitive work, runtime, or maximum feasible physical size.
* Malformed clauses from selectors, particularly repeated/NA symbol names
  and NULL ranges. The known duplicate-name selector defect is unaffected.

**Theorem.** Replacing the stored domains `S_s` by `U_s`, while leaving these
actual input clauses fixed, preserves every source-level decision and
clause/symbol/helper/recursive schedule. All actual range writes are exactly
equal as R vectors, including their order. The ordered returned clause
payloads are exactly equal, with the same TRUE/FALSE result when constant.

“Source-level schedule” includes the explicit `if` outcomes, evaluated
`&&`/`||` operands and short-circuit choices, set inclusion/disjointness
decisions, loop sequences and iterations, helper entries, and recursive
continuations. Raw domain/virtual-range lengths, temporary vector names,
primitive-level comparisons and allocations need not be equal.

The outer universe attribute deliberately remains the input universe of
each execution. Two separately created universe environments are not
`identical()`, and their stored objects have different attributes and
multiplicities. The precise exact-output comparison removes just the outer
`universe` and `class` attributes; it does not sort, deduplicate, flatten, or
otherwise weaken comparison of any returned literal range.

## 2. Why public construction can establish this boundary

`CnfSymbol` checks `assert_character(domain, any.missing = FALSE,
min.len = 1)` and assigns the domain object unchanged into its universe.
It does not require uniqueness or lack of dimensions/names/metadata.

For the same supplied literal values, `CnfAtom` checks membership against
the domain and classifies full coverage with `all(domain %in% values)`.
Both decisions depend only on `U_s`. For a proper atom, `CnfClause` collects
its values through `unique(c(existing, incoming))`. Under the ordinary
base-operation contract, this flattens and deduplicates values and removes
their names/ordinary attributes. Hence an ordinary constructor-built proper
clause has the same literal vector payload under `S_s` and under `U_s`.
The tests additionally compare that payload by `identical()` before calling
the kernel, so the experiment does not assume this conclusion silently.

This is a boundary theorem, not a universal theorem about every accepted
list containing classed CNF objects. The already documented constructor
constant/universe cases and malformed selector outputs remain separate.

## 3. Domain storage is not read before HLA

The source's only original-domain reads affecting computation are the two
HLA extensions and their full-domain length comparisons, at lines 693–694
and 755–756. The other occurrence constructs the returned universe attribute.

Before the HLA phase, every range operation starts from actual constructor
ranges or earlier restrictions of them. Unit merging and ordinary
restrictions intersect unique actual vectors. The second-order union is
formed from unique actual donor ranges and passed only as a restriction;
it is never an original stored domain object. Actual writes preserve their
old vector order because each write is an intersection/filter of an
existing actual range.

Consequently the two executions are **identical in every operational
state component before HLA**, except for the unread original universe
bindings and eventual return attribute. This includes the saved range
lengths, local snapshots, matrices, counters, registry values, pending
callbacks, and unit-domain vectors. Even stale states and any missed
scheduling opportunity are identical in the two executions. No semantic
correctness theorem or cache-exactness assumption is needed for this
initial simulation.

## 4. Base-R HLA storage algebra

Under the stated inert-operation contract, `%in%` uses values and returns
an ordinary flat logical membership vector. One-index filtering of a
character matrix or array flattens it. Names may survive a vector filter;
ordinary dimensions and irrelevant metadata do not become literal values.
The outer `c(...)` likewise concatenates values and can retain names.
Neither operation introduces a value absent from its operands.

For one HLA symbol write `mu(v)` for the number of copies of `v` in `S`,
and `m(v)` for the number in the old virtual range. The exact implementation

```
new = c(old, S[!S %in% c(old, donor)])
```

has these multiplicities, regardless of the names on any element:

```
m_new(v) = m(v)                  if m(v) > 0,
           0                     if m(v) = 0 and v belongs to donor,
           mu(v)                 otherwise.
```

Thus support follows the canonical set equation

```
supp(new) = supp(old) union (U minus (supp(old) union donor)).
```

The virtual range begins as an actual unique range, or as NULL for a missing
symbol. Initially `0 <= m(v) <= mu(v)`. The displayed recurrence preserves
this **bounded-submultiset invariant**, even when the same symbol is
extended several times. It may retain one copy of an original actual value
while appending all copies of a newly introduced value. Names or repeated
values in this local virtual range are therefore expected.

On bounded submultisets, length equality implies full support: omission of
one value loses at least its positive domain multiplicity and no other
value can compensate with excess copies. The converse is false: full
support may still contain fewer than `mu(v)` copies of a value initially
present only once. Therefore this lemma alone does **not** establish
equality of the full-domain predicate across the two executions.

## 5. The missing donor value repairs the cardinality argument

The additional canonical source invariant is:

> Whenever either HLA extension is selected, its selected donor has exactly
> one physically exceptional symbol against the current virtual target.

At that symbol `s`, choose `v in donor_s minus virtual_s`. Since the donor
is an actual domain-contained range, `v in U_s`, and since it is in the
donor, the complement appended by this HLA extension omits `v`. The old
virtual range also omits `v`. Therefore the new virtual range still omits
`v`, both in the normalized run and the stored-domain run.

In the normalized run its length is strictly less than `|U_s|`. In the
stored-domain run the bounded-submultiset invariant gives

```
length(new) <= length(S_s) - mu(v) < length(S_s).
```

Both source length predicates are therefore FALSE whenever actually
evaluated. This is stronger and more precise than trying to characterize
them as full-coverage tests on arbitrary virtual multisets.

### 5.1 Independent source review of the donor-selection premise

I read the production helper call sites and updates, and rechecked the
arguments in `../proof_state/LIFECYCLE_PROOF.md`,
`../proof_state/QUIESCENT_MATRIX_PROOF.md`, and `../branch_review/REVIEW.md`.
The needed chain is as follows:

1. A restriction that finds its symbol present has a live nonunit target.
   Unit birth cleans the two relevant registry entries and marks the
   clause a unit or eliminated before recursive callbacks. A stale
   propagation snapshot may name a different-symbol unit; it exits at the
   missing-symbol guard. Treating every entry as a live nonunit would be
   too strong.
2. FALSE comparison bits consumed by ordinary callbacks are sound under
   the current unit constraints. Reverse updates occur before recursive
   callbacks for nonunit restriction/deletion; unit restriction can defer
   physical target updates because its constraint restricts both sides.
3. Birth-range certificates establish final physical containment in each
   unit without assuming that every skip is physically correct at the
   instant of a nested call. Birth indices are registered only once. A
   retained old unit can shrink through merging, but is never re-registered
   as a new birth. Induction over birth order discharges the frozen
   certificates after all propagation returns. **Nonstrict** containment
   is the needed conclusion; the known equality-skip gap remains possible.
4. A TRUE bit can become physically stale only when its source shrinks.
   Every such source shrink owns a row-update obligation; absent-symbol
   targets cannot contain its nonempty range, uninitialized pairs are
   compared later, and inactive targets cease to matter. Nested shrinks
   create their own obligations. A source deletion clears its old column
   before callbacks. No pending frame remains at HLA entry. Together with
   the preceding physical containment, this gives raw-exact live rows.
5. Stored counters equal the sums of their initialized Boolean rows.
   Guarded flips prevent duplicate increments/decrements under reentry.
   Initial pair construction sets row sums directly. This bookkeeping
   claim is distinct from the semantic meaning of each bit.
6. For a nonunit HLA target, live initial rows are therefore exact. Its
   virtual expansion clears precisely the live donor bits whose range
   becomes contained at the expanded symbol, using the complete symbol
   registry. Expansion cannot invalidate an existing containment. Matrix
   writes affect only the current target's row; a subsequent target starts
   with a different row and the original count matrix. Removed targets
   are removed from the remaining donor set and registry.
7. For unit HLA, final physical containment establishes the initial count
   `width - contains_unit_symbol`. A donor first materialized lazily has
   FALSE at the unit symbol and TRUE at other symbols. If one of those
   other symbols had previously expanded, the complete symbol registry
   would already have materialized that donor. Thus its lazy row is exact
   on first use; the same monotone updates preserve it thereafter.

Steps 6–7 prove the exceptional-value premise for the normalized execution.
The storage theorem need not reprove the entire canonical lifecycle with
multisets: before HLA the two states are exact copies, and the following
simulation carries that established premise through the HLA phase.

## 6. Coupled induction during HLA

At corresponding source positions maintain:

* Actual clauses, actual vector orders, indices, flags, registry values,
  matrix bits/counts, donor-used flags, loop snapshots and continuations
  are exactly equal.
* At every virtual symbol, the stored-domain virtual support equals the
  normalized virtual set; its multiplicities are bounded by the stored
  domain multiplicities. Virtual names and raw cardinalities need not
  correspond exactly.

Both properties hold at HLA entry. A donor-count match chooses the same
index, and its exceptional-symbol mask is identical. The support equation
in Section 4 gives equal next support. Section 5 supplies a missing donor
value and proves both full-domain branches FALSE, closing the only
nontrivial cardinality-control obligation.

The remaining HLA decisions compare unchanged structural objects or use
membership of actual donor values in virtual ranges. Repeating or naming
the membership-table values changes none of those answers. The same
registry indices are visited, the same bits/counts are cleared, and the
same hidden-subsumption deletion happens or fails to happen. The virtual
clause is local; neither HLA loop stores its expanded values into `entries`.
Deletion modifies only flags and registry index vectors. Unit HLA similarly
does not write virtual ranges into the actual unit-domain environment.

The induction applies across both repeat loops and every target, even when
the virtual value order differs because stored domain order differs. On
return, `entries[!eliminated]` selects exactly the same unchanged actual
payloads and positions. This proves the theorem.

## 7. Consequences and failed strengthenings

**Semantic/control extension.** Assignments over `S_s` and `U_s` have the same
possible character values, and exact actual output equality transfers the
existing canonical semantic/control arguments to this ordinary storage
contract. Repeated passes are coupled by induction too. This can be
composed with the canonical preimage/membership-cell theorem after domain
normalization. It establishes no new completeness, saturation, or
idempotence claim; known canonical scheduling gaps are preserved.

The following stronger claims are false or unsupported:

* **Virtual ranges remain unique/unnamed.** For `U_X = U_Y = {a,b,c}`, take
  `(X in {a,b} or Y in {a,b}) and (X in {b,c} or Y=a)`. Both runs return
  `(X=b or Y in {a,b}) and (X in {b,c} or Y=a)`. Store each domain instead
  as `c(c,c,c,c,b,b,b,a,a)` with ordinary names. The first nonunit HLA
  extension has `old="b"`, `donor=c("b","c")`, and a named
  `new=c("b","a","a")`. It omits `"c"`, so the length test remains false.
  The next target similarly makes `c("a","c","c","c","c")` and omits
  `"b"`. These are genuine public-constructor kernel traces, not arbitrary
  invented internal states.
* **Every present virtual value has all its domain copies.** The same
  example retains one `"b"` where the stored domain has three. This is
  precisely why the canonical full-preimage theorem does not, by itself,
  prove the present extension: domain multiplicity changes while actual
  ranges remain single-copy, so they are not full preimages of that lift.
* **Length equality iff full support for all virtual submultisets.** With
  `S=c("a","a","b")`, `old="a"`, and empty donor, the extension is
  `c("a","b")`: full support but unequal lengths. This is an invalid
  selected-HLA situation, because the donor has no exceptional value.
* **A missing donor value alone excludes equal lengths.** Without the
  capacity bound, domain `c("a","b")`, old `c("a","a")`, donor `"b"`
  gives equal lengths while still omitting `"b"`. Initial unique actual
  ranges and bounded appending exclude that state in the theorem.
* **Full returned objects, including universe, are identical.** The
  distinct input environments and their preserved bindings refute this
  independently of simplification. Only the exact payload claim is made.

All new work is confined to this directory. Production source is unchanged,
and no commit was made by this audit.
