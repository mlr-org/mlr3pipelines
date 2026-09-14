# Constructor-to-kernel closure for ordinary character storage

Source review of the unchanged CNF implementation, 2026-09-06. This closes a
public-boundary premise of the [domain-storage theorem](../domain_storage_contract/PROOF.md)
and extends the [canonical operator proof](../operator_proof/README.md) to
ordinary repeated, named and dimensional stored domains and atom values.
It is a source-level argument using the inspected R primitive bodies, not a
formalization of the R interpreter.
The [independent review](../constructor_closure_review/REVIEW.md) accepts the
normalization lemmas and requires the explicit symbol-map premise below for
the semantic corollary.

**Result.** Ordinary dimensions and repeated character values can survive in a
proper `CnfAtom`, but the public paths considered below establish canonical
actual clauses before calling `simplify_cnf`. Negating a formula also
establishes this condition, although it constructs complement clauses directly
and bypasses `CnfClause`. No new accepted ordinary path violating this premise
was found. The already recorded constructor errors, constant result-class
loss, selector defects and pre-R-4.3 mixed dispatch remain exclusions.

## 1. Precise objects and permitted operations

For each successfully registered symbol s, let S_s be its stored domain object
and U_s the plain vector containing the distinct values of S_s in first
occurrence order. Domain objects have positive finite length, character
storage, and no missing **values**. They may have repeated values; arbitrary
ordinary names, including missing/empty/repeated names; valid matrix/array
dimensions and dimnames; and metadata that does not change primitive behavior.
A one-dimensional array is included. Empty string domain **values** are
included. A symbol name is a successfully registered ordinary nonmissing
scalar string; its names or dimensions do not change that binding's identity.

The basic shape class has implicit character/matrix/array classes only. The
same argument covers otherwise inert class markers satisfying the primitive
contract below; the tests separately include one such marker and base `AsIs`.
This is not a claim about every character object with arbitrary built-in or
user-provided classes. In particular, forged matrix classes without valid
dimensions are not ordinary matrices.

Let supp(v) denote the set of scalar character values in v. The equality
relation must be total and shared by membership, `match`, `unique` and the
intended domain semantics. ASCII, including the empty string, suffices.
No part of the proof uses `sort`, locale collation, or `all.equal`. Custom
methods, changed primitive bindings, active bindings, callbacks mutating
operands, external universe mutation, and byte-marked unoperable string
mixtures are excluded. All sizes, integer/index arithmetic, allocation and
runtime stacks must be supported; the calls are allowed to finish.

Symbol identity must also agree across character/list indices, environment
insertion and direct lookup, and environment name enumeration. Registry names
must enumerate the original keys faithfully and retrieve the same bindings;
distinct symbols must not collide. Successful `CnfSymbol` registration alone
does not establish this premise. The concurrent
[Unicode native-name failure](../character_identity_review/FINDING.md) supplies
a canonical three-clause counterexample under `LC_CTYPE=C`. Nonempty ASCII
symbol names suffice on both inspected runtimes. This premise is required
when composing the scalar normalization lemmas with kernel semantics, even
though it is unnecessary for the scalar first-occurrence-order lemmas alone.

A **broad proper atom** has one registered symbol s and a character values
object A with emptyset strictly contained in supp(A), itself strictly
contained in U_s. Its values object need not be flat or duplicate-free.
A **canonical proper clause** is a nonempty ordinary named list with distinct
registered symbol names and plain, unnamed, duplicate-free, nonempty proper
character ranges. A canonical formula is a nonempty list of these clauses, or
a classed scalar Boolean constant. Constants may retain harmless attributes;
their scalar payload is what matters. This usage of canonical is about
representation, not minimization, saturation or a unique normal form.

The safe construction grammar starts with `CnfUniverse`, successful
`CnfSymbol`, and `CnfAtom`/`%among%`. It permits atom negation, clause
construction from atoms or canonical clauses, the ordinary conversions,
formula construction from canonical clauses/formulas, and CNF Boolean
methods. List-constructor owner/constant success conditions are those in
section 7. For general native mixed operators, R >= 4.3's supported dispatch
is required; direct invocation of the selected CNF method also applies on
R 3.6.3. Identity conversions do not validate or repair arbitrary forged or
selector-damaged objects; those are not leaves of this grammar.

## 2. Three normalization lemmas

### 2.1 `unique(values)` need not establish canonical atom storage

For a plain character vector, `unique` removes names and ordinary metadata and
retains first occurrences. For a matrix/array, `CnfAtom` dispatches to
`unique.matrix`/`unique.array`, with default `MARGIN = 1` and `fromLast = FALSE`.
Those functions remove repeated first-dimension slices. They do not remove
all repeated scalar elements and preserve dimensions.

They do preserve scalar support and the first-occurrence order of that
support. Every deleted slice is equal to an earlier retained slice. Each of
its scalar elements therefore already occurs at the corresponding position
of the earlier slice. R's column-major order places that corresponding
element earlier in the original flat sequence. No first scalar occurrence is
deleted, and the order of retained scalar elements is unchanged. Even if
slice metadata prevents an otherwise possible deduplication, merely keeping
an extra slice does not change this conclusion. For a one-dimensional array,
each slice is a scalar and the same argument applies.

Consequently, for these ordinary objects,

```
flat_unique(unique(values)) == flat_unique(values)
```

as exact vectors, although `unique(values)` need not itself be flat or unique
by scalar value. The checks count 17,748 proper atoms still containing repeated
scalar elements, so the premise was actually exercised.

### 2.2 `unique(c(existing, incoming))` establishes canonical actual ranges

For the ordinary operands here, base `c` concatenates scalar values in order,
strips dimensions and ordinary metadata/classes, and may preserve names.
The subsequent vector `unique` removes the remaining names and scalar
duplicates. Thus the result is exactly the plain first-occurrence union of
the two operands' scalar supports. NULL as `existing` is the empty sequence.

The expression in `CnfClause` is used even for the **first** incoming range.
There is no shortcut copying a matrix-valued atom directly into a clause.
`|.CnfFormula` uses the same expression for each unioned range. This is the
key distinction from the already recorded dimensional clause selector:
that selector calls `unique(i)` on a matrix and then immediately uses the
still-repeated indices without a flatten-and-deduplicate boundary.

### 2.3 `setdiff(S, A)` establishes canonical complements on both runtimes

R 3.6.3 begins `setdiff` with `x <- as.vector(x)` and `y <- as.vector(y)`,
then returns a `unique` of unmatched x elements. For the scoped ordinary
character objects, this is the plain distinct domain complement in domain
first-occurrence order.

R 4.6.1 has a different body. It conditionally coerces both operands to
vectors using `.set_ops_need_as_vector`, then does:

```
x <- unique(x)
names(x) <- NULL
y <- unique(y)
names(y) <- NULL
x[match(x, y, 0L) == 0L]
```

It is insufficient to assume that current R still coerces every set-operation
operand unconditionally. The actual guard matters:

* Either operand with at least two dimensions forces coercion. Ordinary
  matrices and higher-dimensional arrays therefore cannot reach the
  class-preserving branch as arrays.
* A one-dimensional ordinary array loses its array class in `x[0L]`; the
  guard's class-stability test therefore also forces coercion.
* A plain or named character vector can stay on the noncoercion branch.
  `unique.default` and `names(x) <- NULL` make it plain and unique before
  filtering, so the output is still canonical.
* Ordinary metadata does not change these operations. For the tested inert
  marker, `[0L]` loses its class. For a base `AsIs` domain and a proper atom
  range, `unique` has already removed the incoming `AsIs` class; incompatibility
  with the plain character operand forces coercion. The formula-negation
  operand is plain by the canonical-clause premise. Neither extension yields
  an attributed actual complement.

It follows, on both inspected versions, that

```
setdiff(S, A) == U[!U %in% flat_unique(A)]
```

as exact plain vectors. In particular a broad proper atom's complement is
nonempty and proper. Duplicates in S cannot survive `setdiff`, and dimensions
in either S or A cannot cause a matrix `unique` to leave repeated complement
elements. Runtime bodies and individual shape/coercion decisions are saved
in `base_contract_r36.txt` and `base_contract_r46.txt`.

## 3. Symbols, atoms and atom negation

`CnfSymbol` (`R/CnfSymbol.R:40`) checks a character, positive-length domain
without missing values and stores it unchanged. Its assertions do not impose
plain-vector storage, uniqueness or nonmissing **names**. The successful
environment binding establishes the symbol identity. `CnfAtom` flattens its
symbol handle using `c(symbol)`; any surviving scalar name is irrelevant to
the environment and list indexing, which use the scalar string value.

`CnfAtom` (`R/CnfAtom.R:69`) checks its values using real `assert_subset`
against S_s. Full coverage is `all(S_s %in% values)`, equivalent to
U_s being contained in supp(values), even with repeats and dimensions. Empty
values become FALSE. On the remaining branch, lemma 2.1 gives a broad proper
atom with exactly the requested membership semantics.

Empty noncharacter inputs admitted by `assert_subset` take the FALSE branch
before `unique` and cannot introduce a noncharacter proper range. The tests
cover ten empty shapes/types; real checkmate rejects the tested nonempty
numeric, logical, raw, complex, factor, list and missing character inputs.

`!.CnfAtom` (`R/CnfAtom.R:209`) uses `setdiff(S_s, A)` directly, without
reentering `CnfAtom`. Lemma 2.3 proves both its exact complement semantics and
its **stronger** plain unique storage condition. The second negation uses
domain order, so exact atom-object identity is not asserted. For example a
matrix-valued atom selecting values in order b,a can become the plain vector
a,b after double negation. Its membership truth is unchanged.

## 4. The clause boundary

`CnfClause` (`R/CnfClause.R:87`) accumulates each proper atom using lemma 2.2.
The list member is addressed by the scalar symbol string, so several atoms
with the same symbol update one entry; they cannot create repeated names.
Incoming canonical clauses are iterated by their unique names and accumulated
with the same normalization expression.

Empty selections have already become FALSE constants and are skipped. Each
accumulated range is a nonempty subset of U_s. After every union, full coverage
is tested by `all(S_s %in% accumulated)`. Multiplicities, names and dimensions
do not change this predicate. A covering range makes the whole disjunction
TRUE; otherwise every retained range is proper. An empty accumulation becomes
FALSE. This establishes the canonical clause condition, with the exact
disjunction semantics, whenever the constructor's owner checks succeed.

`as.CnfClause(atom)` always invokes this one-element construction. Converting
an atom to a formula therefore also passes through this boundary. Ordinary
`as.list` of a **proper** clause copies its plain ranges into broad atoms, so
that proper-clause round trip is safe. The known TRUE-clause `as.list` issue
is a separate path and is not silently included.

## 5. Formula negation supplies its own normalization boundary

For a canonical proper formula F = AND_i C_i, `!.CnfFormula`
(`R/CnfFormula.R:364`) computes

```
!F = OR_i AND_(s in C_i) [s in U_s minus C_i[s]].
```

It constructs each unary clause with `structure(list(setdiff(...)), ...)`,
without calling `CnfClause`. Consequently lemma 2.2 alone would not close this
public path. Lemma 2.3 is the missing premise: every directly constructed
complement range is plain, unique, nonempty and proper on both R versions,
including when the original stored domain is a repeated matrix.

Each unary clause has one registered symbol name and the original universe.
Within a negated input clause, every original symbol occurs once. The input
to its `CnfFormula` call is thus a nonempty list of canonical proper clauses,
all with the same owner, containing neither TRUE/FALSE members nor nested
formulas. This avoids both recorded constructor constant failures. These
unit conjunctions are satisfiable: independently choose a value in each
nonempty complement.

The remaining `Reduce` combines Formula operands by OR. The proof in section 6
therefore establishes the canonical premise at every later kernel entry.
`!.CnfClause` delegates through its one-clause Formula conversion and inherits
the argument. Constant negation directly flips the scalar logical payload.

In combination with the existing operator semantic theorem, the previous
negation-completeness result extends to these ordinary stored domains:

```
isTRUE(c(!F)) iff F has no satisfying valuation,
```

on successful completion with canonical actual clauses. A conjunction of
proper clauses cannot be tautological over the independent Cartesian domains,
so semantic TRUE must have the constant representation. This does not claim
that ordinary construction/simplification alone recognizes every contradiction.

## 6. AND, OR and the kernel premise

For `&.CnfFormula`, successful one-operand conversions establish canonical
formulas; nonconstant same-owner operands contribute the concatenation of
their clause lists. This is canonical kernel input and represents conjunction.
The constant branches return the correctly converted operand.

For `|.CnfFormula`, conversions establish the same premise. Each retained
Cartesian pair clause is the disjunction of one left and one right clause.
Existing right-only ranges remain canonical, and every updated/added range
passes through lemma 2.2. Names remain unique because named replacement
updates an existing symbol or appends exactly one new symbol. The full-range
test reads S_s only through membership and discards exactly tautological pair
clauses. The kernel receives the retained canonical clauses, possibly the
empty conjunction. Local callback assignment cannot accumulate an earlier
left row into a later row; that separate R-evaluation premise is proved and
tested in the existing operator review.

Atom/Clause AND methods delegate to the Formula method. Atom/Clause OR methods
either use their correctly typed constant branch, delegate to Formula OR, or
call the clause constructor. The already recorded raw-TRUE/Clause result-class
exception is specified in section 7. These paths do not otherwise bypass the
normalization boundaries above.

At each kernel entry, the original domain may still be a named repeated array,
but every **actual clause** now meets the domain-storage theorem's canonical
premise. That theorem plus the existing canonical kernel semantic/properness
theorem gives correct meaning and canonical returned clauses. Induction over
the construction grammar closes subsequent operations. Adequate resources
and supported dispatch remain premises; this is not a new bound on execution.

There is also an exact normalization corollary. Replace each stored S_s by its
first-occurrence flat U_s, and replace initial atom values by their flat
first-occurrence unique vectors, keeping all list orders and symbol identities
corresponding. Lemmas 2.1–2.3 give exact actual ranges at each constructor or
complement boundary. The domain-storage simulation then gives the same
ordered kernel payload; induction extends that exact equality through the
Boolean composition. Outer universe environments differ by construction and
are excluded from that comparison. Arbitrarily **reordering** the normalized
domain is not this exact-vector corollary: complement vectors follow domain
order, as the double-negation example demonstrates.

## 7. Exact exclusions and prior findings

This review does not upgrade type acceptance to validation of arbitrary
objects already carrying a CNF class. In particular:

| Public path | Boundary retained from earlier work |
| --- | --- |
| `CnfClause(list(...))` | The upfront type assertion checks every member. The owner is the first member's owner; every visited member must have that identical owner, including constants, through the first TRUE/full-range result. Later members are not visited. Universe-free FALSE combined with a proper owned atom therefore still errors in either order. |
| `CnfFormula(list(...))`, no FALSE encountered | The owner is taken from the first member even if it is TRUE. TRUE members themselves are skipped before owner checks, but all nonconstant members must match that chosen owner. Universe-free TRUE first can therefore make a later proper clause fail. |
| `CnfFormula(list(...))`, FALSE encountered | FALSE is recognized before owner checks. No earlier nonconstant Formula member may have accumulated in `other_entries`; otherwise the known FALSE-after-flattening path forms malformed kernel input and errors. Earlier proper Clause members alone do not cause this defect. |
| `TRUE | CnfClause` with a right operand that is not TRUE | The Clause method may return the bare left logical. Its truth is correct but its promised Clause type is absent; later typed list construction can fail. This is the existing result-class loss, not a dimensional-domain discovery. |
| Native mixed CNF Ops on R 3.6.3 | Differing CNF methods cause the existing incompatible-method warning/fallback. Direct CNF method invocation is the portable evidence here. R 4.6.1 supports the reviewed `chooseOpsMethod` dispatch. |
| Dimensional or missing clause selectors | These can produce duplicate/NA symbol names or NULL ranges. Identity conversions and Formula constructors do not recanonicalize them. The established semantic selector failures remain outside this closure theorem. |
| `all.equal` normalization and Unicode/locale issues | No comparison method is used in this proof or its result oracle. Those previously recorded cases are neither reclassified nor claimed repaired. |

The familiar constant errors and TRUE-class loss are reproduced only as
exclusion controls in `boundary_cases.R`. No new defect is claimed for them.
The empty string is a valid domain value, but an empty symbol name fails the
environment operation and does not enter the successful symbol grammar.

## 8. Executable evidence

The primary scripts source all six unchanged production CNF files and use real
checkmate 2.3.4. `stopf` and `map_chr` are ordinary formatting helpers only;
neither assertions nor CNF algorithms are replaced. Native R 3.6.3 and the
existing isolated `cnf-review-r46` container's R 4.6.1 run the same scripts.
The independent truth oracle compares scalar assignment values with all
original range occurrences using ordinary `==`; it does no CNF simplification
or Boolean operator dispatch.

`primitives.R` exhausts all domain sequences over three labels (one is the
empty string) at lengths one through four, their shape variants, every proper
support subset, two selected-value repetition patterns, and the applicable
selected-value shape variants. It checks retained atom supports/order, exact
plain complements, double-negation order and the exact clause boundary.
Both runtimes pass 1,500 domain objects, 113,064 proper atoms, 226,128 negations,
226,128 clause constructions, and 6,000 constant classifications. Of the
proper atoms, 64,584 retain dimensions and 17,748 retain repeated scalars.

`composition.R` uses seed 908216 and 400 generated formulas with up to three
symbols, two through four domain values, and one through five input clauses.
It compares ordinary stored-domain/value variants with normalized versions
by exact payload, not `all.equal`. It checks atoms, atom complements, clause
round trips, clause negation, formulas, double negation, safe nested
construction, AND and OR against assignment tables. Separately it exercises
all 225 ordered pairs in a 15-object operand pool under AND and OR for each
of 13 stored-domain shapes: 5,850 direct mixed calls per runtime. R 4.6.1 also
checks all 5,850 native calls; R 3.6.3 checks the 3,770 without a known mixed
class conflict. The 52 expected raw-TRUE/Clause class losses are counted
separately, and all their truth tables remain correct.

An optional **secondary diagnostic** adds a checking wrapper at the
`simplify_cnf` binding. It checks the real entries before invoking the original,
unchanged production function body. It never normalizes or changes an entry.
The primary run has no wrapper. All 6,250 saved payloads agree across both
runtimes, and every complete saved record is identical between primary/gated
runs on each runtime. The cross-version records have 182 separately checked
metadata differences: native R 3.6 reports the implicit class of raw logical
matrices as `"matrix"`, whereas R 4.6 reports `c("matrix", "array")`. Their
scalar payloads are identical, and returned CNF class records do not differ.
The comparison asserts exactly that metadata difference instead of using
`all.equal` or normalizing actual clause ranges. Results and reproduction commands
are in `README.md`; the source hashes are saved with each JSON report.

These finite checks support the source argument and expose its boundary
premises; they do not substitute enumeration for a universal proof. No
production files, tests outside this directory, or other audit streams were
edited, and this stream created no commits.
