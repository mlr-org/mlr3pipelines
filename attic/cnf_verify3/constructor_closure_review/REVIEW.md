# Independent review of constructor-to-kernel normalization

Reviewed 2026-09-06, against
[`constructor_domain_closure/PROOF.md`](../constructor_domain_closure/PROOF.md)
with SHA-256
`549495edf12fb2657ffd764972b4303db41efe0a3ee32c192c771ee9cf7d985c`.
The unchanged production source hashes are recorded below and in both result
JSON files. This is a source-level mathematical review, with independent
finite checks; it is not a formalization of the R interpreter.

## Verdict and required scope correction

The scalar normalization arguments are sound for the stated ordinary storage
shapes. I found no counterexample involving repetitions, ordinary names or
metadata, valid dimensions/dimnames, or `I()` applied to those shapes. Both
atom negation and the direct-clause path in formula negation establish plain,
unique, nonempty proper complement ranges. The ordered kernel-payload
corollary passes the independent checks below.

**The full semantic closure claim needs an explicit symbol-identity premise.**
Successful registration of an ordinary nonmissing scalar string is not enough
to justify the kernel semantic theorem. The proof's agreement premise for
`match`, `%in%`, and `unique` concerns character values; it must also cover
environment insertion, direct lookup, and name enumeration. In particular,
enumerated registry names must still identify the original bindings and equal
the corresponding clause symbol strings.

The concurrent character-identity review supplied a concrete public
counterexample, which I reran on both runtimes using its
[`reproduce_public.R`](../character_identity_review/reproduce_public.R).
Let X have the UTF-8 name `\u00e9`, domain `{a,b,c}`, and let Y have ASCII name
`Y`, domain `{a,b}`. All three clauses are constructed using the public API:

```
(X in {a}) AND (X in {b} OR Y in {a}) AND (X in {c} OR Y in {b}).
```

This has no models. Under `LC_CTYPE=C`, both R 3.6.3 and R 4.6.1 return a
formula with two models. A fresh environment demonstrates the underlying
identity failure: after `e[[name]] = "a"`, `names(e)` contains the ASCII
string `"<U+00E9>"`; the original UTF-8 name is not a member of that character
vector. Direct access using the original name still works. The C.UTF-8
control returns FALSE as expected. R 4.6.1 also emits native-encoding warnings;
the calls nevertheless complete with the incorrect truth result. See
[`name_boundary_r36.log`](name_boundary_r36.log) and
[`name_boundary_r46.log`](name_boundary_r46.log).

This is **not a new dimensional/constructor normalization defect**. The
character-identity stream found it, and the input clauses already have
canonical ranges. It shows why passing a normalization boundary cannot by
itself establish the semantic corollary. The proof's general reference to
Unicode/locale exclusions should be replaced or supplemented by this precise
positive premise and a citation to that counterexample. Nonempty ASCII symbol
names are a sufficient concrete restriction on the inspected runtimes. The
empty string remains permitted as a *domain value*, not a symbol name.

Suggested additional premise:

> Symbol identity agrees across character/list indices, environment insertion
> and lookup, and environment name enumeration. Every registry enumerates the
> original symbol keys faithfully, so its enumerated names remain equal to
> the clause names and retrieve the same bindings. Nonempty ASCII symbol names
> suffice for this premise on both inspected runtimes.

With that scope correction, I accept the constructor normalization argument
and its composition with the separately reviewed domain-storage and canonical
kernel theorems. This review does not independently reprove the kernel.

## 1. Scalar first-occurrence order

For a positive-length ordinary array, write its dimensions as
`(d1, d2, ..., dk)` and the column-major location of a scalar as
`i1 + d1 * q`, with zero-based `q` indexing the remaining coordinates.
Both inspected `unique.matrix`/`unique.array` implementations select a stable
subsequence of the first-axis slices using `duplicated.default(...,
fromLast = FALSE)` and `drop = FALSE`.

If a slice at first-axis coordinate `i1` is removed, it matches an earlier
retained slice at coordinate `j1 < i1`. Its scalar at every remaining
coordinate `q` therefore has an equal retained predecessor at
`j1 + d1 * q < i1 + d1 * q`. Thus no first occurrence of a scalar value is
removed. Subsetting the retained slices also preserves their scalar order:
within each `q` block their first-axis coordinates stay ordered, and the
remaining-coordinate blocks themselves keep their order. This proves exact
equality of the two first-occurrence support vectors, not just set equality.

The special cases in the inspected bodies are consistent with that argument:
when the product of all dimensions except the first is one, each slice has a
single scalar, so directly deduplicating `x` is the same slice selection.
One-dimensional arrays fall into this case. Nonempty domains cannot have a
zero dimension; empty values take the constructor's FALSE branch before
`unique`. Ordinary dimnames can inhibit a duplicate-slice comparison, but
retaining additional slices cannot introduce a new value or disturb the
first occurrence of a value.

R 3.6.3 uses `asplit(x, MARGIN)` where current R uses
`asplit(x, MARGIN, TRUE)`. That difference does not change the scalar argument.
The independently selected duplicate-row example has input flat sequence
`b,b,a,a,a,b` and surviving atom sequence `b,a,a,b`. Both have the exact
first-occurrence sequence `b,a`; the atom remains dimensional and repeated.
Both boundary logs show the actual objects using `dput`.

## 2. Clause accumulation and `setdiff`

Every proper-atom insertion in `CnfClause` uses
`unique(c(entries[[symbol]], atom$values))`, including the initial insertion
with NULL on the left. Base concatenation flattens the scoped ordinary
storage and the subsequent default uniqueness removes names and scalar
repetitions. Named replacement updates one symbol entry instead of creating
duplicate names. The same normalization occurs when merging canonical
clauses and when Formula OR updates/adds a range. Existing right-only ranges
in Formula OR were already canonical.

For R 3.6.3, unconditional `as.vector` of both `setdiff` operands removes the
ordinary dimensions/classes before the unmatched elements are deduplicated.
For R 4.6.1, `.set_ops_need_as_vector` forces that coercion for dimensions of
rank at least two, for ordinary one-dimensional arrays whose class is lost by
`[0L]`, and for an inert class marker lost by `[0L]`. Plain named/attributed
character vectors can take the uncoerced branch: `unique.default`, removal of
names, and ordinary filtering still give a plain unique vector. Thus both
versions return exactly the distinct domain complement in stored domain
first-occurrence order.

The `AsIs` extension was exercised independently by applying base `I()` to
every ordinary shape, including the named four-dimensional arrays. `I()` on
an ordinary implicit matrix/array gives an explicit `AsIs` class; neither
runtime supplies a `unique.AsIs` method, so its ordinary constructor call
uses `unique.default` and removes that marker. The domain can retain `AsIs`,
but it does not cause an attributed actual complement in either negation
path. The proof should keep its existing limitation to the stated primitive
contract: this is not a theorem about arbitrary explicit S3 class stacks or
user-supplied methods.

## 3. Both negation paths and induction over the grammar

Atom negation bypasses `CnfAtom`, but its `setdiff` result has the exact
complement support and the stronger plain unique representation. Double atom
negation uses domain order. It preserves truth but need not reproduce the
original atom's dimensions or literal order.

Formula negation is a separate boundary that cannot be discharged by the
clause-accumulation lemma: it calls `structure(list(setdiff(...)), ...)`
directly. Each original canonical range is nonempty and proper, so each
directly created complement is also nonempty and proper. Each unary clause
has exactly one symbol. For each original clause, all these unary clauses
have the same universe and contain no constants or nested Formula members.
Consequently its `CnfFormula` constructor avoids both recorded constant/owner
failures. The resulting conjunction is satisfiable by independently choosing
one value in each complement. The subsequent Formula OR reduction therefore
starts from canonical operands and supplies canonical kernel entries.
Clause negation delegates through its one-clause Formula conversion.

The remaining induction is valid subject to the explicit constructor success
conditions and the semantic theorem's symbol-key premise. Conversions of a
proper atom pass through `CnfClause`; proper-clause `as.list` round trips copy
canonical ranges into atoms and back through accumulation. Formula AND
concatenates canonical clauses, and Formula OR only retains canonical
Cartesian disjunctions after exact full-range filtering. Constant branches
preserve scalar Boolean truth, subject to the already recorded result-class
exception. Identity conversions provide no validation of forged objects or
selector-damaged clauses and must stay outside the set of grammar leaves.

The explanation of negation completeness is correct under these premises.
Each nonconstant proper clause has a falsifying valuation because its
nonempty proper ranges refer to distinct independently valued symbols. A
nonempty conjunction containing such a clause therefore cannot be a
tautology. Hence a sound, canonical returned negation represents semantic
TRUE only as a Boolean constant. This argument is distinct from any claim
that ordinary simplification recognizes all contradictions.

## 4. Public grammar and exclusions checked independently

The 58 directed controls agree with the proof's listed boundaries:

* Clause owner checks occur before its constant test, through the first TRUE
  or full-range result. The upfront list type assertion still checks entries
  that a later constructor loop would not visit.
* Formula takes its owner from the first member but skips TRUE and recognizes
  FALSE before owner checks. A nonconstant nested Formula before FALSE enters
  the known malformed-input path; prior proper Clauses alone do not.
* Raw `TRUE | proper_clause` has correct truth and loses its promised Clause
  class. The later typed Formula list constructor rejects it.
* The TRUE-Clause `as.list` path fails; proper Clauses and the tested constant
  Formula round trips behave as described.
* Empty values of several noncharacter types are accepted as FALSE, while
  the tested nonempty numeric, logical, raw, complex, list, factor and missing
  character inputs are rejected. Missing names are attributes and do not
  imply missing domain values or missing binding strings.
* Ordinary dimensional outer lists of typed atoms or clauses are accepted
  and normalize correctly; their outer layout does not bypass accumulation.
* Native mixed Atom/Clause Ops encounter the documented dispatch problem on
  R 3.6.3 and work on R 4.6.1. The direct selected Formula method works on both.

No claim is made about selector correctness, custom methods, universe
mutation, arbitrary explicit class stacks, unsupported sizes/resources,
comparison normalization, or faithful Unicode identity in arbitrary locales.

## 5. Evidence and reproducibility

[`review.R`](review.R) is independently written and does not source the
author's harness. It loads real checkmate 2.3.4 and the six unchanged CNF
files. Only the ordinary `stopf`/`map_chr` formatting helpers are supplied.
No constructor, Boolean operator or simplifier binding is replaced.

The reference support order and truth evaluator use scalar positional
character comparisons. They do not use CNF `all.equal`, CNF operators,
`unique`, `setdiff`, `match`, or `%in%` to derive expected truth. Actual output
payload comparisons remove only the outer universe and CNF class; they do
not sort, flatten, deduplicate, or strip attributes from actual clause ranges.

Both runtimes passed the same independent checks:

| Check | Count per runtime |
| --- | ---: |
| Scalar-order and flattening shapes | 4,570 |
| Independently crossed domain/value shapes | 1,645 |
| Proper atoms retaining repeated scalar payloads | 470 |
| Proper atoms retaining dimensions | 658 |
| Exact stored-versus-normalized multi-symbol records | 90 |
| Object truth comparisons | 8,290 |
| Positional assignment rows checked | 246,650 |
| Separate grammar/exclusion controls | 58 |

The random seed is 612906. The scalar cases use lengths 6, 8, 9, 12 and 16,
with up to four dimensions. Formula cases use three unusual ASCII binding
names (`a b`, `...`, and `X$Y`), five domain values including `""`, two to five
input clauses, and up to three literals per input clause. All 90 complete
saved output records agree exactly across runtimes; see [`compare.log`](compare.log).

During harness development, an initial reference truth vector accidentally
retained names automatically added by `vapply` over character input. The
scalar truth values already agreed; setting `USE.NAMES = FALSE` on that
reference construction fixed the oracle's metadata mismatch. This was not a
CNF result normalization or a production failure. The final scripts/logs use
the corrected oracle.

From the repository root:

```sh
Rscript attic/cnf_verify3/constructor_closure_review/review.R
Rscript attic/cnf_verify3/constructor_closure_review/boundaries.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/constructor_closure_review/review.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/constructor_closure_review/boundaries.R
Rscript attic/cnf_verify3/constructor_closure_review/compare.R

# Reproduce the separate character-identity stream's supplied boundary.
Rscript attic/cnf_verify3/character_identity_review/reproduce_public.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/character_identity_review/reproduce_public.R
```

Production SHA-256 hashes:

```
34ceb2fb392db49ba47adf2a970a824222dfa792b93a895147f0a437b5bdd13d  R/CnfUniverse.R
19a159daa63d2bafb8f5238cfd91d9940a81c08f366bbc5ecb81eca3a3807341  R/CnfSymbol.R
d03da14a7bce4989477efa09303e099a3d5b7a8abb3be3526ee850161a59dce6  R/CnfAtom.R
40d021025c290a5ad6522fadcda166ff55d83bdd1d27eb0f4fd1fbc6222f12f8  R/CnfClause.R
e94aabfb277cf3bc2c951a09571658fd7dba0b14bce484f8c7d642e8d7e41f60  R/CnfFormula.R
7d34ad60035509a463168752bb5f6f82d87fd7182a94a0e48461be7cdbe35fdc  R/CnfFormula_simplify.R
```

Only this review directory was written. No production files, package tests,
the proof under review, or other audit stream files were edited. No commits
were made and no full package test suite was run.
