# Two ordinary-string failures in CNF all.equal normalization

Found by root, 2026-09-06, after the canonical simplifier and indexing proofs
had completed independent review. Both cases use unchanged public constructors
and ordinary supported strings. They concern the representation comparison
helpers, not changed formula truth or a kernel simplification inference.
The [independent review](../comparison_review/REVIEW.md) confirms both causes
and records a related residual failure in delegated universe comparison.

`comparison_normalization.R` is a standalone public reproduction with real
checkmate, both R versions, and three installed collation locales. No fabricated
CNF representation, universe mutation, custom method or byte-marked string is
involved. Production source remains unchanged.

## 1. Identical formulas compare unequal because digest keys retain encoding

```r
u = CnfUniverse()
v = c("\u00e9", "\u00f6")
X = CnfSymbol(u, "X", c(v, "other"))
Y = CnfSymbol(u, "Y", c("a", "b", "c"))
make_formula = function(values) CnfFormula(list(
  CnfClause(list(X %among% values[[1L]], Y %among% "a")),
  CnfClause(list(X %among% values[[2L]], Y %among% "b"))
))
f = make_formula(v)
g = make_formula(iconv(v, to = "latin1"))
identical(f, g)       # TRUE
all.equal(f, g)       # four string-mismatch messages
```

R treats the UTF-8 and Latin-1 versions as the same strings. The two objects
are `identical()` and each corresponding clause separately passes
`all.equal()`. The complete formula's comparison fails on R 3.6.3 and 4.6.1
in all three tested locales: C, C.UTF-8 and en_US.UTF-8. Independent direct
evaluation agrees on every one of the nine assignments and finds the same
two models.

The formula normalizer first sorts each clause's symbol names and ranges,
then sorts whole clauses using their names followed by
`digest::digest(c(clause), algo="xxhash64")` (`R/CnfFormula.R:283–292`).
The serialized digest distinguishes string encoding marks that R equality
does not distinguish. In this example the two clauses' digest ordering is
reversed between the two encoding representations, so the final list
comparison aligns different clauses. This does not require equal digests or
a collision: the ordering keys are not invariant under the equality the
method is meant to implement.

## 2. Locale collation does not canonically order distinct values or names

```r
# Reproduces with LC_COLLATE C.UTF-8 or en_US.UTF-8 in both measured R versions.
u = CnfUniverse()
v = c("\u00e9", "e\u0301")
X = CnfSymbol(u, "X", c(v, "other"))
a = X %among% v
b = X %among% rev(v)
setequal(a$values, b$values) # TRUE
all.equal(a, b)             # two string mismatches
```

These are two **distinct** R string values: the precomposed character and the
two-code-point spelling. Their membership sets are exactly the same in `a`
and `b`, and their chosen order is the only difference. Their collation
weights tie in the measured Unicode locales. Ordinary `sort()` preserves the
opposite incoming orders here, so it is not a canonical representative of the
value set. The same failure propagates to the Clause and one-clause Formula
comparisons. It also arises when the two spellings are distinct symbol names
and a clause's symbol order is reversed (`order(names(clause))`).

Switching to locale C resolves this second case, but it does not resolve the
encoding/digest case above. Changing global locale is diagnostic evidence,
not a proposed application-wide repair.

R's primary [sorting documentation](https://stat.ethz.ch/R-manual/R-patched/library/base/html/sort.html)
explains that ordinary character sorting uses locale collation while radix
sorting uses a byte ordering. Its
[comparison documentation](https://stat.ethz.ch/R-manual/R-patched/library/base/html/Comparison.html)
also documents translation between marked encodings during comparison. The
particular ties, digest orders and CNF outcomes above are measured local
facts, rather than assumptions inferred only from those documents.

## 3. Evidence and concrete candidate

Per R version the script checks 24 public comparison pairs across the three
locales. There are 13 false negatives: ten collation-related comparisons and
three encoding-related whole-formula comparisons. In the latter three,
`identical()` is TRUE. The two corresponding-clause controls succeed in all
locales. Results are `comparison_normalization_r36.json` and
`comparison_normalization_r46.json`, with logs and lossless RDS records.

A private comparator converts character keys/ranges to UTF-8 before ordering,
uses radix ordering for values and symbol names, and computes formula digest
keys only after that normalization. It succeeds on all 24 pairs in both
versions. This is a concrete candidate for the demonstrated boundary, not a
production repair or a completed exhaustive comparison-helper contract.
The independent review checks 39 reduced pairs and 15,552 permutation/encoding
comparisons per R version, including 3,888 unequal controls in the latter bank.
Production has 5,256 false negatives in that bank; the prototype and private
copies retaining the production guards have no disagreements on its proper
objects. The guard-preserving copies also retain the original outcomes on 36
separate scope cases and preserve argument forwarding. The root prototype
alone is not a replacement for the complete public methods.

The digest is only an ordering aid. With proper ordinary list comparison, a
digest coincidence alone cannot make different clause payloads compare equal:
the final `all.equal.list` still compares the actual content. Its possible
effect is a failure to align equal unordered clause collections. Likewise,
the current collation issue produces incorrect differences, not a loss of
the simplifier's already proved truth preservation.

## 4. A payload-only correction does not normalize universe comparison

A separate late [dimensional-atom study](ATOM_SHAPE_COMPARISON.md), with
[independent review](../atom_shape_review/REVIEW.md), identifies another boundary
of the earlier candidate: accepted matrix/array atom values can retain scalar
duplicates. UTF-8/radix sorting preserves those multiplicities. Flattening and
scalar deduplication are additionally required for that broader atom-storage
class. This is distinct from the ordinary encoding/collation cases above.

The independent reviewer found a second venue for the collation-tie cause.
Create two universes with names `c("38\u00e9", "38e\u0301")`, identical domain
vectors `c("0", "1")`, and opposite insertion orders. Corresponding proper
atoms have the same payload but compare unequal under the two Unicode
collations on both R versions. The same applies to clauses and formulas.
Base `all.equal.environment` delegates to sorted binding lists whose tied names
remain oppositely ordered. All three classes compare successfully under C.
This is ordinary public construction; the universe objects are distinct but
their binding maps and domain-vector orders agree exactly.

Both the root prototype and guard-preserving payload correction retain this
failure, because they leave delegated universe-attribute comparison alone.
The review saves a reduced reproduction in
`../comparison_review/universe_residual.R`. Any complete correction needs a
decision about normalizing that structural comparison as well. Existing tests
explicitly distinguish reversed domain-vector order, so treating domains as
unordered would be a separate policy change. Named logical constants likewise
remain a documented metadata boundary, not an additional confirmed defect.

## 5. Default TRUE results cannot hide different proper content

The completed [independent one-sided proof](../comparison_soundness/PROOF.md)
checks the actual base character/list methods on both runtimes. For proper
ordinary objects in a consistent common universe, a default TRUE result
implies equal symbol-to-value-set maps, and equal clause multisets for
formulas. Every normalization step preserves those contents by permutation;
the final comparison checks all actual names and character leaves. No
collision-free hash or tie-free collation premise is needed. This closes the
opposite comparison-error direction without extrapolating the finite bank.

The guarantee excludes caller options that deliberately suppress names or
attributes, ambiguous native symbol aliases, and custom leaf/sort methods.
It is not a test for arbitrary Boolean equivalence or byte-for-byte storage
identity. The independent controls explicitly exercise these distinctions.
