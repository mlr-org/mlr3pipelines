# Two ordinary-string failures in CNF all.equal normalization

Found by root, 2026-09-06, after the canonical simplifier and indexing proofs
had completed independent review. Both cases use unchanged public constructors
and ordinary supported strings. They concern the representation comparison
helpers, not changed formula truth or a kernel simplification inference.
Independent review of these new examples is pending.

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
Any eventual repair must keep unequal finite sets and differently associated
symbol ranges unequal; the next review should include those negative controls.

The digest is only an ordering aid. With proper ordinary list comparison, a
digest coincidence alone cannot make different clause payloads compare equal:
the final `all.equal.list` still compares the actual content. Its possible
effect is a failure to align equal unordered clause collections. Likewise,
the current collation issue produces incorrect differences, not a loss of
the simplifier's already proved truth preservation.
