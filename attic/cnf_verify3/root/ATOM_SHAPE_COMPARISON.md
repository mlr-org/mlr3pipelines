# Accepted dimensional atoms expose another comparison normalization boundary

Found by root, 2026-09-06, by crossing the completed constructor-storage and
comparison studies. Both native R 3.6.3 and R 4.6.1 reproduce with unchanged
public constructors, real checkmate, one universe, and ASCII names and values.
The [independent review](../atom_shape_review/REVIEW.md) confirms the separate
multiplicity cause and checks larger matrix/array shapes and retained guards.

```r
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
a = CnfAtom(X, matrix(c("a", "a"), nrow = 1L))
b = CnfAtom(X, "a")
all.equal(a, b)
# Component "values": Lengths (2, 1) differ (string compare on first 1)
all.equal(as.CnfClause(a), as.CnfClause(b))
# TRUE
```

Both atoms select precisely the singleton set `{a}`. The matrix is accepted
by the ordinary subset assertion. At `R/CnfAtom.R:78`, `unique(values)`
dispatches to `unique.matrix`: one row with two columns remains one row with
two scalar copies. The resulting atom has a meaningful proper range but its
physical range is not scalar-unique. At `R/CnfAtom.R:180`, the comparison's
`sort()` flattens the range and preserves both copies; list comparison then
rejects their differing multiplicity. Clause conversion explicitly flattens
and deduplicates, which explains its successful comparison and the absence
of a kernel truth failure from this path.

This is an accepted-shape comparison boundary. Matrix-valued atom inputs are
accepted, although their layout is not explicitly promised by the API's
character-value documentation. The issue does not depend on locale collation,
encoding marks, digest ordering, attributes of a symbol handle, named logical
constants, or a different simplified formula normal form. It crosses the
constructor's row uniqueness with the comparator's scalar-set assumption.

[`atom_shape_comparison.R`](atom_shape_comparison.R) checks every sequence over
`{a,b}` of lengths one through five in four shapes: a vector, one-row matrix,
one-column matrix, and a `1 x n x 1` array. The reference support uses a
separate first-occurrence scalar loop. Every original and returned atom is
evaluated on all three assignments before comparison is interpreted.

Each R version passes the same **248 public pairs and 744 positional truth
rows**, with **116 production comparison false negatives**. All 248 clause
round trips compare correctly. All 248 controls selecting the distinct value
`c` compare unequal. Vectors and one-column matrices normalize fully at the
constructor; row matrices and arrays retain the repeated scalar cases.

A private copy retains the complete original comparison guards and replaces
only its two value-sorting expressions by

```r
sort(unique(enc2utf8(as.vector(values))), method = "radix")
```

It passes every positive and unequal control. Flattening before scalar
uniqueness resolves this boundary; UTF-8 conversion and radix ordering retain
the earlier encoding/collation proposal. The previous payload-only UTF-8/radix
candidate did not promise scalar deduplication and does not by itself close
this broader atom-storage class. Universe comparison and metadata policies
remain separate, already documented boundaries. No production correction is
installed by this experiment.

The saved `atom_shape_comparison_r36.json`, `_r46.json`, logs and RDS files
record every case and the first reduced shape examples. Reproduce using
`Rscript attic/cnf_verify3/root/atom_shape_comparison.R` or the existing
current-R launcher. The independent reviewer confirms seven shape pairs,
including `2 x 3`, `2 x 2 x 2`, and partially deduplicated `3 x 2 x 2` inputs.
Its candidate rejects 56 unequal support/symbol directions and preserves
eight logical/type guard outcomes on both R versions. The earlier UTF-8/radix
candidate and flattening without scalar uniqueness both retain the failure.
