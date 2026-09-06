# Dimensional atoms and set comparison

Independent review, 2026-09-06. Both R 3.6.3 and R 4.6.1 confirm the root's
minimal public-constructor example, using real checkmate 2.3.4 and only ASCII:

```r
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
left = CnfAtom(X, matrix(c("a", "a"), nrow = 1L))
right = CnfAtom(X, "a")
all.equal(left, right)
# Component “values”: Lengths (2, 1) differ (string compare on first 1)
```

Both atoms select exactly `a`; direct scalar membership agrees on all three
domain values. The failure also occurs with `LC_COLLATE = "C"`.

This is a **further accepted-input comparison boundary with a distinct cause**,
within the general value-set normalization problem. It is not another instance
of either collation ties or encoding-dependent digest ordering. The dimensional
constructor acceptance was already known; the newly confirmed consequence is
that these accepted atoms violate the comparator's value-set interpretation.
The earlier comparison argument assumed duplicate-free value sets. These
retained scalar duplicates extend its accepted-input coverage rather than
refuting that conditional argument.

The constructor's `assert_subset()` accepts the shaped character values.
`unique(values)` at `R/CnfAtom.R:78` dispatches on the retained shape:
matrix/array uniqueness removes rows or first-dimension slices, rather than
duplicate scalar members. The comparator's `sort()` at lines 180–181 drops
shape but retains scalar multiplicity. It therefore compares the sorted
multisets `c("a", "a")` and `"a"`, not their supports.

The earlier private UTF-8/radix normalizer still fails this case. Explicitly
flattening before that normalizer, without deduplication, also still fails.
This isolates the extra requirement: scalar deduplication after flattening.

The independent controls include the following nontrivial shapes:

| Input shape | Stored shape after constructor `unique()` | Stored scalar count | Scalar support size | Production comparison to flat support |
| --- | --- | ---: | ---: | --- |
| 1 × 2 row, `a,a` | 1 × 2 | 2 | 1 | unequal |
| 2 × 3 matrix with distinct rows | 2 × 3 | 6 | 2 | unequal |
| 2 × 2 × 2 array with distinct slices | 2 × 2 × 2 | 8 | 2 | unequal |
| 3 × 2 × 2 array with duplicate slices | 2 × 2 × 2 | 8 | 2 | unequal |

The last input is `array(rep(c("a", "a", "b"), 4L), c(3L, 2L, 2L))`.
It shows that array-level duplicate removal can occur successfully and still
leave scalar duplicates. The two matrix rows in the second input are
`c("a", "a", "b")` and `c("b", "a", "a")`.

Three positive controls already compare correctly in production: a duplicate
column vector represented as a matrix, a duplicate ordinary vector, and a
row matrix with distinct values. In all seven shape cases, public conversion
to `CnfClause` and `CnfFormula` compares equal to the corresponding reference.
`CnfClause()` flattens with `c()` before its own `unique()`, so this atom
comparison failure does not persist through those normal public conversions.
No changed simplifier truth was observed or inferred.

The root's private candidate

```r
function(x) sort(unique(enc2utf8(as.vector(x))), method = "radix")
```

has the right ordinary-character set normalization for this boundary.
`as.vector()` exposes scalar entries before uniqueness, `enc2utf8()` gives the
earlier supported encoding normalization, `unique()` removes scalar repetitions,
and radix sort orders the resulting distinct values. This does not merge
different code-point sequences through NFC/NFD normalization.

The independent script constructs a private copy by replacing only the two
proper-value assignments in `all.equal.CnfAtom`. Its formals and final
`all.equal.list(target, current, ...)` expression are asserted unchanged.
The original logical/type guards remain in place, and the registered production
method remains callable. It also extracts only the root script's normalizer
assignment, without running that script's output-writing bank, and confirms
agreement with the independently constructed candidate on all seven shapes.

Per runtime the candidate passes all seven equal pairs in forward, reverse,
and reflexive comparisons. It rejects 42 directions with different selected
supports, including partially overlapping supports, and 14 directions with a
different symbol in the same universe. Eight logical/type guard cases match
production exactly, including constructor-created full/empty shaped inputs,
proper versus logical, and wrong current classes. The normalizer is a candidate
for ordinary finite character shapes; this review makes no broader claim about
custom character subclasses, invalid encodings, or unrelated metadata policies.

Production, the prior UTF-8/radix candidate, and flattening without uniqueness
each have four false negatives among the seven shape pairs. The revised
candidate has none. Both runtime reports retain the exact input/stored
dimensions, scalar values, mismatch messages, package versions, and source
hashes. Lossless R objects are saved alongside JSON in `results_r36.rds` and
`results_r46.rds`.

Reproduce from the repository root:

```sh
Rscript attic/cnf_verify3/atom_shape_review/reproduce.R
bash attic/cnf_verify3/review_semantics/run_r46.sh attic/cnf_verify3/atom_shape_review/reproduce.R
```

Only this review directory was written. No production changes or commits were
made; the completed `wide_three_review/` files were left untouched.
