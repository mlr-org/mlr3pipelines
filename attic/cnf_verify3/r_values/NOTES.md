# Ordinary R values at the finite-domain CNF boundary

This is an independent read-only review of the `09770eaa` CNF sources. All
new files from this stream are in this directory; there are no production
edits or commits. The scripts use real checkmate 2.3.4 under native R 3.6.3
and the existing `r-base` container's R 4.6.1. `bootstrap.R` sources the six
CNF files and supplies only the two ordinary formatting helpers `stopf` and
`map_chr`; no constructor assertion or simplifier is replaced.

The largest finding is a semantic consequence of the matrix-selector issue
previously exercised in `representation/duplicate_selector_search.R` but
not described in `representation/NOTES.md`. I independently evaluated and
greedily reduced its saved examples. This is not a new canonical-input
simplifier counterexample: accepted public subsetting first violates the
unique-symbol invariant.

## Reduced semantic examples

### Four clauses: contradictory input becomes a canonical satisfiable output

This example avoids ambiguity about reading duplicate names in the output:
the wrong output has ordinary unique symbol names.

```r
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("a", "b", "c"))
c1 = as.CnfClause(X %among% c("b", "c"))
c2 = X %among% "b" | Y %among% "c"
c3 = X %among% "c" | Y %among% "b"
c4 = X %among% "a" | Y %among% "a"

c(CnfFormula(list(c1[matrix(c(1L, 1L), nrow = 1L)], c2, c3, c4)))
# list(list(X = "c"), list(Y = "a"))

c(CnfFormula(list(c1[c(1L, 1L)], c2, c3, c4)))
# FALSE
```

The selected first clause is `(X in {b,c}) OR (X in {b,c})`, logically just
`X in {b,c}`. The last clause therefore forces `Y=a`. The second then forces
`X=b`, and the third forces `X=c`: no assignment satisfies the input. The
matrix output nevertheless accepts `X=c,Y=a`. Both positional occurrence
evaluation and name-based output evaluation find this same wrong model.

`reproduce_semantic.R` checks all nine assignments, evaluates selected
original atoms before construction, and compares the matrix with exactly
the same selector flattened to a vector. Both native R 3.6.3 and R 4.6.1
produce the displayed outputs (`semantic_r36.log`, `semantic_r46.log`).

### Two clauses: one repeated range remains stale

The smaller example uses `X in {a,b}` and `Y in {a,b,c}`:

```r
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b"))
Y = CnfSymbol(u, "Y", c("a", "b", "c"))
c1 = X %among% "a" | Y %among% c("b", "c")
c2 = X %among% "a" | Y %among% c("c", "a")

c(CnfFormula(list(c1[2:1], c2[matrix(c(2L, 1L, 2L), nrow = 1L)])))
# list(list(Y = "c", X = "a", Y = c("c", "a")))
```

The input is equivalent to `X=a OR Y=c`. The returned clause, if each stored
occurrence contributes to its disjunction, additionally admits `X=b,Y=a`
through the final stale `Y` range. The first `Y` occurrence alone has been
restricted. Reading `cl[["Y"]]` always returns that first occurrence, so a
name-based evaluator and the printer hide the trailing range and happen to
show the correct logical meaning here. This distinction is why the
four-clause example above is the stronger semantic result.

This is the smallest example found by this stream, not a proof of a global
minimality claim. It is checked on both R versions in the same reproducer.
The vector version has one `Y` entry and preserves all six truth-table rows.

### Cause and documented scope

`[.CnfClause` calls `unclass(i)` at `R/CnfClause.R:212`, which leaves a matrix's
dimensions. The next `unique(floor(i))` or `unique(i)` therefore dispatches to
`unique.matrix`/`unique.array`, removing duplicate rows or slices rather than
all repeated elements. A one-row `matrix(c(1,1), 1)` retains both elements.
The range/name assertions at lines 234–238 admit the result, base list
subsetting at line 240 copies both occurrences, and the method reattaches
the CnfClause class without merging them.

Character matrices and numeric/character arrays have the same issue.
Column matrices happen to remove repeated singleton rows, illustrating that
normalization depends on shape. Numeric or character *vectors* globally
deduplicate correctly. In the simplifier, operations such as
`apply_domain_restriction()` use `match(symbol, names(clause))`
(`R/CnfFormula_simplify.R:148`) and update only the first occurrence; other
tables also rely on unique symbols.

The documentation explicitly permits `[` to return clauses containing the
indicated symbols. It does **not** document matrix/array indices or a
separate `i` argument contract. Accordingly these are ordinary accepted R
inputs at a public boundary, not a claim that matrix support was explicitly
promised. The resulting object still has a promised CnfClause type and
enters CnfFormula without additional representation validation. Flattening
before applying uniqueness, or rejecting dimensions, would close this
specific path; no such change was applied.

## Constructor contract and accepted values

The documentation describes a symbol's domain and an atom's range as sets
of character values. Nonconstant clauses are named lists, and the clause
constructor promises to unify atoms with the same symbol. The unique-name
invariant thus follows from the documented unification. Nonempty unique
character ranges are both the constructor's representation and the explicit
assumption of `char_intersect`/`char_setdiff`/`char_union` in the simplifier.
Logical constant objects are stored separately as classed scalar logicals.

| Input class | Accepted normalization and logical meaning |
| --- | --- |
| Named character symbol name | The scalar's value is the symbol identity; its outer name is metadata. Named and 1x1 matrix names create the same registered symbol identity as plain strings. The clause boundary has one plain symbol name. |
| Named character domain | Stored as supplied, including duplicate, empty or NA *names*. Membership uses values; names never select or rename a domain value. |
| Duplicate domain values | Accepted and stored with multiplicities. As an abstract domain they denote the same set. Constructors test coverage with `%in%`, so multiplicity does not change full/empty-atom classification. |
| Character domain matrices/arrays or ordinary attributes | Accepted and stored as supplied. Atoms and clauses use character membership; final constructor-built clause ranges are flattened. |
| Missing domain values, empty domain, factor/numeric/logical/list domain | Rejected. Empty string **values** are allowed. Empty string **symbol names** fail in environment lookup; ordinary nonempty nonsyntactic names work. |
| Named/repeated/attributed character atom values | Accepted. `unique` removes repeated values and ordinary names/attributes; clause unification applies another flat `unique(c(...))`. |
| Character matrix/array atom values | Accepted. Intermediate `unique.matrix`/`unique.array` can retain repeated elements and dimensions, but `CnfClause` flattens and deduplicates them before a formula is built. Their set-membership meaning stays the same. |
| Nonempty factor/numeric/logical/raw/complex/list atom values | Rejected by the real `assert_subset` against a character domain. |
| Empty values, including empty noncharacter vectors, list(), and NULL | Accepted by `assert_subset`; immediately converted to a FALSE atom. They never create a noncharacter proper range. |
| Names on lists of atoms/clauses | Ignored for meaning. Symbols come from each atom; outer list names neither select values nor reorder the input. |

These conclusions concern ordinary classes without user-defined S3 methods
or mutation of a universe. Arbitrary byte-marked character mixtures remain
the explicitly separate limitation recorded in the representation stream.

## Clause selectors

For a proper two-symbol clause, independently checked behavior is:

- `[]` preserves the clause. `NULL`, empty atomic vectors, zero, and plain
  scalar FALSE return a FALSE clause. Missing argument and NA value differ.
- Numeric vectors are floored and globally deduplicated, then checked in
  `[0,length(clause)]`. Zeros are ignored. Negative, infinite, missing,
  out-of-range values are rejected.
- Character vectors are globally deduplicated and must contain existing
  symbols. Character NA and unknown symbols are rejected. A selector's own
  names are ignored: `cl[c(X="Y")]` selects Y.
- A logical selector must have the exact clause length, apart from the
  special scalar-FALSE path. There is no ordinary scalar-TRUE recycling for
  a multi-symbol clause. A nonmissing logical matrix of the right length
  selects positions once and preserves the representation.
- Logical NA is admitted because `check_logical(i, len=...)` does not set
  `any.missing=FALSE`. `cl[c(FALSE,NA)]` contains an NA symbol name and a NULL
  range. `CnfFormula(list(bad))` accepts it. `CnfClause(as.list(bad))` becomes
  TRUE by vacuous coverage of an absent symbol's NULL domain. An NA symbol
  has no ordinary finite-domain Boolean meaning; this is a malformed-input
  acceptance defect, not an independently specified intended truth table.
- Dimensional numeric/character selectors can create duplicate symbol
  entries as described above. Factor selectors are unclassed and select by
  integer codes; Date selectors select by their underlying floored numbers.
- Some attribute-sensitive constant shortcuts are inconsistent: named FALSE
  does not take the plain FALSE shortcut, and FALSE-clause `[FALSE]` errors.
  These are errors/normalization differences, not wrong nonconstant ranges.

An exhaustive check of numeric values `{0,1,2,NA}`, character values
`{X,Y,NA}`, and logical values `{FALSE,TRUE,NA}` at lengths 1–4 compared flat
vectors with one-row matrices. On both R versions:

| Shape | Accepted calls | Noncanonical outputs |
| --- | ---: | ---: |
| Vector | 160 | 5, all logical-NA selectors |
| One-row matrix | 161 | 109, duplicate names or logical NA |

Every accepted canonical output matched the independent positional
disjunction of the selected original ranges. Directed shape/attribute tests
supplement this finite enumeration. `normalization_r*.tsv` contains the
complete individual classification and observed error messages.

## Logical constants and constructor composition

The three `as.Cnf*` logical conversions accept nonmissing scalar logicals,
including names, dimensions, and ordinary attributes; they reject NA,
length-zero and length-greater-than-one flags. Extra attributes may remain,
but their Boolean payload stays intact.

`logical_operands.R` checked 2,322 ordered `&`/`|` combinations involving
proper atom/clause/formula operands and TRUE/FALSE variants (plain, named,
matrix and ordinary attributed). On R 4.6.1: zero wrong truth tables, zero
errors, zero warnings. There were 20 return-class failures, all variants of
the already documented `TRUE | CnfClause` short-circuit that returns the raw
left operand. The wrong class subsequently fails a constructor's element
assertion even though its Boolean value is correct.

Raw logicals inside the list constructors are rejected by their documented
element types; callers need the relevant CNF wrappers. The previously known
universe-inference issues also reproduce: a universe-free FALSE atom can be
rejected on either side of a proper atom in CnfClause, and a universe-free
TRUE clause before a proper clause is rejected by CnfFormula, although the
reverse order succeeds. Separately, a proper nested formula followed by a
FALSE clause/formula enters the previously archived malformed-flattening
path and errors; FALSE first succeeds. These pre-existing issues qualify
any claim that *all* accepted constructor lists establish canonical input.

## Duplicate-domain evidence and limits

`duplicate_domains.R` independently evaluates a plain constructor-built
formula and three variants: repeated values, repeated/named values, and
repeated matrix values in both domains and atom ranges. Every variant uses
the real constructors. Native R checked 2,000 generated cases / 8,000
formulas / 575,216 assignment rows; R 4.6.1 checked the first 1,000 cases /
4,000 formulas / 289,224 rows. There were no semantic failures, no final
range/name invariant failures, and no structural differences after sorting
the values inside ranges.

The implementation explanation is consistent with the representation
stream's argument: stored domain multiplicity is not consulted in early
simplifier phases; the original universe occurs only in the two virtual HLA
extension loops. A virtual range contains no more copies of a value than
the stored domain. Missing any value therefore makes its length strictly
less than the stored domain length, excluding false-positive full-coverage
tests. Full control-flow invariance additionally needs the live-donor
bookkeeping premise that an exceptional donor value remains absent after
extension; the multiset observation alone does not prove that premise or
exclude all possible false negatives. This stream's finite test results are
supporting evidence, not a replacement for that separate invariant proof.

## Reproduction

From the repository root, native R commands are:

```sh
Rscript attic/cnf_verify3/r_values/reproduce_semantic.R
Rscript attic/cnf_verify3/r_values/normalization.R
Rscript attic/cnf_verify3/r_values/duplicate_domains.R
```

The current-R runs mount the repository at `/repo`, set
`CNF_REP_RLIB=/repo/attic/cnf_verify3/representation/r46-library`, and run the
same scripts with `docker.io/library/r-base:latest` under podman. The
operator audit intentionally requires R >=4.3 so mixed-class S3 operator
dispatch uses the supported `chooseOpsMethod` mechanism.

`minimize_selector.R` and `minimized_both.rds` / `minimized_positional.rds`
preserve the reduction of the representation stream's four saved
name-based mismatches. It removes clauses, selector occurrences, symbol
occurrences, values, and domain values while re-evaluating every candidate
from the selected original atoms. All surviving selectors still select
every original symbol at least once. The small public reproducers are
self-contained apart from `bootstrap.R`; they do not depend on those older
saved random cases.
