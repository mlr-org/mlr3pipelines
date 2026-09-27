# Small formulas made through accepted clause selectors

This is an independent audit of the unchanged CNF sources at repository HEAD
`00f5c7ab0979d2309e165b81e0ff57f40b7ba50e`. `SOURCE_HASHES.sha256` records the
actual six files. No production source or production test was edited, and
this stream made no commits. All constructions use real `checkmate` 2.3.4,
the ordinary CNF constructors, and a one-row numeric matrix selector.
There is no forged `CnfClause` representation or replacement simplifier.

The search found no wrong **canonical** result with at most three input
clauses. It did find two-clause stale duplicate ranges and a two-clause
runtime error. The known four-clause example remains the smallest canonical
semantic failure found; this is **not** a proof of global minimality. The
exact exclusions proved by finite enumeration plus a domain argument are
stated below with their symbol and occurrence bounds intact.

## Meaning and outcome categories

Each selected occurrence is evaluated as one disjunct, at its integer list
position. A repeated symbol initially has identical range copies because
the constructor merges its atoms before selection. The input oracle uses
the selected original raw ranges, before calling `CnfFormula`; it separately
checks that the public selected clauses have those same truth tables.

The output is evaluated twice: positionally, and using just the first
occurrence of each symbol, as the current named lookup/printing paths do.
The latter is a diagnostic projection, not a replacement for the positional
oracle. The classification is:

- `canonical_correct`: unique proper symbol ranges and the correct table.
- `noncanonical_correct`: duplicate names, both tables still correct.
- `noncanonical_stale`: positional table wrong, first-occurrence table
  correct; an older trailing range still contributes to the disjunction.
- `noncanonical_named_wrong`: positional table correct, projection wrong.
- `noncanonical_both_wrong`: both tables wrong.
- `canonical_wrong`: unique proper ranges, incorrect table.
- `error`: the clause selectors succeeded, but `CnfFormula` raised an error.

No member of the last two noncanonical mismatch categories was found in
these searches. The preserved four-clause control is `canonical_wrong`.
All `RDS` files and printed `dput` records retain ordered occurrences and
duplicate names. No clause is serialized as a symbol-keyed JSON object.

## Minimal two-clause runtime error

```r
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
a = as.CnfClause(X %among% "a")
b = as.CnfClause(X %among% c("a", "b"))
CnfFormula(list(a[matrix(c(1L, 1L), nrow = 1L)], b))
# Error in clause[[symbol]]:
#   attempt to select less than one element in get1index
```

The intended conjunction is simply `X=a`. Initial unit propagation leaves
both copies of the narrower range `a` unchanged. The physical length-two
clause is then treated as a possible donor for unit HLA. At
`R/CnfFormula_simplify.R:738`, the count is `2 - 1 = 1`: the registry supplies
only the Boolean fact that the clause contains X. At line 750 the actual
mask is `c(FALSE, FALSE)` because both names are X, so line 753 obtains
`character(0)` and line 754 evaluates `clause[[character(0)]]`.

The flat selector `a[c(1L,1L)]` deduplicates to a physical unit and returns
the correct canonical `X=a`. `reproduce.R` captures the error stack and
checks both versions. R 3.6.3 and R 4.6.1 produce the same error. A triple
copy is different: the unit-HLA count is two, so this particular call is
skipped. This is a runtime category, not an incorrect satisfying formula.

## Two-clause stale range control

```r
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b"))
Y = CnfSymbol(u, "Y", c("a", "b", "c"))
a = X %among% "a" | Y %among% c("b", "c")
b = X %among% "a" | Y %among% c("c", "a")
c(CnfFormula(list(a[2:1], b[matrix(c(2L,1L,2L), nrow = 1L)])))
# list(list(Y = "c", X = "a", Y = c("c", "a")))
```

The input is `X=a OR Y=c`. Positional evaluation of the result also admits
`X=b,Y=a`; its first-occurrence projection remains correct. This independently
reproduces the earlier `r_values` finding. It cannot support a claim of a
wrong canonical output, because the returned clause still has duplicate Y
names. Flattening the same selector gives a correct canonical result.

For comparison, `reproduce.R` also reconstructs the known four-clause
contradiction, obtains canonical `X=c AND Y=a`, checks all nine assignments,
and verifies that the vector selector returns FALSE. All controls pass on
both R versions.

## Systematic search results

Every count below is a number of calls to the actual `CnfFormula`, including
calls that error. Clause order and occurrence order are retained. Repeated
input clauses are included. `noncanonical_named_wrong`,
`noncanonical_both_wrong`, and `canonical_wrong` were zero throughout the
search, apart from the separately run four-clause positive control.

| Family | Calls | Canonical correct | Duplicate correct | Stale duplicates | Errors |
| --- | ---: | ---: | ---: | ---: | ---: |
| Two Boolean symbols, 1 clause, at most 3 occurrences | 44 | 12 | 32 | 0 | 0 |
| Same bank, 2 ordered clauses | 1,936 | 512 | 1,424 | 0 | 0 |
| Same bank, 3 ordered clauses | 85,184 | 25,016 | 60,168 | 0 | 0 |
| 2 clauses, at most 2 symbols, all membership profiles, at most 4 occurrences/clause | 26,324 | 917 | 25,353 | 48 | 6 |
| 3 clauses of shape `XX`, `XY`, `XY`, all membership profiles/orders | 16,212 | 440 | 14,028 | 800 | 944 |
| 3 clauses, two ternary symbols, exactly one repeated-selector clause, at most 3 occurrences | 225,792 | 91,592 | 116,028 | 6,432 | 11,740 |
| Seeded 3-clause random cases | 100,000 | 8,673 | 89,730 | 89 | 1,508 |

The membership-profile rows were independently rerun under R 4.6.1 and had
exactly the same category counts. Their `_r46.rds` files preserve separate
version metadata. The other full enumerations used R 3.6.3. Operator dispatch
does not affect them because they call the explicit constructors.

Details of the finite spaces:

- `enumerate_boolean.R` builds all 44 distinct selected proper clauses on
  X,Y with two values each, using every occurrence word of length at most
  three. It checks the entire ordered Cartesian power for one, two, and
  three clauses. Thus it covers all repetitions/permutations within those
  bounds, rather than just one duplication pattern.
- `enumerate_pair_profiles.R` uses the seven possible nonempty membership
  cell sets for two present, proper ranges, plus the two profiles where
  exactly one range is absent. It enumerates one and two symbols. Every
  retained input clause is nonempty, and every occurrence word through
  length four is included: four words on a one-symbol support and 22 words
  on a two-symbol support. There are 112 one-symbol calls and 26,212
  two-symbol calls.
- `enumerate_three_pseudounit.R` uses all 193 nonempty membership cell sets
  of three proper X ranges and all seven cell sets of two proper Y ranges.
  There are two choices for each binary clause's symbol order and three
  positions for XX, hence `193 * 7 * 2 * 2 * 3 = 16,212` calls. The profile
  space already includes both orders of the two ordinary binary clauses.
- `search_three_ternary.R` has 84 ordinary clauses and 28 repeated-selector
  anchors after independent value relabeling and X/Y relabeling. The anchor
  range of size k is sent to the first k domain values. Length-two anchors
  are tried in all three positions; length-three anchors always sort after
  the ordinary clauses, so one position suffices. All choices of the two
  ordinary clauses remain ordered. This covers the stated family exactly.
- `search_three_random.R`, seed 26090637, uses three clauses, 1–5 symbols,
  domain sizes 2–5, supports through four symbols, and up to three extra
  selected occurrences per clause. It supplies broader evidence, not an
  exhaustive exclusion.

## Exact exclusions and why finite domains suffice

**One proper clause.** For any number of symbols, domain values, or repeated
occurrences, a single proper selected clause is returned unchanged by the
simplifier (apart from its enclosing formula class). A physical unit takes
the unit-only return. A physical nonunit has no other clauses for pairwise
processing, second-order resolution, or HLA. Therefore one clause cannot
produce any of the semantic mismatch categories or this runtime error.
This statement concerns nonmissing selectors with an ordinary finite-domain
meaning; it does not assign semantics to the accepted logical-NA defect.
Consequently two is the minimum number of clauses for both the exhibited
stale positional output and the exhibited runtime error in this class.

**Two clauses, at most two symbols, at most four occurrences per clause.**
There is no wrong canonical output and no wrong first-occurrence projection,
for arbitrary finite domains of distinct ordinary character values. Errors
and stale positional output remain possible, exactly as the table shows.

**Three clauses of shape `XX`, `XY`, `XY`.** The same exclusion holds for
arbitrary finite domains, all nonempty proper input ranges, every input
clause order, and either symbol order in each ordinary XY clause. Here XX
means exactly two copies of one range, obtained by selection; neither XY
clause has a duplicated name.

The last two claims need more than testing a few domain cardinalities.
Fix a symbol and its k original input ranges. Partition its domain by the
k-bit vector indicating membership in those ranges. Keep one representative
of each nonempty cell, at most `2^k` representatives. A symbol absent from a
clause has an empty range in that coordinate. The enumerators explicitly
enumerate every possible present cell set with the required proper ranges.

This collapse preserves the **actual implementation's control flow**, even
with duplicated names, for these ordinary unique-valued domains:

1. Each initial occurrence is a union of cells. Intersection, union,
   difference, and complement preserve this property at every later step,
   separately for each occurrence, including stale copies.
2. All inclusion/disjointness/emptiness tests are therefore unchanged.
   The range-length tests in this implementation also reduce to those set
   relations: intersection length equals the original length iff nothing
   was removed; intersection length equals the restringent length iff it
   covers the restringent; subset plus equal lengths means equality; the
   merged unit range is a subset of the incoming unit range; and a virtual
   range has full domain length iff it covers the domain. Every individual
   range is unique-valued throughout these operations. There is no naked
   comparison of cardinalities of two unrelated ranges.
3. Clause widths, duplicate symbol positions, name matching, registry
   entries, cache dimensions/counters, stable clause ordering, and the
   symbol-error paths are unchanged because the collapse changes values,
   not occurrence positions. Inside-range value order does not drive any
   loop or branch in this simplifier.

Thus the quotient executes the same calls, removes the same occurrences
and clauses, and has the same final ranges interpreted as unions of cells,
or the same runtime error. The quotient truth table covers every possible
assignment-cell combination of the original domain. This proves the stated
finite-to-arbitrary-domain extension within the enumerated structural
bounds. It does not remove those bounds.

In particular, these observations do **not** prove all-size Boolean safety.
Even when singleton ranges cannot shrink nonemptily, deleting the first
copy can leave another copy while clearing a cache column and removing all
registrations of that clause for the symbol. The clean 44-clause Boolean
bank addresses only its explicitly enumerated one-/two-/three-clause powers.
Nor is there a claim that every three-clause selector formula is safe, or
that four clauses is a globally minimal canonical semantic failure.

The domain argument assumes ordinary unique-valued finite domains. Duplicate
domain entries, byte-marked character mixtures, logical-NA selectors, class
spoofing, custom S3 methods, mutation of the universe, nested formula-list
constructor defects, and arbitrarily many symbols/occurrences are outside
the asserted exhaustive exclusions.

## Reproduction and verification

From the repository root:

```sh
Rscript attic/cnf_verify3/small_selectors/reproduce.R
Rscript attic/cnf_verify3/small_selectors/enumerate_boolean.R
Rscript attic/cnf_verify3/small_selectors/enumerate_pair_profiles.R
Rscript attic/cnf_verify3/small_selectors/enumerate_three_pseudounit.R
Rscript attic/cnf_verify3/small_selectors/search_three_ternary.R
Rscript attic/cnf_verify3/small_selectors/search_three_random.R
bash attic/cnf_verify3/review_semantics/run_r46.sh \
  attic/cnf_verify3/small_selectors/test_audit.R
```

The isolated R 4.6.1 launcher and environment are documented in
`../review_semantics/R46_ENVIRONMENT.md`. To keep separate current-R results:

```sh
podman exec -e CNF_SMALL_SUFFIX=_r46 cnf-review-r46 Rscript \
  attic/cnf_verify3/small_selectors/enumerate_pair_profiles.R
podman exec -e CNF_SMALL_SUFFIX=_r46 cnf-review-r46 Rscript \
  attic/cnf_verify3/small_selectors/enumerate_three_pseudounit.R
```

`test_audit.R` passed 125 focused `testthat` expectations under R 4.6.1. It
tests positional truth evaluation with deliberately unequal duplicate
ranges, RDS round trips, exact occurrence-word counts, the representative
outcome categories, and 109 nonuniform cell-splitting controls. Those controls
compare the full ordered positional output after projecting split labels
back to the original cells, including the saved error outcomes; they do not
merely compare first-occurrence maps. They support the explicit source
argument above but are not used as its substitute.

An initial profile generator incorrectly used `paste0("v", integer(0))`,
which produces `"v"` in R and was rejected by the real constructor. This was
fixed before any accepted counts were recorded by subsetting the generated
labels instead. No production failure is attributed to that harness error.
