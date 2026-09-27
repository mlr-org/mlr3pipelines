# Representation stream: findings and exclusions

Live notes for campaign 3, initially focused on ordinary accepted R objects
that can reach the finite-domain CNF simplifier. No production source was
edited. The source baseline is the campaign's `09770eaa` revision on
`cnf-verify2`.

## New reproducible public-boundary issues

### R1. `TRUE | CnfClause` can return a bare logical

For a nonconstant clause `cl`, `TRUE | cl` returns plain logical TRUE, whereas
`cl | TRUE` returns a CnfClause TRUE. The reverse-operand path enters
`|.CnfClause`; its second short-circuit returns `e1` without converting it.
That method can be selected when the CnfClause is the right operand, so e1
need not have the CnfClause class.

```
u = CnfUniverse()
x = CnfSymbol(u, "X", c("a", "b"))
cl = as.CnfClause(x %among% "a")
class(TRUE | cl)                  # "logical"
class(cl | TRUE)                  # "CnfClause"
CnfFormula(list(TRUE | cl))       # element-type assertion error
CnfFormula(list(cl | TRUE))       # succeeds
```

The same class loss occurs when the right operand is a FALSE CnfClause. A
TRUE CnfClause happens to follow the first short-circuit and retains its
class. `TRUE | CnfAtom` already returns CnfClause correctly. Boolean truth is
preserved; this is a return-class/composition defect, not a core logical
simplification error. It is distinct from the four API issues in the earlier
campaign's `CLAUDE.md`.

`operator_contracts.R` exhausts 578 ordered binary combinations of seventeen
representative operands: raw logical TRUE/FALSE, each CNF class's TRUE/FALSE
with/without universe, and proper atom/clause/formula. On R 4.6.1 it found
exactly the three expected class failures (proper clause, FALSE clause with
universe, FALSE clause without universe), zero truth-table failures, zero
warnings, and zero operator errors. Earlier `exp12_dispatch_r43.R` tested
truth tables but did not assert result classes, explaining why it missed
this defect even though `TRUE | clause` was among its operands.

The direct reproducer also passes on R 3.6.3. Both versions use real
checkmate 2.3.4, not assertion shims. The minimum local change would be to
coerce the second short-circuit return in `|.CnfClause`, matching the existing
CnfAtom method; this investigation does not apply that change.

### R2. Logical NA selectors create malformed clauses that reach simplify_cnf

`[.CnfClause` rejects missing numeric/character indices, but its logical check
is `check_logical(i, len = true_length)`. Checkmate allows missing values by
default. For a two-symbol clause:

```
bad = cl[c(FALSE, NA)]
```

returns a one-element object with class CnfClause, an NA symbol name, and a
NULL range. Base list subsetting introduced that missing entry, and the method
reapplied the CNF class without rejecting it. `CnfFormula(list(bad))` then
accepts the object and returns a malformed nonconstant formula containing the
same NA/NULL entry. This violates the simplifier's required invariants:
registered symbol names and nonempty unique character ranges.

The conversion round trip `CnfClause(as.list(bad))` becomes TRUE: the missing
symbol's universe lookup is NULL, and the clause constructor's coverage test
is vacuously true. This demonstrates that these accepted public objects do
not have stable representation semantics. A logical NA selector does not
have an obvious intended Boolean clause meaning, so the appropriate boundary
behavior is to reject it; no claim is made that the core simplifier was given
a canonical formula and changed its truth table.

`api_boundaries.R` reproduces the malformed structure, acceptance by
CnfFormula, conversion to TRUE on the round trip, and rejection of the
analogous numeric NA selector. Confirmed on R 3.6.3 and R 4.6.1 with checkmate
2.3.4. No earlier note or selector experiment found in the attic exercised
logical NA indices. The minimum local change is `any.missing = FALSE` on the
logical alternative.

### R3. Byte-marked strings are accepted but not uniformly operable

This is a lower-priority representation limitation, separate from canonical
finite-set logic. Let `b` be the character scalar made from byte 255 with
`Encoding(b) = "bytes"`.

- `CnfSymbol(u, "X", c("a", b))` and `CnfAtom(X, b)` succeed. The atom converts
  to a formula and ordinary Boolean operations work. But `print(atom)` errors
  with `translating strings with "bytes" encoding is not allowed` because
  sprintf combines the byte-marked value with a Unicode membership sign.
- `CnfSymbol(u, "X", c(enc2utf8("\u00e9"), b))` succeeds, but a valid singleton
  atom on its Unicode value errors in ordinary `%in%` processing. The accepted
  domain does not supply total membership operations.

Both outcomes occur on R 3.6.3 and R 4.6.1. The exact reproduction is in
`api_boundaries.R`. These inputs establish an explicit boundary for the
membership-pattern proof: arbitrary accepted R character representations do
not automatically have the total, consistent equality assumed by abstract
set algebra. Ordinary valid UTF-8/Latin-1 strings without byte-marked mixtures
are a different case. There is no evidence here of a wrong Boolean output
from the canonical simplifier.

## Prior findings deliberately not relabeled as new

The four archived issues remain known: TRUE-clause `as.list` dispatch/shape;
missing universe `[[` validation; neutral first constants choosing the wrong
universe; FALSE formula corruption after flattening previous formulas.
`attic/cnf/review_5_2_pro.md` additionally already notes that `as.list.CnfClause`
returns an unnamed list despite its documented named-list behavior. The
current script reproduces that mismatch but does not count it as a discovery.

Clause constructors actually compare universes before discarding logical
constants. Consequently a universe-free FALSE atom can be rejected either
before or after a proper atom in the same constructor. This is the archived
neutral-constant/universe-inference family, not a new core issue.

## Proof progress

The detailed independent review is `PROOF_REVIEW.md`. It validates the
membership-pattern reduction to at most eight value classes per symbol for
three input clauses, the 255 x 255 x 8 enumeration, all length-sensitive
branches, unused domain values, repeated clauses, ordered clauses, arbitrary
symbol orders, constant clauses, and padding formulas with fewer clauses.
It records assumptions precisely and separates semantic preservation from
simplification completeness and constructor correctness.

Independent AST instrumentation (`trace_refinement.R`) checks the stronger
control-flow claim by wrapping every `if` predicate without changing its
value or evaluation point. Results:

| Transformation | Trials | Matching branch decisions | Discrepancies |
| --- | --- | --- | --- |
| Replace each value with 1–9 fresh labels; shuffle domains/ranges | 3,000 | 852,862 | 0 |
| Repeat existing domain/range labels 1–9 times; shuffle them | 3,000 | 852,862 | 0 |

There are 125 instrumented conditional sites across constructors and the
simplifier. This is an empirical check of the implementation coupling,
complementing the mathematical reduction, not a claim that random testing
proves every branch invariant.

### Duplicate domains: a previously open concern can be narrowed rigorously

Only the two HLA loops inspect the original universe's domain contents in
`simplify_cnf`; earlier phases use the unique actual clause ranges. Repeated
domain values can only duplicate values in virtual HLA ranges. They cannot
make an HTE length-equality test falsely report full coverage:

1. Initially a virtual range is unique and is a submultiset of the stored
   domain.
2. Every HLA addition takes only values absent from the old virtual range and
   copies at most each value's multiplicity in the stored domain.
3. The virtual range remains a submultiset of the stored domain. If any value
   is missing, its length is strictly smaller than the stored domain's length.

If the selected donor's exceptional range is genuinely not a subset of the
current virtual range, a value in their difference stays absent after the
extension. Therefore the full-domain branch remains unreachable even with
duplicated domain storage. Under that HLA bookkeeping invariant, duplicate
multiplicity changes no control decision anywhere in the simplifier; the
second trace experiment corroborates this stronger conditional statement.

The distinction matters: without the bookkeeping invariant, duplicates could
in principle suppress a true full-coverage length match because old virtual
values may have fewer copies than the domain. The submultiset argument alone
rules out false positives, not every possible false negative. The actual
HTE-unreachability proof closes the remaining case for correct bookkeeping.
This makes the old note that duplicates might create a false-positive HTE
threshold substantially less concerning.

## Other representation checks and exclusions

- Real checkmate `assert_subset` rejects nonempty numeric, integer, logical,
  factor, list, raw, and complex values against character domains. Empty
  vectors of other types are accepted but become FALSE atoms immediately;
  they do not leak non-character ranges to the simplifier.
- Character matrices and ordinary attribute-bearing character vectors are
  accepted. `CnfClause` flattens each atom range with `c(...)` and applies
  `unique`, so its actual ranges are plain unique character sets. A matrix's
  `unique.matrix` method can leave duplicate *elements* in an intermediate
  atom, but the clause boundary repairs this before simplify_cnf. This may
  affect structural equality of intermediate atoms, not their membership
  truth or the core range invariant.
- Domain names, atom-list names, and formula-list names do not select values
  or reorder logical clauses through the ordinary constructors. Names on a
  domain's values are ignored by membership. Symbol order inside clauses,
  in contrast, is an actual scheduling choice and is included in the
  exhaustive coverage rather than assumed irrelevant.
- The documented universe is by reference, and its documentation explicitly
  excludes modifying/deleting symbols. Arbitrary mutation of an environment
  can invalidate prior constants; it is outside the fixed-domain semantics
  and should not be presented as a simplifier defect. Adding a new unrelated
  symbol is harmless: the simplifier never enumerates the entire universe,
  and only queries domains of symbols occurring in the formula.

## Reproduction files

- `api_boundaries.R`, `api_boundaries_r36.log`, `api_boundaries_r46.log`:
  exact public-path outcomes with real validators.
- `operator_contracts.R`, `operator_contracts_r46.log`,
  `operator_contracts_results.rds`: complete representative pairwise class
  and Boolean contract check.
- `trace_refinement.R`, `trace_refinement.log`, `trace_duplicates.log`,
  `trace_*_results.rds`: the independent branch trace experiment.
- `r46-library/` is an ignored local installation of checkmate/backports used
  only by the current-R container. It is not a proposed repository artifact.

No production changes or commits were made by this stream. These notes remain
open for subsequent rounds and any challenges from the other streams.
