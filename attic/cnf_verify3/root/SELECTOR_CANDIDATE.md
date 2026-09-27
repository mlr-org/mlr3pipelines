# A concrete private selector-normalization correction

This records a diagnostic correction to `[.CnfClause`, evaluated in a private
function while leaving the production method unchanged. The underlying
public defect is established independently in `../r_values/NOTES.md` and
`../selector_semantic_trace/`. The correction is a reviewable proposal, not
an installed repair or a claim that all public constructor issues disappear.

## Exact changes and intended interpretation

```r
i = as.vector(unclass(i))  # production currently has i = unclass(i)
```

and, inside the assertion alternatives:

```r
check_logical(i, len = true_length, any.missing = FALSE)
```

The first change interprets an atomic selector by its vector of values and
discards dimensions, names, and ordinary metadata before flooring,
deduplication, and constant shortcuts. Consequently `unique.matrix` and
`unique.array` cannot preserve repeated individual indices. The second change
requires every accepted logical selection position to be known.

Rejecting dimensional selectors is another possible API choice. Flattening
is the diagnostic choice here because the existing method already accepts
them, and it gives them the same value semantics as an ordinary vector.
Flattening also makes named scalar FALSE behave like scalar FALSE on a
multi-symbol clause. That consistency change is explicit rather than hidden
as an accidental side effect. Custom classes whose methods redefine the
meaning of atomic storage remain outside the ordinary-value proof.

## Why every accepted result has the required representation

Assume the input is a proper canonical clause: distinct registered nonmissing
symbol names, and a nonempty proper ordinary character set at each symbol.
The TRUE/FALSE clause cases are handled separately by the source's scalar
branches. The no-index call returns its input unchanged.

After flattening, each nonempty accepted selector has one of these forms:

* Numeric values are floored, globally deduplicated, and checked to be finite
  nonmissing values between zero and the clause width. Every positive index
  selects a distinct existing position; zero selects none. Flooring occurs
  before checking, preserving the existing handling of fractional values.
* Character values are globally deduplicated and checked against the actual
  clause names. Every value selects a distinct existing position. Missing
  values cannot be valid names under the input premise.
* Logical values have exactly the clause width and contain no missing value.
  Each TRUE selects its own position at most once; FALSE selects none.

The validator alternatives do not defeat this partition. On ordinary
nonmissing nonempty values, a character name cannot satisfy numeric/logical
validation, a numeric vector cannot satisfy character-subset validation, and
logical validation with `any.missing=FALSE` cannot admit the previously
permitted all-missing numeric or character vector. Empty vectors are intentionally
normalized to FALSE before this assertion.

Subsetting neither changes a selected range nor changes the associated
universe. Selecting a subset of unique valid positions therefore preserves
unique valid symbol names, range properness, and every selected disjunct's
meaning. The disjunction of no selected positions is FALSE. Scalar TRUE can
only be retained by its accepted single-position selectors or discarded;
scalar FALSE can only be retained by an empty-selection route. Invalid
constant selections still raise their existing errors.

## Checks on the concrete private function

`selector_candidate.R` creates the function by replacing exactly the two
source expressions above, with assertions that both expressions exist.
It exhausts selector lengths one through four over these value banks:

* Numeric: `-1, 0, 1, 2, 3, 1.9, Inf, NA_real_`.
* Character: `X, Y, Z, absent, NA_character_`.
* Logical: FALSE, TRUE, NA.

Each is tried as a plain vector, a named vector, a one-row matrix, and a
three-dimensional array. Empty vectors of six storage types, NULL, zero,
and FALSE are additional controls. Every selector is applied to proper
clauses of widths one through three and to typed TRUE and FALSE.

Both R 3.6.3 and R 4.6.1 passed the same **111,695 calls**:

| Check | Count per R version |
| --- | ---: |
| Candidate accepted, canonical, and semantically checked | 5,891 |
| Exact agreement with production on the flattened nonmissing selector | 5,891 |
| Missing selectors explicitly rejected | 48,200 |
| Production accepted but returned noncanonical storage | 2,272 |

For each accepted result the harness checks universe identity and an
independent positional-selection truth table. The four-clause accepted
selector counterexample then returns FALSE through the private correction.
The two-clause unit-HLA exception instead returns the intended `X=a` unit.
The live production method remains identical to the original function.
Results are in `selector_candidate_r36.json` and `selector_candidate_r46.json`.

Three research-harness assumptions were corrected before the completed
counts: naming NULL is invalid; `vapply()` over a classed CnfClause invokes
its `as.list()` coercion and sees atoms rather than raw ranges; and real
checkmate accepts an all-missing numeric or character vector through its permissive
logical alternative. The last observation expands the public defect beyond
selectors whose R storage type is logical. The final harness uses raw lists
for positional range checks and requires rejection of all missing selectors.
These corrections are not changes to the production implementation.
