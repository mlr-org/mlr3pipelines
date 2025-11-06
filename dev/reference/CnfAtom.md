# Atoms for CNF Formulas

`CnfAtom` objects represent a single statement that is used to build up
CNF formulae. They are mostly intermediate, created using the `%among%`
operator or `CnfAtom()` directly, and combined into
[`CnfClause`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfClause.md)
and
[`CnfFormula`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfFormula.md)
objects.
[`CnfClause`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfClause.md)
and
[`CnfFormula`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfFormula.md)
do not, however, contain `CnfAtom` objects directly,

`CnfAtom`s contain an indirect reference to a
[`CnfSymbol`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfSymbol.md)
by referencing its name and its
[`CnfUniverse`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfUniverse.md).
They furthermore contain a set of values. An `CnfAtom` represents a
statement asserting that the given symbol takes up one of the given
values.

If the set of values is empty, the `CnfAtom` represents a contradiction
(FALSE). If it is the full domain of the symbol, the `CnfAtom`
represents a tautology (TRUE). These values can be converted to, and
from, `logical(1)` values using
[`as.logical()`](https://rdrr.io/r/base/logical.html) and
`as.CnfAtom()`.

`CnfAtom` objects can be negated using the `!` operator, which will
return the `CnfAtom` representing set membership in the complement of
the symbol with respect to its domain. `CnfAtom`s can furthermore be
combined using the `|` operator to form a
[`CnfClause`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfClause.md),
and using the `&` operator to form a
[`CnfFormula`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfFormula.md).
This happens even if the resulting statement could be represented as a
single `CnfAtom`.

This is part of the CNF representation tooling, which is currently
considered experimental; it is for internal use.

## Usage

``` r
CnfAtom(symbol, values)

e1 %among% e2

as.CnfAtom(x)
```

## Arguments

- symbol:

  ([`CnfSymbol`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfSymbol.md))  
  The symbol to which the atom refers.

- values:

  (`character`)  
  The values that the symbol can take.

- e1:

  (`CnfSymbol`)  
  Left-hand side of the `%among%` operator. Passed as `symbol` to
  `CnfAtom()`.

- e2:

  (`character`)  
  Right-hand side of the `%among%` operator. Passed as `values` to
  `CnfAtom()`.

- x:

  (any)  
  The object to be coerced to a `CnfAtom` by `as.CnfAtom`. Only
  `logical(1)` and `CnfAtom` itself are currently supported.

## Value

A new `CnfAtom` object.

## Details

We would have preferred to overload the `%in%` operator, but this is
currently not easily possible in R. We therefore created the `%among%`
operator.

The internal representation of a `CnfAtom` may change in the future.

## See also

Other CNF representation objects:
[`CnfClause()`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfClause.md),
[`CnfFormula()`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfFormula.md),
[`CnfSymbol()`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfSymbol.md),
[`CnfUniverse()`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfUniverse.md)

## Examples

``` r
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))

CnfAtom(X, c("a", "b"))
#> CnfAtom: X ∈ {a, b}.
X %among% "a"
#> CnfAtom: X ∈ {a}.
X %among% character(0)
#> CnfAtom: <FALSE>
X %among% c("a", "b", "c")
#> CnfAtom: <TRUE>

as.logical(X %among% character(0))
#> [1] FALSE
#> attr(,"universe")
#> CnfUniverse with variables:
#>   X: {a, b, c}
as.CnfAtom(TRUE)
#> CnfAtom: <TRUE>

!(X %among% "a")
#> CnfAtom: X ∈ {b, c}.

X %among% "a" | X %among% "b"  # creates a CnfClause
#> CnfClause:
#>   X ∈ {a, b}

X %among% "a" & X %among% c("a", "b")  # creates a CnfFormula
#> CnfFormula:
#>      (X ∈ {a})
```
