# Symbol Table for CNF Formulas

A symbol table for CNF formulas. The `CnfUniverse` is a by-reference
object that stores the domain of each symbol. Symbols are created with
[`CnfSymbol()`](https://mlr3pipelines.mlr-org.com/reference/CnfSymbol.md)
and can be retrieved with `$`. Using `[[` retrieves a given symbol's
domain.

It is only possible to combine symbols from the same (identical)
universe.

This is part of the CNF representation tooling, which is currently
considered experimental; it is for internal use.

## Usage

``` r
CnfUniverse()
```

## Value

A new `CnfUniverse` object.

## See also

Other CNF representation objects:
[`CnfAtom()`](https://mlr3pipelines.mlr-org.com/reference/CnfAtom.md),
[`CnfClause()`](https://mlr3pipelines.mlr-org.com/reference/CnfClause.md),
[`CnfFormula()`](https://mlr3pipelines.mlr-org.com/reference/CnfFormula.md),
[`CnfSymbol()`](https://mlr3pipelines.mlr-org.com/reference/CnfSymbol.md)

## Examples

``` r
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("d", "e", "f"))

u$X
#> CnfSymbol 'X' with domain {a, b, c}.
u[["Y"]]
#> [1] "d" "e" "f"

X %among% c("a", "c")
#> CnfAtom: X ∈ {a, c}.
u$X %among% c("a", "c")
#> CnfAtom: X ∈ {a, c}.
Y %among% c("d", "e", "f")
#> CnfAtom: <TRUE>
Y %among% character(0)
#> CnfAtom: <FALSE>

u$X %among% "a" | u$Y %among% "d"
#> CnfClause:
#>   X ∈ {a} | Y ∈ {d}
```
