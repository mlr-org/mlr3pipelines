# Symbols for CNF Formulas

Representation of Symbols used in CNF formulas. Symbols have a name and
a domain (a set of possible values), and are stored in a
[`CnfUniverse`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfUniverse.md).

Once created, it is currently not intended to modify or delete symbols.

Symbols can be used in CNF formulas by creating
[`CnfAtom`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfAtom.md)
objects, either by using the `%among%` operator or by using the
[`CnfAtom()`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfAtom.md)
constructor explicitly.

This is part of the CNF representation tooling, which is currently
considered experimental; it is for internal use.

## Usage

``` r
CnfSymbol(universe, name, domain)
```

## Arguments

- universe:

  ([`CnfUniverse`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfUniverse.md))  
  The universe in which the symbol is defined.

- name:

  (`character(1)`)  
  The name of the symbol.

- domain:

  (`character`)  
  The domain, i.e. the set of possible values for the symbol. Must not
  be empty.

## Value

A new `CnfSymbol` object.

## See also

Other CNF representation objects:
[`CnfAtom()`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfAtom.md),
[`CnfClause()`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfClause.md),
[`CnfFormula()`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfFormula.md),
[`CnfUniverse()`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfUniverse.md)

## Examples

``` r
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))

# Use symbols to create CnfAtom objects
X %among% c("a", "b")
#> CnfAtom: X ∈ {a, b}.
X %among% "a"
#> CnfAtom: X ∈ {a}.
X %among% character(0)
#> CnfAtom: <FALSE>
X %among% c("a", "b", "c")
#> CnfAtom: <TRUE>
```
