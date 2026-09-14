# Symbols for CNF Formulas

Representation of Symbols used in CNF formulas. Symbols have a name and
a domain (a set of possible values), and are stored in a
[`CnfUniverse`](https://mlr3pipelines.mlr-org.com/dev/reference/CnfUniverse.md).

Once created, it is currently not intended to modify or delete symbols.
Keep the character locale (`LC_CTYPE`) unchanged while a universe is in
use.

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
  The nonempty name of the symbol. It must be representable without
  substitution in the current character locale so that environment
  bindings preserve its identity.

- domain:

  (`character`)  
  The domain, i.e. the set of possible values for the symbol. Must not
  be empty. Names and domains must be character vectors without
  dimensions or custom classes, missing values, byte-marked strings, or
  invalid encodings. Accepted text is converted to UTF-8; distinct
  Unicode code-point sequences remain distinct.

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
