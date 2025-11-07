# Clauses in CNF Formulas

A `CnfClause` is a disjunction of
[`CnfAtom`](https://mlr3pipelines.mlr-org.com/reference/CnfAtom.md)
objects. It represents a statement that is true if at least one of the
atoms is true. These are for example of the form

      X %among% c("a", "b", "c") | Y %among% c("d", "e", "f") | ...

`CnfClause` objects can be constructed explicitly, using the
`CnfClause()` constructor, or implicitly, by using the `|` operator on
[`CnfAtom`](https://mlr3pipelines.mlr-org.com/reference/CnfAtom.md)s or
other `CnfClause` objects.

`CnfClause` objects which are not tautologies or contradictions are
named lists; the value ranges of each symbol can be accessed using `[[`,
and these clauses can be subset using `[` to get clauses containing only
the indicated symbols. However, to get a list of
[`CnfAtom`](https://mlr3pipelines.mlr-org.com/reference/CnfAtom.md)
objects, use [`as.list()`](https://rdrr.io/r/base/list.html). Note that
the simplified form of a clause containing a contradiction is the empty
list.

Upon construction, the `CnfClause` is simplified by (1) removing
contradictions, (2) unifying atoms that refer to the same symbol, and
(3) evaluating to `TRUE` if any atom is `TRUE`. Note that the order of
atoms in a clause is not preserved.

Using `CnfClause()` on lists that contain other `CnfClause` objects will
create a clause that is the disjunction of all atoms in all clauses.

If a `CnfClause` contains no atoms, or only `FALSE` atoms, it evaluates
to `FALSE`. If it contains at least one atom that is always true, the
clause evaluates to `TRUE`. These values can be converted to, and from,
`logical(1)` values using
[`as.logical()`](https://rdrr.io/r/base/logical.html) and
`as.CnfClause()`.

`CnfClause` objects can be negated using the `!` operator, and combined
using the `&` operator. Both of these operations return a
[`CnfFormula`](https://mlr3pipelines.mlr-org.com/reference/CnfFormula.md),
even if the result could in principle be represented as a single
`CnfClause`.

This is part of the CNF representation tooling, which is currently
considered experimental; it is for internal use.

## Usage

``` r
CnfClause(atoms)

as.CnfClause(x)
```

## Arguments

- atoms:

  (`list` of
  ([`CnfAtom`](https://mlr3pipelines.mlr-org.com/reference/CnfAtom.md)
  \| `CnfClause`))  
  A list of
  [`CnfAtom`](https://mlr3pipelines.mlr-org.com/reference/CnfAtom.md) or
  other `CnfClause` objects. The clause represents the disjunction of
  these atoms.

- x:

  (any)  
  The object to be coerced to a `CnfClause` by `as.CnfClause`. Only
  `logical(1)`,
  [`CnfAtom`](https://mlr3pipelines.mlr-org.com/reference/CnfAtom.md),
  and `CnfClause` itself are currently supported.

## Value

A new `CnfClause` object.

## Details

We are undecided whether it is a better idea to have
[`as.list()`](https://rdrr.io/r/base/list.html) return a named list or
an unnamed one. Calling [`as.list()`](https://rdrr.io/r/base/list.html)
on a `CnfClause` with a tautology returns a tautology-atom, which does
not have a name. We currently return a named list for other clauses, as
this makes subsetting by name commute with
[`as.list()`](https://rdrr.io/r/base/list.html). However, this behaviour
may change in the future.

## See also

Other CNF representation objects:
[`CnfAtom()`](https://mlr3pipelines.mlr-org.com/reference/CnfAtom.md),
[`CnfFormula()`](https://mlr3pipelines.mlr-org.com/reference/CnfFormula.md),
[`CnfSymbol()`](https://mlr3pipelines.mlr-org.com/reference/CnfSymbol.md),
[`CnfUniverse()`](https://mlr3pipelines.mlr-org.com/reference/CnfUniverse.md)

## Examples

``` r
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("d", "e", "f"))

CnfClause(list(X %among% c("a", "b"), Y %among% c("d", "e")))
#> CnfClause:
#>   X ∈ {a, b} | Y ∈ {d, e}
cls = X %among% c("a", "b") | Y %among% c("d", "e")
cls
#> CnfClause:
#>   X ∈ {a, b} | Y ∈ {d, e}

as.list(cls)
#> [[1]]
#> CnfAtom: X ∈ {a, b}.
#> 
#> [[2]]
#> CnfAtom: Y ∈ {d, e}.
#> 

as.CnfClause(X %among% c("a", "b"))
#> CnfClause:
#>   X ∈ {a, b}

# The same symbols are unified
X %among% "a" | Y %among% "d" | X %among% "b"
#> CnfClause:
#>   X ∈ {a, b} | Y ∈ {d}

# tautology evaluates to TRUE
X %among% "a" | X %among% "b" | X %among% "c"
#> CnfClause: TRUE

# contradictions are removed
X %among% "a" | Y %among% character(0)
#> CnfClause:
#>   X ∈ {a}

# create CnfFormula:
!(X %among% "a" | Y %among% "d")
#> CnfFormula:
#>      (X ∈ {b, c})
#>    & (Y ∈ {e, f})

# also a CnfFormula, even if it contains a single clause:
!CnfClause(list(X %among% "a"))
#> CnfFormula:
#>      (X ∈ {b, c})
(X %among% c("a", "c") | Y %among% "d") &
  (X %among% c("a", "b") | Y %among% "d")
#> CnfFormula:
#>      (X ∈ {a} | Y ∈ {d})
```
