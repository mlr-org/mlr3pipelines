# Shorthand Graph Constructor

Creates a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) from
[`mlr_graphs`](https://mlr3pipelines.mlr-org.com/reference/mlr_graphs.md)
from given ID

`ppl()` taks a `character(1)` and returns a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md).
`ppls()` takes a `character` vector of any list and returns a `list` of
possibly muliple
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)s.

## Usage

``` r
ppl(.key, ...)

ppls(.keys, ...)
```

## Arguments

- .key:

  `[character(1)]`  
  The key of the
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) in
  [`mlr_graphs`](https://mlr3pipelines.mlr-org.com/reference/mlr_graphs.md).

- ...:

  `any`  
  Additional parameters to give to constructed object. This may be an
  argument of the constructor of the underlying function.

- .keys:

  `[character]`  
  The key of possibly multiple
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)s in
  [`mlr_graphs`](https://mlr3pipelines.mlr-org.com/reference/mlr_graphs.md).
  If this is named, a named `list` is returned, but unlike
  [`pos()`](https://mlr3pipelines.mlr-org.com/reference/po.md) it will
  not set any `$id` slots.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) (for
`ppl()`) or `list` of
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)s (for
`ppls()`).

## Examples

``` r
library("mlr3")

gr = ppl("bagging", graph = po(lrn("regr.rpart")),
  averager = po("regravg", collect_multiplicity = TRUE))
```
