# Conversion to mlr3pipelines Graph

The argument is turned into a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) if
possible. If `clone` is `TRUE`, a deep copy is made if the incoming
object is a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) to
ensure the resulting object is a different reference from the incoming
object.

`as_graph()` is an S3 method and can therefore be implemented by other
packages that may add objects that can naturally be converted to
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)s.

By default, `as_graph()` tries to

- apply
  [`gunion()`](https://mlr3pipelines.mlr-org.com/reference/gunion.md) to
  `x` if it is a `list`, which recursively applies `as_graph()` to all
  list elements first

- create a
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) with
  only one element if `x` is a
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) or
  can be converted to one using
  [`as_pipeop()`](https://mlr3pipelines.mlr-org.com/reference/as_pipeop.md).

## Usage

``` r
as_graph(x, clone = FALSE)
```

## Arguments

- x:

  (`any`)  
  Object to convert.

- clone:

  (`logical(1)`)  
  Whether to return a (deep copied) clone if `x` is a Graph.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) `x` or a
deep clone of it.

## See also

Other Graph operators: `%>>%()`,
[`as_pipeop()`](https://mlr3pipelines.mlr-org.com/reference/as_pipeop.md),
[`assert_graph()`](https://mlr3pipelines.mlr-org.com/reference/assert_graph.md),
[`assert_pipeop()`](https://mlr3pipelines.mlr-org.com/reference/assert_pipeop.md),
[`chain_graphs()`](https://mlr3pipelines.mlr-org.com/reference/chain_graphs.md),
[`greplicate()`](https://mlr3pipelines.mlr-org.com/reference/greplicate.md),
[`gunion()`](https://mlr3pipelines.mlr-org.com/reference/gunion.md),
[`mlr_graphs_greplicate`](https://mlr3pipelines.mlr-org.com/reference/mlr_graphs_greplicate.md)
