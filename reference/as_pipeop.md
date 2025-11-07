# Conversion to mlr3pipelines PipeOp

The argument is turned into a
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) if
possible. If `clone` is `TRUE`, a deep copy is made if the incoming
object is a
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) to
ensure the resulting object is a different reference from the incoming
object.

`as_pipeop()` is an S3 method and can therefore be implemented by other
packages that may add objects that can naturally be converted to
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s.
Objects that can be converted are for example
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) (using
[`PipeOpLearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.md))
or [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html)
(using
[`PipeOpFilter`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_filter.md)).

## Usage

``` r
as_pipeop(x, clone = FALSE)
```

## Arguments

- x:

  (`any`)  
  Object to convert.

- clone:

  (`logical(1)`)  
  Whether to return a (deep copied) clone if `x` is a PipeOp.

## Value

[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) `x` or
a deep clone of it.

## See also

Other Graph operators: `%>>%()`,
[`as_graph()`](https://mlr3pipelines.mlr-org.com/reference/as_graph.md),
[`assert_graph()`](https://mlr3pipelines.mlr-org.com/reference/assert_graph.md),
[`assert_pipeop()`](https://mlr3pipelines.mlr-org.com/reference/assert_pipeop.md),
[`chain_graphs()`](https://mlr3pipelines.mlr-org.com/reference/chain_graphs.md),
[`greplicate()`](https://mlr3pipelines.mlr-org.com/reference/greplicate.md),
[`gunion()`](https://mlr3pipelines.mlr-org.com/reference/gunion.md),
[`mlr_graphs_greplicate`](https://mlr3pipelines.mlr-org.com/reference/mlr_graphs_greplicate.md)
