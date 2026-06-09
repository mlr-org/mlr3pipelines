# Assertion for mlr3pipelines PipeOp

Function that checks that a given object is a `PipeOp` and throws an
error if not.

## Usage

``` r
assert_pipeop(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
`invisible(x)`

## See also

Other Graph operators: `%>>%()`,
[`as_graph()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_graph.md),
[`as_pipeop()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_pipeop.md),
[`assert_graph()`](https://mlr3pipelines.mlr-org.com/dev/reference/assert_graph.md),
[`chain_graphs()`](https://mlr3pipelines.mlr-org.com/dev/reference/chain_graphs.md),
[`greplicate()`](https://mlr3pipelines.mlr-org.com/dev/reference/greplicate.md),
[`gunion()`](https://mlr3pipelines.mlr-org.com/dev/reference/gunion.md),
[`mlr_graphs_greplicate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_graphs_greplicate.md)
