# Create Disjoint Graph Union of Copies of a Graph

Create a new
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
containing `n` copies of the input `Graph` /
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).
To avoid ID collisions, PipeOp IDs are suffixed with `_i` where `i`
ranges from 1 to `n`.

This function is deprecated and will be removed in the next version in
favor of using pipeline_greplicate / ppl("greplicate").

## Usage

``` r
greplicate(graph, n)
```

## Arguments

- graph:

  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)  
  Graph to replicate.

- n:

  `integer(1)` Number of copies to create.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
containing `n` copies of input `graph`.

## See also

Other Graph operators: `%>>%()`,
[`as_graph()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_graph.md),
[`as_pipeop()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_pipeop.md),
[`assert_graph()`](https://mlr3pipelines.mlr-org.com/dev/reference/assert_graph.md),
[`assert_pipeop()`](https://mlr3pipelines.mlr-org.com/dev/reference/assert_pipeop.md),
[`chain_graphs()`](https://mlr3pipelines.mlr-org.com/dev/reference/chain_graphs.md),
[`gunion()`](https://mlr3pipelines.mlr-org.com/dev/reference/gunion.md),
[`mlr_graphs_greplicate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_graphs_greplicate.md)
