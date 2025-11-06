# Create Disjoint Graph Union of Copies of a Graph

Create a new
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
containing `n` copies of the input
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) /
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).
To avoid ID collisions, PipeOp IDs are suffixed with `_i` where `i`
ranges from 1 to `n`.

All input arguments are cloned and have no references in common with the
returned
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md).

## Usage

``` r
pipeline_greplicate(graph, n)
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
[`greplicate()`](https://mlr3pipelines.mlr-org.com/dev/reference/greplicate.md),
[`gunion()`](https://mlr3pipelines.mlr-org.com/dev/reference/gunion.md)

## Examples

``` r
library("mlr3")

po_pca = po("pca")
pipeline_greplicate(po_pca, n = 2)
#> Graph with 2 PipeOps:
#>      ID         State sccssors prdcssors
#>  <char>        <char>   <char>    <char>
#>   pca_1 <<UNTRAINED>>                   
#>   pca_2 <<UNTRAINED>>                   
```
