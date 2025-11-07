# Disjoint Union of Graphs

Takes an arbitrary amount of
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)s or
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s (or
objects that can be automatically converted into
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)s or
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s, see
[`as_graph()`](https://mlr3pipelines.mlr-org.com/reference/as_graph.md)
and
[`as_pipeop()`](https://mlr3pipelines.mlr-org.com/reference/as_pipeop.md))
as inputs and joins them in a new
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md).

The [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s
of the input
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)s are not
joined with new edges across
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)s, so if
`length(graphs) > 1`, the resulting
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) will be
disconnected.

This operation always creates deep copies of its input arguments, so
they cannot be modified by reference afterwards. To access individual
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s after
composition, use the resulting
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)'s
`$pipeops` list.

## Usage

``` r
gunion(graphs, in_place = FALSE)
```

## Arguments

- graphs:

  `list` of
  ([`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) \|
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) \|
  `NULL` \| `...`)  
  List of elements which are the
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)s to be
  joined. Elements must be convertible to
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) or
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)
  using
  [`as_graph()`](https://mlr3pipelines.mlr-org.com/reference/as_graph.md)
  and
  [`as_pipeop()`](https://mlr3pipelines.mlr-org.com/reference/as_pipeop.md).
  `NULL` values automatically get converted to
  [`PipeOpNOP`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_nop.md)
  with a random ID of the format `nop_********`. The list can be named,
  in which case the IDs of the elements are prefixed with the names,
  separated by a dot (`.`).

- in_place:

  (`logical(1)` \| `logical`)  
  Whether to try to avoid cloning the first element of `graphs`, similar
  to the difference of `%>>!%` over `%>>%`. This can only be avoided if
  `graphs[[1]]` is already a
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md).  
  Unlike
  [`chain_graphs()`](https://mlr3pipelines.mlr-org.com/reference/chain_graphs.md),
  `gunion()` does all checks *before* mutating `graphs[[1]]`, so it will
  not leave `graphs[[1]]` in an incompletely modified state when it
  fails.  
  `in_place` may also be of length `graph`, in which case it determines
  for each element of `graphs` whether it is cloned. This is for
  internal usage and is not recommended.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) the
resulting
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md).

## See also

Other Graph operators: `%>>%()`,
[`as_graph()`](https://mlr3pipelines.mlr-org.com/reference/as_graph.md),
[`as_pipeop()`](https://mlr3pipelines.mlr-org.com/reference/as_pipeop.md),
[`assert_graph()`](https://mlr3pipelines.mlr-org.com/reference/assert_graph.md),
[`assert_pipeop()`](https://mlr3pipelines.mlr-org.com/reference/assert_pipeop.md),
[`chain_graphs()`](https://mlr3pipelines.mlr-org.com/reference/chain_graphs.md),
[`greplicate()`](https://mlr3pipelines.mlr-org.com/reference/greplicate.md),
[`mlr_graphs_greplicate`](https://mlr3pipelines.mlr-org.com/reference/mlr_graphs_greplicate.md)
