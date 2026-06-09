# Chain a Series of Graphs

Takes an arbitrary amount of
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)s or
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
(or objects that can be automatically converted into
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)s or
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s,
see
[`as_graph()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_graph.md)
and
[`as_pipeop()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_pipeop.md))
as inputs and joins them in a serial
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md), as
if connecting them using `%>>%`.

Care is taken to avoid unnecessarily cloning of components. A call of
`chain_graphs(list(g1, g2, g3, g4, ...), in_place = FALSE)` is
equivalent to `g1 %>>% g2 %>>!% g3 %>>!% g4 %>>!% ...`. A call of
`chain_graphs(list(g1, g2, g3, g4, ...), in_place = FALSE)` is
equivalent to `g1 %>>!% g2 %>>!% g3 %>>!% g4 %>>!% ...` (differing in
the first operator being `%>>!%` as well).

## Usage

``` r
chain_graphs(graphs, in_place = FALSE)
```

## Arguments

- graphs:

  `list` of
  ([`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  \|
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  \| `NULL` \| `...`)  
  List of elements which are the
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)s
  to be joined. Elements must be convertible to
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) or
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  using
  [`as_graph()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_graph.md)
  and
  [`as_pipeop()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_pipeop.md).
  `NULL` is the neutral element of `%>>%` and skipped.

- in_place:

  (`logical(1)`)  
  Whether to try to avoid cloning the first element of `graphs`, similar
  to the difference of `%>>!%` over `%>>%`. This can only be avoided if
  `graphs[[1]]` is already a
  [`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md).
  Beware that, if `chain_graphs()` fails because of id collisions, then
  `graphs[[1]]` will possibly be in an incompletely modified state when
  `in_place` is `TRUE`.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) the
resulting
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md), or
`NULL` if there are no non-null values in `graphs`.

## See also

Other Graph operators: `%>>%()`,
[`as_graph()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_graph.md),
[`as_pipeop()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_pipeop.md),
[`assert_graph()`](https://mlr3pipelines.mlr-org.com/dev/reference/assert_graph.md),
[`assert_pipeop()`](https://mlr3pipelines.mlr-org.com/dev/reference/assert_pipeop.md),
[`greplicate()`](https://mlr3pipelines.mlr-org.com/dev/reference/greplicate.md),
[`gunion()`](https://mlr3pipelines.mlr-org.com/dev/reference/gunion.md),
[`mlr_graphs_greplicate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_graphs_greplicate.md)
