# PipeOp Composition Operator

These operators creates a connection that "pipes" data from the source
`g1` into the sink `g2`. Both source and sink can either be a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) or a
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) (or an
object that can be automatically converted into a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) or
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md), see
[`as_graph()`](https://mlr3pipelines.mlr-org.com/reference/as_graph.md)
and
[`as_pipeop()`](https://mlr3pipelines.mlr-org.com/reference/as_pipeop.md)).

`%>>%` and `%>>!%` try to automatically match output channels of `g1` to
input channels of `g2`; this is only possible if either

- the number of output channels of `g1` (as given by `g1$output`) is
  equal to the number of input channels of `g2` (as given by
  `g2$input`), or

- `g1` has only one output channel (i.e. `g1$output` has one line), or

- `g2` has only one input channel, which is a *vararg* channel (i.e.
  `g2$input` has one line, with `name` entry `"..."`).

Connections between channels are created in the order in which they
occur in `g1` and `g2`, respectively: `g1`'s output channel 1 is
connected to `g2`'s input channel 1, channel 2 to 2 etc.

`%>>%` always creates deep copies of its input arguments, so they cannot
be modified by reference afterwards. To access individual
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s after
composition, use the resulting
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)'s
`$pipeops` list. `%>>!%`, on the other hand, tries to avoid cloning its
first argument: If it is a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md), then
this [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)
will be modified in-place.

When `%>>!%` fails, then it leaves `g1` in an incompletely modified
state. It is therefore usually recommended to use `%>>%`, since the very
marginal gain of performance from using `%>>!%` often does not outweigh
the risk of either modifying objects by-reference that should not be
modified or getting graphs that are in an incompletely modified state.
However, when creating long
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)s,
chaining with `%>>!%` instead of `%>>%` can give noticeable performance
benefits because `%>>%` makes a number of `clone()`-calls that is
quadratic in chain length, `%>>!%` only linear.

`concat_graphs(g1, g2, in_place = FALSE)` is equivalent to `g1 %>>% g2`.
`concat_graphs(g1, g2, in_place = TRUE)` is equivalent to `g1 %>>!% g2`.

Both arguments of `%>>%` are automatically converted to
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)s using
[`as_graph()`](https://mlr3pipelines.mlr-org.com/reference/as_graph.md);
this means that objects on either side may be objects that can be
automatically converted to
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s (such
as [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)s or
[`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html)s), or
that can be converted to
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)s. This
means, in particular, `list`s of
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)s,
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s or
objects convertible to that, because
[`as_graph()`](https://mlr3pipelines.mlr-org.com/reference/as_graph.md)
automatically applies
[`gunion()`](https://mlr3pipelines.mlr-org.com/reference/gunion.md) to
`list`s. See examples. If the first argument of `%>>!%` is not a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md), then it
is cloned just as when `%>>%` is used; `%>>!%` only avoids `clone()` if
the first argument is a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md).

Note that if `g1` is `NULL`, `g2` converted to a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) will be
returned. Analogously, if `g2` is `NULL`, `g1` converted to a
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) will be
returned.

## Usage

``` r
g1 %>>% g2

concat_graphs(g1, g2, in_place = FALSE)

g1 %>>!% g2
```

## Arguments

- g1:

  ([`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) \|
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) \|
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) \|
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html) \|
  `list` \| `...`)  
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) /
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) /
  object-convertible-to-[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)
  to put in front of `g2`.

- g2:

  ([`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) \|
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) \|
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) \|
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html) \|
  `list` \| `...`)  
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) /
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) /
  object-convertible-to-[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)
  to put after `g1`.

- in_place:

  (`logical(1)`)  
  Whether to try to avoid cloning `g1`. If `g1` is not a
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md), then
  it is cloned regardless.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md): the
constructed
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md).

## See also

Other Graph operators:
[`as_graph()`](https://mlr3pipelines.mlr-org.com/reference/as_graph.md),
[`as_pipeop()`](https://mlr3pipelines.mlr-org.com/reference/as_pipeop.md),
[`assert_graph()`](https://mlr3pipelines.mlr-org.com/reference/assert_graph.md),
[`assert_pipeop()`](https://mlr3pipelines.mlr-org.com/reference/assert_pipeop.md),
[`chain_graphs()`](https://mlr3pipelines.mlr-org.com/reference/chain_graphs.md),
[`greplicate()`](https://mlr3pipelines.mlr-org.com/reference/greplicate.md),
[`gunion()`](https://mlr3pipelines.mlr-org.com/reference/gunion.md),
[`mlr_graphs_greplicate`](https://mlr3pipelines.mlr-org.com/reference/mlr_graphs_greplicate.md)

## Examples

``` r
o1 = PipeOpScale$new()
o2 = PipeOpPCA$new()
o3 = PipeOpFeatureUnion$new(2)

# The following two are equivalent:
pipe1 = o1 %>>% o2

pipe2 = Graph$new()$
  add_pipeop(o1)$
  add_pipeop(o2)$
  add_edge(o1$id, o2$id)

# Note automatical gunion() of lists.
# The following three are equivalent:
graph1 = list(o1, o2) %>>% o3

graph2 = gunion(list(o1, o2)) %>>% o3

graph3 = Graph$new()$
  add_pipeop(o1)$
  add_pipeop(o2)$
  add_pipeop(o3)$
  add_edge(o1$id, o3$id, dst_channel = 1)$
  add_edge(o2$id, o3$id, dst_channel = 2)

pipe1 %>>!% o3  # modify pipe1 in-place
#> 
#> ── Graph with 3 PipeOps: ───────────────────────────────────────────────────────
#>            ID         State     sccssors prdcssors
#>        <char>        <char>       <char>    <char>
#>         scale <<UNTRAINED>>          pca          
#>           pca <<UNTRAINED>> featureunion     scale
#>  featureunion <<UNTRAINED>>                    pca
#> 
#> ── Pipeline: non-sequential 

pipe1  # contains o1, o2, and o3 now.
#> 
#> ── Graph with 3 PipeOps: ───────────────────────────────────────────────────────
#>            ID         State     sccssors prdcssors
#>        <char>        <char>       <char>    <char>
#>         scale <<UNTRAINED>>          pca          
#>           pca <<UNTRAINED>> featureunion     scale
#>  featureunion <<UNTRAINED>>                    pca
#> 
#> ── Pipeline: non-sequential 

o1 %>>!% o2
#> 
#> ── Graph with 2 PipeOps: ───────────────────────────────────────────────────────
#>      ID         State sccssors prdcssors
#>  <char>        <char>   <char>    <char>
#>   scale <<UNTRAINED>>      pca          
#>     pca <<UNTRAINED>>              scale
#> 
#> ── Pipeline: <INPUT> -> scale -> pca -> <OUTPUT> 

o1  # not changed, becuase not a Graph.
#> 
#> ── PipeOp <scale>: not trained ─────────────────────────────────────────────────
#> Values: robust=FALSE
#> 
#> ── Input channels: 
#>    name  train predict
#>  <char> <char>  <char>
#>   input   Task    Task
#> 
#> ── Output channels: 
#>    name  train predict
#>  <char> <char>  <char>
#>  output   Task    Task
```
