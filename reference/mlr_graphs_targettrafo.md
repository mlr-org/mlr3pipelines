# Transform and Re-Transform the Target Variable

Wraps a [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)
that transforms a target during training and inverts the transformation
during prediction. This is done as follows:

- Specify a transformation and inversion function using any subclass of
  [`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTargetTrafo.md),
  defaults to
  [`PipeOpTargetMutate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targetmutate.md),
  afterwards apply `graph`.

- At the very end, during prediction the transformation is inverted
  using
  [`PipeOpTargetInvert`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targetinvert.md).

- To set a transformation and inversion function for
  [`PipeOpTargetMutate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targetmutate.md)
  see the parameters `trafo` and `inverter` of the `param_set` of the
  resulting
  [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md).

- Note that the input `graph` is not explicitly checked to actually
  return a
  [`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)
  during prediction.

All input arguments are cloned and have no references in common with the
returned
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md).

## Usage

``` r
pipeline_targettrafo(
  graph,
  trafo_pipeop = PipeOpTargetMutate$new(),
  id_prefix = ""
)
```

## Arguments

- graph:

  [`PipeOpLearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.md)
  \| [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)  
  A
  [`PipeOpLearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.md)
  or [`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md) to
  wrap between a transformation and re-transformation of the target
  variable.

- trafo_pipeop:

  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)  
  A [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)
  that is a subclass of
  [`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTargetTrafo.md).
  Default is
  [`PipeOpTargetMutate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targetmutate.md).

- id_prefix:

  `character(1)`  
  Optional id prefix to prepend to
  [`PipeOpTargetInvert`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targetinvert.md)
  ID. The resulting ID will be `"[id_prefix]targetinvert"`. Default is
  `""`.

## Value

[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md)

## Examples

``` r
library("mlr3")

tt = pipeline_targettrafo(PipeOpLearner$new(LearnerRegrRpart$new()))
tt$param_set$values$targetmutate.trafo = function(x) log(x, base = 2)
tt$param_set$values$targetmutate.inverter = function(x) list(response = 2 ^ x$response)

# gives the same as
g = Graph$new()
g$add_pipeop(PipeOpTargetMutate$new(param_vals = list(
  trafo = function(x) log(x, base = 2),
  inverter = function(x) list(response = 2 ^ x$response))
  )
)
g$add_pipeop(LearnerRegrRpart$new())
g$add_pipeop(PipeOpTargetInvert$new())
g$add_edge(src_id = "targetmutate", dst_id = "targetinvert",
  src_channel = 1, dst_channel = 1)
g$add_edge(src_id = "targetmutate", dst_id = "regr.rpart",
  src_channel = 2, dst_channel = 1)
g$add_edge(src_id = "regr.rpart", dst_id = "targetinvert",
  src_channel = 1, dst_channel = 2)
```
