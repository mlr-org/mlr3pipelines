# Conversion of Graph or PipeOp to GraphLearner

Methods for
[`as_learner()`](https://mlr3.mlr-org.com/reference/as_learner.html)
that wrap a
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md) or a
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md) in
a
[`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md).
A [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
is first converted to a
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
using
[`as_graph()`](https://mlr3pipelines.mlr-org.com/dev/reference/as_graph.md).

## Usage

``` r
# S3 method for class 'Graph'
as_learner(
  x,
  clone = FALSE,
  discard_state = FALSE,
  ...,
  id = NULL,
  param_vals = list(),
  task_type = NULL,
  predict_type = NULL
)

# S3 method for class 'PipeOp'
as_learner(x, clone = FALSE, ...)
```

## Arguments

- x:

  ([`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)
  \|
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md))  
  Object to convert.

- clone:

  (`logical(1)`)  
  Whether to deep clone `x` before wrapping it. If `FALSE` (default),
  `x` is wrapped by reference, so changes to `x`, such as new
  hyperparameter values, also affect the resulting
  [`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md),
  and vice versa. Furthermore, if `x` is trained, its `$state` is reset
  by reference during conversion, as well as whenever the
  [`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md)
  is trained or used for prediction. If `TRUE`, `x` is left unchanged.

- discard_state:

  (`logical(1)`)  
  Exists for compatibility with the
  [`as_learner()`](https://mlr3.mlr-org.com/reference/as_learner.html)
  method for
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)s, which
  [`resample()`](https://mlr3.mlr-org.com/reference/resample.html)
  relies on. Has no effect, since the resulting
  [`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md)
  is always untrained, even if `x` is trained.

- ...:

  (any)  
  For `as_learner.PipeOp()`, passed on to `as_learner.Graph()`. For
  `as_learner.Graph()`, must be empty.

- id:

  (`character(1)` \| `NULL`)  
  Identifier of the resulting
  [`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md).
  If `NULL` (default), the IDs of the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
  in `x` are concatenated.

- param_vals:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Hyperparameter settings of the resulting
  [`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md).
  Default [`list()`](https://rdrr.io/r/base/list.html).

- task_type:

  (`character(1)` \| `NULL`)  
  Task type of the resulting
  [`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md).
  If `NULL` (default), it is inferred from `x`.

- predict_type:

  (`character(1)` \| `NULL`)  
  Predict type of the resulting
  [`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md).
  If `NULL` (default), it is inferred from `x`.

## Value

[`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md)

## Internals

The arguments `id`, `param_vals`, `task_type`, and `predict_type` are
passed on to the
[`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md)
constructor. Unknown arguments raise an error.

## See also

[`mlr_learners_graph`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md)
for details on
[`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md).

## Examples

``` r
library("mlr3")

graph = po("pca") %>>% lrn("classif.rpart")
learner = as_learner(graph, id = "pca_rpart", param_vals = list(classif.rpart.cp = 0.1))
learner$id
#> [1] "pca_rpart"
learner$param_set$values$classif.rpart.cp
#> [1] 0.1

# x is wrapped by reference, unless clone is TRUE
identical(learner$graph, graph)
#> [1] TRUE
identical(as_learner(graph, clone = TRUE)$graph, graph)
#> [1] FALSE
```
