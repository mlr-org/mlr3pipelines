# Wrap a Learner into a PipeOp with Cross-validated Predictions as Features

Wraps an
[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) into
a [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

Returns cross-validated predictions during training as a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) and stores a
model of the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) trained on
the whole data in `$state`. This is used to create a similar
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) during
prediction. Optionally, the fitted models obtained during the resampling
phase can be reused for prediction by averaging their predictions,
avoiding the need for an additional fit on the complete training data.

The [`Task`](https://mlr3.mlr-org.com/reference/Task.html) gets features
depending on the capsuled
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)'s
`$predict_type`. If the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)'s
`$predict.type` is `"response"`, a feature `<ID>.response` is created,
for `$predict.type` `"prob"` the `<ID>.prob.<CLASS>` features are
created, and for `$predict.type` `"se"` the new columns are
`<ID>.response` and `<ID>.se`. `<ID>` denotes the `$id` of the
`PipeOpLearnerCV` object.

Inherits the `$param_set` (and therefore `$param_set$values`) from the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) it is
constructed from.

`PipeOpLearnerCV` can be used to create "stacking" or "super learning"
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)s
that use the output of one
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) as feature
for another
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html). Because
the `PipeOpLearnerCV` erases the original input features, it is often
useful to use
[`PipeOpFeatureUnion`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_featureunion.md)
to bind the prediction
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) to the original
input [`Task`](https://mlr3.mlr-org.com/reference/Task.html).

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Construction

    PipeOpLearnerCV$new(learner, id = NULL, param_vals = list())

- `learner` ::
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)  
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) to use
  for cross validation / prediction, or a string identifying a
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) in the
  [`mlr3::mlr_learners`](https://mlr3.mlr-org.com/reference/mlr_learners.html)
  [`Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html).
  This argument is always cloned; to access the
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) inside
  `PipeOpLearnerCV` by-reference, use `$learner`.  

- `id` :: `character(1)` Identifier of the resulting object, internally
  defaulting to the `id` of the
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) being
  wrapped.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

`PipeOpLearnerCV` has one input channel named `"input"`, taking a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) specific to the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) type given
to `learner` during construction; both during training and prediction.

`PipeOpLearnerCV` has one output channel named `"output"`, producing a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) specific to the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) type given
to `learner` during construction; both during training and prediction.

The output is a task with the same target as the input task, with
features replaced by predictions made by the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html). During
training, this prediction is the out-of-sample prediction made by
[`resample`](https://mlr3.mlr-org.com/reference/resample.html), during
prediction, this is the ordinary prediction made on the data by a
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) trained on
the training phase data.

## State

The `$state` is set to the `$state` slot of the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) object,
together with the `$state` elements inherited from the
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md).
It is a named `list` with the inherited members, as well as:

- `model` :: `any`  
  Model created by the
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)'s
  `$.train()` function.

- `train_log` ::
  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with columns `class` (`character`), `msg` (`character`)  
  Errors logged during training.

- `train_time` :: `numeric(1)`  
  Training time, in seconds.

- `predict_log` :: `NULL` \|
  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with columns `class` (`character`), `msg` (`character`)  
  Errors logged during prediction.

- `predict_time` :: `NULL` \| `numeric(1)` Prediction time, in seconds.

- `predict_method` :: `character(1)`  
  `"full"` when prediction uses a learner fitted on all training data,
  `"cv_ensemble"` when predictions are averaged over models trained on
  resampling folds.

- `cv_model_states` :: `NULL` \| `list`  
  Present for `predict_method = "cv_ensemble"`. Contains the states of
  the learners trained on each resampling fold.

This state is given the class `"pipeop_learner_cv_state"`.

## Parameters

The parameters are the parameters inherited from the
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
as well as the parameters of the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) wrapped by
this object. Besides that, parameters introduced are:

- `resampling.method` :: `character(1)`  
  Which resampling method do we want to use. Currently only supports
  `"cv"` and `"insample"`. `"insample"` generates predictions with the
  model trained on all training data.

- `resampling.folds` :: `numeric(1)`  
  Number of cross validation folds. Initialized to 3. Only used for
  `resampling.method = "cv"`.

- `resampling.keep_response` :: `logical(1)`  
  Only effective during `"prob"` prediction: Whether to keep response
  values, if available. Initialized to `FALSE`.

- `resampling.predict_method` :: `character(1)`  
  Controls how predictions are produced after training. `"full"`
  (default) fits the wrapped learner on the entire training data.
  `"cv_ensemble"` reuses the models fitted during resampling and
  averages their predictions. This option currently supports
  classification and regression learners together with
  `resampling.method = "cv"`.

- `resampling.prob_aggr` :: `character(1)`  
  Probability aggregation used when `"cv_ensemble"` predictions are
  produced for classification learners that can emit class
  probabilities. Shares the semantics with
  [`PipeOpClassifAvg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classifavg.md):
  `"mean"` (linear opinion pool, default) and `"log"` (log opinion pool
  / product of experts). Only present for learners that support `"prob"`
  predictions.

- `resampling.prob_aggr_eps` :: `numeric(1)`  
  Stabilization constant applied when `resampling.prob_aggr = "log"` to
  clamp probabilities before taking logarithms. Defaults to `1e-12`.
  Only present for learners that support `"prob"` predictions.

- `resampling.se_aggr` :: `character(1)`  
  Standard error aggregation used when `"cv_ensemble"` predictions are
  produced for regression learners with `predict_type` containing
  `"se"`. Shares the definitions with
  [`PipeOpRegrAvg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_regravg.md),
  i.e. `"predictive"`, `"mean"`, `"within"`, `"between"`, `"none"`.
  Initialized to `"predictive"` (within-fold variance plus between-fold
  disagreement) when constructed with a
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) that has
  `predict_type = "se"`; otherwise to `"none"`.  
  Only present for learners that support `"se"` predictions.

- `resampling.se_aggr_rho` :: `numeric(1)`  
  Equicorrelation parameter for `resampling.se_aggr = "mean"`,
  interpreted as in
  [`PipeOpRegrAvg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_regravg.md).
  Ignored otherwise. Defaults to `0` when
  `resampling.se_aggr = "mean"`.  
  Only present for learners that support `"se"` predictions.

## Internals

The `$state` is currently not updated by prediction, so the
`$state$predict_log` and `$state$predict_time` will always be `NULL`.

## Fields

Fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
as well as:

- `learner` ::
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)  
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) that is
  being wrapped. Read-only.

- `learner_model` ::
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)  
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) that is
  being wrapped. This learner contains the model if the `PipeOp` is
  trained. Read-only.

## Methods

Methods inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## See also

https://mlr-org.com/pipeops.html

Other Meta PipeOps:
[`mlr_pipeops_learner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner.md),
[`mlr_pipeops_learner_pi_cvplus`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_pi_cvplus.md),
[`mlr_pipeops_learner_quantiles`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_quantiles.md)

## Examples

``` r
library("mlr3")

task = tsk("iris")
learner = lrn("classif.rpart")

lrncv_po = po("learner_cv", learner)
lrncv_po$learner$predict_type = "response"

nop = mlr_pipeops$get("nop")

graph = gunion(list(
  lrncv_po,
  nop
)) %>>% po("featureunion")

graph$train(task)
#> $featureunion.output
#> 
#> ── <TaskClassif> (150x6): Iris Flowers ─────────────────────────────────────────
#> • Target: Species
#> • Target classes: setosa (33%), versicolor (33%), virginica (33%)
#> • Properties: multiclass
#> • Features (5):
#>   • dbl (4): Petal.Length, Petal.Width, Sepal.Length, Sepal.Width
#>   • fct (1): classif.rpart.response
#> 

graph$pipeops$classif.rpart$learner$predict_type = "prob"
graph$pipeops$classif.rpart$param_set$values$resampling.predict_method = "cv_ensemble"

graph$train(task)
#> $featureunion.output
#> 
#> ── <TaskClassif> (150x8): Iris Flowers ─────────────────────────────────────────
#> • Target: Species
#> • Target classes: setosa (33%), versicolor (33%), virginica (33%)
#> • Properties: multiclass
#> • Features (7):
#>   • dbl (7): Petal.Length, Petal.Width, Sepal.Length, Sepal.Width,
#>   classif.rpart.prob.setosa, classif.rpart.prob.versicolor,
#>   classif.rpart.prob.virginica
#> 
```
