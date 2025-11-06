# Wrap a Learner into a PipeOp

Wraps an
[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) into
a [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

Inherits the `$param_set` (and therefore `$param_set$values`) from the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) it is
constructed from.

Using `PipeOpLearner`, it is possible to embed
[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html)s into
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md)s,
which themselves can be turned into Learners using
[`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md).
This way, preprocessing and ensemble methods can be included into a
machine learning pipeline which then can be handled as singular object
for resampling, benchmarking and tuning.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Construction

    PipeOpLearner$new(learner, id = NULL, param_vals = list())

- `learner` ::
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) \|
  `character(1)`  
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) to wrap,
  or a string identifying a
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) in the
  [`mlr3::mlr_learners`](https://mlr3.mlr-org.com/reference/mlr_learners.html)
  [`Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html).
  This argument is always cloned; to access the
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) inside
  `PipeOpLearner` by-reference, use `$learner`.  

- `id` :: `character(1)`  
  Identifier of the resulting object, internally defaulting to the `id`
  of the [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)
  being wrapped.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

`PipeOpLearner` has one input channel named `"input"`, taking a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) specific to the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) type given
to `learner` during construction; both during training and prediction.

`PipeOpLearner` has one output channel named `"output"`, producing
`NULL` during training and a
[`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)
subclass during prediction; this subclass is specific to the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) type given
to `learner` during construction.

The output during prediction is the
[`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html) on
the prediction input data, produced by the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) trained on
the training input data.

## State

The `$state` is set to the `$state` slot of the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) object. It
is a named `list` with members:

- `model` :: `any`  
  Model created by the
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)'s
  `$.train()` function.

- `train_log` ::
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with columns `class` (`character`), `msg` (`character`)  
  Errors logged during training.

- `train_time` :: `numeric(1)`  
  Training time, in seconds.

- `predict_log` :: `NULL` \|
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with columns `class` (`character`), `msg` (`character`)  
  Errors logged during prediction.

- `predict_time` :: `NULL` \| `numeric(1)` Prediction time, in seconds.

## Parameters

The parameters are exactly the parameters of the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) wrapped by
this object.

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

- `validate` :: `"predefined"` or `NULL`  
  This field can only be set for `Learner`s that have the `"validation"`
  property. Setting the field to `"predefined"` means that the wrapped
  `Learner` will use the internal validation task, otherwise it will be
  ignored. Note that specifying *how* the validation data is created is
  possible via the `$validate` field of the
  [`GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_graph.md).
  For each `PipeOp` it is then only possible to either use it
  (`"predefined"`) or not use it (`NULL`). Also see
  [`set_validate.GraphLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/set_validate.GraphLearner.md)
  for more information.

- `internal_tuned_values` :: named
  [`list()`](https://rdrr.io/r/base/list.html) or `NULL`  
  The internally tuned values if the wrapped `Learner` supports internal
  tuning, `NULL` otherwise.

- `internal_valid_scores` :: named
  [`list()`](https://rdrr.io/r/base/list.html) or `NULL`  
  The internal validation scores if the wrapped `Learner` supports
  internal validation, `NULL` otherwise.

## Methods

Methods inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## See also

https://mlr-org.com/pipeops.html

Other PipeOps:
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEncodePL.md),
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md),
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreprocSimple.md),
[`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops.md),
[`mlr_pipeops_adas`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_adas.md),
[`mlr_pipeops_blsmote`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_blsmote.md),
[`mlr_pipeops_boxcox`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_boxcox.md),
[`mlr_pipeops_branch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_branch.md),
[`mlr_pipeops_chunk`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_chunk.md),
[`mlr_pipeops_classbalancing`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classbalancing.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_classweights`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classweights.md),
[`mlr_pipeops_colapply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_colapply.md),
[`mlr_pipeops_collapsefactors`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_collapsefactors.md),
[`mlr_pipeops_colroles`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_colroles.md),
[`mlr_pipeops_copy`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_copy.md),
[`mlr_pipeops_datefeatures`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_datefeatures.md),
[`mlr_pipeops_decode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_decode.md),
[`mlr_pipeops_encode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encode.md),
[`mlr_pipeops_encodeimpact`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encodeimpact.md),
[`mlr_pipeops_encodelmer`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encodelmer.md),
[`mlr_pipeops_encodeplquantiles`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encodeplquantiles.md),
[`mlr_pipeops_encodepltree`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_encodepltree.md),
[`mlr_pipeops_featureunion`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_featureunion.md),
[`mlr_pipeops_filter`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_filter.md),
[`mlr_pipeops_fixfactors`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_fixfactors.md),
[`mlr_pipeops_histbin`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_histbin.md),
[`mlr_pipeops_ica`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ica.md),
[`mlr_pipeops_imputeconstant`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeconstant.md),
[`mlr_pipeops_imputehist`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputehist.md),
[`mlr_pipeops_imputelearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputelearner.md),
[`mlr_pipeops_imputemean`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemean.md),
[`mlr_pipeops_imputemedian`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemedian.md),
[`mlr_pipeops_imputemode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemode.md),
[`mlr_pipeops_imputeoor`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeoor.md),
[`mlr_pipeops_imputesample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputesample.md),
[`mlr_pipeops_info`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_info.md),
[`mlr_pipeops_isomap`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_isomap.md),
[`mlr_pipeops_kernelpca`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_kernelpca.md),
[`mlr_pipeops_learner_pi_cvplus`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_pi_cvplus.md),
[`mlr_pipeops_learner_quantiles`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_quantiles.md),
[`mlr_pipeops_missind`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_missind.md),
[`mlr_pipeops_modelmatrix`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_modelmatrix.md),
[`mlr_pipeops_multiplicityexply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_multiplicityexply.md),
[`mlr_pipeops_multiplicityimply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_multiplicityimply.md),
[`mlr_pipeops_mutate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_mutate.md),
[`mlr_pipeops_nearmiss`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_nearmiss.md),
[`mlr_pipeops_nmf`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_nmf.md),
[`mlr_pipeops_nop`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_nop.md),
[`mlr_pipeops_ovrsplit`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrsplit.md),
[`mlr_pipeops_ovrunite`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrunite.md),
[`mlr_pipeops_pca`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_pca.md),
[`mlr_pipeops_proxy`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_proxy.md),
[`mlr_pipeops_quantilebin`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_quantilebin.md),
[`mlr_pipeops_randomprojection`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_randomprojection.md),
[`mlr_pipeops_randomresponse`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_randomresponse.md),
[`mlr_pipeops_regravg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_regravg.md),
[`mlr_pipeops_removeconstants`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_removeconstants.md),
[`mlr_pipeops_renamecolumns`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_renamecolumns.md),
[`mlr_pipeops_replicate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_replicate.md),
[`mlr_pipeops_rowapply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_rowapply.md),
[`mlr_pipeops_scale`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_scale.md),
[`mlr_pipeops_scalemaxabs`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_scalemaxabs.md),
[`mlr_pipeops_scalerange`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_scalerange.md),
[`mlr_pipeops_select`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_select.md),
[`mlr_pipeops_smote`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_smote.md),
[`mlr_pipeops_smotenc`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_smotenc.md),
[`mlr_pipeops_spatialsign`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_spatialsign.md),
[`mlr_pipeops_subsample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_subsample.md),
[`mlr_pipeops_targetinvert`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targetinvert.md),
[`mlr_pipeops_targetmutate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targetmutate.md),
[`mlr_pipeops_targettrafoscalerange`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targettrafoscalerange.md),
[`mlr_pipeops_textvectorizer`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_textvectorizer.md),
[`mlr_pipeops_threshold`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_threshold.md),
[`mlr_pipeops_tomek`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_tomek.md),
[`mlr_pipeops_tunethreshold`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_tunethreshold.md),
[`mlr_pipeops_unbranch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_unbranch.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_updatetarget.md),
[`mlr_pipeops_vtreat`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_vtreat.md),
[`mlr_pipeops_yeojohnson`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_yeojohnson.md)

Other Meta PipeOps:
[`mlr_pipeops_learner_cv`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_cv.md),
[`mlr_pipeops_learner_pi_cvplus`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_pi_cvplus.md),
[`mlr_pipeops_learner_quantiles`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_quantiles.md)

## Examples

``` r
library("mlr3")

task = tsk("iris")
learner = lrn("classif.rpart", cp = 0.1)
lrn_po = mlr_pipeops$get("learner", learner)

lrn_po$train(list(task))
#> $output
#> NULL
#> 
lrn_po$predict(list(task))
#> $output
#> 
#> ── <PredictionClassif> for 150 observations: ───────────────────────────────────
#>  row_ids     truth  response
#>        1    setosa    setosa
#>        2    setosa    setosa
#>        3    setosa    setosa
#>      ---       ---       ---
#>      148 virginica virginica
#>      149 virginica virginica
#>      150 virginica virginica
#> 
```
