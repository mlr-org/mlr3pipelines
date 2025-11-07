# Wrap a Learner into a PipeOp to predict multiple Quantiles

Wraps a
[`LearnerRegr`](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
into a [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)
to predict multiple quantiles.

`PipeOpLearnerQuantiles` only supports
[`LearnerRegr`](https://mlr3.mlr-org.com/reference/LearnerRegr.html)s
that have `quantiles` as a possible `pedict_type`.

It produces quantile-based predictions for multiple quantiles in one
[`PredictionRegr`](https://mlr3.mlr-org.com/reference/Prediction.html).
This is especially helpful if the
[`LearnerRegr`](https://mlr3.mlr-org.com/reference/LearnerRegr.html) can
only predict one quantile (like for example `LearnerRegrGBM` in
`mlr3extralearners`)

Inherits the `$param_set` (and therefore `$param_set$values`) from the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) it is
constructed from.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Construction

    PipeOpLearnerQuantiles$new(learner, id = NULL, param_vals = list())

- `learner` ::
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) \|
  `character(1)`  
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) to wrap,
  or a string identifying a
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) in the
  [`mlr3::mlr_learners`](https://mlr3.mlr-org.com/reference/mlr_learners.html)
  [`Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html).
  The [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) has
  to be a
  [`LearnerRegr`](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
  with `predict_type` `"quantiles"`. This argument is always cloned; to
  access the
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) inside
  `PipeOpLearnerQuantiles` by-reference, use `$learner`.

- `id` :: `character(1)` Identifier of the resulting object, internally
  defaulting to the `id` of the
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) being
  wrapped.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

`PipeOpLearnerQuantiles` has one input channel named `"input"`, taking a
[`TaskRegr`](https://mlr3.mlr-org.com/reference/TaskRegr.html) specific
to the [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) type
given to `learner` during construction; both during training and
prediction.

`PipeOpLearnerQuantiles` has one output channel named `"output"`,
producing `NULL` during training and a
[`PredictionRegr`](https://mlr3.mlr-org.com/reference/Prediction.html)
object during prediction.

The output during prediction is a
[`PredictionRegr`](https://mlr3.mlr-org.com/reference/PredictionRegr.html)
on the prediction input data that aggregates all `result`s produced by
the [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) for
each quantile in `quantiles`. trained on the training input data.

## State

The `$state` is set during training. It is a named `list` with the
member:

- `model_states` :: `list`  
  List of the states of all models created by the
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)'s
  `$.train()` function.

## Parameters

The parameters are exactly the parameters of the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) wrapped by
this object.

- `q_vals` :: `numeric`  
  Quantiles to use for training and prediction. Initialized to
  `c(0.05, 0.5, 0.95)`

- `q_response` :: `numeric(1)`  
  Which quantile in `quantiles` to use as a `response` for the
  [`PredictionRegr`](https://mlr3.mlr-org.com/reference/PredictionRegr.html)
  during prediction. Initialized to `0.5`.

## Internals

The `$state` is updated during training.

## Fields

Fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md), as
well as:

- `learner` ::
  [`LearnerRegr`](https://mlr3.mlr-org.com/reference/LearnerRegr.html)  
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) that is
  being wrapped. Read-only.

- `learner_model` ::
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)  
  If `PipeOpLearnerQuantiles` has been trained, this is a `list`
  containing the
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)s for each
  quantile. Otherwise, this contains the
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) that is
  being wrapped. Read-only.

- `predict_type` :: `character(1)`  
  Predict type of the `PipeOpLearnerQuantiles`, which is always
  `"response" "quantiles"`.

## Methods

Methods inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## See also

https://mlr-org.com/pipeops.html

Other PipeOps:
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md),
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md),
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.md),
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/reference/PipeOpImpute.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md),
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreprocSimple.md),
[`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops.md),
[`mlr_pipeops_adas`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_adas.md),
[`mlr_pipeops_blsmote`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_blsmote.md),
[`mlr_pipeops_boxcox`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_boxcox.md),
[`mlr_pipeops_branch`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_branch.md),
[`mlr_pipeops_chunk`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_chunk.md),
[`mlr_pipeops_classbalancing`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classbalancing.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_classweights`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classweights.md),
[`mlr_pipeops_colapply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_colapply.md),
[`mlr_pipeops_collapsefactors`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_collapsefactors.md),
[`mlr_pipeops_colroles`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_colroles.md),
[`mlr_pipeops_copy`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_copy.md),
[`mlr_pipeops_datefeatures`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_datefeatures.md),
[`mlr_pipeops_decode`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_decode.md),
[`mlr_pipeops_encode`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encode.md),
[`mlr_pipeops_encodeimpact`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodeimpact.md),
[`mlr_pipeops_encodelmer`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodelmer.md),
[`mlr_pipeops_encodeplquantiles`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodeplquantiles.md),
[`mlr_pipeops_encodepltree`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodepltree.md),
[`mlr_pipeops_featureunion`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_featureunion.md),
[`mlr_pipeops_filter`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_filter.md),
[`mlr_pipeops_fixfactors`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_fixfactors.md),
[`mlr_pipeops_histbin`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_histbin.md),
[`mlr_pipeops_ica`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ica.md),
[`mlr_pipeops_imputeconstant`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputeconstant.md),
[`mlr_pipeops_imputehist`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputehist.md),
[`mlr_pipeops_imputelearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputelearner.md),
[`mlr_pipeops_imputemean`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputemean.md),
[`mlr_pipeops_imputemedian`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputemedian.md),
[`mlr_pipeops_imputemode`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputemode.md),
[`mlr_pipeops_imputeoor`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputeoor.md),
[`mlr_pipeops_imputesample`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_imputesample.md),
[`mlr_pipeops_info`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_info.md),
[`mlr_pipeops_isomap`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_isomap.md),
[`mlr_pipeops_kernelpca`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_kernelpca.md),
[`mlr_pipeops_learner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.md),
[`mlr_pipeops_learner_pi_cvplus`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner_pi_cvplus.md),
[`mlr_pipeops_missind`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_missind.md),
[`mlr_pipeops_modelmatrix`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_modelmatrix.md),
[`mlr_pipeops_multiplicityexply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityexply.md),
[`mlr_pipeops_multiplicityimply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityimply.md),
[`mlr_pipeops_mutate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_mutate.md),
[`mlr_pipeops_nearmiss`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_nearmiss.md),
[`mlr_pipeops_nmf`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_nmf.md),
[`mlr_pipeops_nop`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_nop.md),
[`mlr_pipeops_ovrsplit`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrsplit.md),
[`mlr_pipeops_ovrunite`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrunite.md),
[`mlr_pipeops_pca`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_pca.md),
[`mlr_pipeops_proxy`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_proxy.md),
[`mlr_pipeops_quantilebin`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_quantilebin.md),
[`mlr_pipeops_randomprojection`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_randomprojection.md),
[`mlr_pipeops_randomresponse`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_randomresponse.md),
[`mlr_pipeops_regravg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_regravg.md),
[`mlr_pipeops_removeconstants`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_removeconstants.md),
[`mlr_pipeops_renamecolumns`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_renamecolumns.md),
[`mlr_pipeops_replicate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_replicate.md),
[`mlr_pipeops_rowapply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_rowapply.md),
[`mlr_pipeops_scale`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_scale.md),
[`mlr_pipeops_scalemaxabs`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_scalemaxabs.md),
[`mlr_pipeops_scalerange`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_scalerange.md),
[`mlr_pipeops_select`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_select.md),
[`mlr_pipeops_smote`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_smote.md),
[`mlr_pipeops_smotenc`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_smotenc.md),
[`mlr_pipeops_spatialsign`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_spatialsign.md),
[`mlr_pipeops_subsample`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_subsample.md),
[`mlr_pipeops_targetinvert`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targetinvert.md),
[`mlr_pipeops_targetmutate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targetmutate.md),
[`mlr_pipeops_targettrafoscalerange`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_targettrafoscalerange.md),
[`mlr_pipeops_textvectorizer`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_textvectorizer.md),
[`mlr_pipeops_threshold`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_threshold.md),
[`mlr_pipeops_tomek`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_tomek.md),
[`mlr_pipeops_tunethreshold`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_tunethreshold.md),
[`mlr_pipeops_unbranch`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_unbranch.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_updatetarget.md),
[`mlr_pipeops_vtreat`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_vtreat.md),
[`mlr_pipeops_yeojohnson`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_yeojohnson.md)

Other Meta PipeOps:
[`mlr_pipeops_learner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.md),
[`mlr_pipeops_learner_cv`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner_cv.md),
[`mlr_pipeops_learner_pi_cvplus`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner_pi_cvplus.md)

## Examples

``` r
library("mlr3")

task = tsk("boston_housing")
learner = lrn("regr.debug")
po = mlr_pipeops$get("learner_quantiles", learner)

po$train(list(task))
#> $output
#> NULL
#> 
po$predict(list(task))
#> $output
#> 
#> ── <PredictionRegr> for 506 observations: ──────────────────────────────────────
#>  row_ids truth q0.05 q0.5 q0.95 response
#>        1  24.0  10.2 21.2  43.4     21.2
#>        2  21.6  10.2 21.2  43.4     21.2
#>        3  34.7  10.2 21.2  43.4     21.2
#>      ---   ---   ---  ---   ---      ---
#>      504  23.9  10.2 21.2  43.4     21.2
#>      505  22.0  10.2 21.2  43.4     21.2
#>      506  19.0  10.2 21.2  43.4     21.2
#> 
```
