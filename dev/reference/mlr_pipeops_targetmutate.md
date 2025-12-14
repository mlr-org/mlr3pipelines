# Transform a Target by a Function

Changes the *target* of a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) according to a
function given as hyperparameter. An inverter-function that undoes the
transformation during prediction must also be given.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)

## Construction

    PipeOpTargetMutate$new(id = "targetmutate", param_vals = list(), new_task_type = NULL)

- `id` :: `character(1)`  
  Identifier of resulting object, default `"targetmutate"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

- `new_task_type` :: `character(1)` \| `NULL`  
  The task type to which the output is converted, must be one of
  `mlr_reflections$task_types$type`. Defaults to `NULL`: no change in
  task type.

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md).

## State

The `$state` is left empty
([`list()`](https://rdrr.io/r/base/list.html)).

## Parameters

The parameters are the parameters inherited from
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md),
as well as:

- `trafo` :: `function` `data.table` -\> `data.frame` \| `data.table` \|
  `matrix`  
  Transformation function for the target. Should only be a function of
  the target, i.e., taking a single `data.table` argument, typically
  with one column. The return value is used as the new target of the
  resulting [`Task`](https://mlr3.mlr-org.com/reference/Task.html). To
  change target names, change the column name of the data using e.g.
  [`setnames()`](https://rdatatable.gitlab.io/data.table/reference/setattr.html).  
  Note that this function also gets called during prediction and should
  thus gracefully handle `NA` values.  
  Initialized to [`identity()`](https://rdrr.io/r/base/identity.html).

- `inverter` :: `function` `data.table` -\> `data.table` \| named
  `list`  
  Inversion of the transformation function for the target. Called on a
  `data.table` created from a
  [`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)
  using
  [`as.data.table()`](https://rdatatable.gitlab.io/data.table/reference/as.data.table.html),
  without the `$row_ids` and `$truth` columns, and should return a
  `data.table` or named `list` that contains the new relevant slots of a
  [`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)
  subclass (e.g., `$response`, `$prob`, `$se`, ...). Initialized to
  [`identity()`](https://rdrr.io/r/base/identity.html).

## Internals

Overloads
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md)'s
`.transform()` and `.invert()` functions. Should be used in combination
with
[`PipeOpTargetInvert`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targetinvert.md).

## Fields

Fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
as well as:

- `new_task_type` :: `character(1)`  
  `new_task_type` construction argument. Read-only.

## Methods

Only methods inherited from
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

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
[`mlr_pipeops_learner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner.md),
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
[`mlr_pipeops_targettrafoscalerange`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targettrafoscalerange.md),
[`mlr_pipeops_textvectorizer`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_textvectorizer.md),
[`mlr_pipeops_threshold`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_threshold.md),
[`mlr_pipeops_tomek`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_tomek.md),
[`mlr_pipeops_tunethreshold`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_tunethreshold.md),
[`mlr_pipeops_unbranch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_unbranch.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_updatetarget.md),
[`mlr_pipeops_vtreat`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_vtreat.md),
[`mlr_pipeops_yeojohnson`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_yeojohnson.md)

## Examples

``` r
library(mlr3)
task = tsk("boston_housing")
po = PipeOpTargetMutate$new("logtrafo", param_vals = list(
  trafo = function(x) log(x, base = 2),
  inverter = function(x) list(response = 2 ^ x$response))
)
# Note that this example is ill-equipped to work with
# `predict_type == "se"` predictions.

po$train(list(task))
#> $fun
#> NULL
#> 
#> $output
#> 
#> ── <TaskRegr> (506x18): Boston Housing Prices ──────────────────────────────────
#> • Target: cmedv
#> • Properties: -
#> • Features (17):
#>   • dbl (12): age, b, crim, dis, indus, lat, lon, lstat, nox, ptratio, rm, zn
#>   • int (3): rad, tax, tract
#>   • fct (2): chas, town
#> 
po$predict(list(task))
#> $fun
#> function (inputs) 
#> {
#>     assert_list(inputs, len = 1L, types = "Prediction")
#>     list(private$.invert(inputs[[1L]], predict_phase_state))
#> }
#> <bytecode: 0x565395137360>
#> <environment: 0x56538dc93260>
#> 
#> $output
#> 
#> ── <TaskRegr> (506x18): Boston Housing Prices ──────────────────────────────────
#> • Target: cmedv
#> • Properties: -
#> • Features (17):
#>   • dbl (12): age, b, crim, dis, indus, lat, lon, lstat, nox, ptratio, rm, zn
#>   • int (3): rad, tax, tract
#>   • fct (2): chas, town
#> 

g = Graph$new()
g$add_pipeop(po)
g$add_pipeop(LearnerRegrRpart$new())
g$add_pipeop(PipeOpTargetInvert$new())
g$add_edge(src_id = "logtrafo", dst_id = "targetinvert",
  src_channel = 1, dst_channel = 1)
g$add_edge(src_id = "logtrafo", dst_id = "regr.rpart",
  src_channel = 2, dst_channel = 1)
g$add_edge(src_id = "regr.rpart", dst_id = "targetinvert",
  src_channel = 1, dst_channel = 2)

g$train(task)
#> $targetinvert.output
#> NULL
#> 
g$predict(task)
#> $targetinvert.output
#> 
#> ── <PredictionRegr> for 506 observations: ──────────────────────────────────────
#>  row_ids truth response
#>        1  24.0 22.97871
#>        2  21.6 22.97871
#>        3  34.7 33.42115
#>      ---   ---      ---
#>      504  23.9 25.44298
#>      505  22.0 25.44298
#>      506  19.0 20.16845
#> 

#syntactic sugar using ppl():
tt = ppl("targettrafo", graph = PipeOpLearner$new(LearnerRegrRpart$new()))
tt$param_set$values$targetmutate.trafo = function(x) log(x, base = 2)
tt$param_set$values$targetmutate.inverter = function(x) list(response = 2 ^ x$response)
```
