# Unite Binary Classification Tasks

Perform "One vs. Rest" classification by (weighted) majority vote
prediction from [classification
Predictions](https://mlr3.mlr-org.com/reference/PredictionClassif.html).
This works in combination with
[`PipeOpOVRSplit`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrsplit.md).

Weights can be set as a parameter; if none are provided, defaults to
equal weights for each prediction.

Always returns a `"prob"` prediction, regardless of the incoming
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)'s
`$predict_type`. The label of the class with the highest predicted
probability is selected as the `"response"` prediction.

Missing values during prediction are treated as each class label being
equally likely.

This [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)
uses a
[`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)
input, which is created by
[`PipeOpOVRSplit`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrsplit.md)
and causes
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s on
the way to this
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) to be
called once for each individual [binary
Task](https://mlr3.mlr-org.com/reference/TaskClassif.html).

Note that
[`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)
is currently an experimental features and the implementation or UI may
change.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) inheriting from
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Construction

    PipeOpOVRUnite$new(id = "ovrunite", param_vals = list())

- `id` :: `character(1)`  
  Identifier of the resulting object, default `"ovrunite"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.md).
Instead of a
[`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html), a
[`PredictionClassif`](https://mlr3.mlr-org.com/reference/PredictionClassif.html)
is used as input and output during prediction and
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.md)'s
`collect` parameter is initialized with `TRUE` to allow for collecting a
[`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)
input.

## State

The `$state` is left empty
([`list()`](https://rdrr.io/r/base/list.html)).

## Parameters

The parameters are the parameters inherited from the
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.md).

## Internals

Inherits from
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.md)
by implementing the `private$.predict()` method.

Should be used in combination with
[`PipeOpOVRSplit`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrsplit.md).

## Fields

Only fields inherited from
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Methods

Only methods inherited from
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

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
[`mlr_pipeops_learner_quantiles`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner_quantiles.md),
[`mlr_pipeops_missind`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_missind.md),
[`mlr_pipeops_modelmatrix`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_modelmatrix.md),
[`mlr_pipeops_multiplicityexply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityexply.md),
[`mlr_pipeops_multiplicityimply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityimply.md),
[`mlr_pipeops_mutate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_mutate.md),
[`mlr_pipeops_nearmiss`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_nearmiss.md),
[`mlr_pipeops_nmf`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_nmf.md),
[`mlr_pipeops_nop`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_nop.md),
[`mlr_pipeops_ovrsplit`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrsplit.md),
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

Other Ensembles:
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.md),
[`mlr_learners_avg`](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_avg.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_regravg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_regravg.md)

Other Multiplicity PipeOps:
[`Multiplicity()`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md),
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_featureunion`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_featureunion.md),
[`mlr_pipeops_multiplicityexply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityexply.md),
[`mlr_pipeops_multiplicityimply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityimply.md),
[`mlr_pipeops_ovrsplit`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrsplit.md),
[`mlr_pipeops_regravg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_regravg.md),
[`mlr_pipeops_replicate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_replicate.md)

Other Experimental Features:
[`Multiplicity()`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md),
[`mlr_pipeops_multiplicityexply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityexply.md),
[`mlr_pipeops_multiplicityimply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityimply.md),
[`mlr_pipeops_ovrsplit`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrsplit.md),
[`mlr_pipeops_replicate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_replicate.md)

## Examples

``` r
library(mlr3)
task = tsk("iris")
gr = po("ovrsplit") %>>% lrn("classif.rpart") %>>% po("ovrunite")
gr$train(task)
#> $ovrunite.output
#> NULL
#> 
gr$predict(task)
#> $ovrunite.output
#> 
#> ── <PredictionClassif> for 150 observations: ───────────────────────────────────
#>  row_ids     truth  response prob.setosa prob.versicolor prob.virginica
#>        1    setosa    setosa           1               0              0
#>        2    setosa    setosa           1               0              0
#>        3    setosa    setosa           1               0              0
#>      ---       ---       ---         ---             ---            ---
#>      148 virginica virginica           0               0              1
#>      149 virginica virginica           0               0              1
#>      150 virginica virginica           0               0              1
#> 
gr$pipeops$classif.rpart$learner$predict_type = "prob"
gr$predict(task)
#> $ovrunite.output
#> 
#> ── <PredictionClassif> for 150 observations: ───────────────────────────────────
#>  row_ids     truth  response prob.setosa prob.versicolor prob.virginica
#>        1    setosa    setosa   0.9541284      0.00000000     0.04587156
#>        2    setosa    setosa   0.9541284      0.00000000     0.04587156
#>        3    setosa    setosa   0.9541284      0.00000000     0.04587156
#>      ---       ---       ---         ---             ---            ---
#>      148 virginica virginica   0.0000000      0.02173913     0.97826087
#>      149 virginica virginica   0.0000000      0.02173913     0.97826087
#>      150 virginica virginica   0.0000000      0.02173913     0.97826087
#> 
```
