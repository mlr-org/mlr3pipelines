# Impute Features by Fitting a Learner

Impute features by fitting a
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) for each
feature. Uses the features indicated by the `context_columns` parameter
as features to train the imputation
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html). Note this
parameter is part of the
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md)
base class and explained there.

Additionally, only features supported by the learner can be imputed;
i.e. learners of type `regr` can only impute features of type `integer`
and `numeric`, while `classif` can impute features of type `factor`,
`ordered` and `logical`.

The [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) used
for imputation is trained on all `context_columns`; if these contain
missing values, the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) typically
either needs to be able to handle missing values itself, or needs to do
its own imputation (see examples).

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Construction

    PipeOpImputeLearner$new(learner, id = NULL, param_vals = list())

- `id` :: `character(1)`  
  Identifier of resulting object, default `"impute."`, followed by the
  `id` of the `Learner`.

- `learner` ::
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) \|
  `character(1)`
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) to wrap,
  or a string identifying a
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) in the
  [`mlr3::mlr_learners`](https://mlr3.mlr-org.com/reference/mlr_learners.html)
  [`Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html).
  The [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)
  usually needs to be able to handle missing values, i.e. have the
  `missings` property, unless care is taken that `context_columns` do
  not contain missings; see examples.  
  This argument is always cloned; to access the
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) inside
  `PipeOpImputeLearner` by-reference, use `$learner`.  

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md).

The output is the input
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) with missing
values from all affected features imputed by the trained model.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md).

The `$state$models` is a named `list` of `models` created by the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)'s
`$.train()` function for each column. If a column consists of missing
values only during training, the `model` is `0` or the levels of the
feature; these are used for sampling during prediction.

This state is given the class `"pipeop_impute_learner_state"`.

## Parameters

The parameters are the parameters inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md),
in addition to the parameters of the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) used for
imputation.

## Internals

Uses the `$train` and `$predict` functions of the provided learner.
Features that are entirely `NA` are imputed as `0` or randomly sampled
from available (`factor` / `logical`) levels.

The [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) does
*not* necessarily need to handle missing values in cases where
`context_columns` is chosen well (or there is only one column with
missing values present).

## Fields

Fields inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
as well as:

- `learner` ::
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)  
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) that is
  being wrapped. Read-only.

- `learner_models` :: `list` of
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) \|
  `NULL`  
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) that is
  being wrapped. This list is named by features for which a `Learner`
  was fitted, and contains the same `Learner`, but with different
  respective models for each feature. If this `PipeOp` is not trained,
  this is an empty `list`. For features that were entirely `NA` during
  training, the `list` contains `NULL` elements.

## Methods

Only methods inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

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
[`mlr_pipeops_imputemean`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemean.md),
[`mlr_pipeops_imputemedian`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemedian.md),
[`mlr_pipeops_imputemode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemode.md),
[`mlr_pipeops_imputeoor`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeoor.md),
[`mlr_pipeops_imputesample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputesample.md),
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

Other Imputation PipeOps:
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md),
[`mlr_pipeops_imputeconstant`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeconstant.md),
[`mlr_pipeops_imputehist`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputehist.md),
[`mlr_pipeops_imputemean`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemean.md),
[`mlr_pipeops_imputemedian`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemedian.md),
[`mlr_pipeops_imputemode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemode.md),
[`mlr_pipeops_imputeoor`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeoor.md),
[`mlr_pipeops_imputesample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputesample.md)

## Examples

``` r
library("mlr3")

task = tsk("pima")
task$missings()
#> diabetes      age  glucose  insulin     mass pedigree pregnant pressure 
#>        0        0        5      374       11        0        0       35 
#>  triceps 
#>      227 

po = po("imputelearner", lrn("regr.rpart"))
new_task = po$train(list(task = task))[[1]]
new_task$missings()
#> diabetes      age pedigree pregnant  glucose  insulin     mass pressure 
#>        0        0        0        0        0        0        0        0 
#>  triceps 
#>        0 

# '$state' of the "regr.rpart" Learner, trained to predict the 'mass' column:
po$state$model$mass
#> $model
#> n= 757 
#> 
#> node), split, n, deviance, yval
#>       * denotes terminal node
#> 
#>  1) root 757 36254.3300 32.45746  
#>    2) triceps< 25.5 219  5537.6560 27.93196  
#>      4) triceps< 20.5 144  3140.7800 26.68333 *
#>      5) triceps>=20.5 75  1741.3150 30.32933  
#>       10) pressure< 83 64  1081.6090 29.37813 *
#>       11) pressure>=83 11   264.8855 35.86364 *
#>    3) triceps>=25.5 538 24405.7800 34.29963  
#>      6) triceps< 35.5 380 14414.2500 32.50474  
#>       12) pressure< 74.5 223  6772.1180 31.49013  
#>         24) glucose< 73.5 8    44.1000 24.20000 *
#>         25) glucose>=73.5 215  6287.0300 31.76140  
#>           50) pregnant>=0.5 190  4822.6790 31.28947 *
#>           51) pregnant< 0.5 25  1100.4420 35.34800 *
#>       13) pressure>=74.5 157  7086.5100 33.94586  
#>         26) insulin< 187 122  4736.5000 33.05656 *
#>         27) insulin>=187 35  1917.2070 37.04571 *
#>      7) triceps>=35.5 158  5822.9770 38.61646  
#>       14) pregnant>=1.5 92  2351.3170 37.02174 *
#>       15) pregnant< 1.5 66  2911.5580 40.83939 *
#> 
#> $param_vals
#> $param_vals$xval
#> [1] 0
#> 
#> 
#> $log
#> Empty data.table (0 rows and 3 cols): stage,class,msg
#> 
#> $train_time
#> [1] 0.003
#> 
#> $task_hash
#> [1] "7682038aa5360fe2"
#> 
#> $feature_names
#> [1] "age"      "glucose"  "insulin"  "pedigree" "pregnant" "pressure" "triceps" 
#> 
#> $validate
#> NULL
#> 
#> $mlr3_version
#> [1] ‘1.2.0’
#> 
#> $data_prototype
#> Empty data.table (0 rows and 8 cols): .impute_col,age,glucose,insulin,pedigree,pregnant...
#> 
#> $task_prototype
#> Empty data.table (0 rows and 8 cols): .impute_col,age,glucose,insulin,pedigree,pregnant...
#> 
#> $train_task
#> 
#> ── <TaskRegr> (768x8) ──────────────────────────────────────────────────────────
#> • Target: .impute_col
#> • Properties: -
#> • Features (7):
#>   • dbl (7): age, glucose, insulin, pedigree, pregnant, pressure, triceps
#> 
#> attr(,"class")
#> [1] "learner_state" "list"         

library("mlr3learners")
# To use the "regr.lm" Learner, prefix it with its own imputation method!
# The "imputehist" PipeOp is used to train "regr.lm"; predictions of this
# trained Learner are then used to impute the missing values in the Task.
po = po("imputelearner",
  po("imputehist") %>>% lrn("regr.lm")
)

new_task = po$train(list(task = task))[[1]]
new_task$missings()
#> diabetes      age pedigree pregnant  glucose  insulin     mass pressure 
#>        0        0        0        0        0        0        0        0 
#>  triceps 
#>        0 
```
