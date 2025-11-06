# Tune the Threshold of a Classification Prediction

Tunes optimal probability thresholds over different
[`PredictionClassif`](https://mlr3.mlr-org.com/reference/PredictionClassif.html)s.

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html)
`predict_type`: `"prob"` is required. Thresholds for each learner are
optimized using the
[`Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html)
supplied via the `param_set`. Defaults to
[`GenSA`](https://rdrr.io/pkg/GenSA/man/GenSA.html). Returns a single
[`PredictionClassif`](https://mlr3.mlr-org.com/reference/PredictionClassif.html).

This PipeOp should be used in conjunction with
[`PipeOpLearnerCV`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_cv.md)
in order to optimize thresholds of cross-validated predictions. In order
to optimize thresholds without cross-validation, use
[`PipeOpLearnerCV`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner_cv.md)
in conjunction with
[`ResamplingInsample`](https://mlr3.mlr-org.com/reference/mlr_resamplings_insample.html).

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Construction

    PipeOpTuneThreshold$new(id = "tunethreshold", param_vals = list())

- `id` :: `character(1)`  
  Identifier of resulting object. Default: "tunethreshold".

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## State

The `$state` is a named `list` with elements

- `thresholds` :: `numeric`  
  Learned thresholds;

## Parameters

The parameters are the parameters inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
as well as:

- `measure` ::
  [`Measure`](https://mlr3.mlr-org.com/reference/Measure.html) \|
  `character`  
  [`Measure`](https://mlr3.mlr-org.com/reference/Measure.html) to
  optimize for. Will be converted to a
  [`Measure`](https://mlr3.mlr-org.com/reference/Measure.html) in case
  it is `character`. Initialized to `"classif.ce"`, i.e.
  misclassification error.

- `optimizer` ::
  [`Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html)\|`character(1)`  
  [`Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html) used
  to find optimal thresholds. If `character`, converts to
  [`Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html) via
  [`opt`](https://bbotk.mlr-org.com/reference/opt.html). Initialized to
  `OptimizerGenSA`.

- `log_level` :: `character(1)` \| `integer(1)`  
  Set a temporary log-level for `lgr::get_logger("mlr3/bbotk")`.
  Initialized to: "warn".

## Internals

Uses the `optimizer` provided as a `param_val` in order to find an
optimal threshold. See the `optimizer` parameter for more info.

## Fields

Fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
as well as:

- `predict_type` :: `character(1)`  
  Type of prediction to return. Either `"prob"` (default) or
  `"response"`. Setting to `"response"` should rarely be used; it may
  potentially save some memory but has no other benefits.

## Methods

Only methods inherited from
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
[`mlr_pipeops_unbranch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_unbranch.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_updatetarget.md),
[`mlr_pipeops_vtreat`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_vtreat.md),
[`mlr_pipeops_yeojohnson`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_yeojohnson.md)

## Examples

``` r
library("mlr3")

task = tsk("iris")
pop = po("learner_cv", lrn("classif.rpart", predict_type = "prob")) %>>%
  po("tunethreshold")

task$data()
#>        Species Petal.Length Petal.Width Sepal.Length Sepal.Width
#>         <fctr>        <num>       <num>        <num>       <num>
#>   1:    setosa          1.4         0.2          5.1         3.5
#>   2:    setosa          1.4         0.2          4.9         3.0
#>   3:    setosa          1.3         0.2          4.7         3.2
#>   4:    setosa          1.5         0.2          4.6         3.1
#>   5:    setosa          1.4         0.2          5.0         3.6
#>  ---                                                            
#> 146: virginica          5.2         2.3          6.7         3.0
#> 147: virginica          5.0         1.9          6.3         2.5
#> 148: virginica          5.2         2.0          6.5         3.0
#> 149: virginica          5.4         2.3          6.2         3.4
#> 150: virginica          5.1         1.8          5.9         3.0
pop$train(task)
#> OptimInstanceSingleCrit is deprecated. Use OptimInstanceBatchSingleCrit instead.
#> $tunethreshold.output
#> NULL
#> 

pop$state
#> $classif.rpart
#> $model
#> n= 150 
#> 
#> node), split, n, loss, yval, (yprob)
#>       * denotes terminal node
#> 
#> 1) root 150 100 setosa (0.33333333 0.33333333 0.33333333)  
#>   2) Petal.Length< 2.45 50   0 setosa (1.00000000 0.00000000 0.00000000) *
#>   3) Petal.Length>=2.45 100  50 versicolor (0.00000000 0.50000000 0.50000000)  
#>     6) Petal.Width< 1.75 54   5 versicolor (0.00000000 0.90740741 0.09259259) *
#>     7) Petal.Width>=1.75 46   1 virginica (0.00000000 0.02173913 0.97826087) *
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
#> [1] 0.004
#> 
#> $task_hash
#> [1] "abc694dd29a7a8ce"
#> 
#> $feature_names
#> [1] "Petal.Length" "Petal.Width"  "Sepal.Length" "Sepal.Width" 
#> 
#> $validate
#> NULL
#> 
#> $mlr3_version
#> [1] ‘1.2.0’
#> 
#> $data_prototype
#> Empty data.table (0 rows and 5 cols): Species,Petal.Length,Petal.Width,Sepal.Length,Sepal.Width
#> 
#> $task_prototype
#> Empty data.table (0 rows and 5 cols): Species,Petal.Length,Petal.Width,Sepal.Length,Sepal.Width
#> 
#> $train_task
#> 
#> ── <TaskClassif> (150x5): Iris Flowers ─────────────────────────────────────────
#> • Target: Species
#> • Target classes: setosa, versicolor, virginica
#> • Properties: multiclass
#> • Features (4):
#>   • dbl (4): Petal.Length, Petal.Width, Sepal.Length, Sepal.Width
#> 
#> $predict_method
#> [1] "full"
#> 
#> $affected_cols
#> [1] "Petal.Length" "Petal.Width"  "Sepal.Length" "Sepal.Width" 
#> 
#> $intasklayout
#> Key: <id>
#>              id    type
#>          <char>  <char>
#> 1: Petal.Length numeric
#> 2:  Petal.Width numeric
#> 3: Sepal.Length numeric
#> 4:  Sepal.Width numeric
#> 
#> $outtasklayout
#> Key: <id>
#>                               id    type
#>                           <char>  <char>
#> 1:     classif.rpart.prob.setosa numeric
#> 2: classif.rpart.prob.versicolor numeric
#> 3:  classif.rpart.prob.virginica numeric
#> 
#> $outtaskshell
#> Empty data.table (0 rows and 4 cols): Species,classif.rpart.prob.setosa,classif.rpart.prob.versicolor,classif.rpart.prob.virginica
#> 
#> attr(,"class")
#> [1] "pipeop_learner_cv_state" "learner_state"          
#> [3] "list"                   
#> 
#> $tunethreshold
#> $tunethreshold$threshold
#>     setosa versicolor  virginica 
#>  0.5852014  0.3398626  0.6166942 
#> 
#> 
```
