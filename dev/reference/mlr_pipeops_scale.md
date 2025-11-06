# Center and Scale Numeric Features

Centers all numeric features to mean = 0 (if `center` parameter is
`TRUE`) and scales them by dividing them by their root-mean-square (if
`scale` parameter is `TRUE`).

The root-mean-square here is defined as `sqrt(sum(x^2)/(length(x)-1))`.
If the `center` parameter is `TRUE`, this corresponds to the
[`sd()`](https://rdrr.io/r/stats/sd.html).

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Construction

    PipeOpScale$new(id = "scale", param_vals = list())

- `id` :: `character(1)`  
  Identifier of resulting object, default `"scale"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md).

The output is the input
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) with all affected
numeric parameters centered and/or scaled.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
as well as:

- `center` :: `numeric`  
  The mean / median (depending on `robust`) of each numeric feature
  during training, or 0 if `center` is `FALSE`. Will be subtracted
  during the predict phase.

- `scale` :: `numeric`  
  The value by which features are divided. 1 if `scale` is `FALSE`  
  If `robust` is `FALSE`, this is the root mean square, defined as
  `sqrt(sum(x^2)/(length(x)-1))`, of each feature, possibly after
  centering. If `robust` is `TRUE`, this is the median absolute
  deviation multiplied by 1.4826 (see
  [stats::mad](https://rdrr.io/r/stats/mad.html)) of each feature,
  possibly after centering. This is 1 for features that are constant
  during training if `center` is `TRUE`, to avoid division-by-zero.

## Parameters

The parameters are the parameters inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
as well as:

- `center` :: `logical(1)`  
  Whether to center features, i.e. subtract their
  [`mean()`](https://rdrr.io/r/base/mean.html) from them. Default
  `TRUE`.

- `scale` :: `logical(1)`  
  Whether to scale features, i.e. divide them by
  `sqrt(sum(x^2)/(length(x)-1))`. Default `TRUE`.

- `robust` :: `logical(1)`  
  Whether to use robust scaling; instead of scaling / centering with
  mean / standard deviation, median and median absolute deviation
  [`mad`](https://rdrr.io/r/stats/mad.html) are used. Initialized to
  `FALSE`.

## Internals

Imitates the [`scale()`](https://rdrr.io/r/base/scale.html) function for
`robust = FALSE` and alternatively subtracts the `median` and divides by
[`mad`](https://rdrr.io/r/stats/mad.html) for `robust = TRUE`.

## Fields

Only fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Methods

Only methods inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

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

## Examples

``` r
library("mlr3")

task = tsk("iris")
pos = po("scale")

pos$train(list(task))[[1]]$data()
#>        Species Petal.Length Petal.Width Sepal.Length Sepal.Width
#>         <fctr>        <num>       <num>        <num>       <num>
#>   1:    setosa   -1.3357516  -1.3110521  -0.89767388  1.01560199
#>   2:    setosa   -1.3357516  -1.3110521  -1.13920048 -0.13153881
#>   3:    setosa   -1.3923993  -1.3110521  -1.38072709  0.32731751
#>   4:    setosa   -1.2791040  -1.3110521  -1.50149039  0.09788935
#>   5:    setosa   -1.3357516  -1.3110521  -1.01843718  1.24503015
#>  ---                                                            
#> 146: virginica    0.8168591   1.4439941   1.03453895 -0.13153881
#> 147: virginica    0.7035638   0.9192234   0.55148575 -1.27867961
#> 148: virginica    0.8168591   1.0504160   0.79301235 -0.13153881
#> 149: virginica    0.9301544   1.4439941   0.43072244  0.78617383
#> 150: virginica    0.7602115   0.7880307   0.06843254 -0.13153881

one_line_of_iris = task$filter(13)

one_line_of_iris$data()
#>    Species Petal.Length Petal.Width Sepal.Length Sepal.Width
#>     <fctr>        <num>       <num>        <num>       <num>
#> 1:  setosa          1.4         0.1          4.8           3

pos$predict(list(one_line_of_iris))[[1]]$data()
#>    Species Petal.Length Petal.Width Sepal.Length Sepal.Width
#>     <fctr>        <num>       <num>        <num>       <num>
#> 1:  setosa    -1.335752   -1.442245    -1.259964  -0.1315388
```
