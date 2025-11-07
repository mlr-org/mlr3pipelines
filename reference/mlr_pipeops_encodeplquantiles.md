# Piecewise Linear Encoding using Quantiles

Encodes `numeric` and `integer` feature columns using piecewise lienar
encoding. For details, see documentation of
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md)
or Gorishniy et al. (2022).

Bins are constructed by taking the quantiles of the respective feature
column as bin boundaries. The first and last boundaries are set to the
minimum and maximum value of the feature, respectively. The number of
bins can be controlled with the `numsplits` hyperparameter. Affected
feature columns may contain `NA`s. These are ignored when calculating
quantiles.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md)/[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreprocSimple.md)/[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Construction

    PipeOpEncodePLQuantiles$new(id = "encodeplquantiles", param_vals = list())

- `id` :: `character(1)`  
  Identifier of resulting object, default `"encodeplquantiles"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md).

The output is the input
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) with all affected
`numeric` and `integer` columns encoded using piecewise linear encoding
with bins being derived from the quantiles of the respective original
feature column.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md)/[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md).

## Parameters

The parameters are the parameters inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md),
as well as:

- `numsplits` :: `integer(1)`  
  Number of bins to create. Initialized to `2`.

- `type` :: `integer(1)`  
  Method used to calculate sample quantiles. See help of
  [`stats::quantile`](https://rdrr.io/r/stats/quantile.html). Default is
  `7`.

## Internals

This overloads the `private$.get_bins()` method of
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md)
and uses the [`stats::quantile`](https://rdrr.io/r/stats/quantile.html)
function to derive the bins used for piecewise linear encoding.

## Fields

Only fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Methods

Only methods inherited from
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md)/[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## References

Gorishniy Y, Rubachev I, Babenko A (2022). “On Embeddings for Numerical
Features in Tabular Deep Learning.” In *Advances in Neural Information
Processing Systems*, volume 35, 24991–25004.
<https://proceedings.neurips.cc/paper_files/paper/2022/hash/9e9f0ffc3d836836ca96cbf8fe14b105-Abstract-Conference.html>.

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

Other Piecewise Linear Encoding PipeOps:
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md),
[`mlr_pipeops_encodepltree`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodepltree.md)

## Examples

``` r
library(mlr3)

task = tsk("iris")$select(c("Petal.Width", "Petal.Length"))
pop = po("encodeplquantiles")

train_out = pop$train(list(task))[[1L]]
# Calculated bin boundaries per feature
pop$state$bins
#> $Petal.Length
#> [1] 1.00 4.35 6.90
#> 
#> $Petal.Width
#> [1] 0.1 1.3 2.5
#> 
# Each feature was split into two encoded features using piecewise linear encoding
train_out$head()
#>    Species Petal.Length.bin1 Petal.Length.bin2 Petal.Width.bin1
#>     <fctr>             <num>             <num>            <num>
#> 1:  setosa        0.11940299                 0       0.08333333
#> 2:  setosa        0.11940299                 0       0.08333333
#> 3:  setosa        0.08955224                 0       0.08333333
#> 4:  setosa        0.14925373                 0       0.08333333
#> 5:  setosa        0.11940299                 0       0.08333333
#> 6:  setosa        0.20895522                 0       0.25000000
#>    Petal.Width.bin2
#>               <num>
#> 1:                0
#> 2:                0
#> 3:                0
#> 4:                0
#> 5:                0
#> 6:                0

# Prediction works the same as training, using the bins learned during training
predict_out = pop$predict(list(task))[[1L]]
predict_out$head()
#>    Species Petal.Length.bin1 Petal.Length.bin2 Petal.Width.bin1
#>     <fctr>             <num>             <num>            <num>
#> 1:  setosa        0.11940299                 0       0.08333333
#> 2:  setosa        0.11940299                 0       0.08333333
#> 3:  setosa        0.08955224                 0       0.08333333
#> 4:  setosa        0.14925373                 0       0.08333333
#> 5:  setosa        0.11940299                 0       0.08333333
#> 6:  setosa        0.20895522                 0       0.25000000
#>    Petal.Width.bin2
#>               <num>
#> 1:                0
#> 2:                0
#> 3:                0
#> 4:                0
#> 5:                0
#> 6:                0

# Binning into three bins per feature
# Using the nearest even order statistic for caluclating quantiles
pop$param_set$set_values(numsplits = 4, type = 3)

train_out = pop$train(list(task))[[1L]]
# Calculated bin boundaries per feature
pop$state$bins
#> $Petal.Length
#> [1] 1.0 1.6 4.3 5.1 6.9
#> 
#> $Petal.Width
#> [1] 0.1 0.3 1.3 1.8 2.5
#> 
# Each feature was split into three encoded features using
# piecewise linear encoding
train_out$head()
#>    Species Petal.Length.bin1 Petal.Length.bin2 Petal.Length.bin3
#>     <fctr>             <num>             <num>             <num>
#> 1:  setosa         0.6666667        0.00000000                 0
#> 2:  setosa         0.6666667        0.00000000                 0
#> 3:  setosa         0.5000000        0.00000000                 0
#> 4:  setosa         0.8333333        0.00000000                 0
#> 5:  setosa         0.6666667        0.00000000                 0
#> 6:  setosa         1.0000000        0.03703704                 0
#>    Petal.Length.bin4 Petal.Width.bin1 Petal.Width.bin2 Petal.Width.bin3
#>                <num>            <num>            <num>            <num>
#> 1:                 0              0.5              0.0                0
#> 2:                 0              0.5              0.0                0
#> 3:                 0              0.5              0.0                0
#> 4:                 0              0.5              0.0                0
#> 5:                 0              0.5              0.0                0
#> 6:                 0              1.0              0.1                0
#>    Petal.Width.bin4
#>               <num>
#> 1:                0
#> 2:                0
#> 3:                0
#> 4:                0
#> 5:                0
#> 6:                0
```
