# Piecewise Linear Encoding using Decision Trees

Encodes `numeric` and `integer` feature columns using piecewise lienar
encoding. For details, see documentation of
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md)
or Gorishniy et al. (2022).

Bins are constructed by trainig one decision tree
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) per feature
column, taking the target column into account, and using decision
boundaries as bin boundaries.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md)/[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreprocSimple.md)/[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Construction

    PipeOpEncodePLTree$new(task_type, id = "encodepltree", param_vals = list())

- `task_type` :: `character(1)`  
  The class of [`Task`](https://mlr3.mlr-org.com/reference/Task.html)
  that should be accepted as input, given as a `character(1)`. This is
  used to construct the appropriate
  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) to be
  used for obtaining the bins for piecewise linear encoding. Supported
  options are `"TaskClassif"`for
  [`LearnerClassifRpart`](https://mlr3.mlr-org.com/reference/mlr_learners_classif.rpart.html)
  or `"TaskRegr"`for
  [`LearnerRegrRpart`](https://mlr3.mlr-org.com/reference/mlr_learners_regr.rpart.html).

- `id` :: `character(1)`  
  Identifier of resulting object, default `"encodeplquantiles"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md).
Instead of a [`Task`](https://mlr3.mlr-org.com/reference/Task.html), a
[`TaskClassif`](https://mlr3.mlr-org.com/reference/TaskClassif.html) or
[`TaskRegr`](https://mlr3.mlr-org.com/reference/TaskRegr.html) is used
as input and output during training and prediction, depending on the
`task_type` construction argument.

The output is the input
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) with all affected
`numeric` and `integer` columns encoded using piecewise linear encoding
with bins being derived from a decision tree
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) trained on
the respective feature column.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md)/[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md).

## Parameters

The parameters are the parameters inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md),
as well as the parameters of the
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) used for
obtaining the bins for piecewise linear encoding.

## Internals

This overloads the `private$.get_bins()` method of
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md).
To derive the bins for each feature, the
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) is split into
smaller [`Tasks`](https://mlr3.mlr-org.com/reference/Task.html) with
only the target and respective feature as columns. On these
[`Tasks`](https://mlr3.mlr-org.com/reference/Task.html) either a
[`LearnerClassifRpart`](https://mlr3.mlr-org.com/reference/mlr_learners_classif.rpart.html)
or
[`LearnerRegrRpart`](https://mlr3.mlr-org.com/reference/mlr_learners_regr.rpart.html)
gets trained and the respective splits extracted as bin boundaries used
for piecewise linear encodings.

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
[`mlr_pipeops_encodeplquantiles`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodeplquantiles.md),
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
[`mlr_pipeops_encodeplquantiles`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_encodeplquantiles.md)

## Examples

``` r
library(mlr3)

# For classification task
task = tsk("iris")$select(c("Petal.Width", "Petal.Length"))
pop = po("encodepltree", task_type = "TaskClassif")
train_out = pop$train(list(task))[[1L]]

# Calculated bin boundaries per feature
pop$state$bins
#> $Petal.Length
#> [1] 1.00 2.45 4.75 6.90
#> 
#> $Petal.Width
#> [1] 0.10 0.80 1.75 2.50
#> 
# Each feature was split into three encoded features using piecewise linear encoding
train_out$head()
#>    Species Petal.Length.bin1 Petal.Length.bin2 Petal.Length.bin3
#>     <fctr>             <num>             <num>             <num>
#> 1:  setosa         0.2758621                 0                 0
#> 2:  setosa         0.2758621                 0                 0
#> 3:  setosa         0.2068966                 0                 0
#> 4:  setosa         0.3448276                 0                 0
#> 5:  setosa         0.2758621                 0                 0
#> 6:  setosa         0.4827586                 0                 0
#>    Petal.Width.bin1 Petal.Width.bin2 Petal.Width.bin3
#>               <num>            <num>            <num>
#> 1:        0.1428571                0                0
#> 2:        0.1428571                0                0
#> 3:        0.1428571                0                0
#> 4:        0.1428571                0                0
#> 5:        0.1428571                0                0
#> 6:        0.4285714                0                0

# Prediction works the same as training, using the bins learned during training
predict_out = pop$predict(list(task))[[1L]]
predict_out$head()
#>    Species Petal.Length.bin1 Petal.Length.bin2 Petal.Length.bin3
#>     <fctr>             <num>             <num>             <num>
#> 1:  setosa         0.2758621                 0                 0
#> 2:  setosa         0.2758621                 0                 0
#> 3:  setosa         0.2068966                 0                 0
#> 4:  setosa         0.3448276                 0                 0
#> 5:  setosa         0.2758621                 0                 0
#> 6:  setosa         0.4827586                 0                 0
#>    Petal.Width.bin1 Petal.Width.bin2 Petal.Width.bin3
#>               <num>            <num>            <num>
#> 1:        0.1428571                0                0
#> 2:        0.1428571                0                0
#> 3:        0.1428571                0                0
#> 4:        0.1428571                0                0
#> 5:        0.1428571                0                0
#> 6:        0.4285714                0                0

# Controlling behavior of the tree learner, here: setting minimum number of
# observations per node for a split to be attempted
pop$param_set$set_values(minsplit = 5)

train_out = pop$train(list(task))[[1L]]
# feature "hp" now gets split into five encoded features instead of three
pop$state$bins
#> $Petal.Length
#> [1] 1.00 2.45 4.75 6.90
#> 
#> $Petal.Width
#> [1] 0.10 0.80 1.75 2.50
#> 
train_out$head()
#>    Species Petal.Length.bin1 Petal.Length.bin2 Petal.Length.bin3
#>     <fctr>             <num>             <num>             <num>
#> 1:  setosa         0.2758621                 0                 0
#> 2:  setosa         0.2758621                 0                 0
#> 3:  setosa         0.2068966                 0                 0
#> 4:  setosa         0.3448276                 0                 0
#> 5:  setosa         0.2758621                 0                 0
#> 6:  setosa         0.4827586                 0                 0
#>    Petal.Width.bin1 Petal.Width.bin2 Petal.Width.bin3
#>               <num>            <num>            <num>
#> 1:        0.1428571                0                0
#> 2:        0.1428571                0                0
#> 3:        0.1428571                0                0
#> 4:        0.1428571                0                0
#> 5:        0.1428571                0                0
#> 6:        0.4285714                0                0

# For regression task
task = tsk("mtcars")$select(c("cyl", "hp"))
pop = po("encodepltree", task_type = "TaskRegr")
train_out = pop$train(list(task))[[1L]]

# Calculated bin boundaries per feature
pop$state$bins
#> $cyl
#> [1] 4 5 7 8
#> 
#> $hp
#> [1]  52 118 335
#> 
# First feature was split into three encoded features,
# second into two, using piecewise linear encoding
train_out$head()
#>      mpg cyl.bin1 cyl.bin2 cyl.bin3   hp.bin1   hp.bin2
#>    <num>    <num>    <num>    <num>     <num>     <num>
#> 1:  21.0        1      0.5        0 0.8787879 0.0000000
#> 2:  21.0        1      0.5        0 0.8787879 0.0000000
#> 3:  22.8        0      0.0        0 0.6212121 0.0000000
#> 4:  21.4        1      0.5        0 0.8787879 0.0000000
#> 5:  18.7        1      1.0        1 1.0000000 0.2626728
#> 6:  18.1        1      0.5        0 0.8030303 0.0000000
```
