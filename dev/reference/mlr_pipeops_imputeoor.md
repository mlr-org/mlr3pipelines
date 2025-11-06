# Out of Range Imputation

Impute factorial features by adding a new level `".MISSING"`.

Impute numerical features by constant values shifted below the minimum
or above the maximum by using \\min(x) - offset - multiplier \*
diff(range(x))\\ or \\max(x) + offset + multiplier \* diff(range(x))\\.

This type of imputation is especially sensible in the context of
tree-based methods, see also Ding & Simonoff (2010).

[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)s expect
input [`Task`](https://mlr3.mlr-org.com/reference/Task.html)s to have
the same `factor` (or `ordered`) levels during training as well as
prediction. This `PipeOp` modifies the levels of `factor` and `ordered`
features, and since it may occur that a `factor` or `ordered` feature
contains missing values only during prediction, but not during training,
the output `Task` could also have different levels during the two
stages.

To avoid problems with the `Learner`s' expectation, controlling the
`PipeOp`s' handling of this edge-case is necessary. For this, use the
`create_empty_level` hyperparameter inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md).  
If `create_empty_level` is set to `TRUE`, then an unseen level
`".MISSING"` is added to the feature during training and missing values
are imputed as `".MISSING"` during prediction. However, empty factor
levels during training can be a problem for many
[`Learners`](https://mlr3.mlr-org.com/reference/Learner.html).  
If `create_empty_level` is set to `FALSE`, then no empty level is
introduced during training, but columns that have missing values only
during prediction will *not* be imputed. This is why it may still be
necessary to use
[`po("imputesample", affect_columns = selector_type(types = c("factor", "ordered")))`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputesample.md)
(or another imputation method) after this imputation method. Note that
setting `create_empty_level` to `FALSE` is the same as setting it to
`TRUE` and using
[`PipeOpFixFactors`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_fixfactors.md)
after this `PipeOp`.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Construction

    PipeOpImputeOOR$new(id = "imputeoor", param_vals = list())

- `id` :: `character(1)`  
  Identifier of resulting object, default `"imputeoor"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md).

The output is the input
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) with all affected
features having missing values imputed as described above.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md).

The `$state$model` contains either `".MISSING"` used for `character` and
`factor` (also `ordered`) features or `numeric(1)` indicating the
constant value used for imputation of `integer` and `numeric` features.

## Parameters

The parameters are the parameters inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md),
as well as:

- `min` :: `logical(1)`  
  Should `integer` and `numeric` features be shifted below the minimum?
  Initialized to `TRUE`. If `FALSE` they are shifted above the maximum.
  See also the description above.

- `offset` :: `numeric(1)`  
  Numerical non-negative offset as used in the description above for
  `integer` and `numeric` features. Initialized to `1`.

- `multiplier` :: `numeric(1)`  
  Numerical non-negative multiplier as used in the description above for
  `integer` and `numeric` features. Initialized to `1`.

## Internals

Adds an explicit new `level()` to `factor` and `ordered` features, but
not to `character` features. For `integer` and `numeric` features uses
the `min`, `max`, `diff` and `range` functions. `integer` and `numeric`
features that are entirely `NA` are imputed as `0`. `factor` and
`ordered` features that are entirely `NA` are imputed as `".MISSING"`.

## Fields

Only fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Methods

Only methods inherited from
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## References

Ding Y, Simonoff JS (2010). “An Investigation of Missing Data Methods
for Classification Trees Applied to Binary Response Data.” *Journal of
Machine Learning Research*, **11**(6), 131-170.
<https://jmlr.org/papers/v11/ding10a.html>.

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
[`mlr_pipeops_tunethreshold`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_tunethreshold.md),
[`mlr_pipeops_unbranch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_unbranch.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_updatetarget.md),
[`mlr_pipeops_vtreat`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_vtreat.md),
[`mlr_pipeops_yeojohnson`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_yeojohnson.md)

Other Imputation PipeOps:
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md),
[`mlr_pipeops_imputeconstant`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeconstant.md),
[`mlr_pipeops_imputehist`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputehist.md),
[`mlr_pipeops_imputelearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputelearner.md),
[`mlr_pipeops_imputemean`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemean.md),
[`mlr_pipeops_imputemedian`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemedian.md),
[`mlr_pipeops_imputemode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemode.md),
[`mlr_pipeops_imputesample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputesample.md)

## Examples

``` r
library("mlr3")
set.seed(2409)
data = tsk("pima")$data()
data$y = factor(c(NA, sample(letters, size = 766, replace = TRUE), NA))
data$z = ordered(c(NA, sample(1:10, size = 767, replace = TRUE)))
task = TaskClassif$new("task", backend = data, target = "diabetes")
task$missings()
#> diabetes      age  glucose  insulin     mass pedigree pregnant pressure 
#>        0        0        5      374       11        0        0       35 
#>  triceps        y        z 
#>      227        2        1 
po = po("imputeoor")
new_task = po$train(list(task = task))[[1]]
new_task$missings()
#> diabetes      age pedigree pregnant  glucose  insulin     mass pressure 
#>        0        0        0        0        0        0        0        0 
#>  triceps        y        z 
#>        0        0        0 
new_task$data()
#>      diabetes   age pedigree pregnant glucose insulin  mass pressure triceps
#>        <fctr> <num>    <num>    <num>   <num>   <num> <num>    <num>   <num>
#>   1:      pos    50    0.627        6     148    -819  33.6       72      35
#>   2:      neg    31    0.351        1      85    -819  26.6       66      29
#>   3:      pos    32    0.672        8     183    -819  23.3       64     -86
#>   4:      neg    21    0.167        1      89      94  28.1       66      23
#>   5:      pos    33    2.288        0     137     168  43.1       40      35
#>  ---                                                                        
#> 764:      neg    63    0.171       10     101     180  32.9       76      48
#> 765:      neg    27    0.340        2     122    -819  36.8       70      27
#> 766:      neg    30    0.245        5     121     112  26.2       72      23
#> 767:      pos    47    0.349        1     126    -819  30.1       60     -86
#> 768:      neg    23    0.315        1      93    -819  30.4       70      31
#>             y        z
#>        <fctr>    <ord>
#>   1: .MISSING .MISSING
#>   2:        l        9
#>   3:        q        6
#>   4:        f        3
#>   5:        l        3
#>  ---                  
#> 764:        o        7
#> 765:        n        5
#> 766:        e        6
#> 767:        c        8
#> 768: .MISSING        9

# recommended use when missing values are expected during prediction on
# factor columns that had no missing values during training
gr = po("imputeoor", create_empty_level = FALSE) %>>%
  po("imputesample", affect_columns = selector_type(types = c("factor", "ordered")))
t1 = as_task_classif(data.frame(l = as.ordered(letters[1:3]), t = letters[1:3]), target = "t")
t2 = as_task_classif(data.frame(l = as.ordered(c("a", NA, NA)), t = letters[1:3]), target = "t")
gr$train(t1)[[1]]$data()
#>         t     l
#>    <fctr> <ord>
#> 1:      a     a
#> 2:      b     b
#> 3:      c     c

# missing values during prediction are sampled randomly
gr$predict(t2)[[1]]$data()
#>         t     l
#>    <fctr> <ord>
#> 1:      a     a
#> 2:      b     c
#> 3:      c     c
```
