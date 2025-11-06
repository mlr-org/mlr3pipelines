# Transform Columns by Constructing a Model Matrix

Transforms columns using a given `formula` using the
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
function.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreprocSimple.md)/[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Construction

    PipeOpModelMatrix$new(id = "modelmatrix", param_vals = list())

- `id` :: `character(1)`  
  Identifier of resulting object, default `"modelmatrix"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md).

The output is the input
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) with transformed
columns according to the used `formula`.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md).

## Parameters

The parameters are the parameters inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
as well as:

- `formula` :: `formula`  
  Formula to use. Higher order interactions can be created using
  constructs like `~. ^ 2`. By default, an `(Intercept)` column of all
  `1`s is created, which can be avoided by adding `0 +` to the term. See
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html).

## Internals

Uses the [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
function.

## Fields

Only fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Methods

Only methods inherited from
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreprocSimple.md)/[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

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

## Examples

``` r
library("mlr3")

task = tsk("iris")
pop = po("modelmatrix", formula = ~ .  ^ 2)

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
pop$train(list(task))[[1]]$data()
#>        Species (Intercept) Petal.Length Petal.Width Sepal.Length Sepal.Width
#>         <fctr>       <num>        <num>       <num>        <num>       <num>
#>   1:    setosa           1          1.4         0.2          5.1         3.5
#>   2:    setosa           1          1.4         0.2          4.9         3.0
#>   3:    setosa           1          1.3         0.2          4.7         3.2
#>   4:    setosa           1          1.5         0.2          4.6         3.1
#>   5:    setosa           1          1.4         0.2          5.0         3.6
#>  ---                                                                        
#> 146: virginica           1          5.2         2.3          6.7         3.0
#> 147: virginica           1          5.0         1.9          6.3         2.5
#> 148: virginica           1          5.2         2.0          6.5         3.0
#> 149: virginica           1          5.4         2.3          6.2         3.4
#> 150: virginica           1          5.1         1.8          5.9         3.0
#>      Petal.Length:Petal.Width Petal.Length:Sepal.Length
#>                         <num>                     <num>
#>   1:                     0.28                      7.14
#>   2:                     0.28                      6.86
#>   3:                     0.26                      6.11
#>   4:                     0.30                      6.90
#>   5:                     0.28                      7.00
#>  ---                                                   
#> 146:                    11.96                     34.84
#> 147:                     9.50                     31.50
#> 148:                    10.40                     33.80
#> 149:                    12.42                     33.48
#> 150:                     9.18                     30.09
#>      Petal.Length:Sepal.Width Petal.Width:Sepal.Length Petal.Width:Sepal.Width
#>                         <num>                    <num>                   <num>
#>   1:                     4.90                     1.02                    0.70
#>   2:                     4.20                     0.98                    0.60
#>   3:                     4.16                     0.94                    0.64
#>   4:                     4.65                     0.92                    0.62
#>   5:                     5.04                     1.00                    0.72
#>  ---                                                                          
#> 146:                    15.60                    15.41                    6.90
#> 147:                    12.50                    11.97                    4.75
#> 148:                    15.60                    13.00                    6.00
#> 149:                    18.36                    14.26                    7.82
#> 150:                    15.30                    10.62                    5.40
#>      Sepal.Length:Sepal.Width
#>                         <num>
#>   1:                    17.85
#>   2:                    14.70
#>   3:                    15.04
#>   4:                    14.26
#>   5:                    18.00
#>  ---                         
#> 146:                    20.10
#> 147:                    15.75
#> 148:                    19.50
#> 149:                    21.08
#> 150:                    17.70

pop$param_set$values$formula = ~ 0 + . ^ 2

pop$train(list(task))[[1]]$data()
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
#>      Petal.Length:Petal.Width Petal.Length:Sepal.Length
#>                         <num>                     <num>
#>   1:                     0.28                      7.14
#>   2:                     0.28                      6.86
#>   3:                     0.26                      6.11
#>   4:                     0.30                      6.90
#>   5:                     0.28                      7.00
#>  ---                                                   
#> 146:                    11.96                     34.84
#> 147:                     9.50                     31.50
#> 148:                    10.40                     33.80
#> 149:                    12.42                     33.48
#> 150:                     9.18                     30.09
#>      Petal.Length:Sepal.Width Petal.Width:Sepal.Length Petal.Width:Sepal.Width
#>                         <num>                    <num>                   <num>
#>   1:                     4.90                     1.02                    0.70
#>   2:                     4.20                     0.98                    0.60
#>   3:                     4.16                     0.94                    0.64
#>   4:                     4.65                     0.92                    0.62
#>   5:                     5.04                     1.00                    0.72
#>  ---                                                                          
#> 146:                    15.60                    15.41                    6.90
#> 147:                    12.50                    11.97                    4.75
#> 148:                    15.60                    13.00                    6.00
#> 149:                    18.36                    14.26                    7.82
#> 150:                    15.30                    10.62                    5.40
#>      Sepal.Length:Sepal.Width
#>                         <num>
#>   1:                    17.85
#>   2:                    14.70
#>   3:                    15.04
#>   4:                    14.26
#>   5:                    18.00
#>  ---                         
#> 146:                    20.10
#> 147:                    15.75
#> 148:                    19.50
#> 149:                    21.08
#> 150:                    17.70
```
