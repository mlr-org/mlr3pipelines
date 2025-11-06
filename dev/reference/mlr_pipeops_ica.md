# Independent Component Analysis

Extracts statistically independent components from data. Only affects
numerical features. See
[fastICA::fastICA](https://rdrr.io/pkg/fastICA/man/fastICA.html) for
details.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Construction

    PipeOpICA$new(id = "ica", param_vals = list())

- `id` :: `character(1)`  
  Identifier of resulting object, default `"ica"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md).

The output is the input
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) with all affected
numeric parameters replaced by independent components.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
as well as the elements of the function
[`fastICA::fastICA()`](https://rdrr.io/pkg/fastICA/man/fastICA.html),
with the exception of the `$X` and `$S` slots. These are in particular:

- `K` :: `matrix`  
  Matrix that projects data onto the first `n.comp` principal
  components. See
  [`fastICA()`](https://rdrr.io/pkg/fastICA/man/fastICA.html).

- `W` :: `matrix`  
  Estimated un-mixing matrix. See
  [`fastICA()`](https://rdrr.io/pkg/fastICA/man/fastICA.html).

- `A` :: `matrix`  
  Estimated mixing matrix. See
  [`fastICA()`](https://rdrr.io/pkg/fastICA/man/fastICA.html).

- `center` :: `numeric`  
  The mean of each numeric feature during training.

## Parameters

The parameters are the parameters inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
as well as the following parameters based on
[`fastICA()`](https://rdrr.io/pkg/fastICA/man/fastICA.html):

- `n.comp` :: `numeric(1)`  
  Number of components to extract. Default is `NULL`, which sets it to
  the number of available numeric columns.

- `alg.typ`:: `character(1)`  
  Algorithm type. One of "parallel" (default) or "deflation".

- `fun` :: `character(1)`  
  One of "logcosh" (default) or "exp".

- `alpha` :: `numeric(1)`  
  In range `[1, 2]`, Used for negentropy calculation when `fun` is
  "logcosh". Default is 1.0.

- `method` :: `character(1)`  
  Internal calculation method. "C" (default) or "R". See
  [`fastICA()`](https://rdrr.io/pkg/fastICA/man/fastICA.html).

- `row.norm` :: `logical(1)`  
  Logical value indicating whether rows should be standardized
  beforehand. Default is `FALSE`.

- `maxit` :: `numeric(1)`  
  Maximum number of iterations. Default is 200.

- `tol` :: `numeric(1)`  
  Tolerance for convergence, default is `1e-4`.

- `verbose` `logical(1)`  
  Logical value indicating the level of output during the run of the
  algorithm. Default is `FALSE`.

- `w.init`:: `matrix`  
  Initial un-mixing matrix. See
  [`fastICA()`](https://rdrr.io/pkg/fastICA/man/fastICA.html). Default
  is `NULL`.

## Internals

Uses the [`fastICA()`](https://rdrr.io/pkg/fastICA/man/fastICA.html)
function.

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
[`mlr_pipeops_imputeconstant`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeconstant.md),
[`mlr_pipeops_imputehist`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputehist.md),
[`mlr_pipeops_imputelearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputelearner.md),
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

## Examples

``` r
library("mlr3")

task = tsk("iris")
pop = po("ica")

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
#>        Species          V1          V2         V3         V4
#>         <fctr>       <num>       <num>      <num>      <num>
#>   1:    setosa  0.37500059  0.02943659  1.3924519 -0.2614936
#>   2:    setosa -0.97225996 -0.08167029  1.3294610 -0.3857646
#>   3:    setosa -0.34913589 -0.15929558  1.3492190  0.3572738
#>   4:    setosa -0.38004875  0.31177235  1.2048972  0.8805817
#>   5:    setosa  0.73635117  0.10791837  1.3728500  0.2002559
#>  ---                                                        
#> 146: virginica -0.31596226 -2.59266317 -0.8292284 -1.2709946
#> 147: virginica -1.36041153 -1.05409554 -0.7570305 -0.7271920
#> 148: virginica  0.09333273 -0.98343101 -0.8277795 -0.3432144
#> 149: virginica  1.39423603 -1.76937234 -1.0485595  1.2489928
#> 150: virginica  0.57294721  0.04233626 -0.8751853  1.5511628

pop$state
#> $K
#>            [,1]       [,2]       [,3]      [,4]
#> [1,] -0.4180098  0.3531217  0.2735163  3.118456
#> [2,] -0.1748261  0.1537381  1.9583093 -4.897992
#> [3,] -0.1763375 -1.3373258 -2.0881803 -2.050340
#> [4,]  0.0412425 -1.4871770  2.1451574  2.077869
#> 
#> $W
#>             [,1]         [,2]        [,3]      [,4]
#> [1,] -0.06938934 -0.012593052  0.98684804 0.1454561
#> [2,] -0.79805659  0.003084436 -0.14237669 0.5855126
#> [3,]  0.47946944 -0.596461056 -0.06824042 0.6400676
#> [4,]  0.35832924  0.802537295 -0.03468520 0.4757426
#> 
#> $A
#>             [,1]        [,2]        [,3]        [,4]
#> [1,]  0.09053775  0.05276721  0.21348058  0.37160785
#> [2,]  0.06895140 -0.17444975  0.06613582 -0.06320762
#> [3,] -1.74870327 -0.73625778 -0.67223450  0.20890405
#> [4,] -0.15680615 -0.04289852 -0.42340889 -0.05462953
#> 
#> $center
#> Petal.Length  Petal.Width Sepal.Length  Sepal.Width 
#>     3.758000     1.199333     5.843333     3.057333 
#> 
#> $dt_columns
#> [1] "Petal.Length" "Petal.Width"  "Sepal.Length" "Sepal.Width" 
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
#>        id    type
#>    <char>  <char>
#> 1:     V1 numeric
#> 2:     V2 numeric
#> 3:     V3 numeric
#> 4:     V4 numeric
#> 
#> $outtaskshell
#> Empty data.table (0 rows and 5 cols): Species,V1,V2,V3,V4
#> 
```
