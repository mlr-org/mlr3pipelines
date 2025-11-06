# Linearly Transform a Numeric Target to Match Given Boundaries

Linearly transforms a numeric target of a
[`TaskRegr`](https://mlr3.mlr-org.com/reference/TaskRegr.html) so it is
between `lower` and `upper`. The formula for this is \\x' = offset + x
\* scale\\, where \\scale\\ is \\(upper - lower) / (max(x) - min(x))\\
and \\offset\\ is \\-min(x) \* scale + lower\\. The same transformation
is applied during training and prediction.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)

## Construction

    PipeOpTargetTrafoScaleRange$new(id = "targettrafoscalerange", param_vals = list())

- `id` :: `character(1)`  
  Identifier of resulting object, default `"targettrafoscalerange"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md).

## State

The `$state` is a named `list` containing the slots `$offset` and
`$scale`.

## Parameters

The parameters are the parameters inherited from
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md),
as well as:

- `lower` :: `numeric(1)`  
  Target value of smallest item of input target. Initialized to 0.

- `upper` :: `numeric(1)`  
  Target value of greatest item of input target. Initialized to 1.

## Internals

Overloads
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md)'s
`.get_state()`, `.transform()`, and `.invert()`. Should be used in
combination with
[`PipeOpTargetInvert`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targetinvert.md).

## Fields

Only fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

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
po = PipeOpTargetTrafoScaleRange$new()

po$train(list(task))
#> $fun
#> NULL
#> 
#> $output
#> 
#> ── <TaskRegr> (506x18): Boston Housing Prices ──────────────────────────────────
#> • Target: cmedv.scaled
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
#> <bytecode: 0x561cedcd5938>
#> <environment: 0x561cee032e48>
#> 
#> $output
#> 
#> ── <TaskRegr> (506x18): Boston Housing Prices ──────────────────────────────────
#> • Target: cmedv.scaled
#> • Properties: -
#> • Features (17):
#>   • dbl (12): age, b, crim, dis, indus, lat, lon, lstat, nox, ptratio, rm, zn
#>   • int (3): rad, tax, tract
#>   • fct (2): chas, town
#> 

#syntactic sugar for a graph using ppl():
ttscalerange = ppl("targettrafo", trafo_pipeop = PipeOpTargetTrafoScaleRange$new(),
  graph = PipeOpLearner$new(LearnerRegrRpart$new()))
ttscalerange$train(task)
#> $targetinvert.output
#> NULL
#> 
ttscalerange$predict(task)
#> $targetinvert.output
#> 
#> ── <PredictionRegr> for 506 observations: ──────────────────────────────────────
#>  row_ids truth response
#>        1  24.0 24.53538
#>        2  21.6 26.91481
#>        3  34.7 32.96875
#>      ---   ---      ---
#>      504  23.9 24.53538
#>      505  22.0 24.53538
#>      506  19.0 20.96400
#> 
ttscalerange$state$regr.rpart
#> $model
#> n= 506 
#> 
#> node), split, n, deviance, yval
#>       * denotes terminal node
#> 
#>  1) root 506 21.0260400 0.3895301  
#>    2) town=Arlington,Ashland,Beverly,Boston Allston-Brighton,Boston Charlestown,Boston Dorchester,Boston Downtown,Boston East Boston,Boston Forest Hills,Boston Hyde Park,Boston Mattapan,Boston North End,Boston Roxbury,Boston Savin Hill,Boston South Boston,Boston West Roxbury,Braintree,Burlington,Cambridge,Chelsea,Danvers,Dedham,Everett,Framingham,Hamilton,Hanover,Holbrook,Hull,Lynn,Malden,Marshfield,Medford,Melrose,Middleton,Millis,Nahant,Natick,Norfolk,North Reading,Norwell,Norwood,Peabody,Pembroke,Quincy,Randolph,Reading,Revere,Rockland,Salem,Sargus,Scituate,Sharon,Somerville,Stoneham,Wakefield,Walpole,Waltham,Watertown,Weymouth,Wilmington,Winthrop,Woburn 400  7.5706490 0.3174944  
#>      4) lstat>=14.4 176  1.6065670 0.2189646  
#>        8) town=Boston Charlestown,Boston East Boston,Boston Forest Hills,Boston North End,Boston Roxbury,Boston Savin Hill,Boston South Boston,Chelsea,Lynn 93  0.4849460 0.1569654 *
#>        9) town=Arlington,Beverly,Boston Allston-Brighton,Boston Dorchester,Boston Downtown,Boston Hyde Park,Boston Mattapan,Cambridge,Everett,Framingham,Malden,Medford,Middleton,Norwood,Peabody,Quincy,Revere,Salem,Somerville,Waltham,Watertown 83  0.3635828 0.2884337 *
#>      5) lstat< 14.4 224  2.9129590 0.3949107  
#>       10) lstat>=4.63 215  0.9995600 0.3787494  
#>         20) lstat>=7.765 150  0.5226398 0.3547556 *
#>         21) lstat< 7.765 65  0.1912833 0.4341197 *
#>       11) lstat< 4.63 9  0.5157443 0.7809877 *
#>    3) town=Bedford,Belmont,Boston Back Bay,Boston Beacon Hill,Brookline,Canton,Cohasset,Concord,Dover,Duxbury,Hingham,Lexington,Lincoln,Lynnfield,Manchester,Marblehead,Medfield,Milton,Needham,Newton,Sherborn,Sudbury,Swampscott,Topsfield,Wayland,Wellesley,Wenham,Weston,Westwood,Winchester 106  3.5470870 0.6613627  
#>      6) rm< 7.437 82  1.5639410 0.5961518  
#>       12) crim< 4.12641 75  0.6617538 0.5730963  
#>         24) rm< 6.727 27  0.1925600 0.4869959 *
#>         25) rm>=6.727 48  0.1564460 0.6215278 *
#>       13) crim>=4.12641 7  0.4351788 0.8431746 *
#>      7) rm>=7.437 24  0.4430451 0.8841667 *
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
#> [1] 0.008
#> 
#> $task_hash
#> [1] "88d153e8827d7aff"
#> 
#> $feature_names
#>  [1] "age"     "b"       "chas"    "crim"    "dis"     "indus"   "lat"    
#>  [8] "lon"     "lstat"   "nox"     "ptratio" "rad"     "rm"      "tax"    
#> [15] "town"    "tract"   "zn"     
#> 
#> $validate
#> NULL
#> 
#> $mlr3_version
#> [1] ‘1.2.0’
#> 
#> $data_prototype
#> Empty data.table (0 rows and 18 cols): cmedv.scaled,age,b,chas,crim,dis...
#> 
#> $task_prototype
#> Empty data.table (0 rows and 18 cols): cmedv.scaled,age,b,chas,crim,dis...
#> 
#> $train_task
#> 
#> ── <TaskRegr> (506x18): Boston Housing Prices ──────────────────────────────────
#> • Target: cmedv.scaled
#> • Properties: -
#> • Features (17):
#>   • dbl (12): age, b, crim, dis, indus, lat, lon, lstat, nox, ptratio, rm, zn
#>   • int (3): rad, tax, tract
#>   • fct (2): chas, town
#> 
#> attr(,"class")
#> [1] "learner_state" "list"         
```
