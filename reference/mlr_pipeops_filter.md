# Feature Filtering

Feature filtering using a
[`mlr3filters::Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html)
object, see the
[mlr3filters](https://CRAN.R-project.org/package=mlr3filters) package.

If a `Filter` can only operate on a subset of columns based on column
type, then only these features are considered and filtered. `nfeat` and
`frac` will count for the features of the type that the `Filter` can
operate on; this means e.g. that setting `nfeat` to 0 will only remove
features of the type that the `Filter` can work with.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreprocSimple.md)/[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Construction

    PipeOpFilter$new(filter, id = filter$id, param_vals = list())

- `filter` ::
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html)  
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html) used
  for feature filtering. This argument is always cloned; to access the
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html)
  inside `PipeOpFilter` by-reference, use `$filter`.  

- `id` :: `character(1)`  
  Identifier of the resulting object, defaulting to the `id` of the
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html)
  being used.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md).

The output is the input
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) with features
removed that were filtered out.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md),
as well as:

- `scores` :: named `numeric`  
  Scores calculated for all features of the training
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) which are being
  used as cutoff for feature filtering. If `frac` or `nfeat` is given,
  the underlying
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html) may
  choose to not calculate scores for all features that are given. This
  only includes features on which the
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html) can
  operate; e.g. if the
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html) can
  only operate on numeric features, then scores for factorial features
  will not be given.

- `features` :: `character`  
  Names of features that are being kept. Features of types that the
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html) can
  not operate on are always being kept.

## Parameters

The parameters are the parameters inherited from the
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md),
as well as the parameters of the
[`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html) used
by this object. Besides, parameters introduced are:

- `filter.nfeat` :: `numeric(1)`  
  Number of features to select. Mutually exclusive with `frac`,
  `cutoff`, and `permuted`.

- `filter.frac` :: `numeric(1)`  
  Fraction of features to keep. Mutually exclusive with `nfeat`,
  `cutoff`, and `permuted`.

- `filter.cutoff` :: `numeric(1)`  
  Minimum value of filter heuristic for which to keep features. Mutually
  exclusive with `nfeat`, `frac`, and `permuted`.

- `filter.permuted` :: `integer(1)`  
  If this parameter is set, a random permutation of each feature is
  added to the task before applying the filter. All features selected
  before the `permuted`-th permuted features is selected are kept. This
  is similar to the approach in Wu (2007) and Thomas (2017). Mutually
  exclusive with `nfeat`, `frac`, and `cutoff`.

Note that at least one of `filter.nfeat`, `filter.frac`,
`filter.cutoff`, and `filter.permuted` must be given.

## Internals

This does *not* use the `$.select_cols` feature of
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md)
to select only features compatible with the
[`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html);
instead the whole [`Task`](https://mlr3.mlr-org.com/reference/Task.html)
is used by `private$.get_state()` and subset internally.

## Fields

Fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md), as
well as:

- `filter` ::
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html)  
  [`Filter`](https://mlr3filters.mlr-org.com/reference/Filter.html) that
  is being used for feature filtering. Do *not* use this slot to get to
  the feature filtering scores after training; instead, use
  `$state$scores`. Read-only.

## Methods

Methods inherited from
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreprocSimple.md)/[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## References

Wu Y, Boos DD, Stefanski LA (2007). “Controlling Variable Selection by
the Addition of Pseudovariables.” *Journal of the American Statistical
Association*, **102**(477), 235–243.
[doi:10.1198/016214506000000843](https://doi.org/10.1198/016214506000000843)
.

Thomas J, Hepp T, Mayr A, Bischl B (2017). “Probing for Sparse and Fast
Variable Selection with Model-Based Boosting.” *Computational and
Mathematical Methods in Medicine*, **2017**, 1–8.
[doi:10.1155/2017/1421409](https://doi.org/10.1155/2017/1421409) .

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

## Examples

``` r
library("mlr3")
library("mlr3filters")

# setup PipeOpFilter to keep the 5 most important
# features of the spam task w.r.t. their AUC
task = tsk("spam")
filter = flt("auc")
po = po("filter", filter = filter)
po$param_set
#> <ParamSetCollection(5)>
#>                 id    class lower upper nlevels        default  value
#>             <char>   <char> <num> <num>   <num>         <list> <list>
#> 1:    filter.nfeat ParamInt     0   Inf     Inf <NoDefault[0]> [NULL]
#> 2:     filter.frac ParamDbl     0     1     Inf <NoDefault[0]> [NULL]
#> 3:   filter.cutoff ParamDbl  -Inf   Inf     Inf <NoDefault[0]> [NULL]
#> 4: filter.permuted ParamInt     1   Inf     Inf <NoDefault[0]> [NULL]
#> 5:  affect_columns ParamUty    NA    NA     Inf  <Selector[1]> [NULL]
po$param_set$values$filter.nfeat = 5

# filter the task
filtered_task = po$train(list(task))[[1]]

# filtered task + extracted AUC scores
filtered_task$feature_names
#> [1] "capitalAve"      "capitalLong"     "charDollar"      "charExclamation"
#> [5] "your"           
head(po$state$scores, 10)
#> charExclamation     capitalLong      capitalAve            your      charDollar 
#>       0.3290461       0.3041626       0.2882004       0.2801659       0.2721394 
#>    capitalTotal            free             our             you          remove 
#>       0.2622801       0.2327285       0.2109325       0.2104681       0.2031303 

# feature selection embedded in a 3-fold cross validation
# keep 30% of features based on their AUC score
task = tsk("spam")
gr = po("filter", filter = flt("auc"), filter.frac = 0.5) %>>%
  po("learner", lrn("classif.rpart"))
learner = GraphLearner$new(gr)
rr = resample(task, learner, rsmp("holdout"), store_models = TRUE)
rr$learners[[1]]$model$auc$scores
#>   charExclamation       capitalLong        capitalAve              your 
#>       0.328759080       0.306751518       0.288703808       0.279083484 
#>        charDollar      capitalTotal              free               you 
#>       0.273296589       0.263313380       0.230684960       0.215739152 
#>               our            remove             money                hp 
#>       0.213188551       0.204291119       0.179973626       0.176336364 
#>               all            num000          business              over 
#>       0.176079754       0.157094167       0.149369524       0.140389728 
#>              mail            george               hpl          internet 
#>       0.136271572       0.135293399       0.133370707       0.131148231 
#>           receive             email           address             order 
#>       0.129122806       0.127931640       0.127513404       0.113377057 
#>              make           num1999          charHash            credit 
#>       0.106704315       0.102212867       0.101519423       0.096648263 
#>            people              will         addresses              labs 
#>       0.090943952       0.089247316       0.073828362       0.073427396 
#>            num650             num85               edu               lab 
#>       0.068441105       0.067097612       0.062289775       0.060257929 
#>        technology            telnet           meeting              data 
#>       0.054653251       0.052352619       0.052065676       0.048215864 
#>                pm            report           project            num857 
#>       0.041177580       0.038103133       0.037171012       0.034819678 
#> charSquarebracket            num415          original        conference 
#>       0.033739215       0.032761485       0.029735304       0.024965106 
#>     charSemicolon                cs              font                re 
#>       0.024724216       0.024623477       0.023837706       0.023239245 
#>  charRoundbracket            direct             num3d             table 
#>       0.020505763       0.012178452       0.008840974       0.005753906 
#>             parts 
#>       0.001509991 
```
