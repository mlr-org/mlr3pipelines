# Dictionary of PipeOps

A simple
[`Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).
Each
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
has an associated help page, see `mlr_pipeops_[id]`.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`mlr3misc::Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Fields

Fields inherited from
[`Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html),
as well as:

- `metainf` :: `environment`  
  Environment that stores the `metainf` argument of the `$add()` method.
  Only for internal use.

## Methods

Methods inherited from
[`Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html),
as well as:

- `add(key, value, metainf = NULL)`  
  (`character(1)`, `R6ClassGenerator`, `NULL` \| `list`)  
  Adds constructor `value` to the dictionary with key `key`, potentially
  overwriting a previously stored item. If `metainf` is not `NULL` (the
  default), it must be a `list` of arguments that will be given to the
  `value` constructor (i.e. `value$new()`) when it needs to be
  constructed for `as.data.table`
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  listing.

## S3 methods

- `as.data.table(dict)`  
  [`Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  -\>
  [`data.table::data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Returns a `data.table` with the following columns:

  - `key` :: (`character`)  
    Key with which the
    [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
    was registered to the
    [`Dictionary`](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
    using the `$add()` method.

  - `label` :: (`character`)  
    Description of the
    [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
    functionality.

  - `packages` :: (`character`)  
    Set of all required packages for the
    [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
    train and predict methods.

  - `tags` :: (`character`)  
    A set of tags associated with the
    [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
    describing its purpose.

  - `feature_types` :: (`character`)  
    Feature types the
    [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
    operates on. Is `NA` for
    [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
    that do not directly operate on a
    [Task](https://mlr3.mlr-org.com/reference/Task.html).

  - `input.num`, `output.num` :: (`integer`)  
    Number of the
    [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
    input and output channels. Is `NA` for
    [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
    which accept a varying number of input and/or output channels
    depending a construction argument. See `input` and `output` fields
    of
    [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

  - `input.type.train`, `input.type.predict`, `output.type.train`,
    `output.type.predict` :: (`character`)  
    Types that are allowed as input to or returned as output of the
    [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
    `$train()` and `$predict()` methods.  
    A value of `NULL` means that a null object, e.g. no data, is taken
    as input or being returned as output. A value of "`*`" means that
    any type is possible.  
    If both `input.type.train` and `output.type.train` or both
    `input.type.predict` and `output.type.predict` contain values
    enclosed by square brackets ("`[`", "`]`"), then the respective
    input or channel is
    [`Multiplicity`](https://mlr3pipelines.mlr-org.com/dev/reference/Multiplicity.md)-aware.
    For more information, see
    [`Multiplicity`](https://mlr3pipelines.mlr-org.com/dev/reference/Multiplicity.md).

## See also

Other mlr3pipelines backend related:
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md),
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreprocSimple.md),
[`mlr_graphs`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_graphs.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_updatetarget.md)

Other PipeOps:
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEncodePL.md),
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md),
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreprocSimple.md),
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
[`mlr_pipeops_tunethreshold`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_tunethreshold.md),
[`mlr_pipeops_unbranch`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_unbranch.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_updatetarget.md),
[`mlr_pipeops_vtreat`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_vtreat.md),
[`mlr_pipeops_yeojohnson`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_yeojohnson.md)

Other Dictionaries:
[`mlr_graphs`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_graphs.md)

## Examples

``` r
library("mlr3")

mlr_pipeops$get("learner", lrn("classif.rpart"))
#> 
#> ── PipeOp <classif.rpart>: not trained ─────────────────────────────────────────
#> Values: xval=0
#> 
#> ── Input channels: 
#>    name       train     predict
#>  <char>      <char>      <char>
#>   input TaskClassif TaskClassif
#> 
#> ── Output channels: 
#>    name  train           predict
#>  <char> <char>            <char>
#>  output   NULL PredictionClassif

# equivalent:
po("learner", learner = lrn("classif.rpart"))
#> 
#> ── PipeOp <classif.rpart>: not trained ─────────────────────────────────────────
#> Values: xval=0
#> 
#> ── Input channels: 
#>    name       train     predict
#>  <char>      <char>      <char>
#>   input TaskClassif TaskClassif
#> 
#> ── Output channels: 
#>    name  train           predict
#>  <char> <char>            <char>
#>  output   NULL PredictionClassif

# all PipeOps currently in the dictionary:
as.data.table(mlr_pipeops)[, c("key", "input.num", "output.num", "packages")]
#> Key: <key>
#>                       key input.num output.num                         packages
#>                    <char>     <int>      <int>                           <list>
#>  1:                  adas         1          1        mlr3pipelines,smotefamily
#>  2:               blsmote         1          1        mlr3pipelines,smotefamily
#>  3:                boxcox         1          1      mlr3pipelines,bestNormalize
#>  4:                branch         1         NA                    mlr3pipelines
#>  5:                 chunk         1         NA                    mlr3pipelines
#>  6:        classbalancing         1          1                    mlr3pipelines
#>  7:            classifavg        NA          1              mlr3pipelines,stats
#>  8:          classweights         1          1                    mlr3pipelines
#>  9:              colapply         1          1                    mlr3pipelines
#> 10:       collapsefactors         1          1                    mlr3pipelines
#> 11:              colroles         1          1                    mlr3pipelines
#> 12:                  copy         1         NA                    mlr3pipelines
#> 13:          datefeatures         1          1                    mlr3pipelines
#> 14:                decode         1          1                    mlr3pipelines
#> 15:                encode         1          1              mlr3pipelines,stats
#> 16:          encodeimpact         1          1                    mlr3pipelines
#> 17:            encodelmer         1          1        mlr3pipelines,lme4,nloptr
#> 18:     encodeplquantiles         1          1              mlr3pipelines,stats
#> 19:          encodepltree         1          1         mlr3pipelines,mlr3,rpart
#> 20:          featureunion        NA          1                    mlr3pipelines
#> 21:                filter         1          1                    mlr3pipelines
#> 22:            fixfactors         1          1                    mlr3pipelines
#> 23:               histbin         1          1           mlr3pipelines,graphics
#> 24:                   ica         1          1            mlr3pipelines,fastICA
#> 25:        imputeconstant         1          1                    mlr3pipelines
#> 26:            imputehist         1          1           mlr3pipelines,graphics
#> 27:         imputelearner         1          1                    mlr3pipelines
#> 28:            imputemean         1          1                    mlr3pipelines
#> 29:          imputemedian         1          1              mlr3pipelines,stats
#> 30:            imputemode         1          1                    mlr3pipelines
#> 31:             imputeoor         1          1                    mlr3pipelines
#> 32:          imputesample         1          1                    mlr3pipelines
#> 33:                  info         1          1                    mlr3pipelines
#> 34:                isomap         1          1       mlr3pipelines,dimRed,stats
#> 35:             kernelpca         1          1            mlr3pipelines,kernlab
#> 36:               learner         1          1                    mlr3pipelines
#> 37:            learner_cv         1          1                    mlr3pipelines
#> 38:     learner_pi_cvplus         1          1                    mlr3pipelines
#> 39:     learner_quantiles         1          1                    mlr3pipelines
#> 40:               missind         1          1                    mlr3pipelines
#> 41:           modelmatrix         1          1              mlr3pipelines,stats
#> 42:     multiplicityexply         1         NA                    mlr3pipelines
#> 43:     multiplicityimply        NA          1                    mlr3pipelines
#> 44:                mutate         1          1                    mlr3pipelines
#> 45:              nearmiss         1          1             mlr3pipelines,themis
#> 46:                   nmf         1          1               mlr3pipelines,MASS
#> 47:                   nop         1          1                    mlr3pipelines
#> 48:              ovrsplit         1          1                    mlr3pipelines
#> 49:              ovrunite         1          1                    mlr3pipelines
#> 50:                   pca         1          1                    mlr3pipelines
#> 51:                 proxy        NA          1                    mlr3pipelines
#> 52:           quantilebin         1          1              mlr3pipelines,stats
#> 53:      randomprojection         1          1                    mlr3pipelines
#> 54:        randomresponse         1          1                    mlr3pipelines
#> 55:               regravg        NA          1                    mlr3pipelines
#> 56:       removeconstants         1          1                    mlr3pipelines
#> 57:         renamecolumns         1          1                    mlr3pipelines
#> 58:             replicate         1          1                    mlr3pipelines
#> 59:              rowapply         1          1                    mlr3pipelines
#> 60:                 scale         1          1                    mlr3pipelines
#> 61:           scalemaxabs         1          1                    mlr3pipelines
#> 62:            scalerange         1          1                    mlr3pipelines
#> 63:                select         1          1                    mlr3pipelines
#> 64:                 smote         1          1        mlr3pipelines,smotefamily
#> 65:               smotenc         1          1             mlr3pipelines,themis
#> 66:           spatialsign         1          1                    mlr3pipelines
#> 67:             subsample         1          1                    mlr3pipelines
#> 68:          targetinvert         2          1                    mlr3pipelines
#> 69:          targetmutate         1          2                    mlr3pipelines
#> 70: targettrafoscalerange         1          2                    mlr3pipelines
#> 71:        textvectorizer         1          1 mlr3pipelines,quanteda,stopwords
#> 72:             threshold         1          1                    mlr3pipelines
#> 73:                 tomek         1          1             mlr3pipelines,themis
#> 74:         tunethreshold         1          1              mlr3pipelines,bbotk
#> 75:              unbranch        NA          1                    mlr3pipelines
#> 76:                vtreat         1          1             mlr3pipelines,vtreat
#> 77:            yeojohnson         1          1      mlr3pipelines,bestNormalize
#>                       key input.num output.num                         packages
```
