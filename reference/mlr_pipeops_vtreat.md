# Interface to the vtreat Package

Provides an interface to the vtreat package.

`PipeOpVtreat` naturally works for [classification
tasks](https://mlr3.mlr-org.com/reference/TaskClassif.html) and
[regression tasks](https://mlr3.mlr-org.com/reference/TaskRegr.html).
Internally, `PipeOpVtreat` follows the fit/prepare interface of vtreat,
i.e., first creating a data treatment transform object via
[`vtreat::NumericOutcomeTreatment()`](https://winvector.github.io/vtreat/reference/NumericOutcomeTreatment.html),
[`vtreat::BinomialOutcomeTreatment()`](https://winvector.github.io/vtreat/reference/BinomialOutcomeTreatment.html),
or
[`vtreat::MultinomialOutcomeTreatment()`](https://winvector.github.io/vtreat/reference/MultinomialOutcomeTreatment.html),
followed by calling
[`vtreat::fit_prepare()`](https://winvector.github.io/vtreat/reference/fit_prepare.html)
on the training data and
[`vtreat::prepare()`](https://winvector.github.io/vtreat/reference/prepare.html)
during predicton.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Construction

    PipeOpVtreat$new(id = "vtreat", param_vals = list())

- `id` :: `character(1)`  
  Identifier of resulting object, default `"vtreat"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md).
Instead of a [`Task`](https://mlr3.mlr-org.com/reference/Task.html), a
[`TaskSupervised`](https://mlr3.mlr-org.com/reference/TaskSupervised.html)
is used as input and output during training and prediction.

The output is the input
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) with all affected
features "prepared" by vtreat. If vtreat found "no usable vars", the
input [`Task`](https://mlr3.mlr-org.com/reference/Task.html) is returned
unaltered.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md),
as well as:

- `treatment_plan` :: object of class `vtreat_pipe_step` \| `NULL`  
  The treatment plan as constructed by vtreat based on the training
  data, i.e., an object of class `treatment_plan`. If vtreat found "no
  usable vars" and designing the treatment would have failed, this is
  `NULL`.

## Parameters

The parameters are the parameters inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md),
as well as:

- `recommended` :: `logical(1)`  
  Whether only the "recommended" prepared features should be returned,
  i.e., non constant variables with a significance value smaller than
  vtreat's threshold. Initialized to `TRUE`.

- `cols_to_copy` :: `function` \|
  [`Selector`](https://mlr3pipelines.mlr-org.com/reference/Selector.md)  
  [`Selector`](https://mlr3pipelines.mlr-org.com/reference/Selector.md)
  function, takes a
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) as argument and
  returns a [`character()`](https://rdrr.io/r/base/character.html) of
  features to copy.  
  See
  [`Selector`](https://mlr3pipelines.mlr-org.com/reference/Selector.md)
  for example functions. Initialized to
  [`selector_none()`](https://mlr3pipelines.mlr-org.com/reference/Selector.md).

- `minFraction` :: `numeric(1)`  
  Minimum frequency a categorical level must have to be converted to an
  indicator column.

- `smFactor` :: `numeric(1)`  
  Smoothing factor for impact coding models.

- `rareCount` :: `integer(1)`  
  Allow levels with this count or below to be pooled into a shared
  rare-level.

- `rareSig` :: `numeric(1)`  
  Suppress levels from pooling at this significance value greater.

- `collarProb` :: `numeric(1)`  
  What fraction of the data (pseudo-probability) to collar data at if
  `doCollar = TRUE`.

- `doCollar` :: `logical(1)`  
  If `TRUE` collar numeric variables by cutting off after a
  tail-probability specified by `collarProb` during treatment design.

- `codeRestriction` ::
  [`character()`](https://rdrr.io/r/base/character.html)  
  What types of variables to produce.

- `customCoders` :: named `list`  
  Map from code names to custom categorical variable encoding functions.

- `splitFunction` :: `function`  
  Function taking arguments nSplits, nRows, dframe, and y; returning a
  user desired split.

- `ncross` :: `integer(1)`  
  Integer larger than one, number of cross-validation rounds to design.

- `forceSplit` :: `logical(1)`  
  If `TRUE` force cross-validated significance calculations on all
  variables.

- `catScaling` :: `logical(1)`  
  If `TRUE` use [`stats::glm()`](https://rdrr.io/r/stats/glm.html)
  linkspace, if FALSE use
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) for scaling.

- `verbose` :: `logical(1)`  
  If `TRUE` print progress.

- `use_parallel` :: `logical(1)`  
  If `TRUE` use parallel methods.

- `missingness_imputation` :: `function`  
  Function of signature f(values: numeric, weights: numeric), simple
  missing value imputer.  
  Typically, an imputation via a
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)
  should be preferred, see
  [`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/reference/PipeOpImpute.md).

- `pruneSig` :: `numeric(1)`  
  Suppress variables with significance above this level. Only effects
  \[regression
  tasks[mlr3::TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html)
  and binary [classification
  tasks](https://mlr3.mlr-org.com/reference/TaskClassif.html).

- `scale` :: `logical(1)`  
  If `TRUE` replace numeric variables with single variable model
  regressions ("move to outcome-scale"). These have mean zero and (for
  variables with significant less than 1) slope 1 when regressed (lm for
  regression problems/glm for classification problems) against outcome.

- `varRestriction` :: [`list()`](https://rdrr.io/r/base/list.html)  
  List of treated variable names to restrict to. Only effects
  \[regression
  tasks[mlr3::TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html)
  and binary [classification
  tasks](https://mlr3.mlr-org.com/reference/TaskClassif.html).

- `trackedValues` :: named
  [`list()`](https://rdrr.io/r/base/list.html)  
  Named list mapping variables to know values, allows warnings upon
  novel level appearances (see
  [`vtreat::track_values()`](https://winvector.github.io/vtreat/reference/track_values.html)).
  Only effects \[regression
  tasks[mlr3::TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html)
  and binary [classification
  tasks](https://mlr3.mlr-org.com/reference/TaskClassif.html).

- `y_dependent_treatments` ::
  [`character()`](https://rdrr.io/r/base/character.html)  
  Character what treatment types to build per-outcome level. Only
  effects multiclass [classification
  tasks](https://mlr3.mlr-org.com/reference/TaskClassif.html).

- `imputation_map` :: named `list`  
  List of map from column names to functions of signature f(values:
  numeric, weights: numeric), simple missing value imputers.  
  Typically, an imputation via a
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) is
  to be preferred, see
  [`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/reference/PipeOpImpute.md).

For more information, see
[`vtreat::regression_parameters()`](https://winvector.github.io/vtreat/reference/regression_parameters.html),
[`vtreat::classification_parameters()`](https://winvector.github.io/vtreat/reference/classification_parameters.html),
or
[`vtreat::multinomial_parameters()`](https://winvector.github.io/vtreat/reference/multinomial_parameters.html).

## Internals

Follows vtreat's fit/prepare interface. See
[`vtreat::NumericOutcomeTreatment()`](https://winvector.github.io/vtreat/reference/NumericOutcomeTreatment.html),
[`vtreat::BinomialOutcomeTreatment()`](https://winvector.github.io/vtreat/reference/BinomialOutcomeTreatment.html),
[`vtreat::MultinomialOutcomeTreatment()`](https://winvector.github.io/vtreat/reference/MultinomialOutcomeTreatment.html),
[`vtreat::fit_prepare()`](https://winvector.github.io/vtreat/reference/fit_prepare.html)
and
[`vtreat::prepare()`](https://winvector.github.io/vtreat/reference/prepare.html).

## Fields

Only fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Methods

Only methods inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

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
[`mlr_pipeops_yeojohnson`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_yeojohnson.md)

## Examples

``` r
library("mlr3")

set.seed(2020)

make_data <- function(nrows) {
    d <- data.frame(x = 5 * rnorm(nrows))
    d["y"] = sin(d[["x"]]) + 0.01 * d[["x"]] + 0.1 * rnorm(nrows)
    d[4:10, "x"] = NA  # introduce NAs
    d["xc"] = paste0("level_", 5 * round(d$y / 5, 1))
    d["x2"] = rnorm(nrows)
    d[d["xc"] == "level_-1", "xc"] = NA  # introduce a NA level
    return(d)
}

task = TaskRegr$new("vtreat_regr", backend = make_data(100), target = "y")

pop = PipeOpVtreat$new()
pop$train(list(task))
#> $output
#> 
#> ── <TaskRegr> (100x8) ──────────────────────────────────────────────────────────
#> • Target: y
#> • Properties: -
#> • Features (7):
#>   • dbl (7): xc_catD, xc_catN, xc_catP, xc_lev_NA, xc_lev_x_level_0_5,
#>   xc_lev_x_level_1, xc_lev_x_level_minus_0_5
#> 
```
