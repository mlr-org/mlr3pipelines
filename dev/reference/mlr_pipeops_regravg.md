# Weighted Prediction Averaging

Perform (weighted) prediction averaging from regression
[`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)s by
connecting `PipeOpRegrAvg` to multiple
[`PipeOpLearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_learner.md)
outputs.

The resulting `"response"` prediction is a weighted average of the
incoming `"response"` predictions. Aggregation of `"se"` predictions is
controlled by the `se_aggr` parameter (see below). When `"se"` is not
requested or `se_aggr = "none"`, `"se"` is dropped.

## Format

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html) inheriting from
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## `"se"` Aggregation

Let there be `K` incoming predictions with weights `w` (sum to 1). For a
given row `j`, denote per-model means `mu_i[j]` and, if available,
per-model standard errors `se_i[j]`. Define

    mu_bar[j]      = sum_i w[i] * mu_i[j]
    var_between[j] = sum_i w[i] * (mu_i[j] - mu_bar[j])^2 # weighted var of means
    var_within[j]  = sum_i w[i] * se_i[j]^2               # weighted mean of SE^2s

The following aggregation methods are available:

- **`se_aggr = "predictive"`** – *Within + Between (mixture/predictive
  SD)*

      se[j] = sqrt(var_within[j] + var_between[j])

  **Interpretation.** Treats each incoming `se_i` as that model's
  predictive SD at the point (or, if the learner reports SE of the
  conditional mean–as many `mlr3` regression learners do–then as that
  mean-SE). The returned `se` is the SD of the *mixture ensemble* under
  weighted averaging: it increases when base models disagree (epistemic
  spread) and when individual models are uncertain (aleatoric spread).
  **Notes.** If `se_i` represents *mean* SE (common in
  `predict.lm(se.fit=TRUE)`-style learners), the result aggregates those
  mean-SEs and still adds model disagreement correctly, but it will
  *underestimate* a true predictive SD that would additionally include
  irreducible noise. Requires `"se"` to be present from **all** inputs.

- **`se_aggr = "mean"`** – *SE of the weighted average of means under
  equicorrelation* With a correlation parameter `se_aggr_rho = rho`,
  assume `Cov(mu_i_hat, mu_j_hat) = rho * se_i * se_j` for all `i != j`.
  Then

      # components:
      a[j] = sum_i (w[i]^2 * se_i[j]^2)
      b[j] = (sum_i w[i] * se_i[j])^2
      var_mean[j] = (1 - rho) * a[j] + rho * b[j]
      se[j] = sqrt(var_mean[j])

  **Interpretation.** Returns the *standard error of the averaged
  estimator* `sum_i w[i] * mu_i`, not a predictive SD. Use when you
  specifically care about uncertainty of the averaged mean itself.
  **Notes.** `rho` is clamped to the PSD range `[-1/(K-1), 1]` for
  `K > 1`. Typical settings: `rho = 0` (assume independence; often
  optimistic for CV/bagging) and `rho = 1` (perfect correlation;
  conservative and equal to the weighted arithmetic mean of SEs).
  Requires `"se"` from **all** inputs.

- **`se_aggr = "within"`** – *Within-model component only*

      se[j] = sqrt(var_within[j])

  **Interpretation.** Aggregates only the average per-model uncertainty
  and **ignores** disagreement between models. Useful as a diagnostic of
  the aleatoric component; not a full ensemble uncertainty. **Notes.**
  Typically *underestimates* the uncertainty of the ensemble prediction
  when models disagree. Requires `"se"` from **all** inputs.

- **`se_aggr = "between"`** – *Between-model component only (works
  without `"se"`)*

      se[j] = sqrt(var_between[j])

  **Interpretation.** Captures only the spread of the base means
  (epistemic/model disagreement). **Notes.** This is the only method
  that does not use incoming `"se"`. It is a *lower bound* on a full
  predictive SD, because it omits within-model noise.

- **`se_aggr = "none"`** – *Do not return `"se"`* `"se"` is dropped from
  the output prediction.

**Relationships and edge cases.** For any row,
`se("predictive") >= max(se("within"), se("between"))`. With a single
input (`K = 1`), `"predictive"` and `"within"` return the input `"se"`,
`"between"` returns `0`. Methods `"predictive"`, `"mean"`, and
`"within"` require all inputs to provide `"se"`; otherwise aggregation
errors.

Weights can be set as a parameter; if none are provided, defaults to
equal weights for each prediction.

## Construction

    PipeOpRegrAvg$new(innum = 0, collect_multiplicity = FALSE, id = "regravg", param_vals = list())

- `innum` :: `numeric(1)`  
  Determines the number of input channels. If `innum` is 0 (default), a
  vararg input channel is created that can take an arbitrary number of
  inputs.

- `collect_multiplicity` :: `logical(1)`  
  If `TRUE`, the input is a
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/dev/reference/Multiplicity.md)
  collecting channel. This means, a
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/dev/reference/Multiplicity.md)
  input, instead of multiple normal inputs, is accepted and the members
  are aggregated. This requires `innum` to be 0. Default is `FALSE`.

- `id` :: `character(1)` Identifier of the resulting object, default
  `"regravg"`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md).
Instead of a
[`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html), a
[`PredictionRegr`](https://mlr3.mlr-org.com/reference/PredictionRegr.html)
is used as input and output during prediction.

## State

The `$state` is left empty
([`list()`](https://rdrr.io/r/base/list.html)).

## Parameters

The parameters are the parameters inherited from the
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md),
as well as:

- `se_aggr` :: `character(1)`  
  Controls how incoming `"se"` values are aggregated into an ensemble
  `"se"`. One of `"predictive"`, `"mean"`, `"within"`, `"between"`,
  `"none"`. See the description above for definitions and
  interpretation.

- `se_aggr_rho` :: `numeric(1)`  
  Equicorrelation parameter used only for `se_aggr = "mean"`.
  Interpreted as the common correlation between per-model mean
  estimators. Recommended range `[0, 1]`; values are clamped to
  `[-1/(K-1), 1]` for validity.

## Internals

Inherits from
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md)
by implementing the `private$weighted_avg_predictions()` method.

## Fields

Only fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Methods

Only methods inherited from
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

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

Other Multiplicity PipeOps:
[`Multiplicity()`](https://mlr3pipelines.mlr-org.com/dev/reference/Multiplicity.md),
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_featureunion`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_featureunion.md),
[`mlr_pipeops_multiplicityexply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_multiplicityexply.md),
[`mlr_pipeops_multiplicityimply`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_multiplicityimply.md),
[`mlr_pipeops_ovrsplit`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrsplit.md),
[`mlr_pipeops_ovrunite`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrunite.md),
[`mlr_pipeops_replicate`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_replicate.md)

Other Ensembles:
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md),
[`mlr_learners_avg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_learners_avg.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_ovrunite`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_ovrunite.md)

## Examples

``` r
library("mlr3")

# Simple Bagging for Regression
gr = ppl("greplicate",
  po("subsample") %>>%
  po("learner", lrn("regr.rpart")),
  n = 5
) %>>%
  po("regravg")

resample(tsk("mtcars"), GraphLearner$new(gr), rsmp("holdout"))
#> 
#> ── <ResampleResult> with 1 resampling iterations ───────────────────────────────
#>  task_id
#>   mtcars
#>                                                                                                                            learner_id
#>  subsample_1.subsample_2.subsample_3.subsample_4.subsample_5.regr.rpart_1.regr.rpart_2.regr.rpart_3.regr.rpart_4.regr.rpart_5.regravg
#>  resampling_id iteration  prediction_test warnings errors
#>        holdout         1 <PredictionRegr>        0      0
```
