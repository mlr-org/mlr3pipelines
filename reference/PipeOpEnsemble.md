# Ensembling Base Class

Parent class for
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s that
aggregate predictions. Implements the `private$.train()` and
`private$.predict()` methods necessary for a `PipeOp` and requires
deriving classes to create the `private$weighted_avg_predictions()`
function.

## Format

Abstract [`R6Class`](https://r6.r-lib.org/reference/R6Class.html)
inheriting from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Construction

Note: This object is typically constructed via a derived class, e.g.
[`PipeOpClassifAvg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classifavg.md)
or
[`PipeOpRegrAvg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_regravg.md).

    PipeOpEnsemble$new(innum = 0, collect_multiplicity = FALSE, id, param_set = ps(), param_vals = list(), packages = character(0), prediction_type = "Prediction")

- `innum` :: `numeric(1)`  
  Determines the number of input channels. If `innum` is 0 (default), a
  vararg input channel is created that can take an arbitrary number of
  inputs.

- `collect_multiplicity` :: `logical(1)`  
  If `TRUE`, the input is a
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)
  collecting channel. This means, a
  [`Multiplicity`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md)
  input, instead of multiple normal inputs, is accepted and the members
  are aggregated. This requires `innum` to be 0. Default is `FALSE`.

- `id` :: `character(1)`  
  Identifier of the resulting object.

- `param_set` ::
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html)  
  ("Hyper"-)Parameters in form of a
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html) for
  the resulting
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction. Default
  [`list()`](https://rdrr.io/r/base/list.html).

- `packages` :: `character`  
  Set of packages required for this `PipeOp`. These packages are loaded
  during `$train()` and `$predict()`, but not attached. Default
  `character(0)`.

- `prediction_type` :: `character(1)`  
  The `predict` entry of the `$input` and `$output` type specifications.
  Should be `"Prediction"` (default) or one of its subclasses, e.g.
  `"PredictionClassif"`, and correspond to the type accepted by
  `private$.train()` and `private$.predict()`.

## Input and Output Channels

`PipeOpEnsemble` has multiple input channels depending on the `innum`
construction argument, named `"input1"`, `"input2"`, ... if `innum` is
nonzero; if `innum` is 0, there is only one *vararg* input channel named
`"..."`. All input channels take only `NULL` during training and take a
[`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)
during prediction.

`PipeOpEnsemble` has one output channel named `"output"`, producing
`NULL` during training and a
[`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)
during prediction.

The output during prediction is in some way a weighted averaged
representation of the input.

## State

The `$state` is left empty
([`list()`](https://rdrr.io/r/base/list.html)).

## Parameters

- `weights` :: `numeric`  
  Relative weights of input predictions. If this has length 1, it is
  ignored and weighs all inputs equally. Otherwise it must have length
  equal to the number of connected inputs. Initialized to 1 (equal
  weights).

## Internals

The commonality of ensemble methods using `PipeOpEnsemble` is that they
take a `NULL`-input during training and save an empty `$state`. They can
be used following a set of
[`PipeOpLearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.md)
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s to
perform (possibly weighted) prediction averaging. See e.g.
[`PipeOpClassifAvg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classifavg.md)
and
[`PipeOpRegrAvg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_regravg.md)
which both inherit from this class.

Should it be necessary to use the output of preceding
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)s during the
"training" phase, then `PipeOpEnsemble` should not be used. In fact, if
training time behaviour of a
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html) is
important, then one should use a
[`PipeOpLearnerCV`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner_cv.md)
instead of a
[`PipeOpLearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.md),
and the ensemble can be created with a
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)
encapsulated by a
[`PipeOpLearner`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.md).
See
[`LearnerClassifAvg`](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_avg.md)
and
[`LearnerRegrAvg`](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_avg.md)
for examples.

## Fields

Only fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Methods

Methods inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md) as
well as:

- `weighted_avg_prediction(inputs, weights, row_ids, truth)`  
  (`list` of
  [`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html),
  `numeric`, `integer` \| `character`, `list`) -\> `NULL`  
  Create
  [`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)s
  that correspond to the weighted average of incoming
  [`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)s.
  This is called by `private$.predict()` with cleaned and sanity-checked
  values: `inputs` are guaranteed to fit together, `row_ids` and `truth`
  are guaranteed to be the same as each one in `inputs`, and `weights`
  is guaranteed to have the same length as `inputs`.  
  This method is abstract, it must be implemented by deriving classes.

## See also

https://mlr-org.com/pipeops.html

Other PipeOps:
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md),
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md),
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
[`mlr_pipeops_vtreat`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_vtreat.md),
[`mlr_pipeops_yeojohnson`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_yeojohnson.md)

Other Multiplicity PipeOps:
[`Multiplicity()`](https://mlr3pipelines.mlr-org.com/reference/Multiplicity.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_featureunion`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_featureunion.md),
[`mlr_pipeops_multiplicityexply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityexply.md),
[`mlr_pipeops_multiplicityimply`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_multiplicityimply.md),
[`mlr_pipeops_ovrsplit`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrsplit.md),
[`mlr_pipeops_ovrunite`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrunite.md),
[`mlr_pipeops_regravg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_regravg.md),
[`mlr_pipeops_replicate`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_replicate.md)

Other Ensembles:
[`mlr_learners_avg`](https://mlr3pipelines.mlr-org.com/reference/mlr_learners_avg.md),
[`mlr_pipeops_classifavg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_classifavg.md),
[`mlr_pipeops_ovrunite`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_ovrunite.md),
[`mlr_pipeops_regravg`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_regravg.md)
