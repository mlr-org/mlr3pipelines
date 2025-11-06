# Target Transformation Base Class

Base class for handling target transformation operations. Target
transformations are different from feature transformation because they
have to be "inverted" after prediction. The target is transformed during
the training phase and information to invert this transformation is sent
along to
[`PipeOpTargetInvert`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targetinvert.md)
which then inverts this transformation during the prediction phase. This
inversion may need info about both the training and the prediction data.

Users can overload up to four `private$`-functions: `.get_state()`
(optional), `.transform()` (mandatory), `.train_invert()` (optional),
and `.invert()` (mandatory).

## Format

Abstract [`R6Class`](https://r6.r-lib.org/reference/R6Class.html)
inheriting from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Construction

    PipeOpTargetTrafo$new(id, param_set = ps(), param_vals = list(), packages = character(0), task_type_in = "Task", task_type_out = task_type_in, tags = NULL)

- `id` :: `character(1)`  
  Identifier of resulting object. See `$id` slot of
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

- `param_set` ::
  [`ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html)  
  Parameter space description. This should be created by the subclass
  and given to `super$initialize()`.

- `param_vals` :: named `list`  
  List of hyperparameter settings, overwriting the hyperparameter
  settings given in `param_set`. The subclass should have its own
  `param_vals` parameter and pass it on to `super$initialize()`. Default
  [`list()`](https://rdrr.io/r/base/list.html).

- `task_type_in` :: `character(1)`  
  The class of [`Task`](https://mlr3.mlr-org.com/reference/Task.html)
  that should be accepted as input. This should generally be a
  `character(1)` identifying a type of
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html), e.g. `"Task"`,
  `"TaskClassif"` or `"TaskRegr"` (or another subclass introduced by
  other packages). Default is `"Task"`.

- `task_type_out` :: `character(1)`  
  The class of [`Task`](https://mlr3.mlr-org.com/reference/Task.html)
  that is produced as output. This should generally be a `character(1)`
  identifying a type of
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html), e.g. `"Task"`,
  `"TaskClassif"` or `"TaskRegr"` (or another subclass introduced by
  other packages). Default is the value of `task_type_in`.

- `packages` :: `character`  
  Set of all required packages for the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
  methods. See `$packages` slot. Default is `character(0)`.

- `tags` :: `character` \| `NULL`  
  Tags of the resulting `PipeOp`. This is added to the tag
  `"target transform"`. Default `NULL`.

## Input and Output Channels

`PipeOpTargetTrafo` has one input channels named `"input"` taking a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) (or whatever
class was specified by the `task_type` during construction) both during
training and prediction.

`PipeOpTargetTrafo` has two output channels named `"fun"` and
`"output"`. During training, `"fun"` returns `NULL` and during
prediction, `"fun"` returns a function that can later be used to invert
the transformation done during training according to the overloaded
`.train_invert()` and `.invert()` functions. `"output"` returns the
modified input [`Task`](https://mlr3.mlr-org.com/reference/Task.html)
(or `task_type`) according to the overloaded
[`transform()`](https://rdrr.io/r/base/transform.html) function both
during training and prediction.

## State

The `$state` is a named `list` and should be returned explicitly by the
user in the overloaded `.get_state()` function.

## Internals

`PipeOpTargetTrafo` is an abstract class inheriting from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).
It implements the `private$.train()` and `private$.predict()` functions.
These functions perform checks and go on to call `.get_state()`,
`.transform()`, `.train_invert()`. `.invert()` is packaged and sent
along the `"fun"` output to be applied to a
[`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html) by
[`PipeOpTargetInvert`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targetinvert.md).
A subclass of `PipeOpTargetTrafo` should implement these functions and
be used in combination with
[`PipeOpTargetInvert`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targetinvert.md).

## Fields

Fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Methods

Methods inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
as well as:

- `.get_state(task)`  
  ([`Task`](https://mlr3.mlr-org.com/reference/Task.html)) -\> `list`  
  Called by `PipeOpTargetTrafo`'s implementation of `private$.train()`.
  Takes a single [`Task`](https://mlr3.mlr-org.com/reference/Task.html)
  as input and returns a `list` to set the `$state`. `.get_state()` will
  be called a single time during *training* right before `.transform()`
  is called. The return value (i.e. the `$state`) should contain info
  needed in `.transform()` as well as in `.invert()`.  
  The base implementation returns
  [`list()`](https://rdrr.io/r/base/list.html) and should be overloaded
  if setting the state is desired.

- `.transform(task, phase)`  
  ([`Task`](https://mlr3.mlr-org.com/reference/Task.html),
  `character(1)`) -\>
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html)  
  Called by `PipeOpTargetTrafo`'s implementation of `private$.train()`
  and `private$.predict()`. Takes a single
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) as input and
  modifies it. This should typically consist of calculating a new target
  and modifying the
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) by using the
  [`convert_task`](https://mlr3.mlr-org.com/reference/convert_task.html)
  function. `.transform()` will be called during training and prediction
  because the target (and if needed also type) of the input
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) must be
  transformed both times. Note that unlike `$.train()`, the argument is
  *not* a list but a singular
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html), and the return
  object is also *not* a list but a singular
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html). The `phase`
  argument is `"train"` during training phase and `"predict"` during
  prediction phase and can be used to enable different behaviour during
  training and prediction. When `phase` is `"train"`, the `$state` slot
  (as previously set by `.get_state()`) may also be modified,
  alternatively or in addition to overloading `.get_state()`.  
  The input should *not* be cloned and if possible should be changed
  in-place.  
  This function is abstract and should be overloaded by inheriting
  classes.

- `.train_invert(task)`  
  ([`Task`](https://mlr3.mlr-org.com/reference/Task.html)) -\> `any`  
  Called by `PipeOpTargetTrafo`'s implementation of
  `private$.predict()`. Takes a single
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) as input and
  returns an arbitrary value that will be given as `predict_phase_state`
  to `.invert()`. This should not modify the input
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html).  
  The base implementation returns a list with a single element, the
  `$truth` column of the
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html), and should be
  overloaded if a more training-phase-dependent state is desired.

- `.invert(prediction, predict_phase_state)`  
  ([`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html),
  `any`) -\>
  [`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)  
  Takes a
  [`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html) and
  a `predict_phase_state` object as input and inverts the prediction.
  This function is sent as `"fun"` to
  [`PipeOpTargetInvert`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_targetinvert.md).  
  This function is abstract and should be overloaded by inheriting
  classes. Care should be taken that the `predict_type` of the
  [`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)
  being inverted is handled well.

- `.invert_help(predict_phase_state)`  
  (`predict_phase_state` object) -\> `function`  
  Helper function that packages `.invert()` that can later be used for
  the inversion.

## See also

https://mlr-org.com/pipeops.html

Other mlr3pipelines backend related:
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md),
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreprocSimple.md),
[`mlr_graphs`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_graphs.md),
[`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_updatetarget.md)

Other PipeOps:
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEncodePL.md),
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md),
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md),
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
