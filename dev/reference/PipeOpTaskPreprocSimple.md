# Simple Task Preprocessing Base Class

Base class for handling many "preprocessing" operations that perform
essentially the same operation during training and prediction. Instead
implementing a `private$.train_task()` and a `private$.predict_task()`
operation, only a `private$.get_state()` and a `private$.transform()`
operation needs to be defined, both of which take one argument: a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html).

Alternatively, analogously to the
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)
approach of offering `private$.train_dt()`/`private$.predict_dt()`, the
`private$.get_state_dt()` and `private$.transform_dt()` functions may be
implemented.

`private$.get_state` must not change its input value in-place and must
return something that will be written into `$state` (which must not be
NULL), `private$.transform()` should modify its argument in-place; it is
called both during training and prediction.

This inherits from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)
and behaves essentially the same.

## Format

Abstract [`R6Class`](https://r6.r-lib.org/reference/R6Class.html)
inheriting from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)/[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Construction

    PipeOpTaskPreprocSimple$new(id, param_set = ps(), param_vals = list(), can_subset_cols = TRUE,
      packages = character(0), task_type = "Task", tags = NULL, feature_types = mlr_reflections$task_feature_types)

(Construction is identical to
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md).)

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

- `can_subset_cols` :: `logical(1)`  
  Whether the `affect_columns` parameter should be added which lets the
  user limit the columns that are modified by the
  `PipeOpTaskPreprocSimple`. This should generally be `FALSE` if the
  operation adds or removes rows from the
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html), and `TRUE`
  otherwise. Default is `TRUE`.

- `packages` :: `character`  
  Set of all required packages for the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
  `private$.train()` and `private$.predict()` methods. See `$packages`
  slot. Default is `character(0)`.

- `task_type` :: `character(1)`  
  The class of [`Task`](https://mlr3.mlr-org.com/reference/Task.html)
  that should be accepted as input and will be returned as output. This
  should generally be a `character(1)` identifying a type of
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html), e.g. `"Task"`,
  `"TaskClassif"` or `"TaskRegr"` (or another subclass introduced by
  other packages). Default is `"Task"`.

- `tags` :: `character` \| `NULL`  
  Tags of the resulting `PipeOp`. This is added to the tag
  `"data transform"`. Default `NULL`.

- `feature_types` :: `character`  
  Feature types affected by the `PipeOp`. See `private$.select_cols()`
  for more information. Defaults to all available feature types.

## Input and Output Channels

Input and output channels are inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md).

The output during training and prediction is the
[`Task`](https://mlr3.mlr-org.com/reference/Task.html), modified by
`private$.transform()` or `private$.transform_dt()`.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md).

## Parameters

The parameters are the parameters inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md).

## Internals

`PipeOpTaskPreprocSimple` is an abstract class inheriting from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)
and implementing the `private$.train_task()` and
`private$.predict_task()` functions. A subclass of
`PipeOpTaskPreprocSimple` may implement the functions
`private$.get_state()` and `private$.transform()`, or alternatively the
functions `private$.get_state_dt()` and `private$.transform_dt()` (as
well as `private$.select_cols()`, in the latter case). This works by
having the default implementations of `private$.get_state()` and
`private$.transform()` call `private$.get_state_dt()` and
`private$.transform_dt()`.

## Fields

Fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Methods

Methods inherited from
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
as well as:

- `.get_state(task)`  
  ([`Task`](https://mlr3.mlr-org.com/reference/Task.html)) -\> named
  `list`  
  Store create something that will be stored in `$state` during training
  phase of `PipeOpTaskPreprocSimple`. The state can then influence the
  `private$.transform()` function. Note that `private$.get_state()` must
  *return* the state, and should not store it in `$state`. It is not
  strictly necessary to implement either `private$.get_state()` or
  `private$.get_state_dt()`; if they are not implemented, the state will
  be stored as [`list()`](https://rdrr.io/r/base/list.html).  
  This method can optionally be overloaded when inheriting from
  `PipeOpTaskPreprocSimple`, together with `private$.transform()`;
  alternatively, `private$.get_state_dt()` (optional) and
  `private$.transform_dt()` (and possibly `private$.select_cols()`, from
  [`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md))
  can be overloaded.

- `.transform(task)`  
  ([`Task`](https://mlr3.mlr-org.com/reference/Task.html)) -\>
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html)  
  Predict on new data in `task`, possibly using the stored `$state`.
  `task` should not be cloned, instead it should be changed in-place.
  This method is called both during training and prediction phase, and
  should essentially behave the same independently of phase. (If this is
  incongruent with the functionality to be implemented, then it should
  inherit from
  [`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
  not from `PipeOpTaskPreprocSimple`.)  
  This method can be overloaded when inheriting from
  `PipeOpTaskPreprocSimple`, optionally with `private$.get_state()`;
  alternatively, `private$.get_state_dt()` (optional) and
  `private$.transform_dt()` (and possibly `private$.select_cols()`, from
  [`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md))
  can be overloaded.

- `.get_state_dt(dt)`  
  ([`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html))
  -\> named `list`  
  Create something that will be stored in `$state` during training phase
  of `PipeOpTaskPreprocSimple`. The state can then influence the
  `private$.transform_dt()` function. Note that
  `private$.get_state_dt()` must *return* the state, and should not
  store it in `$state`. If neither `private$.get_state()` nor
  `private$.get_state_dt()` are overloaded, the state will be stored as
  [`list()`](https://rdrr.io/r/base/list.html).  
  This method can optionally be overloaded when inheriting from
  `PipeOpTaskPreprocSimple`, together with `private$.transform_dt()`
  (and optionally `private$.select_cols()`, from
  [`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md));
  Alternatively, `private$.get_state()` (optional) and
  `private$.transform()` can be overloaded.

- `.transform_dt(dt)`  
  ([`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html))
  -\> [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  \| `data.frame` \| `matrix`  
  Predict on new data in `dt`, possibly using the stored `$state`. A
  transformed object must be returned that can be converted to a
  `data.table` using
  [`as.data.table`](https://rdrr.io/pkg/data.table/man/as.data.table.html).
  `dt` does not need to be copied deliberately, it is possible and
  encouraged to change it in-place. This method is called both during
  training and prediction phase, and should essentially behave the same
  independently of phase. (If this is incongruent with the functionality
  to be implemented, then it should inherit from
  [`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
  not from `PipeOpTaskPreprocSimple`.)  
  This method can optionally be overloaded when inheriting from
  `PipeOpTaskPreprocSimple`, together with `private$.transform_dt()`
  (and optionally `private$.select_cols()`, from
  [`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md));
  Alternatively, `private$.get_state()` (optional) and
  `private$.transform()` can be overloaded.

## See also

https://mlr-org.com/pipeops.html

Other PipeOps:
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEncodePL.md),
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md),
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpImpute.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
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

Other mlr3pipelines backend related:
[`Graph`](https://mlr3pipelines.mlr-org.com/dev/reference/Graph.md),
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md),
[`mlr_graphs`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_graphs.md),
[`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_updatetarget.md)
