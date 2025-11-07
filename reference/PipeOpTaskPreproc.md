# Task Preprocessing Base Class

Base class for handling most "preprocessing" operations. These are
operations that have exactly one
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) input and one
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) output, and
expect the column layout of these
[`Task`](https://mlr3.mlr-org.com/reference/Task.html)s during input and
output to be the same.

Prediction-behavior of preprocessing operations should always be
independent for each row in the
input-[`Task`](https://mlr3.mlr-org.com/reference/Task.html). This means
that the prediction-operation of
preprocessing-[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)s
should commute with [`rbind()`](https://rdrr.io/r/base/cbind.html):
Running prediction on an `n`-row
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) should result in
the same result as [`rbind()`](https://rdrr.io/r/base/cbind.html)-ing
the prediction-result from `n` 1-row
[`Task`](https://mlr3.mlr-org.com/reference/Task.html)s with the same
content. In the large majority of cases, the number and order of rows
should also not be changed during prediction.

Users must implement `private$.train_task()` and
`private$.predict_task()`, which have a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) input and should
return that [`Task`](https://mlr3.mlr-org.com/reference/Task.html). The
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) should, if
possible, be manipulated in-place, and should not be cloned.

Alternatively, the `private$.train_dt()` and `private$.predict_dt()`
functions can be implemented, which operate on
[`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
objects instead. This should generally only be done if all data is in
some way altered (e.g. PCA changing all columns to principal components)
and not if only a few columns are added or removed (e.g. feature
selection) because this should be done at the
[`Task`](https://mlr3.mlr-org.com/reference/Task.html)-level with
`private$.train_task()`. The `private$.select_cols()` function can be
overloaded for `private$.train_dt()` and `private$.predict_dt()` to
operate only on subsets of the
[`Task`](https://mlr3.mlr-org.com/reference/Task.html)'s data, e.g. only
on numerical columns.

If the `can_subset_cols` argument of the constructor is `TRUE` (the
default), then the hyperparameter `affect_columns` is added, which can
limit the columns of the
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) that is modified
by the `PipeOpTaskPreproc` using a
[`Selector`](https://mlr3pipelines.mlr-org.com/reference/Selector.md)
function. Note this functionality is entirely independent of the
`private$.select_cols()` functionality.

`PipeOpTaskPreproc` is useful for operations that behave differently
during training and prediction. For operations that perform essentially
the same operation and only need to perform extra work to build a
`$state` during training, the
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreprocSimple.md)
class can be used instead.

## Format

Abstract [`R6Class`](https://r6.r-lib.org/reference/R6Class.html)
inheriting from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Construction

    PipeOpTaskPreproc$new(id, param_set = ps(), param_vals = list(), can_subset_cols = TRUE,
      packages = character(0), task_type = "Task", tags = NULL, feature_types = mlr_reflections$task_feature_types)

- `id` :: `character(1)`  
  Identifier of resulting object. See `$id` slot of
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

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
  user limit the columns that are modified by the `PipeOpTaskPreproc`.
  This should generally be `FALSE` if the operation adds or removes rows
  from the [`Task`](https://mlr3.mlr-org.com/reference/Task.html), and
  `TRUE` otherwise. Default is `TRUE`.

- `packages` :: `character`  
  Set of all required packages for the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)'s
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

`PipeOpTaskPreproc` has one input channel named `"input"`, taking a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html), or a subclass of
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) if the
`task_type` construction argument is given as such; both during training
and prediction.

`PipeOpTaskPreproc` has one output channel named `"output"`, producing a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html), or a subclass;
the [`Task`](https://mlr3.mlr-org.com/reference/Task.html) type is the
same as for input; both during training and prediction.

The output [`Task`](https://mlr3.mlr-org.com/reference/Task.html) is the
modified input [`Task`](https://mlr3.mlr-org.com/reference/Task.html)
according to the overloaded
`private$.train_task()`/`private$.predict_taks()` or
`private$.train_dt()`/`private$.predict_dt()` functions.

## State

The `$state` is a named `list`; besides members added by inheriting
classes, the members are:

- `affect_cols` :: `character`  
  Names of features being selected by the `affect_columns` parameter, if
  present; names of *all* present features otherwise.

- `intasklayout` ::
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Copy of the training
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html)'s
  `$feature_types` slot. This is used during prediction to ensure that
  the prediction [`Task`](https://mlr3.mlr-org.com/reference/Task.html)
  has the same features, feature layout, and feature types as during
  training.

- `outtasklayout` ::
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Copy of the trained
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html)'s
  `$feature_types` slot. This is used during prediction to ensure that
  the [`Task`](https://mlr3.mlr-org.com/reference/Task.html) resulting
  from the prediction operation has the same features, feature layout,
  and feature types as after training.

- `dt_columns` :: `character`  
  Names of features selected by the `private$.select_cols()` call during
  training. This is only present if the `private$.train_dt()`
  functionality is used, and not present if the `private$.train_task()`
  function is overloaded instead.

- `feature_types` :: `character`  
  Feature types affected by the `PipeOp`. See `private$.select_cols()`
  for more information.

## Parameters

- `affect_columns` :: `function` \|
  [`Selector`](https://mlr3pipelines.mlr-org.com/reference/Selector.md)
  \| `NULL`  
  What columns the `PipeOpTaskPreproc` should operate on. This parameter
  is only present if the constructor is called with the
  `can_subset_cols` argument set to `TRUE` (the default).  
  The parameter must be a
  [`Selector`](https://mlr3pipelines.mlr-org.com/reference/Selector.md)
  function, which takes a
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) as argument and
  returns a `character` of features to use.  
  See
  [`Selector`](https://mlr3pipelines.mlr-org.com/reference/Selector.md)
  for example functions. Defaults to `NULL`, which selects all features.

## Internals

`PipeOpTaskPreproc` is an abstract class inheriting from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md). It
implements the `private$.train()` and `$.predict()` functions. These
functions perform checks and go on to call `private$.train_task()` and
`private$.predict_task()`. A subclass of `PipeOpTaskPreproc` may
implement these functions, or implement `private$.train_dt()` and
`private$.predict_dt()` instead. This works by having the default
implementations of `private$.train_task()` and `private$.predict_task()`
call `private$.train_dt()` and `private$.predict_dt()`, respectively.

The `affect_columns` functionality works by unsetting columns by
removing their "col_role" before processing, and adding them afterwards
by setting the col_role to `"feature"`.

## Fields

Fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md).

## Methods

Methods inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md), as
well as:

- `.train_task(task)`  
  ([`Task`](https://mlr3.mlr-org.com/reference/Task.html)) -\>
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html)  
  Called by the `PipeOpTaskPreproc`'s implementation of
  `private$.train()`. Takes a single
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) as input and
  modifies it (ideally in-place without cloning) while storing
  information in the `$state` slot. Note that unlike `$.train()`, the
  argument is *not* a list but a singular
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html), and the return
  object is also *not* a list but a singular
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html). Also, contrary
  to `private$.train()`, the `$state` being generated must be a `list`,
  which the `PipeOpTaskPreproc` will add additional slots to (see
  Section *State*). Care should be taken to avoid name collisions
  between `$state` elements added by `private$.train_task()` and
  `PipeOpTaskPreproc`.  
  By default this function calls the `private$.train_dt()` function, but
  it can be overloaded to perform operations on the
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) directly.

- `.predict_task(task)`  
  ([`Task`](https://mlr3.mlr-org.com/reference/Task.html)) -\>
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html)  
  Called by the `PipeOpTaskPreproc`'s implementation of `$.predict()`.
  Takes a single [`Task`](https://mlr3.mlr-org.com/reference/Task.html)
  as input and modifies it (ideally in-place without cloning) while
  using information in the `$state` slot. Works analogously to
  `private$.train_task()`. If `private$.predict_task()` should only be
  overloaded if `private$.train_task()` is overloaded (i.e.
  `private$.train_dt()` is *not* used).

- `.train_dt(dt, levels, target)`  
  ([`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html),
  named `list`, `any`) -\>
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  \| `data.frame` \| `matrix`  
  Train `PipeOpTaskPreproc` on `dt`, transform it and store a state in
  `$state`. A transformed object must be returned that can be converted
  to a `data.table` using
  [`as.data.table`](https://rdatatable.gitlab.io/data.table/reference/as.data.table.html).
  `dt` does not need to be copied deliberately, it is possible and
  encouraged to change it in-place.  
  The `levels` argument is a named list of factor levels for factorial
  or character features. If the input
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) inherits from
  [`TaskSupervised`](https://mlr3.mlr-org.com/reference/TaskSupervised.html),
  the `target` argument contains the `$truth()` information of the
  training [`Task`](https://mlr3.mlr-org.com/reference/Task.html); its
  type depends on the
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) type being
  trained on.  
  This method can be overloaded when inheriting from
  `PipeOpTaskPreproc`, together with `private$.predict_dt()` and
  optionally `private$.select_cols()`; alternatively,
  `private$.train_task()` and `private$.predict_task()` can be
  overloaded.

- `.predict_dt(dt, levels)`  
  ([`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html),
  named `list`) -\>
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  \| `data.frame` \| `matrix`  
  Predict on new data in `dt`, possibly using the stored `$state`. A
  transformed object must be returned that can be converted to a
  `data.table` using
  [`as.data.table`](https://rdatatable.gitlab.io/data.table/reference/as.data.table.html).
  `dt` does not need to be copied deliberately, it is possible and
  encouraged to change it in-place.  
  The `levels` argument is a named list of factor levels for factorial
  or character features.  
  This method can be overloaded when inheriting `PipeOpTaskPreproc`,
  together with `private$.train_dt()` and optionally
  `private$.select_cols()`; alternatively, `private$.train_task()` and
  `private$.predict_task()` can be overloaded.

- `.select_cols(task)`  
  ([`Task`](https://mlr3.mlr-org.com/reference/Task.html)) -\>
  `character`  
  Selects which columns the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md)
  operates on, if `private$.train_dt()` and `private$.predict_dt()` are
  overloaded. This function is not called if `private$.train_task()` and
  `private$.predict_task()` are overloaded. In contrast to the
  `affect_columns` parameter. `private$.select_cols()` is for the
  *inheriting class* to determine which columns the operator should
  function on, e.g. based on feature type, while `affect_columns` is a
  way for the *user* to limit the columns that a `PipeOpTaskPreproc`
  should operate on.  
  This method can optionally be overloaded when inheriting
  `PipeOpTaskPreproc`, together with `private$.train_dt()` and
  `private$.predict_dt()`; alternatively, `private$.train_task()` and
  `private$.predict_task()` can be overloaded.  
  If this method is not overloaded, it defaults to selecting of type
  indicated by the `feature_types` construction argument.

## See also

https://mlr-org.com/pipeops.html

Other mlr3pipelines backend related:
[`Graph`](https://mlr3pipelines.mlr-org.com/reference/Graph.md),
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTargetTrafo.md),
[`PipeOpTaskPreprocSimple`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreprocSimple.md),
[`mlr_graphs`](https://mlr3pipelines.mlr-org.com/reference/mlr_graphs.md),
[`mlr_pipeops`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops.md),
[`mlr_pipeops_updatetarget`](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_updatetarget.md)

Other PipeOps:
[`PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.md),
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEncodePL.md),
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.md),
[`PipeOpImpute`](https://mlr3pipelines.mlr-org.com/reference/PipeOpImpute.md),
[`PipeOpTargetTrafo`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTargetTrafo.md),
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
