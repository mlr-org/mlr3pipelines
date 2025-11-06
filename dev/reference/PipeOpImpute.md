# Imputation Base Class

Abstract base class for feature imputation.

## Format

Abstract [`R6Class`](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Construction

    PipeOpImpute$$new(id, param_set = ps(), param_vals = list(), whole_task_dependent = FALSE, empty_level_control = FALSE,
      packages = character(0), task_type = "Task")

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

- `whole_task_dependent` :: `logical(1)`  
  Whether the `context_columns` parameter should be added which lets the
  user limit the columns that are used for imputation inference. This
  should generally be `FALSE` if imputation depends only on individual
  features (e.g. mode imputation), and `TRUE` if imputation depends on
  other features as well (e.g. kNN-imputation).

- `empty_level_control` :: `logical(1)`  
  Control how to handle edge cases where `NA`s occur in `factor` or
  `ordered` features only during prediction but not during training. Can
  be one of `"never"`, `"always"`, or `"param"`:

  - If set to `"never"`, no empty level is introduced during training,
    but columns that have missing values only during prediction will
    *not* be imputed.

  - If set to `"always"`, an unseen level is added to the feature during
    training and missing values are imputed as that value during
    prediction.

  - Finally, if set to `"param"`, the hyperparameter
    `create_empty_level` is added and control over this behavior is left
    to the user.

  For implementation details, see Internals below. Default is `"never"`.

- `packages` :: `character`  
  Set of all required packages for the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)'s
  `private$.train` and `private$.predict` methods. See `$packages` slot.
  Default is `character(0)`.

- `task_type` :: `character(1)`  
  The class of [`Task`](https://mlr3.mlr-org.com/reference/Task.html)
  that should be accepted as input and will be returned as output. This
  should generally be a `character(1)` identifying a type of
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html), e.g. `"Task"`,
  `"TaskClassif"` or `"TaskRegr"` (or another subclass introduced by
  other packages). Default is `"Task"`.

- `feature_types` :: `character`  
  Feature types affected by the `PipeOp`. See `private$.select_cols()`
  for more information.

## Input and Output Channels

`PipeOpImpute` has one input channel named `"input"`, taking a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html), or a subclass of
[`Task`](https://mlr3.mlr-org.com/reference/Task.html) if the
`task_type` construction argument is given as such; both during training
and prediction.

`PipeOpImpute` has one output channel named `"output"`, producing a
[`Task`](https://mlr3.mlr-org.com/reference/Task.html), or a subclass;
the [`Task`](https://mlr3.mlr-org.com/reference/Task.html) type is the
same as for input; both during training and prediction.

The output [`Task`](https://mlr3.mlr-org.com/reference/Task.html) is the
modified input [`Task`](https://mlr3.mlr-org.com/reference/Task.html)
with features imputed according to the `private$.impute()` function.

## State

The `$state` is a named `list`; besides members added by inheriting
classes, the members are:

- `affected_cols` :: `character`  
  Names of features being selected by the `affect_columns` parameter.

- `context_cols` :: `character`  
  Names of features being selected by the `context_columns` parameter.

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

- `model` :: named `list`  
  Model used for imputation. This is a list named by
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) features,
  containing the result of the `private$.train_imputer()` or
  `private$.train_nullmodel()` function for each one.

- `imputed_train` :: `character`  
  Names of features that were imputed during training. This is used to
  ensure that factor levels that were added during training are also
  added during prediction. Note that features that are imputed during
  prediction but not during training will still have inconsistent factor
  levels.

## Parameters

- `affect_columns` :: `function` \|
  [`Selector`](https://mlr3pipelines.mlr-org.com/dev/reference/Selector.md)
  \| `NULL`  
  What columns the `PipeOpImpute` should operate on. The parameter must
  be a
  [`Selector`](https://mlr3pipelines.mlr-org.com/dev/reference/Selector.md)
  function, which takes a
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) as argument and
  returns a `character` of features to use.  
  See
  [`Selector`](https://mlr3pipelines.mlr-org.com/dev/reference/Selector.md)
  for example functions. Defaults to `NULL`, which selects all features.

- `context_columns` :: `function` \|
  [`Selector`](https://mlr3pipelines.mlr-org.com/dev/reference/Selector.md)
  \| `NULL`  
  What columns the `PipeOpImpute` imputation may depend on. This
  parameter is only present if the constructor is called with the
  `whole_task_dependent` argument set to `TRUE`.  
  The parameter must be a
  [`Selector`](https://mlr3pipelines.mlr-org.com/dev/reference/Selector.md)
  function, which takes a
  [`Task`](https://mlr3.mlr-org.com/reference/Task.html) as argument and
  returns a `character` of features to use.  
  See
  [`Selector`](https://mlr3pipelines.mlr-org.com/dev/reference/Selector.md)
  for example functions. Defaults to `NULL`, which selects all features.

- `create_empty_level` :: `logical(1)`  
  Whether an empty level should always be created for `factor` or
  `ordered` columns during training. If `FALSE`, columns that had no
  `NA`s during training but have `NA`s during prediction will not be
  imputed. This parameter is only present if the constructor is called
  with the `empty_level_control` argument set to `"param"`. Initialized
  to `FALSE`.  

## Internals

`PipeOpImpute` is an abstract class inheriting from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
that makes implementing imputer
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)s
simple.

Internally, the construction argument `empty_level_control` and the
hyperparameter `create_empty_level` (should it exist) modify the
`private$.create_empty_level` field. Behavior then depends on whether
this field is set to `TRUE` or `FALSE` and works by controlling for
which cases imputation is performed on `factor` or `ordered` columns.
Its setting has no impact on columns of other types.  
If `private$.create_empty_level` is set to `TRUE`, `private$.impute()`
is called for all `factor` or `ordered` columns during training,
regardless of whether they have any missing values. For this to lead to
the creation of an empty level for columns with no missing values,
inheriting `PipeOp`s must implement `private$.train_imputer()` in such a
way that it returns the name of the level to be created for the feature
types `factor` and `ordered`.  
If `private$.create_empty_level` is set to `FALSE`, `private$.impute()`
is not called during prediction for `factor` or `ordered` columns which
were not modified during training. This means that `NA`s will not be
imputed for these columns.  
See
[`PipeOpImputeOOR`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeoor.md),
for a detailed explanation of why these controls are necessary.

## Fields

Fields inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md).

## Methods

Methods inherited from
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
as well as:

- `.select_cols(task)`  
  ([`Task`](https://mlr3.mlr-org.com/reference/Task.html)) -\>
  `character`  
  Selects which columns the
  [`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md)
  operates on. In contrast to the `affect_columns` parameter.
  `private$.select_cols()` is for the *inheriting class* to determine
  which columns the operator should function on, e.g. based on feature
  type, while `affect_columns` is a way for the *user* to limit the
  columns that a
  [`PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpTaskPreproc.md)
  should operate on. This method can optionally be overloaded when
  inheriting `PipeOpImpute`; If this method is not overloaded, it
  defaults to selecting the columns of type indicated by the
  `feature_types` construction argument.

- `.train_imputer(feature, type, context)`  
  (`atomic`, `character(1)`,
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))
  -\> `any`  
  Abstract function that must be overloaded when inheriting. Called once
  for each feature selected by `affect_columns` to create the model
  entry to be used for `private$.impute()`. This function is only called
  for features with at least one non-missing value.

- `.train_nullmodel(feature, type, context)`  
  (`atomic`, `character(1)`,
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))
  -\> `any`  
  Like `.train_imputer()`, but only called for each feature that only
  contains missing values. This is not an abstract function and, if not
  overloaded, gives a default response of `0` (`integer`, `numeric`),
  `c(TRUE, FALSE)` (`logical`), all available levels
  (`factor`/`ordered`), or the empty string (`character`).

- `.impute(feature, type, model, context)`  
  (`atomic`, `character(1)`, `any`,
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))
  -\> `atomic`  
  Imputes the features. `model` is the model created by
  `private$.train_imputer()`. Default behaviour is to assume `model` is
  an atomic vector from which values are sampled to impute missing
  values of `feature`. `model` may have an attribute `probabilities` for
  non-uniform sampling. If `model` has length zero, `feature` is
  returned unchanged.

## See also

https://mlr-org.com/pipeops.html

Other PipeOps:
[`PipeOp`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOp.md),
[`PipeOpEncodePL`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEncodePL.md),
[`PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/dev/reference/PipeOpEnsemble.md),
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

Other Imputation PipeOps:
[`mlr_pipeops_imputeconstant`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeconstant.md),
[`mlr_pipeops_imputehist`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputehist.md),
[`mlr_pipeops_imputelearner`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputelearner.md),
[`mlr_pipeops_imputemean`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemean.md),
[`mlr_pipeops_imputemedian`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemedian.md),
[`mlr_pipeops_imputemode`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputemode.md),
[`mlr_pipeops_imputeoor`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputeoor.md),
[`mlr_pipeops_imputesample`](https://mlr3pipelines.mlr-org.com/dev/reference/mlr_pipeops_imputesample.md)
