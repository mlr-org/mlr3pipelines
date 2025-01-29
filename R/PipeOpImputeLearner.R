#' @title Impute Features by Fitting a Learner
#'
#' @usage NULL
#' @name mlr_pipeops_imputelearner
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @description
#' Impute features by fitting a [`Learner`][mlr3::Learner] for each feature.
#' Uses the features indicated by the `context_columns` parameter as features to train the imputation [`Learner`][mlr3::Learner].
#' Note this parameter is part of the [`PipeOpImpute`] base class and explained there.
#'
#' Additionally, only features supported by the learner can be imputed; i.e. learners of type
#' `regr` can only impute features of type `integer` and `numeric`, while `classif` can impute
#' features of type `factor`, `ordered` and `logical`.
#'
#' The [`Learner`][mlr3::Learner] used for imputation is trained on all `context_columns`; if these contain missing values,
#' the [`Learner`][mlr3::Learner] typically either needs to be able to handle missing values itself, or needs to do its
#' own imputation (see examples).
#'
#' @section Construction:
#' ```
#' PipeOpImputeLearner$new(learner, id = NULL, param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"impute."`, followed by the `id` of the `Learner`.
#' * `learner` :: [`Learner`][mlr3::Learner] | `character(1)`
#'   [`Learner`][mlr3::Learner] to wrap, or a string identifying a [`Learner`][mlr3::Learner] in the [`mlr3::mlr_learners`] [`Dictionary`][mlr3misc::Dictionary].
#'   The [`Learner`][mlr3::Learner] usually needs to be able to handle missing values, i.e. have the `missings` property, unless care is taken
#'   that `context_columns` do not contain missings; see examples.\cr
#'  This argument is always cloned; to access the [`Learner`][mlr3::Learner] inside `PipeOpImputeLearner` by-reference, use `$learner`.\cr
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpImpute`].
#'
#' The output is the input [`Task`][mlr3::Task] with missing values from all affected features imputed by the trained model.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpImpute`].
#'
#' The `$state$models` is a named `list` of `models` created by the [`Learner`][mlr3::Learner]'s `$.train()` function
#' for each column. If a column consists of missing values only during training, the `model` is `0` or the levels of the
#' feature; these are used for sampling during prediction.
#'
#' This state is given the class `"pipeop_impute_learner_state"`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpImpute`], in addition to the parameters of the [`Learner`][mlr3::Learner]
#' used for imputation.
#'
#' @section Internals:
#' Uses the `$train` and `$predict` functions of the provided learner. Features that are entirely `NA` are imputed as `0`
#' or randomly sampled from available (`factor` / `logical`) levels.
#'
#' The [`Learner`][mlr3::Learner] does *not* necessarily need to handle missing values in cases
#' where `context_columns` is chosen well (or there is only one column with missing values present).
#'
#' @section Fields:
#' Fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`], as well as:
#' * `learner` :: [`Learner`][mlr3::Learner]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. Read-only.
#' * `learner_models` :: `list` of [`Learner`][mlr3::Learner] | `NULL`\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. This list is named by features for which a `Learner` was fitted, and
#'   contains the same `Learner`, but with different respective models for each feature. If this `PipeOp` is not trained,
#'   this is an empty `list`. For features that were entirely `NA` during training, the `list` contains `NULL` elements.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpImpute`]/[`PipeOp`].
#'
#' @examplesIf requireNamespace("rpart")
#' library("mlr3")
#'
#' task = tsk("pima")
#' task$missings()
#'
#' po = po("imputelearner", lrn("regr.rpart"))
#' new_task = po$train(list(task = task))[[1]]
#' new_task$missings()
#'
#' # '$state' of the "regr.rpart" Learner, trained to predict the 'mass' column:
#' po$state$model$mass
#'
#' library("mlr3learners")
#' # to use the "regr.kknn" Learner, prefix it with its own imputation method!
#' # The "imputehist" PipeOp is used to train "regr.kknn"; predictions of this
#' # trained Learner are then used to impute the missing values in the Task.
#' po = po("imputelearner",
#'   po("imputehist") %>>% lrn("regr.kknn")
#' )
#'
#' new_task = po$train(list(task = task))[[1]]
#' new_task$missings()
#'
#' @family PipeOps
#' @family Imputation PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpImpute.R
#' @export
PipeOpImputeLearner = R6Class("PipeOpImputeLearner",
  inherit = PipeOpImpute,
  public = list(
    initialize = function(learner, id = "imputelearner", param_vals = list()) {
      private$.learner = as_learner(learner, clone = TRUE)
      if (paradox_info$is_old) {
        private$.learner$param_set$set_id = ""
      }
      id = id %??% private$.learner$id
      feature_types = switch(private$.learner$task_type,
        regr = c("integer", "numeric"),
        classif = c("logical", "factor", "ordered"),
        stop("Only `classif` or `regr` Learners are currently supported by PipeOpImputeLearner.")
        # FIXME: at least ordinal should also be possible. When Moore's law catches up with us we could even do `character`
        # with generative text models, but by the time R/mlr3 can do that it is probably post-singularity.
      )
      super$initialize(id, param_set = alist(private$.learner$param_set), param_vals = param_vals,
        whole_task_dependent = TRUE, feature_types = feature_types)
    }
  ),
  active = list(
    learner = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$learner is read-only.")
        }
      }
      private$.learner
    },
    learner_models = function(val) {
      if (!missing(val)) {
        stop("$learner_models is read-only.")
      }
      if (is.null(self$state) || is_noop(self$state)) {
        list()
      } else {
        multiplicity_recurse(self$state$model, map, function(x) {
          if (!is.atomic(x)) clone_with_state(private$.learner, x)
        })
      }
    }
  ),
  private = list(
    .learner = NULL,
    .state_class = "pipeop_impute_learner_state",

    .train_imputer = function(feature, type, context) {
      on.exit({private$.learner$state = NULL})
      task = private$.create_imputation_task(feature, context)
      private$.learner$train(task, row_ids = which(!is.na(feature)))$state
    },

    .impute = function(feature, type, model, context) {
      nas = which(is.na(feature))
      if (!length(nas)) return(feature)
      if (is.atomic(model)) {  # handle nullmodel, making use of the fact that `Learner$state` is always a list
        return(super$.impute(feature, type, model, context))
      }
      on.exit({private$.learner$state = NULL})

      private$.learner$state = model

      # Use the trained learner to perform the imputation
      task = private$.create_imputation_task(feature, context)
      pred = private$.learner$predict(task, nas)

      # Replace the missing values with imputed values of the correct format
      imp_vals = private$.convert_to_type(pred$response, type)

      if (type %in% c("factor", "ordered")) {
        # in some edge cases there may be levels during training that are missing during predict.
        levels(feature) = c(levels(feature), as.character(type))
      }

      feature[nas] = imp_vals
      feature
    },

    .create_imputation_task = function(feature, context) {
      # Create a task that can be used by the learner based on imputation context
      context = cbind(context, ".impute_col" = private$.convert_to_predictable(feature))
      convert_to_task(data = context, target = ".impute_col", task_type = private$.learner$task_type)
    },

    .convert_to_predictable = function(feature) {
      # Convert non-factor imputation targets to a factor
      if (is.numeric(feature)) {
        feature
      } else {
        if (!is.null(levels(feature))) {
          factor(feature, levels = levels(feature), ordered = FALSE)
        } else {
          factor(feature, ordered = FALSE)
        }
      }
    },

    .convert_to_type = function(feature, type) {
      # Convert an imputed feature to its original type
      if(type == "integer"){
        feature = round(feature)
      }
      if (type == "logical") feature = as.logical(feature) # FIXME mlr-org/mlr3#475
      auto_convert(feature, "feature to be imputed", type, levels = levels(feature))
    },
    .additional_phash_input = function() private$.learner$phash
  )
)

mlr_pipeops$add("imputelearner", PipeOpImputeLearner, list(R6Class("Learner", public = list(id = "learner", task_type = "classif", param_set = ps()))$new()))

# See mlr-org/mlr#470
convert_to_task = function(id = "imputing", data, target, task_type, ...) {
  get(mlr_reflections$task_types[task_type, mult = "first"]$task)$new(id = id, backend = data, target = target, ...)
}

#' @export
marshal_model.pipeop_impute_learner_state = function(model, inplace = FALSE, ...) {
  prev_class = class(model)
  model$model = map(model$model, marshal_model, inplace = inplace, ...)

  if (!some(model$model, is_marshaled_model)) {
    return(model)
  }

  structure(
    list(marshaled = model, packages = "mlr3pipelines"),
    class = c(paste0(prev_class, "_marshaled"), "marshaled")
  )
}

#' @export
unmarshal_model.pipeop_impute_learner_state_marshaled = function(model, inplace = FALSE, ...) {
  state = model$marshaled
  state$model = map(state$model, unmarshal_model, inplace = inplace, ...)
  return(state)
}
