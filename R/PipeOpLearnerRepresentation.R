#' @title PipeOpLearnerRepresentation
#'
#' @usage NULL
#' @name mlr_pipeops_learner_representation
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Extracts a representation from an [`mlr3::Learner`].
#' 
#' Inherits the `$param_set` (and therefore `$param_set$values`) from the [`Learner`][mlr3::Learner] it is constructed from.
#'
#' Using [`PipeOpLearnerRepresentation`], it is possible to extract a representation (i.e. intermediate features)
#' from a [`mlr3::Learner`], in order to use them for subsequent modeling steps.
#' 
#' Currently implemented for the following models:
#' * xgboost
#' * rpart
#' 
#' See [`learner_repr`] for details.
#'
#' @section Construction:
#' ```
#' PipeOpLearnerRepresentation$new(learner, id = if (is.character(learner)) learner else learner$id, param_vals = list())\cr
#' ```
#'
#' * `learner` :: [`Learner`][mlr3::Learner] | `character(1)`
#'   [`Learner`][mlr3::Learner] to wrap, or a string identifying a [`Learner`][mlr3::Learner] in the [`mlr3::mlr_learners`] [`Dictionary`][mlr3misc::Dictionary].
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, defaulting to the `id` of the [`Learner`][mlr3::Learner] being wrapped.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#'   Same as {`PipeOpTaskPreproc`].
#'
#' @section State:
#' The `$state` is set to the `$state` slot of the [`Learner`][mlr3::Learner] object. It is a named `list` with members:
#' * `model` :: `any`\cr
#'   Model created by the [`Learner`][mlr3::Learner]'s `$train_internal()` function.
#' * `train_log` :: [`data.table`] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during training.
#' * `train_time` :: `numeric(1)`\cr
#'   Training time, in seconds.
#' * `predict_log` :: `NULL` | [`data.table`] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during prediction.
#' * `predict_time` :: `NULL` | `numeric(1)`
#'   Prediction time, in seconds.
#'
#' @section Parameters:
#' The parameters are exactly the parameters of the [`Learner`][mlr3::Learner] wrapped by this object.
#'
#' @section Internals:
#' The `$state` is currently not updated by prediction, so the `$state$predict_log` and `$state$predict_time` will always be `NULL`.
#'
#' @section Fields:
#' Fields inherited from [`PipeOp`], as well as:
#' * `learner`  :: [`Learner`][mlr3::Learner]\cr
#'   [`Learner`][mlr3::Learner] that is being wrapped. Read-only.
#'
#' @section Methods:
#' Methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Meta PipeOps
#' @include PipeOp.R
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' learner = lrn("classif.rpart", cp = 0.1)
#' lrn_po = mlr_pipeops$get("learner_repr", learner)
#'
#' lrn_po$train(list(task))
#' lrn_po$predict(list(task))
PipeOpLearnerRepresentation = R6Class(
  "PipeOpLearnerRepresentation",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(learner, id = if (is.character(learner)) learner else learner$id, param_vals = list()) {
      private$.learner = as_learner(learner)$clone(deep = TRUE) 
      super$initialize(id, param_set = alist(private$.learner$param_set), param_vals = param_vals)
    },
    train_task = function(task) {
      on.exit({private$.learner$state = NULL}) 
      cols = self$select_cols(task)
      if (!length(cols)) {
        self$state = list(dt_columns = dt_columns)
        return(task)
      }   
      task = task$select(cols)
      self$state = private$.learner$train(task)$state
      dt = learner_repr(self$state$model, task)
      self$state$dt_columns = cols
      task$select(setdiff(task$feature_names, cols))$cbind(dt)
    },

    predict_task = function(task) {
      cols = self$state$dt_columns
      if (!length(cols)) {
        self$state = list(dt_columns = dt_columns)
        return(task)
      }
      task = task$select(cols)
      dt = learner_repr(self$state$model, task)
      task$select(setdiff(task$feature_names, cols))$cbind(dt)    }
  ),
  active = list(
    id = function(val) {
      if (!missing(val)) {
        private$.id = paste0("repr_", val)
        private$.learner$param_set$set_id = paste0("repr_", val)
      }
      private$.id
    },
    learner = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learner)) {
          stop("$learner is read-only.")
        }
      }
      private$.learner
    }
  ),
  private = list(
    .learner = NULL
  )
)

mlr_pipeops$add("learner_repr", PipeOpLearnerRepresentation, list(R6Class("Learner", public = list(id = "learner", task_type = "classif", param_set = ParamSet$new()))$new()))



#' Extract a representation from a learner
#' 
#' @rdname learner_repr
#' 
#' @param x [`Learner`] \cr
#'   A trained learner (in the pipeop's state)
#' @param task [`Task`] \cr
#'   A task, the learner was trained on.
#' @param ... [`any`] \cr
#'   Further arguments
#' @return [`data.table`] containing extracted features.
#' @export
learner_repr = function(x, task, ...) {
  UseMethod("learner_repr")
}

learner_repr.default = function(x, task, ...) {
  stopf("Not implemented for Learner of class %s!", class(x)[[1]])
}


#' @rdname learner_repr
#' 
#' @details 
#'   for ' xgb.Booster' (xgboost), internally uses xgboost::xgb.create.features.
learner_repr.xgb.Booster = function(x, task, ...) {
  data = data.matrix(task$data())
  fts  = data.table(as.matrix(xgboost::xgb.create.features(x, data)))
  fts[, task$feature_names := NULL]
}

#' @rdname learner_repr
#' 
#' @details 
#'   for 'rpart',  treeClust::rpart.predict.leaves is used.
learner_repr.rpart = function(x, task, ...) {
  data = task$data()
  fts  = treeClust::rpart.predict.leaves(x, data)
  data.table("leaf" = as.factor(fts))
}
