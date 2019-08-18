#' @title PipeOpLearner
#'
#' @usage NULL
#' @name mlr_pipeops_learner
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Wraps an [`mlr3::Learner`] into a [`PipeOp`].
#'
#' Inherits the `$param_set` (and therefore `$param_set$values`) from the [`Learner`][mlr3::Learner] it is constructed from.
#'
#' Using [`PipeOpLearner`], it is possible to embed [`mlr3::Learner`]s into [`Graph`]s, which themselves can be
#' turned into Learners using [`GraphLearner`]. This way, preprocessing and ensemble methods can be included
#' into a machine learning pipeline which then can be handled as singular object for resampling, benchmarking
#' and tuning.
#'
#' @section Construction:
#' ```
#' PipeOpLearner$new(learner, id = if (is.character(learner)) learner else learner$id, param_vals = list())` \cr
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
#' [`PipeOpLearner`] has one input channel named `"input"`, taking a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' [`PipeOpLearner`] has one output channel named `"output"`, producing `NULL` during training and a [`Prediction`][mlr3::Prediction] subclass
#' during prediction; this subclass is specific to the [`Learner`][mlr3::Learner] type given to `learner` during construction.
#'
#' The output during prediction is the [`Prediction`][mlr3::Prediction] on the prediction input data, produced by the [`Learner`][mlr3::Learner]
#' trained on the training input data.
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
#' @examples
#' lrn_po = mlr_pipeops$get("learner", "classif.rpart")
#'
#' lrn_po$param_set$values$cp = 0.7
#'
#' lrn_po$train(list("iris"))
#'
#' lrn_po$predict(list("iris"))
#'
#' @family PipeOps
#' @family Meta PipeOps
#' @include PipeOp.R
#' @export
PipeOpLearner = R6Class("PipeOpLearner", inherit = PipeOp,
  public = list(
    initialize = function(learner, id = if (is.character(learner)) learner else learner$id, param_vals = list()) {
      private$.learner = assert_learner(learner, clone = TRUE)
      stop(str_collapse(deparse(mlr_reflections$task_types[private$.learner$task_type])))
      task_type = mlr_reflections$task_types[private$.learner$task_type]$task
      out_type = mlr_reflections$task_types[private$.learner$task_type]$prediction
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = "input", train = task_type, predict = task_type),
        output = data.table(name = "output", train = "NULL", predict = out_type)
      )
      private$.param_set = NULL
    },

    train_internal = function(inputs) {
      on.exit({private$.learner$state = NULL})
      task = inputs[[1L]]
      self$state = private$.learner$train(task)$state

      list(NULL)
    },

    predict_internal = function(inputs) {
      on.exit({private$.learner$state = NULL})
      task = inputs[[1]]
      private$.learner$state = self$state
      list(private$.learner$predict(task))
    }
  ),
  active = list(
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.learner$param_set)) {
        stop("$param_set is read-only.")
      }
      private$.learner$param_set
    },
    id = function(val) {
      if (!missing(val)) {
        private$.id = val
        private$.learner$param_set$set_id = val
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

mlr_pipeops$add("learner", PipeOpLearner, list(R6Class("Learner", public = list(id = "learner", task_type = "classif", param_set = ParamSet$new()))$new()))
