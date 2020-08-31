#' @title PipeOpPseudoLabelLearner
#'
#' @usage NULL
#' @name mlr_pipeops_learner
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Wraps an [`mlr3::Learner`] into a [`PipeOp`] together with pseudo-labelling of missing observations.
#' Pseudo-labelling iteratively predicts labels of data points with missing target values and afterwards adds those
#' labels to the pool of observations, finally training the learner on original and pseudo-labelled data.
#'
#' Inherits the `$param_set` (and therefore `$param_set$values`) from the [`Learner`][mlr3::Learner] it is constructed from.
#'
#' Using [`PipeOpPseudoLabelLearner`], it is possible to embed [`mlr3::Learner`]s into [`Graph`]s
#' toegether with pseudo-labelling of observations.
#' This way, preprocessing and ensemble methods can be included
#' into a machine learning pipeline which then can be handled as singular object for resampling, benchmarking
#' and tuning.
#'
#' @section Construction:
#' ```
#' PipeOpPseudoLabelLearner$new(learner, id = NULL, param_vals = list())
#' ```
#'
#' * `learner` :: [`Learner`][mlr3::Learner] | `character(1)`
#'   [`Learner`][mlr3::Learner] to wrap, or a string identifying a [`Learner`][mlr3::Learner] in the [`mlr3::mlr_learners`] [`Dictionary`][mlr3misc::Dictionary].
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, internally defaulting to the `id` of the [`Learner`][mlr3::Learner] being wrapped.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpPseudoLabelLearner`] has one input channel named `"input"`, taking a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' [`PipeOpPseudoLabelLearner`] has one output channel named `"output"`, producing `NULL` during training and a [`Prediction`][mlr3::Prediction] subclass
#' during prediction; this subclass is specific to the [`Learner`][mlr3::Learner] type given to `learner` during construction.
#'
#' The output during prediction is the [`Prediction`][mlr3::Prediction] on the prediction input data, produced by the [`Learner`][mlr3::Learner]
#' trained on the training input data.
#'
#' @section State:
#'   The trained model and some additional info. See [`PipeOpLearner`] for more information.
#'
#' @section Parameters:
#' The parameters are exactly the parameters of the [`Learner`][mlr3::Learner] wrapped by this object.
#'
#' @section Fields:
#' Fields inherited from [`PipeOpLearner`].

#'
#' @section Methods:
#' Methods inherited from [`PipeOpLearner`].
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
#' lrn_po = mlr_pipeops$get("pllearner", learner)
#'
#' lrn_po$train(list(task))
#' lrn_po$predict(list(task))
PipeOpPseudoLabelLearner = R6Class("PipeOpPseudoLabelLearner", inherit = PipeOpPseudoLabelLearner,
  public = list(
    initialize = function(learner, id = NULL, param_vals = list()) {
      id = id %??% private$.learner$id
      super$initialize(learner, id, param_vals)
    }
  ),
  private = list(
    .train = function(inputs) {
      on.exit({private$.learner$state = NULL})
      task = inputs[[1L]]

      # Batch-wise: Predict pseudo-labels and add to task
      usrows = which(any(is.na(t$data(cols = t$target_names))))
      while (length(usrows)) {
          private$.learner$train(task)
          # FIXME: Batch size to param.
          rows = sample(usrows, min(length(usrows), 8))
          dt = task$data(rows = rows)
          dt[[task$target_names]] = predict(private$.learner, dt)
          task$rbind(dt)
          usrows = setdiff(usrows, rows)
      }
      # Final train step on pseudo-labelled data.
      self$state = private$.learner$train(task)$state

      list(NULL)
    }
  )
)

mlr_pipeops$add("pllearner", PipeOpPseudoLabelLearner, list(R6Class("Learner", public = list(id = "learner", task_type = "classif", param_set = ParamSet$new()))$new()))

