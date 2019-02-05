#' @title PipeOpLearner
#'
#' @name mlr_pipeop_learner
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#'   Wraps an [`mlr3::Learner`] into a [`PipeOp`].
#'   Inherits the `$param_set` and `$param_vals` from the `Learner` it is constructed from.
#'
#' @section Public Members / Active Bindings:
#' * `learner`  :: [`Learner`] \cr
#'   Learner to use for cross validation / prediction.
#' @section Methods:
#' * `PipeOpLearner$new(learner, id = learner$id)` \cr
#'   ([`Learner`], `character(1)`) -> `self` \cr
#'   Constructor. The given learner will be used for crossvalidation.
#' @family PipeOps
#' @family Meta PipeOps
#' @include PipeOp.R
#' @export
PipeOpLearner = R6Class("PipeOpLearner", inherit = PipeOp,
  public = list(
    learner = NULL,

    initialize = function(learner, id = learner$id) {
      assert_learner(learner)
      self$learner = learner
      super$initialize(id,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction")
      )
    },

    train = function(inputs) {
      # FIXME: clone has advantages and disadvantages here, if there is ever a reason
      # to change parameter values after train we may not want to clone here.
      task = inputs[[1L]]
      self$state = self$learner$clone(deep = TRUE)$train(task)

      list(NULL)
    },

    predict = function(inputs) {
      task = inputs[[1]]
      list(self$state$predict(task))
    }

  ),

  active = list(
    param_set = function() self$learner$param_set,

    param_vals = function(value) {
      if (missing(value))
        return(self$learner$param_vals)
      self$learner$param_vals = value
    }
  )
)

# This does not work because we do not have a "default" learner.
# Maybe we initialize with NULL and let the user set $learner?
# Would have to be careful with param_set and param_vals
# #' @include mlr_pipeops.R
# mlr_pipeops$add("learner", PipeOpLearner)
