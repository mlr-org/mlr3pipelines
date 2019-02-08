#' @title PipeOpLearner
#'
#' @name mlr_pipeop_learner
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#'   Wraps an [`mlr3::Learner`] into a [`PipeOp`].
#'   Inherits the `$param_set` and `$values` from the `Learner` it is constructed from.
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
      self$learner = learner$clone(deep = TRUE)
      super$initialize(id,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction")
      )
      private$.param_set = NULL
    },

    train = function(inputs) {
      task = inputs[[1L]]
      self$state = self$learner$train(task)

      list(NULL)
    },

    predict = function(inputs) {
      task = inputs[[1]]
      list(self$state$predict(task))
    }
  ),
  active = list(
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, self$learner$param_set)) {
        stop("param_set is read-only.")
      }
      self$learner$param_set
    },
    id = function(val) {
      if (!missing(val)) {
        private$.id = val
        self$learner$param_set$set_id = val
      }
      private$.id
    }
  )
)

# This does not work because we do not have a "default" learner.
# Maybe we initialize with NULL and let the user set $learner?
# Would have to be careful with param_set and values
# #' @include mlr_pipeops.R
# mlr_pipeops$add("learner", PipeOpLearner)
