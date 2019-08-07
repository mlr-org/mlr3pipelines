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
#'   Learner to use for prediction.
#' @section Methods:
#' * `PipeOpLearner$new(learner, id = learner$id)` \cr
#'   ([`Learner`], `character(1)`) -> `self` \cr
#'   Constructor. The given learner will be used for prediction.
#' @family PipeOps
#' @family Meta PipeOps
#' @include PipeOp.R
#' @export
PipeOpLearner = R6Class("PipeOpLearner", inherit = PipeOp,
  public = list(
    learner = NULL,

    initialize = function(learner, id = if (is.character(learner)) learner else learner$id, param_vals = list()) {
      self$learner = assert_learner(learner, clone = TRUE)
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction")
      )
      private$.param_set = NULL
    },

    train_internal = function(inputs) {
      on.exit({self$learner$state = NULL})
      task = inputs[[1L]]
      self$state = self$learner$train(task)$state

      list(NULL)
    },

    predict_internal = function(inputs) {
      on.exit({self$learner$state = NULL})
      task = inputs[[1]]
      self$learner$state = self$state
      list(self$learner$predict(task))
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

mlr_pipeops$add("learner", PipeOpLearner, list(R6Class("Learner", public = list(id = "learner", param_set = ParamSet$new()))$new()))
