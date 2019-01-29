#' @title PipeOpLearner
#'
#' @name PipeOpLearner
#' @format [R6Class] PipeOpLearner
#'
#' @description
#'   Wraps a [mlr3::Learner] into a [PipeOp].
#'   Inherits the `param_set` from the [mlr3::Learner] it is constructed from.
#' @section Usage:
#' Inherits from [PipeOp]
#' * `f = PipeOpLearner$new(outnum, id)` \cr
#'   `[Learner]` -> [PipeOpLearner]
#' @family PipeOp
NULL

#' @include PipeOp.R
#' @export
PipeOpLearner = R6Class("PipeOpLearner", inherit = PipeOp,
  public = list(
    learner = NULL,

    initialize = function(learner) {
      assert_learner(learner)
      self$learner = learner
      super$initialize(learner$id,
        input = data.table(name = "task", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction")
      )
    },

    train = function(inputs) {
      task = inputs[[1L]]

      # FIXME: clone has advantages and disadvantages here, if there is ever a reason
      # to change parameter values after train we may not want to clone here.
      self$state = self$learner$clone(deep = TRUE)$train(task)

      return(list(NULL))
    },

    predict = function(inputs) {
      task = inputs[[1]]

      list(output = self$state$predict(task))
    }

  ),

  active = list(
    param_set = function() self$learner$param_set,

    param_vals = function(value) {
      if (missing(value)) return(self$learner$param_vals)
      else self$learner$param_vals = value
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpLearner", PipeOpLearner)
