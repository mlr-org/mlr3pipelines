#' @title PipeOpLearner
#' @format [R6Class] PipeOpLearner
#'
#' @description
#'   Wraps a [mlr3::Learner] into a [PipeOp].
#'   Inherits the `param_set` from the [mlr3::Learner] it is constructed from.
#' @section Usage:
#' Inherits from [PipeOp]
#' * `f = PipeOpLearner$new(outnum, id)` \cr
#'   `[Learner]` -> [PipeOpLearner]
#' @name PipeOpLearner
#' @family PipeOp, PipeOpLearner
#' @export
PipeOpLearner = R6Class("PipeOpLearner", inherit = PipeOp,
  public = list(
    learner = NULL,

    initialize = function(learner) {
      self$learner = learner
      super$initialize(learner$id)
      private$.intype = list("data.frame")
      private$.outtype = list("model")
    },

    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      task = inputs[[1L]]

      experiment = Experiment$new(task = task, learner = self$learner)
      experiment$train()
      self$state = experiment
      return(list(NULL))
      # FIXME: what do we return here? prediction on train data?
      # experiment$predict()
      # d = experiment$prediction[,-1]
      # colnames(d)[seq_along(task$target_names)] = task$target_names

      # db = as_data_backend(d)
      # private$.result = TaskClassif$new(id = task$id, backend = db, target = task$target_names)
      # private$.result
    },

    predict = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      task = inputs[[1L]]
      list(self$state$learner$predict(task))
    }
  ),

  active = list(
    param_set = function() self$learner$param_set,

    param_vals = function(value) {
      if (missing(value)) return(self$learner$param_vals)
      else self$learner$param_set = value
    }
  )
)


