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
      assert_learner(learner)
      self$learner = learner
      super$initialize(learner$id)
      self$train_intypes = "Task"
      self$train_outtypes = "any"
      self$predict_intypes = "Task"
      self$predict_outtypes = "Prediction"
    },

    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      task = inputs[[1L]]

      # FIXME: clone has advantages and disadvantages here, if there is ever a reason
      # to change parameter values after train we may not want to clone here.
      self$state = self$learner$clone(deep = TRUE)$train(task)


      # experiment$predict()
      # d = experiment$prediction[,-1]
      # colnames(d)[seq_along(task$target_names)] = task$target_names

      # db = as_data_backend(d)
      # private$.result = TaskClassif$new(id = task$id, backend = db, target = task$target_names)
      # private$.result
      return(list(NULL))
    },

    predict = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      list(self$state$predict(task))
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

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpLearner", PipeOpLearner)

