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
        output = data.table(name = "output", train = "*", predict = "Prediction")
      )
    },

    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      task = inputs[[1L]]

      # FIXME: clone has advantages and disadvantages here, if there is ever a reason
      # to change parameter values after train we may not want to clone here.
      self$state = Experiment$new(
        task = task,
        learner = self$learner$clone(deep = TRUE)
      )$train()


      # experiment$predict()
      # d = experiment$prediction[,-1]
      # colnames(d)[seq_along(task$target_names)] = task$target_names

      # db = as_data_backend(d)
      # private$.result = TaskClassif$new(id = task$id, backend = db, target = task$target_names)
      # private$.result
      return(list(NULL))
    },

    predict = function(inputs) {
      assert_list(inputs, len = 1L, type = c("Task", "data.frame"))
      task = inputs[[1]]

      if (is.data.frame(task)) {
        self$state$predict(newdata = task)
      } else {
        self$state$predict(row_ids = task$row_ids[[1L]])
      }
      list(output = self$state$prediction)
    }

  ),

  active = list(
    param_set = function() self$learner$param_set,

    param_vals = function(value) {
      # FIXME This is a bug, add tests
      if (missing(value)) return(self$learner$param_vals)
      else self$learner$param_set = value
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpLearner", PipeOpLearner)
