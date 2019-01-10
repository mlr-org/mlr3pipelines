#' @title PipeOpLearnerCV
#' @format [R6Class] PipeOpLearnerCV
#'
#' @description
#' Wraps a [mlr3::Learner] into a [PipeOp].
#' Returns cross-validated predictions during training and
#' Inherits the `param_set` from the [mlr3::Learner] it is constructed from.
#' Parameters:
#' * `resamping`                         :: [character]
#' Which resampling method do we want to use. Currently only supports 'cv'.
#' * `folds`                   :: [integer]
#' 
#' @section Usage:
#' Inherits from [PipeOp]
#' * `f = PipeOpLearner$new(outnum, id)` \cr
#'   `[Learner]` -> [PipeOpLearnerCV]
#' @name PipeOpLearnerCV
#' @family PipeOp, PipeOpLearner
#' @export
NULL

#' @include PipeOp.R
#' @export
PipeOpLearnerCV = R6Class("PipeOpLearnerCV",
  inherit = PipeOp,
  public = list(
    learner = NULL,

    initialize = function(learner) {
      assert_learner(learner)
      self$learner = learner

      #FIXME: It is unclear if we want to keep this as-is
      #       Maybe use ParamSetCollection in the future
      #       Resampling will be extended in the future
      ps = ParamSet$new(params = list(
        ParamFct$new("resampling", values = "cv", default = "cv"),
        ParamInt$new("folds", lower = 2L, upper = Inf, default = 3L)
      ))
      ps$add(self$learner$param_set)

      super$initialize(learner$id,
        param_set = ps,
        input = data.table(name = "task", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    },

    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      task = inputs[[1L]]
      learner = self$learner$clone(deep = TRUE)

      # Train a learner for predicting
      self$state = Experiment$new(task = task, learner = learner)$train()

      # Compute CV Predictions
      rdesc = mlr_resamplings$get(self$param_vals[["resampling"]])
      rdesc$param_vals = list(folds = self$param_vals[["folds"]])
      res = resample(task, learner, rdesc)
      prds = do.call("rbind", map(res$data$prediction, function(x) as.data.table(x)))
      
      newtsk = private$pred_to_task(prds, task)
      return(list(newtsk))
    },
    predict = function(inputs) {
      assert_list(inputs, len = 1L, type = c("Task", "data.frame"))
      task = inputs[[1]]

      if (is.data.frame(task)) {
        self$state$predict(newdata = task)
      } else {
        self$state$predict(row_ids = task$row_ids[[1L]])
      }
      newtsk = private$pred_to_task(self$state$prediction, task)
      list(output = newtsk)
    }
  ),

  private = list(
    pred_to_task = function(prds, task) {
      prds = as.data.table(prds)
      setnames(prds, "row_id", task$backend$primary_key)
      prds[, truth := NULL]
      task$clone()$select(character())$cbind(prds)
    }
  ),

  active = list(
    param_set = function() private$.param_set,
    # FIXME: Not sure if this works
    param_vals = function(vals) {
      if (!missing(vals)) {
        # FIXME: param check
        if (!self$param_set$test(vals)) {
          stop("Parameters out of bounds")
        }
        private$.param_vals = vals
      }
      private$.param_vals
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpLearnerCV", PipeOpLearnerCV)
