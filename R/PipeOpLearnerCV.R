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

      private$.crossval_param_set = ParamSet$new(params = list(
        ParamFct$new("resampling", values = "cv", default = "cv"),
        ParamInt$new("folds", lower = 2L, upper = Inf, default = 3L)
        )
      )
      private$.crossval_param_vals = list(resampling = "cv", folds = 3)

      super$initialize(id = learner$id,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    },

    train = function(inputs) {
      task = inputs[[1L]]
      learner = self$learner$clone(deep = TRUE)  # FIXME: see PipeOpLearner FIXME about cloning learner

      # Train a learner for predicting
      self$state = learner$train(task)

      # Compute CV Predictions
      rdesc = mlr_resamplings$get(self$param_vals[["resampling"]])
      rdesc$param_vals = list(folds = self$param_vals[["folds"]])
      res = resample(task, learner, rdesc)
      prds = do.call("rbind", map(res$data$prediction, function(x) as.data.table(x)))

      newtsk = private$pred_to_task(prds, task)
      list(newtsk)
    },

    predict = function(inputs) {
      task = inputs[[1]]

      prediction = self$state$predict(task)

      newtsk = private$pred_to_task(prediction, task)
      list(newtsk)
    }
  ),

  private = list(
    pred_to_task = function(prds, task) {
      prds = as.data.table(prds)
      setnames(prds, "row_id", task$backend$primary_key)
      prds[, truth := NULL]
      task$clone()$select(character())$cbind(prds)
    },
    .crossval_param_set = NULL,
    .crossval_param_vals = NULL
  ),

  active = list(
    param_set = function() {
      union_param_sets(list(self$learner$param_set, private$.crossval_param_set))
    },
    # FIXME: Not sure if this works
    param_vals = function(vals) {
      union_param_vals(list(self$learner$param_set, private$.crossval_param_set),
        list(self$learner, private), c("param_vals", ".crossval_param_vals"), vals)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpLearnerCV", PipeOpLearnerCV)
