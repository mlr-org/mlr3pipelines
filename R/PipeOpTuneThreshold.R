#' @title PipeOpTuneThreshold
#'
#' @usage NULL
#' @name mlr_pipeops_tunethreshold
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Tunes optimal probability thresholds over different [`PredictionClassif`]s.
#' Learner's `predict_type` `"prob"` is required.
#' Thresholds for each learner are optimized using (GenSA)[GenSA::GenSA].
#' Returns a single [`PredictionClassif`].
#' As a default, optimizes the miss-classification error [`MeasureClassif`].
#' This PipeOp should be used in conjunction with [`PipeOpLearnerCV`] in order to
#' optimize thresholds of predictions.
#'
#' @section Construction:
#' ```
#' * `PipeOpTuneThreshold$new(id, param_vals = list)` \cr
#'   (`character(1)`, `list`) -> `self` \cr
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings
#'   that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpPredPostproc`].
#'
#' @section State:
#' The `$state` is a named `list` with elements
#' * `thresholds` :: `character` learned thresholds
#'
#' @section Parameters:
#'  * `measure` :: [`Measure`]\cr
#'    [`Measure`] to optimize.
#'    Defaults to `msr("classif.ce")`, i.e. misclassification error.
#'  * `optimizer` :: [`Optimizer`]\cr
#'    [`Optimizer`] used to find optimal thresholds.
#'    Defaults to `OptimizerGenSA$new()`
#' @section Internals:
#' Uses the provided optimizer in order to
#' @section Methods:
#' Only methods inherited from [`PipeOpPredPostproc`].
#'
#' @family PipeOps
#' @include PipeOpPredPostproc.R
#' @export
PipeOpTuneThreshold = R6Class("PipeOpTuneThreshold",
  inherit = PipeOpPredPostproc,

  public = list(
    initialize = function(id = "tunethreshold", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("measure", custom_check = function(x) check_r6(x, classes = "MeasureClassif"), tags = "train"),
        ParamUty$new("optimizer", custom_check = check_optimizer, tags = "train")
      ))
      ps$values = list(measure = msr("classif.ce"), optimizer = OptimizerGenSA$new())
      super$initialize(id, param_vals = param_vals, param_set = ps, packages = "GenSA")
    },
    train = function(input) {
      pred = private$task_to_prediction(input[[1]])
      th = private$optimize_objfun(pred)
      self$state = list("threshold" = th)
      return(list(NULL))
    },
    predict = function(input) {#
      pred = private$task_to_prediction(input[[1]])
      pred$set_threshold(self$state$threshold)
      return(list(pred))
    }
  ),
  private = list(
    objfun = function(threshold, pred, measure) {
      lvls = colnames(pred$prob)
      res = pred$set_threshold(set_names(threshold, lvls))$score(measure)
      if (!measure$minimize) res = -res
      res
    },
    optimize_objfun = function(pred) {
      cnames = colnames(pred$prob)
      opt = self$param_set$values$optimizer
      thr = opt$optimize(private$objfun, init_weights = rep(1, length(cnames)) / length(cnames),
        pred = pred, measure = self$param_set$values$measure)
      set_names(thr, cnames)
    },
    task_to_prediction = function(input) {
      prob = as.matrix(input$data(cols = input$feature_names))
      colnames(prob) = unlist(input$levels())
      PredictionClassif$new(input, row_ids = input$row_ids, truth = input$truth(),
        response = factor(colnames(prob)[max.col(prob, ties.method = "random")], levels = unlist(input$levels())),
        prob = prob)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("tunethreshold", PipeOpTuneThreshold)
