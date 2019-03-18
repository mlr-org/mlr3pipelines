#' @title PipeOpTuneThreshold
#'
#' @format [R6Class] PipeOpTuneThreshold
#'
#' @name mlr_pipeop_tunethreshold
#' @format [`R6Class`] inheriting from [`PipeOpPredPostproc`].
#'
#' @description
#' Tunes optimal probability thresholds over different [`PredictionClassif`]s.
#' Learner's `predict_type` `"prob"` is required.
#' Thresholds for each learner are optimized using (GenSa)[GenSA::GenSA].
#' Returns a single [`PredictionClassif`].
#' As a default, optimizes [`MeasureClassifMMCE`].
#' Used for classification [`Prediction`]s.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpTuneThreshold$new()
#' @export
PipeOpTuneThreshold = R6Class("PipeOpTuneThreshold",
  inherit = PipeOpPredPostproc,

  public = list(
    measure = NULL,
    threshold = NULL,
    initialize = function(id = "tunethreshold", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("measure", default = NULL),
        ParamFct$new("algorithm", default = "GenSA"),
        ParamInt$new("maxit", default = , lower = 1L, upper = Inf),
        ParamDbl$new("threshold.stop", default = NULL, lower = -Inf, upper = Inf),
        ParamInt$new("nb.stop.improvement", default = NULL, lower = 0L, upper = Inf),
        ParamLgl$new("smooth", default = FALSE),
        ParamInt$new("max.call", default = 3000L, lower = 1L, upper = Inf),
        ParamDbl$new("max.time", default = NULL, lower = 1L, upper = Inf),
        ParamInt$new("temperature", default = 250L, lower = 1L, upper = Inf),
        ParamDbl$new("visiting.param", default = 2.5, lower = 0L, upper = Inf),
        ParamDbl$new("acceptance.param", default = -15, lower = -Inf, upper = Inf),
        ParamLgl$new("verbose", default = FALSE),
        ParamLgl$new("simple.function", default = TRUE),
        ParamDbl$new("lower", default = 0, lower = -Inf, upper = Inf),
        ParamDbl$new("upper", default = 1, lower = -Inf, upper = Inf)
        # FIXME: Possibly implement more params, currently not important
      ))
      ps$values = list(measure = NULL, algorithm = "GenSA", smooth = FALSE,
        max.call = 3000L, temperature = 250, visiting.param = 2.5,
        acceptance.param = -15, simple.function = TRUE, lb = 0, ub = 1)
      super$initialize(id, param_vals = param_vals, param_set = ps, packages = "GenSa")
    },
    train = function(input) {
      assert_list(input, "PredictionClassif")
      self$measure = self$param_set$values$measure
      if (is.null(self$measure)) self$measure = mlr_measures$get("classif.mmce")
      assert_measure(self$measure)
      assert_true(self$measure$task_type == "classif")
      th = private$optimize_objfun_gensa(input)
      self$state = list("threshold" = th)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpTuneThreshold", PipeOpTuneThreshold)
