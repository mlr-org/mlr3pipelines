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
#' Thresholds for each learner are optimized using (GenSA)[GenSA::GenSA].
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
        ParamFct$new("algorithm", default = "GenSA", levels = c("GenSA")),
        ParamInt$new("maxit", default = 10000L, lower = 1L, upper = Inf),
        ParamDbl$new("threshold.stop", default = NULL, lower = -Inf, upper = Inf, special_vals = list(NULL)),
        ParamInt$new("nb.stop.improvement", default = NULL, lower = 0L, upper = Inf, special_vals = list(NULL)),
        ParamLgl$new("smooth", default = FALSE),
        ParamInt$new("max.call", default = 3000L, lower = 1L, upper = Inf),
        ParamDbl$new("max.time", default = NULL, lower = 1L, upper = Inf, special_vals = list(NULL)),
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
        acceptance.param = -15, simple.function = TRUE, lower = 0, upper = 1)
      super$initialize(id, param_vals = param_vals, param_set = ps, packages = "GenSA")
    },
    train = function(input) {
      pred = private$make_prediction_classif(input[[1]])
      assert_class(pred, "PredictionClassif")
      self$measure = self$param_set$values$measure
      if (is.null(self$measure)) self$measure = mlr_measures$get("classif.mmce")
      assert_measure(self$measure)
      assert_true(self$measure$task_type == "classif")
      th = private$optimize_objfun_gensa(pred)
      self$state = list("threshold" = th)
      return(list(NULL))
    },
    predict = function(input) {#
      pred = private$make_prediction_classif(input[[1]])
      pred$threshold = self$state$threshold
      return(list(pred))
    }
  ),
  private = list(
    objfun = function(threshold, pred) {
      pred$threshold = threshold / sum(threshold)
      e = list("prediction" = pred)
      res = self$measure$calculate(e)
      if (!self$measure$minimize) res = -res
      res
    },
    optimize_objfun_gensa = function(pred) { #
      requireNamespace("GenSA")
      nclass = ncol(pred$prob)
      cnames = colnames(pred$prob)
      pv = self$param_set$values
      ctrl = pv[which(!(names(pv) %in% c("measure", "algorithm", "lower", "upper")))]
      if (nclass > 2) {
        # init_threshold = rep(1 / nclass, nclass)
        # or = GenSA::GenSA(par = init_threshold, fn = private$objfun, pred = pred,
        or = GenSA::GenSA(fn = private$objfun, pred = pred,
        lower = rep(pv$lower, nclass), upper = rep(pv$upper, nclass), control = ctrl)
        th = or$par / sum(or$par)
        names(th) = cnames
      } else if (nclass == 2) {
        or = GenSA::GenSA(fn = private$objfun, pred = pred,
          lower = pv$lower, upper = pv$upper, control = ctrl)
        th = or$par
      }
      return(th)
    },
    # FIXME This is ugly, but currently the best way
    make_prediction_classif = function(input) {
      p = PredictionClassif$new()
      prob = as.matrix(input$data(cols = input$feature_names))
      colnames(prob) = unlist(input$levels())
      p$prob = prob
      i = max.col(prob, ties.method = "random")
      response = factor(colnames(prob)[i], levels = unlist(input$levels()))
      p$response = response
      p$truth = input$truth()
      p$predict_types = c("response", "prob")
      p$row_ids = input$row_ids
      return(p)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpTuneThreshold", PipeOpTuneThreshold)
