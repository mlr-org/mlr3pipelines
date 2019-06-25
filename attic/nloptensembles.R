
  private = list(
    objfun = function(wts, inputs) {
      prd = private$weighted_avg_predictions(inputs, wts)
      e = list("prediction" = prd)
      res = self$measure$calculate(e)
      if (!self$measure$minimize) res = -res
      res
    },
    optimize_objfun_nlopt = function(inputs) {
      requireNamespace("nloptr")
      init_weights = rep(1 / length(inputs), length(inputs))
      pv = self$param_set$values
      eval_g_ineq =
        opts = pv[which(!(names(pv) %in% c("measure", "eval_g_ineq", "lb", "ub")))]
      opt = nloptr::nloptr(
        x0 = init_weights,
        eval_f = private$objfun,
        lb = rep(pv$lb, length(inputs)),
        ub = rep(pv$ub, length(inputs)),
        eval_g_ineq = pv$eval_g_ineq,
        opts = opts,
        inputs = inputs
      )
      return(opt$solution)
    }
  )


#' @title PipeOpNlOptModelAvg
#'
#' @format [R6Class] PipeOpNlOptModelAvg
#'
#' @name mlr_pipeop_nloptmajorityvote
#' @format [`R6Class`] inheriting from [`PipeOpModelAvg`].
#'
#' @description
#' Aggregates over different [`PredictionRegr`]s.
#' Weights for each learner are learned using (nloptr)[nloptr::nloptr].
#' For help with nloptr see [`nloptr::nloptr.print.options()`].
#' Returns a single [`PredictionRegr`].
#' By default, optimizes [`MeasureRegrMSE`] and only allows weights between 0 and 1.
#' Used for regression [`Prediction`]s.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpNlOptModelAvg$new(3)
#' @export
PipeOpNlOptModelAvg = R6Class("nloptmodelavg",
  inherit = PipeOpModelAvg,

  public = list(
    measure = NULL,
    initialize = function(innum, id = "nloptmodelavg", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("measure", default = NULL),
        ParamFct$new("algorithm", default = "NLOPT_LN_COBYLA",
          levels = strsplit(nloptr::nloptr.get.default.options()[1, "possible_values"], ", ")[[1]]),
        ParamUty$new("eval_g_ineq", default = function(x) max(x) - 1),
        ParamDbl$new("xtol_rel", default = 10^-4, lower = 0, upper = Inf),
        ParamDbl$new("xtol_abs", default = 0, lower = 0, upper = Inf),
        ParamDbl$new("ftol_rel", default = 0, lower = 0, upper = Inf),
        ParamDbl$new("ftol_abs", default = 0, lower = 0, upper = Inf),
        ParamDbl$new("stopval", default = -Inf, lower = -Inf, upper = Inf),
        ParamInt$new("maxeval", default = 100, lower = 1L, upper = Inf),
        ParamInt$new("maxtime", default = -1L, lower = 0L, upper = Inf, special_vals = list(-1L)),
        ParamDbl$new("lb", default = 0, lower = -Inf, upper = Inf),
        ParamDbl$new("ub", default = 1, lower = -Inf, upper = Inf)
        # FIXME: Possibly implement more aprams, currently not important
      ))
      ps$values = list(measure = NULL, algorithm = "NLOPT_LN_BOBYQA", xtol_rel = 10^-8, lb = 0, ub = 1)
      super$initialize(innum, id, weights = NULL, param_vals = param_vals, param_set = ps, packages = "nloptr")
    },
    train = function(inputs) {
      assert_list(inputs, "PredictionRegr")
      self$measure = self$param_set$values$measure
      if (is.null(self$measure)) self$measure = mlr_measures$get("regr.mse")
      assert_measure(self$measure)
      assert_true(self$measure$task_type == "regr")
      wts = private$optimize_objfun_nlopt(inputs)
      self$weights = wts
      self$state = list("weights" = wts)
      list(NULL)
    })
)

register_pipeop("nloptmodelavg", PipeOpNlOptModelAvg, list("N"))

#' @title PipeOpNlOptMajorityVote
#'
#' @format [R6Class] PipeOpNlOptMajorityVote
#'
#' @name mlr_pipeop_nloptmajorityvote
#' @format [`R6Class`] inheriting from [`PipeOpMajorityVote`].
#'
#' @description
#' Aggregates over different [`PredictionClassif`]s.
#' Either computes the mode, if `predict_type` is `"response"`,
#' or averages probabilities if `predict_type` is `"prob"`.
#' Weights for each learner are learned using (nloptr)[nloptr::nloptr].
#' For help with nloptr see [`nloptr::nloptr.print.options()`].
#' Returns a single [`PredictionClassif`].
#' As a default, optimizes [`MeasureClassifCE`] and only allows weights between 0 and 1.
#' Used for classification [`Prediction`]s.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpNlOptMajorityVote$new(3)
#' @export
PipeOpNlOptMajorityVote = R6Class("PipeOpNlOptMajorityVote",
  inherit = PipeOpMajorityVote,

  public = list(
    measure = NULL,

    initialize = function(innum, id = "nloptmajorityvote", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("measure", default = NULL),
        ParamFct$new("algorithm", default = "NLOPT_LN_COBYLA",
          levels = strsplit(nloptr::nloptr.get.default.options()[1, "possible_values"], ", ")[[1]]),
        ParamUty$new("eval_g_ineq", default = function(x) max(x) - 1),
        ParamDbl$new("xtol_rel", default = 10^-4, lower = 0, upper = Inf),
        ParamDbl$new("xtol_abs", default = 0, lower = 0, upper = Inf),
        ParamDbl$new("ftol_rel", default = 0, lower = 0, upper = Inf),
        ParamDbl$new("ftol_abs", default = 0, lower = 0, upper = Inf),
        ParamDbl$new("stopval", default = -Inf, lower = -Inf, upper = Inf),
        ParamInt$new("maxeval", default = 100, lower = 1L, upper = Inf),
        ParamInt$new("maxtime", default = -1L, lower = 0L, upper = Inf, special_vals = list(-1L)),
        ParamDbl$new("lb", default = 0, lower = -Inf, upper = Inf),
        ParamDbl$new("ub", default = 1, lower = -Inf, upper = Inf)
        # FIXME: Possibly implement more aprams, currently not important
      ))
      ps$values = list(measure = NULL, algorithm = "NLOPT_LN_BOBYQA", xtol_rel = 10^-8, lb = 0, ub = 1)
      super$initialize(innum, id, weights = NULL, param_vals = param_vals, param_set = ps, packages = "nloptr")
    },
    train = function(inputs) {
      assert_list(inputs, "PredictionClassif")
      self$measure = self$param_set$values$measure
      if (is.null(self$measure)) self$measure = mlr_measures$get("classif.ce")
      assert_measure(self$measure)
      assert_true(self$measure$task_type == "classif")
      wts = private$optimize_objfun_nlopt(inputs)
      self$weights = wts
      self$state = list("weights" = wts)
    })
)

register_pipeop("nloptmajorityvote", PipeOpNlOptMajorityVote, list("N"))
