#' @title PipeOpEnsemble
#'
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Parent class for PipeOps that aggregate a list of predictions.
#' @section Methods:
#' * `PipeOpEnsemble$new(innum, id)` \cr
#'   (`numeric(1)`, `character(1)`) -> `self` \cr
#'   Constructor. `innum` determines the number of input channels.
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpEnsemble = R6Class("PipeOpEnsemble",
  inherit = PipeOp,

  public = list(
    weights = NULL,
    measure = NULL,
    initialize = function(innum, id, param_set = ParamSet$new(), param_vals = list(), packages = character(0)) {
      assert_integerish(innum, lower = 1)
      super$initialize(id, param_set = param_set, param_vals = param_vals, packages = packages,
        input = data.table(name = rep_suffix("input", innum), train = "NULL", predict = "PredictionData"),
        output = data.table(name = "output", train = "NULL", predict = "PredictionData")
      )
    },
    train = function(inputs) {
      self$state = list()
      list(NULL)
    },
    predict = function(inputs) stop("abstract")
  ),
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
)

#' @title PipeOpModelAvg
#'
#' @name mlr_pipeop_modelavg
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`].
#'
#' @description
#' Averages its input (a `list` of [`PredictionDataRegr`]).
#' Only used for regression `Prediction`s.
#' Weights can be set by the user, if none are provided, defaults to
#' equal weights `rep(1/innum, innum)` for each prediction.
#' Offers a `$weights` slot to set/get weights for each learner.
#' Returns a single [`PredictionDataRegr`].
#' Defaults to equal weights for each model.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpModelAvg$new(3)
#' @export
PipeOpModelAvg = R6Class("PipeOpModelAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum, weights = NULL, id = "modelavg", param_vals = list(),
      param_set = ParamSet$new(), packages = character(0)) {
      super$initialize(innum, id, param_vals = param_vals, param_set = param_set, packages = packages)
      if (is.null(weights)) weights = rep(1 / innum, innum)
      assert_numeric(weights, len = innum)
      self$weights = weights
    },

    predict = function(inputs) {
      assert_list(inputs, types = "PredictionDataRegr")
      list(private$weighted_avg_predictions(inputs, self$weights))
    }),

  private = list(
    weighted_avg_predictions = function(inputs, weights) {
      map(inputs, function(x) assert_true(identical(inputs[[1]]$row_ids, x$row_ids)))
      assert_numeric(weights, len = length(inputs))
      ret = set_class(list(row_ids = inputs[[1]]$row_ids,
        response = c(simplify2array(map(inputs, "response")) %*% (weights / sum(weights)))),
        c("PredictionDataRegr", "PredictionData"))
      if (all(map_lgl(inputs, function(x) "se" %in% names(x)))) {
        ret$se = c(sqrt(simplify2array(map(inputs, "se"))^2 %*% (weights / sum(weights))))
      }
      ret
    }
  )
)


## #' @title PipeOpNlOptModelAvg
## #'
## #' @format [R6Class] PipeOpNlOptModelAvg
## #'
## #' @name mlr_pipeop_nloptmajorityvote
## #' @format [`R6Class`] inheriting from [`PipeOpModelAvg`].
## #'
## #' @description
## #' Aggregates over different [`PredictionDataRegr`]s.
## #' Weights for each learner are learned using (nloptr)[nloptr::nloptr].
## #' For help with nloptr see [`nloptr::nloptr.print.options()`].
## #' Returns a single [`PredictionDataRegr`].
## #' By default, optimizes [`MeasureRegrMSE`] and only allows weights between 0 and 1.
## #' Used for regression [`Prediction`]s.
## #'
## #' @family PipeOps
## #' @examples
## #' op = PipeOpNlOptModelAvg$new(3)
## #' @export
## PipeOpNlOptModelAvg = R6Class("nloptmodelavg",
##   inherit = PipeOpModelAvg,

##   public = list(
##     measure = NULL,
##     initialize = function(innum, id = "nloptmodelavg", param_vals = list()) {
##       ps = ParamSet$new(params = list(
##         ParamUty$new("measure", default = NULL),
##         ParamFct$new("algorithm", default = "NLOPT_LN_COBYLA",
##           levels = strsplit(nloptr::nloptr.get.default.options()[1, "possible_values"], ", ")[[1]]),
##         ParamUty$new("eval_g_ineq", default = function(x) max(x) - 1),
##         ParamDbl$new("xtol_rel", default = 10^-4, lower = 0, upper = Inf),
##         ParamDbl$new("xtol_abs", default = 0, lower = 0, upper = Inf),
##         ParamDbl$new("ftol_rel", default = 0, lower = 0, upper = Inf),
##         ParamDbl$new("ftol_abs", default = 0, lower = 0, upper = Inf),
##         ParamDbl$new("stopval", default = -Inf, lower = -Inf, upper = Inf),
##         ParamInt$new("maxeval", default = 100, lower = 1L, upper = Inf),
##         ParamInt$new("maxtime", default = -1L, lower = 0L, upper = Inf, special_vals = list(-1L)),
##         ParamDbl$new("lb", default = 0, lower = -Inf, upper = Inf),
##         ParamDbl$new("ub", default = 1, lower = -Inf, upper = Inf)
##         # FIXME: Possibly implement more aprams, currently not important
##       ))
##       ps$values = list(measure = NULL, algorithm = "NLOPT_LN_BOBYQA", xtol_rel = 10^-8, lb = 0, ub = 1)
##       super$initialize(innum, id, weights = NULL, param_vals = param_vals, param_set = ps, packages = "nloptr")
##     },
##     train = function(inputs) {
##       assert_list(inputs, "PredictionDataRegr")
##       self$measure = self$param_set$values$measure
##       if (is.null(self$measure)) self$measure = mlr_measures$get("regr.mse")
##       assert_measure(self$measure)
##       assert_true(self$measure$task_type == "regr")
##       wts = private$optimize_objfun_nlopt(inputs)
##       self$weights = wts
##       self$state = list("weights" = wts)
##       list(NULL)
##     })
## )


#' @title PipeOpMajorityVote
#'
#' @format [R6Class] PipeOpMajorityVote
#'
#' @name mlr_pipeop_majorityvote
#' @format [`R6Class`] inheriting from [`PipeOpMajorityVote`].
#'
#' @description
#' Aggregates over different [`PredictionDataClassif`]s.
#' Either computes the mode, if `predict_type` is `"response"`,
#' or averages probabilities if `predict_type` is `"prob"`.
#' Returns a single [`PredictionDataClassif`].
#' Weights can be set by the user, if none are provided, defaults to
#' equal weights `rep(1/innum, innum)` for each prediction.
#' Used for classification `Prediction`s.
#' Defaults to equal weights for each model.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpMajorityVote$new(3)
#' @export
PipeOpMajorityVote = R6Class("PipeOpMajorityVote",
  inherit = PipeOpEnsemble,

  public = list(
    weights = NULL,
    initialize = function(innum, weights = NULL, id = "majorityvote", param_vals = list(),
      param_set = ParamSet$new(), packages = character(0)) {
      super$initialize(innum, id, param_vals = param_vals, param_set = param_set, packages = packages)
      if (is.null(weights)) weights = rep(1 / innum, innum)
      assert_numeric(weights, len = innum)
      self$weights = setNames(weights, self$input$name)
    },
    predict = function(inputs) {
      assert_list(inputs, "PredictionDataClassif")
      list(private$weighted_avg_predictions(inputs, self$weights))
    }),
  private = list(
    weighted_avg_predictions = function(inputs, wts) {
      assert_numeric(wts, len = length(inputs))
      assert_true(sum(wts) != 0)
      wts = wts / sum(wts)
      # Drop zero-weights for efficiency
      inputs = inputs[wts != 0]
      wts = wts[wts != 0]

      has_probs = all(map_lgl(inputs, function(x) {
        !is.null(x$prob)
      }))
      if (has_probs) {
        private$weighted_prob_avg(inputs, wts)
      } else {
        private$weighted_majority_vote(inputs, wts)
      }
    },
    weighted_majority_vote = function(inputs, wts) {
      map(inputs, function(x) assert_true(identical(inputs[[1]]$row_ids, x$row_ids)))
      map(inputs, function(x) assert_true(identical(levels(inputs[[1]]$response), levels(x$response))))

      wts = wts / sum(wts)

      alllevels = levels(inputs[[1]]$response)
      accmat = matrix(0, nrow = length(inputs[[1]]$response), ncol = length(alllevels))
      for (idx in seq_along(inputs)) {
        rdf = data.frame(x = inputs[[idx]]$response)
        curmat = model.matrix(~ 0 + x, rdf) * wts[idx]
        accmat = accmat + curmat
      }
      set_class(list(row_ids = inputs[[1]]$row_ids,
        response = factor(alllevels[max.col(accmat)], levels = alllevels)),
        c("PredictionDataClassif", "PredictionData"))
    },
    weighted_prob_avg = function(inputs, wts) {
      map(inputs, function(x) assert_true(identical(inputs[[1]]$row_ids, x$row_ids)))
      map(inputs, function(x) assert_true(identical(ncol(inputs[[1]]$prob), ncol(x$prob))))
      map(inputs, function(x) assert_true(identical(is.null(inputs[[1]]$response), is.null(x$response))))

      wts = wts / sum(wts)

      accmat = inputs[[1]]$prob * wts[1]
      for (idx in seq_along(inputs)[-1]) {
        accmat = accmat + inputs[[idx]]$prob * wts[idx]
      }

      response = NULL
      if (!is.null(inputs[[1]]$response)) {
        map(inputs, function(x) assert_true(identical(levels(inputs[[1]]$response), levels(x$response))))
        max.prob = max.col(accmat)
        alllevels = levels(inputs[[1]]$response)
        response = factor(alllevels[max.prob], levels = alllevels)
      }
      ret = set_class(list(row_ids = inputs[[1]]$row_ids,
        response = response,
        prob = accmat),
        c("PredictionDataClassif", "PredictionData"))
      ret
    }
  )
)


## #' @title PipeOpNlOptMajorityVote
## #'
## #' @format [R6Class] PipeOpNlOptMajorityVote
## #'
## #' @name mlr_pipeop_nloptmajorityvote
## #' @format [`R6Class`] inheriting from [`PipeOpMajorityVote`].
## #'
## #' @description
## #' Aggregates over different [`PredictionDataClassif`]s.
## #' Either computes the mode, if `predict_type` is `"response"`,
## #' or averages probabilities if `predict_type` is `"prob"`.
## #' Weights for each learner are learned using (nloptr)[nloptr::nloptr].
## #' For help with nloptr see [`nloptr::nloptr.print.options()`].
## #' Returns a single [`PredictionDataClassif`].
## #' As a default, optimizes [`MeasureClassifCE`] and only allows weights between 0 and 1.
## #' Used for classification [`Prediction`]s.
## #'
## #' @family PipeOps
## #' @examples
## #' op = PipeOpNlOptMajorityVote$new(3)
## #' @export
## PipeOpNlOptMajorityVote = R6Class("PipeOpNlOptMajorityVote",
##   inherit = PipeOpMajorityVote,

##   public = list(
##     measure = NULL,

##     initialize = function(innum, id = "nloptmajorityvote", param_vals = list()) {
##       ps = ParamSet$new(params = list(
##         ParamUty$new("measure", default = NULL),
##         ParamFct$new("algorithm", default = "NLOPT_LN_COBYLA",
##           levels = strsplit(nloptr::nloptr.get.default.options()[1, "possible_values"], ", ")[[1]]),
##         ParamUty$new("eval_g_ineq", default = function(x) max(x) - 1),
##         ParamDbl$new("xtol_rel", default = 10^-4, lower = 0, upper = Inf),
##         ParamDbl$new("xtol_abs", default = 0, lower = 0, upper = Inf),
##         ParamDbl$new("ftol_rel", default = 0, lower = 0, upper = Inf),
##         ParamDbl$new("ftol_abs", default = 0, lower = 0, upper = Inf),
##         ParamDbl$new("stopval", default = -Inf, lower = -Inf, upper = Inf),
##         ParamInt$new("maxeval", default = 100, lower = 1L, upper = Inf),
##         ParamInt$new("maxtime", default = -1L, lower = 0L, upper = Inf, special_vals = list(-1L)),
##         ParamDbl$new("lb", default = 0, lower = -Inf, upper = Inf),
##         ParamDbl$new("ub", default = 1, lower = -Inf, upper = Inf)
##         # FIXME: Possibly implement more aprams, currently not important
##       ))
##       ps$values = list(measure = NULL, algorithm = "NLOPT_LN_BOBYQA", xtol_rel = 10^-8, lb = 0, ub = 1)
##       super$initialize(innum, id, weights = NULL, param_vals = param_vals, param_set = ps, packages = "nloptr")
##     },
##     train = function(inputs) {
##       assert_list(inputs, "PredictionDataClassif")
##       self$measure = self$param_set$values$measure
##       if (is.null(self$measure)) self$measure = mlr_measures$get("classif.ce")
##       assert_measure(self$measure)
##       assert_true(self$measure$task_type == "classif")
##       wts = private$optimize_objfun_nlopt(inputs)
##       self$weights = wts
##       self$state = list("weights" = wts)
##     })
## )
