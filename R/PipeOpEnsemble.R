#' @title PipeOpEnsemble
#'
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Parent class for PipeOps that aggregate a list of predictions.
#' Offers the private method `$merge_predictions()` which does exactly that.
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
        input = data.table(name = rep_suffix("input", innum), train = "NULL", predict = "Prediction"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction")
      )
    },
    train = function(inputs) {
      self$state = list()
      list(NULL)
    },
    predict = function(inputs) stop("abstract")
  ),
  private = list(
    merge_predictions = function(inputs) {
      do.call("rbind", map(inputs, function(x) as.data.table(x)))
    },
    objfun = function(wts, inputs) {
      prd = private$weighted_avg_predictions(inputs, wts)
      e = list("prediction" = prd)
      res = self$measure$calculate(e)
      if (!self$measure$minimize) res = -res
      res
    },
    optimize_objfun_nlopt = function(inputs, algorithm = "NLOPT_LN_COBYLA", xtol_rel = 10^-8) {
      requireNamespace("nloptr")
      init_weights = rep(1, length(inputs))
      opt = nloptr::nloptr(
        x0 = init_weights,
        eval_f = private$objfun,
        opts = list("algorithm" = "NLOPT_LN_COBYLA", xtol_rel = 10^-8),
        inputs = inputs
      )
      return(opt$solution)
    }
  )
)

#' @title PipeOpWtModelAvg
#'
#' @name mlr_pipeop_weightedmodelavg
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`].
#'
#' @description
#' Averages its input (a `list` of [`PredictionRegr`]).
#' Only used for regression `Prediction`s.
#' Weights can be set by the user, if none are provided, defaults to
#' equal weights `rep(1/innum, innum)` for each prediction.
#' Offers a `$weights` slot to set/get weights for each learner.
#' Returns a single [`PredictionRegr`].
#' Defaults to equal weights for each model.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpWtModelAvg$new(3)
#' @export
PipeOpWtModelAvg = R6Class("PipeOpWtModelAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum, weights = NULL, id = "modelavg", param_vals = list(),
      param_set = ParamSet$new(), packages = character(0)) {
      super$initialize(innum, id, param_vals = param_vals, param_set = param_set, packages = packages)
      if (is.null(weights)) weights = rep(1/innum, innum)
      assert_numeric(weights, len = innum)
      self$weights = weights
    },
    predict = function(inputs) {
      assert_true(unique(map_chr(inputs, "task_type")) == "regr")
      prds = private$weighted_avg_predictions(inputs, self$weights)
      list(private$make_prediction_regr(prds))
    }
  ),
  private = list(
    weighted_avg_predictions = function(inputs, weights) {
      assert_numeric(weights, len = length(inputs))
      df = map_dtr(inputs, function(x) {data.frame("row_id" = x$row_ids, "response" = x$response)})
      df = unique(df[, lapply(.SD, weighted.mean, w = weights), by = "row_id"])
      merge(df, as.data.table(inputs[[1]])[, c("row_id", "truth")], by = "row_id")
    },
    make_prediction_regr = function(prds) {
      p = PredictionRegr$new()
      p$row_ids = prds$row_id
      p$response = prds$response
      p$truth = prds$truth
      p$predict_types = "response"
      return(p)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("wtmodelavg", PipeOpWtModelAvg)


#' @title PipeOpNlOptModelAvg
#'
#' @format [R6Class] PipeOpNlOptModelAvg
#'
#' @name mlr_pipeop_nloptmajorityvote
#' @format [`R6Class`] inheriting from [`PipeOpWtModelAvg`].
#'
#' @description
#' Aggregates over different [`PredictionRegr`]s.
#' Weights for each learner are learned using (nloptr)[nloptr::nloptr].
#' For help with nloptr see [`nloptr::nloptr.print.options()`].
#' Returns a single [`PredictionRegr`].
#' Uses [`MeasureRegrMSE`] as a default.
#' Used for regression [`Prediction`]s.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpNlOptModelAvg$new(3)
#' @export
PipeOpNlOptModelAvg = R6Class("PipeOpNlOptModelAvg",
  inherit = PipeOpWtModelAvg,

  public = list(
    measure = NULL,
    initialize = function(innum, id = "majorityvote", param_vals = list()) {
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
        ParamInt$new("maxtime", default = -1L, lower = 0L, upper = Inf, special_vals = list(-1L))
        # FIXME: Possibly implement more, currently not important
      ))
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
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("nloptmodelavg", PipeOpNlOptModelAvg)



#' @title PipeOpWtMajorityVote
#'
#' @format [R6Class] PipeOpWtMajorityVote
#'
#' @name mlr_pipeop_weightedmajorityvote
#' @format [`R6Class`] inheriting from [`PipeOpMajorityVote`].
#'
#' @description
#' Aggregates over different [`PredictionClassif`]s.
#' Either computes the mode, if `predict_type` is `"response"`,
#' or averages probabilities if `predict_type` is `"prob"`.
#' Returns a single [`PredictionClassif`].
#' Weights can be set by the user, if none are provided, defaults to
#' equal weights `rep(1/innum, innum)` for each prediction.
#' Used for classification `Prediction`s.
#' Defaults to equal weights for each model.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpWtMajorityVote$new(3)
#' @export
PipeOpWtMajorityVote = R6Class("PipeOpWtMajorityVote",
  inherit = PipeOpEnsemble,

  public = list(
    weights = NULL,
    initialize = function(innum, weights = NULL, id = "majorityvote", param_vals = list(),
      param_set = ParamSet$new(), packages = character(0)) {
      super$initialize(innum, id, param_vals = param_vals, param_set = param_set, packages = packages)
      if(is.null(weights)) weights = rep(1/innum, innum)
      assert_numeric(weights, len = innum)
      self$weights = weights
    },
    predict = function(inputs) {
      assert_list(inputs, "PredictionClassif")
      prds = private$weighted_avg_predictions(inputs, self$weights)
      p = private$make_prediction_classif(prds, inputs[[1]]$predict_types)
      list(p)
    }
  ),
  private = list(
    weighted_avg_predictions = function(inputs, wts) {
      assert_numeric(wts, len = length(inputs))
      assert_true(sum(wts) != 0)
      # Drop zero-weights for efficiency
      inputs = inputs[!(wts == 0)]
      wts = wts[!(wts == 0)]

      has_probs = all(map_lgl(inputs, function(x) {"prob" %in% x$predict_types}))
        if (has_probs) {
          private$weighted_prob_avg(inputs, wts)
        } else {
          private$weighted_majority_vote(inputs, wts)
        }
    },
    weighted_majority_vote = function(inputs, wts) {
      # Unpack predictions, add weights
      df = imap_dtr(inputs, function(x, i) {
        data.frame("row_id" = x$row_ids, "response" = x$response, "weight" = wts[i])
      })
      # Sum weights over response, keep max row.
      df[, weight := sum(weight), by = list(response, row_id)]
      df = unique(df[, maxwt := max(weight), by = "row_id"])[weight == maxwt]
      merge(df[, c("row_id", "response")], as.data.table(inputs[[1]])[, c("row_id", "truth")],
        by = "row_id")
    },
    weighted_prob_avg = function(inputs, wts) {
      df = map_dtr(inputs, function(x) {data.frame("row_id" = x$row_ids, x$prob)})
      df = unique(df[, lapply(.SD, weighted.mean, w = wts), by = row_id])
      max.prob = max.col(df[, -"row_id"], ties.method='first')
      df$response = factor(max.prob, labels = colnames(df[, -"row_id"])[unique(max.prob)])
      levels(df$response) = colnames(df[, -c("row_id", "response")])
      merge(df, as.data.table(inputs[[1]])[, c("row_id", "truth")], by = "row_id")
    },
    # FIXME This is ugly, but currently the best way
    make_prediction_classif = function(prds, type) {
      p = PredictionClassif$new()
      p$row_ids = prds$row_id
      p$truth = prds$truth
      p$predict_types = type
      if ("prob" %in% type) {
        p$prob = as.matrix(prds[, -c("row_id", "response", "truth")])
      }
      if ("response" %in% type) p$response = prds$response
      return(p)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("wtmajorityvote", PipeOpWtMajorityVote)


#' @title PipeOpNlOptMajorityVote
#'
#' @format [R6Class] PipeOpNlOptMajorityVote
#'
#' @name mlr_pipeop_nloptmajorityvote
#' @format [`R6Class`] inheriting from [`PipeOpWtMajorityVote`].
#'
#' @description
#' Aggregates over different [`PredictionClassif`]s.
#' Either computes the mode, if `predict_type` is `"response"`,
#' or averages probabilities if `predict_type` is `"prob"`.
#' Weights for each learner are learned using (nloptr)[nloptr::nloptr].
#' For help with nloptr see [`nloptr::nloptr.print.options()`].
#' Returns a single [`PredictionClassif`].
#' As a default, optimizes [`MeasureClassifMMCE`].
#' Used for classification [`Prediction`]s.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpNlOptMajorityVote$new(3)
#' @export
PipeOpNlOptMajorityVote = R6Class("PipeOpNlOptMajorityVote",
  inherit = PipeOpWtMajorityVote,

  public = list(
    measure = NULL,

    initialize = function(innum, id = "majorityvote", param_vals = list()) {
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
        ParamInt$new("maxtime", default = -1L, lower = 0L, upper = Inf, special_vals = list(-1L))
        # FIXME: Possibly implement more, currently not important
      ))
      super$initialize(innum, id, weights = NULL, param_vals = param_vals, param_set = ps, packages = "nloptr")
    },
    train = function(inputs) {
      assert_list(inputs, "PredictionClassif")
      self$measure = self$param_set$values$measure
      if (is.null(self$measure)) self$measure = mlr_measures$get("classif.mmce")
      assert_measure(self$measure)
      assert_true(self$measure$task_type == "classif")
      wts = private$optimize_objfun_nlopt(inputs)
      self$weights = wts
      self$state = list("weights" = wts)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("nloptmajorityvote", PipeOpNlOptMajorityVote)
