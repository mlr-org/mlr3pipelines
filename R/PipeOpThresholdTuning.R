#' @title PipeOpThresholdTuning
#'
#' @format [R6Class] PipeOpThresholdTuning
#'
#' @name mlr_pipeop_thresholdtuning
#' @format [`R6Class`] inheriting from [`PipeOpThresholdTuning`].
#'
#' @description
#' Tunes optimal probability thresholds over different [`PredictionClassif`]s.
#' `predict_type` `"prob"` is required.
#' Returns a single [`PredictionClassif`].
#' Used for classification `Prediction`s.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpThresholdTuning$new(3)
#' @export
PipeOpThresholdTuning = R6Class("PipeOpThresholdTuning",
  inherit = PipeOpEnsemble,

  public = list(
    weights = NULL,
    initialize = function(innum, id = "thresholdtuning", param_vals = list(),
      param_set = ParamSet$new(), packages = character(0)) {
      super$initialize(innum, id, param_vals = param_vals, param_set = param_set, packages = packages)
    },
    predict = function(input) {
      assert_list(input, "PredictionClassif")
      prds = private$tune_treshold(input, )
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
        data.table("row_id" = x$row_ids, "response" = x$response, "weight" = wts[i])
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


#' @title PipeOpTuneThreshold
#'
#' @format [R6Class] PipeOpTuneThreshold
#'
#' @name mlr_pipeop_tunethreshold
#' @format [`R6Class`] inheriting from [`PipeOpPredPostproc`].
#'
#' @description
#' Aggregates over different [`PredictionClassif`]s.
#' Either computes the mode, if `predict_type` is `"response"`,
#' or averages probabilities if `predict_type` is `"prob"`.
#' Weights for each learner are learned using (nloptr)[nloptr::nloptr].
#' For help with nloptr see [`nloptr::nloptr.print.options()`].
#' Returns a single [`PredictionClassif`].
#' As a default, optimizes [`MeasureClassifMMCE`] and only allows weights between 0 and 1.
#' Used for classification [`Prediction`]s.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpTuneThreshold$new(3)
#' @export
PipeOpTuneThreshold = R6Class("PipeOpTuneThreshold",
  inherit = PipeOpPredPostproc,

  public = list(
    measure = NULL,

    initialize = function(id = "tunethreshold", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("measure", default = NULL),
        ParamFct$new("algorithm", default = "GenSA",
          levels = strsplit(GenSA:: nloptr.get.default.options()[1, "possible_values"], ", ")[[1]]),
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
mlr_pipeops$add("PipeOpTuneThreshold", PipeOpNlOptMajorityVote)
