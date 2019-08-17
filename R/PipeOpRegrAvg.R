#' @title PipeOpRegrAvg
#'
#' @usage NULL
#' @name mlr_pipeops_regravg
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @description
#' Perform (weighted) prediction averaging from regression [`Prediction`][mlr3::prediction]s by connecting
#' [`PipeOpRegrAvg`] to multiple [`PipeOpLearner`] outputs.
#'
#' The resulting `"response"` prediction is a weighted average of the incoming `"response"` predictions.
#' `"se"` prediction is currently not aggregated but discarded if present.
#'
#' Averages its input (a `list` of `PredictionRegr`).
#' Only used for regression `Prediction`s.
#' Weights can be set by the user, if none are provided, defaults to
#' equal weights for each prediction.
#' Offers a `$weights` slot to set/get weights for each learner.
#' Returns a single `PredictionRegr`.
#' Defaults to equal weights for each incoming prediction.
#'
#' @family PipeOps
#' @include PipeOpEnsemble.R
#' @examples
#' op = PipeOpRegrAvg$new()
#' @export
PipeOpRegrAvg = R6Class("PipeOpRegrAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum = 0, id = "regravg", param_vals = list(), ...) {
      super$initialize(innum, id, param_vals = param_vals, prediction_type = "PredictionRegr", ...)
    }
  ),
  private = list(
    weighted_avg_predictions = function(inputs, weights, row_ids, truth) {
      response_matrix = simplify2array(map(inputs, "response"))
      response = c(response_matrix %*% weights)
      se = NULL

      PredictionRegr$new(row_ids = row_ids, truth = truth, response = response, se = se)
    }
  )
)

mlr_pipeops$add("regravg", PipeOpRegrAvg)
