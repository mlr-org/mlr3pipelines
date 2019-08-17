#' @title PipeOpModelAvg
#'
#' @usage NULL
#' @name mlr_pipeops_modelavg
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @description
#' Perform (weighted) prediction averaging from regression [`Prediction`][mlr3::prediction]s by connecting
#' [`PipeOpModelAvg`] to multiple [`PipeOpLearner`] outputs.
#'
#' The `"se"` prediction is calculated from the (weighted) standard error of the `"response"` predictions and
#' (if the incoming [`Learner`][mlr3::Learner]'s `$predict_type`s are all `"se"`) the `"se"` predictions of all
#' incoming [`Prediction`][mlr3::Prediction]`s according to the following pseudocode:
#'
#' ```
#' # response: vector of N "response" values predicted by different Learners for a
#' #   prediction sample
#' # se: vector of N "se" values predicted by different Learners for a prediction
#' #   sample
#' # weights: N weights set by $param_set$values$weights
#' norm_weights = weights / sum(weights)
#'
#' var_prediction = sum( (response - sum(response * norm_weights))^2 * norm_weights^2 ) /
#'   (1 - sum(norm_weights^2))
#'
#' if (previous learners have $predict_type "se") {
#'   var_prediction = var_prediction + sum( se^2 * norm_weights^2 )
#' }
#' se_prediction = sqrt(var_prediction)
#' ```
#'
#' Averages its input (a `list` of `PredictionRegr`).
#' Only used for regression `Prediction`s.
#' Weights can be set by the user, if none are provided, defaults to
#' equal weights for each prediction.
#' Offers a `$weights` slot to set/get weights for each learner.
#' Returns a single `PredictionRegr`.
#' Defaults to equal weights for each model.
#'
#' @family PipeOps
#' @include PipeOpEnsemble.R
#' @examples
#' op = PipeOpModelAvg$new()
#' @export
PipeOpModelAvg = R6Class("PipeOpModelAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum = 0, id = "modelavg", param_vals = list(), ...) {
      super$initialize(innum, id, param_vals = param_vals, prediction_type = "PredictionRegr", ...)
    }
  ),
  private = list(
    weighted_avg_predictions = function(inputs, weights, row_ids, truth) {
      has_se = every(inputs, function(x) "se" %in% names(x$data))
      est_se = if (has_se) "both" else "between"

      response_matrix = simplify2array(map(inputs, "response"))
      response = c(response_matrix %*% weights)
      if (has_se || length(inputs) > 1) {
        if (length(inputs) == 1) {
          est_se = "within"
        }
        se = weighted_se(response_matrix, simplify2array(map(inputs, "se")), response, weights, est_se)
      } else {
        se = NULL
      }

      PredictionRegr$new(row_ids = row_ids, truth = truth, response = response, se = se)
    }
  )
)

mlr_pipeops$add("modelavg", PipeOpModelAvg)
