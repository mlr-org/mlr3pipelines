#' @title PipeOpMajorityVote
#'
#' @format [R6Class] PipeOpMajorityVote
#'
#' @name mlr_pipeop_majorityvote
#' @format [`R6Class`] inheriting from [`PipeOpMajorityVote`].
#'
#' @description
#' Aggregates over different `PredictionClassif`s.
#' Either computes the mode, if `predict_type` is `"response"`,
#' or averages probabilities if `predict_type` is `"prob"`.
#' Returns a single `PredictionClassif`.
#' Weights can be set by the user, if none are provided, defaults to
#' equal weights for each prediction.
#' Used for classification `Prediction`s.
#' Defaults to equal weights for each model.
#'
#' @family PipeOps
#' @include PipeOpEnsemble.R
#' @examples
#' op = PipeOpMajorityVote$new(3)
#' @export
PipeOpMajorityVote = R6Class("PipeOpMajorityVote",
  inherit = PipeOpEnsemble,
  public = list(
    initialize = function(innum = 0, id = "majorityvote", param_vals = list()) {
      super$initialize(innum, id, param_vals = param_vals, prediction_type = "PredictionClassif", packages = "stats")
    },

    predict_internal = function(inputs) {
      list(weighted_avg_predictions(inputs, self$param_set$values$weights))
    }
  )
)

mlr_pipeops$add("majorityvote", PipeOpMajorityVote)
