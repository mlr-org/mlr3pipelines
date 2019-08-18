#' @title PipeOpRegrAvg
#'
#' @usage NULL
#' @name mlr_pipeops_regravg
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @description
#' Perform (weighted) prediction averaging from regression [`Prediction`][mlr3::Prediction]s by connecting
#' [`PipeOpRegrAvg`] to multiple [`PipeOpLearner`] outputs.
#'
#' The resulting `"response"` prediction is a weighted average of the incoming `"response"` predictions.
#' `"se"` prediction is currently not aggregated but discarded if present.
#'
#' Weights can be set as a parameter; if none are provided, defaults to
#' equal weights for each prediction.
#' Defaults to equal weights for each model.
#'
#' @section Construction:
#' ```
#' PipeOpRegrAvg$new(innum = 0, id = "regravg", param_vals = list())
#' ```
#' * `innum` :: `numeric(1)`\cr
#'   Determines the number of input channels.
#'   If `innum` is 0 (default), a vararg input channel is created that can take an arbitrary number of inputs.
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"regravg"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpEnsemble`]. Instead of a [`Prediction`][mlr3::Prediction], a [`PredictionRegr`][mlr3::PredictionRegr]
#' is used as input and output during prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOpEnsemble`].
#'
#' @section Internals:
#' Inherits from [`PipeOpEnsemble`] by implementing the `private$weighted_avg_predictions()` method.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @examples
#' # Simple Bagging
#' gr = greplicate(n = 5,
#'   mlr_pipeops$get("subsample") %>>%
#'   mlr_pipeops$get("learner", "classif.rpart")
#' ) %>>%
#'   mlr_pipeops$get("classifavg")
#'
#  mlr3::resample("iris", GraphLearner$new(gr), "cv")
#'
#' @family PipeOps
#' @include PipeOpEnsemble.R
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
