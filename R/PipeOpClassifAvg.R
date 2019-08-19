#' @title PipeOpClassifAvg
#'
#' @usage NULL
#' @name mlr_pipeops_classifavg
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @description
#' Perform (weighted) majority vote prediction from classification [`Prediction`][mlr3::Prediction]s by connecting
#' [`PipeOpClassifAvg`] to multiple [`PipeOpLearner`] outputs.
#'
#' If the incoming [`Learner`][mlr3::Learner]'s
#' `$predict_type` is set to `"response"`, the prediction obtained is also a `"response"` prediction
#' with each instance predicted to the prediction from incoming [`Learner`][mlr3::Learner]s with the
#' highest total weight. If the [`Learner`][mlr3::Learner]'s `$predict_type` is set to `"prob"`, the
#' prediction obtained is also a `"prob"` type prediction with the probability predicted to be a weighted
#' average of incoming predictions.
#'
#' All incoming [`Learner`][mlr3::Learner]'s `$predict_type` must agree.
#'
#' Weights can be set as a parameter; if none are provided, defaults to
#' equal weights for each prediction.
#' Defaults to equal weights for each model.
#'
#' @section Construction:
#' ```
#' PipeOpClassifAvg$new(innum = 0, id = "classifavg", param_vals = list())
#' ```
#' * `innum` :: `numeric(1)`\cr
#'   Determines the number of input channels.
#'   If `innum` is 0 (default), a vararg input channel is created that can take an arbitrary number of inputs.
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"classifavg"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpEnsemble`]. Instead of a [`Prediction`][mlr3::Prediction], a [`PredictionClassif`][mlr3::PredictionClassif]
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
PipeOpClassifAvg = R6Class("PipeOpClassifAvg",
  inherit = PipeOpEnsemble,
  public = list(
    initialize = function(innum = 0, id = "classifavg", param_vals = list()) {
      super$initialize(innum, id, param_vals = param_vals, prediction_type = "PredictionClassif", packages = "stats")
    }
  ),
  private = list(
    weighted_avg_predictions = function(inputs, weights, row_ids, truth) {
      has_probs = every(inputs, function(x) !is.null(x$prob))
      has_classif_response = every(inputs, function(x) !is.null(x$response))
      if (!(has_probs || has_classif_response)) {
        stop("PipeOpClassifAvg input predictions had missing 'prob' and missing 'response' values. At least one of them must be given for all predictions.")
      }
      prob = response = NULL
      if (has_probs) {
        alllevels = colnames(inputs[[1]]$prob)
        assert_character(alllevels, any.missing = FALSE, len = ncol(inputs[[1]]$prob))
        matrices = map(inputs, function(x) {
          mat = x$prob
          assert_set_equal(alllevels, colnames(mat))
          mat[, alllevels, drop = FALSE]
        })
        prob = weighted_matrix_sum(matrices, weights)
      }

      if (has_classif_response) {
        alllevels = levels(inputs[[1]]$response)
        map(inputs, function(x) assert_set_equal(alllevels, levels(x$response)))
        response = weighted_factor_mean(map(inputs, "response"), weights, alllevels)
      }

      PredictionClassif$new(row_ids = row_ids, truth = truth, response = response, prob = prob)
    }
  )
)

mlr_pipeops$add("classifavg", PipeOpClassifAvg)
