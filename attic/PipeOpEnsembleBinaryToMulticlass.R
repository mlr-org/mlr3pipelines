#' @title PipeOpEnsembleBinaryToMultiClass
#'
#' @usage NULL
#' @name mlr_pipeops_ensemble_bin2multiclass
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @description
#' Perform a multiclass prediction from binary classification [`Prediction`][mlr3::Prediction]s by connecting
#' [`PipeOpEnsembleBinaryToMultiClass`] to multiple [`PipeOpLearner`] outputs.
#'
#' If the incoming [`Learner`][mlr3::Learner]'s
#' `$predict_type` is set to `"response"`, the prediction obtained is also a `"response"` prediction
#' with each instance predicted to the prediction from incoming [`Learner`][mlr3::Learner]s with the
#' highest total weight.
#' If the [`Learner`][mlr3::Learner]'s `$predict_type` is set to `"prob"`, the
#' prediction obtained is also a `"prob"` type prediction with the probability predicted to be a weighted
#' average of incoming predictions.
#'
#' All incoming [`Learner`][mlr3::Learner]'s `$predict_type` must agree.
#'
#' @section Construction:
#' ```
#' PipeOpEnsembleBinaryToMultiClass$new(innum = 0, id = "classifavg", param_vals = list())
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
#' @family PipeOps
#' @family Ensembles
#' @include PipeOpEnsemble.R
#' @export
#'
#' @examples
#' library("mlr3")
#'
#' # Simple Bagging
#' gr = greplicate(n = 5,
#'   po("subsample") %>>%
#'   po("learner", lrn("classif.rpart"))
#' ) %>>%
#'   po("classifavg")
#'
#'  mlr3::resample(tsk("iris"), GraphLearner$new(gr), rsmp("holdout"))
PipeOpEnsembleBinaryToMultiClass = R6Class("PipeOpEnsembleBinaryToMultiClass",
  inherit = PipeOpEnsemble,
  public = list(
    initialize = function(innum = 0, id = "ensemble_bin2multiclass", param_vals = list()) {
      super$initialize(innum, id, param_vals = param_vals, prediction_type = "PredictionClassif", packages = "stats")
    },
    predict_internal = function(inputs) {
      weights = self$param_set$values$weights
      assert_true("multiclass" %in% inputs[[1]]$properties)
      row_ids = inputs[[1]]$row_ids
      map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))

      truth = inputs[[1]]$truth
      if (length(weights) == 1) weights = rep(1, length(inputs))
      weights = weights / sum(weights)
      list(private$weighted_avg_predictions(inputs, weights, row_ids, truth))
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

mlr_pipeops$add("ensemble_bin2multiclass", PipeOpEnsembleBinaryToMultiClass)
