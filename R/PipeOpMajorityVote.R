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
#' @examples
#' op = PipeOpMajorityVote$new(3)
#' @export
PipeOpMajorityVote = R6Class("PipeOpMajorityVote",
  inherit = PipeOpEnsemble,
  public = list(
    initialize = function(innum = 0, id = "majorityvote", param_vals = list(), ...) {
      super$initialize(innum, id, param_vals = param_vals, prediction_type = "PredictionClassif", ...)
    },

    predict = function(inputs) {
      list(weighted_avg_predictions(inputs, self$weights))
    }
  )
)

mlr_pipeops$add("majorityvote", PipeOpMajorityVote)


weighted_avg_predictions = function(inputs, weights) {
  row_ids = inputs[[1]]$row_ids
  map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))
  truth = inputs[[1]]$truth
  if (length(weights) == 1) weights = rep(weights, length(inputs))
  weights = weights / sum(weights)

  # Drop zero-weights for efficiency
  # FIXME: this is not numerically stable
  inputs = inputs[weights != 0]
  weights = weights[weights != 0]

  has_probs = every(inputs, function(x) { !is.null(x$prob) })
  has_response = every(inputs, function(x) { !is.null(x$response) })
  if (!(has_probs || has_response)) {
    stop("PipeOpMajorityVote input predictions had missing 'prob' and missing 'response' values. At least one of them must be given for all predictions.")
  }

  prob = response = NULL
  if (has_probs) {
    alllevels = colnames(inputs[[1]]$prob)
    assert_character(alllevels, any.missing = FALSE)
    map(inputs, function(x) assert_true(identical(sort(alllevels), sort(colnames(x$prob)))))

    accmat = inputs[[1]]$prob * weights[1]
    for (idx in seq_along(inputs)[-1]) {
      accmat = accmat + inputs[[idx]]$prob[, alllevels, drop = FALSE] * weights[idx]
    }
    prob = accmat
  }

  if (has_response) {
    alllevels = levels(inputs[[1]]$response)
    map(inputs, function(x) assert_true(identical(sort(alllevels), sort(levels(x$response)))))

    accmat = matrix(0, nrow = length(inputs[[1]]$response), ncol = length(alllevels))
    for (idx in seq_along(inputs)) {
      rdf = data.frame(x = factor(inputs[[idx]]$response, levels = alllevels))
      curmat = model.matrix(~ 0 + x, rdf) * weights[idx]
      accmat = accmat + curmat
    }
    response = factor(alllevels[max.col(accmat)], levels = alllevels)
  }

  PredictionClassif$new(row_ids = row_ids, truth = truth, response = response, prob = prob)
}
