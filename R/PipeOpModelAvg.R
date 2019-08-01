#' @title PipeOpModelAvg
#'
#' @name mlr_pipeop_modelavg
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`].
#'
#' @description
#' Averages its input (a `list` of `PredictionRegr`).
#' Only used for regression `Prediction`s.
#' Weights can be set by the user, if none are provided, defaults to
#' equal weights for each prediction.
#' Offers a `$weights` slot to set/get weights for each learner.
#' Returns a single `PredictionRegr`.
#' Defaults to equal weights for each model.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpModelAvg$new()
#' @export
PipeOpModelAvg = R6Class("PipeOpModelAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum = 0, id = "modelavg", param_vals = list(), ...) {
      super$initialize(innum, id, param_vals = param_vals, prediction_type = "PredictionRegr", ...)
    },

    predict = function(inputs) {
      list(private$weighted_avg_predictions(inputs, self$weights))
    }),

  private = list(
    weighted_avg_predictions = function(inputs, weights) {
      row_ids = inputs[[1]]$row_ids
      map(inputs, function(x) assert_true(identical(row_ids, x$row_ids)))
      truth = inputs[[1]]$truth
      if (length(weights) == 1) weights = rep(weights, length(inputs))
      weights = weights / sum(weights)
      responsematrix = simplify2array(map(inputs, "response"))
      response = c(responsematrix %*% weights)
      se = NULL
      if (all(map_lgl(inputs, function(x) "se" %in% names(x$data)))) {
        se = c(sqrt(
          (
            simplify2array(map(inputs, "se"))^2 +
            (responsematrix - response)^2
          ) %*% weights
        ))
      }
      PredictionRegr$new(row_ids = row_ids, truth = truth, response = response, se = se)
    }
  )
)

mlr_pipeops$add("modelavg", PipeOpModelAvg)
