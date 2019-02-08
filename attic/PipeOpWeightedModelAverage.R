#' @title PipeOpWeightedModelAvg
#'
#' @name mlr_pipeop_weightedmodelavg
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`].
#'
#' @description
#' Averages its input (a `list` of [`Prediction`]).
#' Weights can be learned to optimize a specified
#' measure.
#' Returns a single [`Prediction`].
#' Used for regression `Prediction`s.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpWeightedModelAvg$new(3)
#' @export
PipeOpWeightedModelAvg = R6Class("PipeOpWeightedModelAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum, id = "PipeOpWeightedModelAvg", param_vals = list()) {
      super$initialize(innum, id, param_vals = param_vals)
    },
    train = function(inputs) {

    },
    predict = function(inputs) {
      prds = private$merge_predictions(inputs)
      prds = prds[, list(response = mean(response, na.rm = TRUE), truth = truth[1]), by = "row_id"]
      # FIXME This is ugly, but currently the best way
      p = PredictionRegr$new()
      p$row_ids = prds$row_id
      p$response = prds$response
      p$truth = prds$truth
      list(p)
    }
  )
)

# See issue #117
# #' @include mlr_pipeops.R
# mlr_pipeops$add("modelavg", PipeOpModelAvg)
