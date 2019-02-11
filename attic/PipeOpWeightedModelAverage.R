#' @title PipeOpWeightedModelAvg
#'
#' @name mlr_pipeop_weightedmodelavg
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`].
#'
#' @description
#' Averages its inputs (a `list` of ldarner [`Prediction`]).
#' Weights can be learned to optimize a specified measure.
#' This is internally done using (nloptr)[https://cran.r-project.org/web/packages/nloptr/index.html].
#' Returns a single [`Prediction`].
#'
#' @family PipeOps
#' @examples
#' op = PipeOpWeightedModelAvg$new(3)
#' @export
PipeOpWeightedModelAvg = R6Class("PipeOpWeightedModelAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum, id = "PipeOpWeightedModelAvg", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("measure", default = NULL),
        ParamFct$new("strategy", default = "1", levels = c("1"))
      ))
      ps$values = list(measure = NULL, strategy = "1")
      super$initialize(innum, id, param_set = ps, param_vals = param_vals)
    },
    train = function(inputs) {
      browser()
      meas = mlr_measures$get("regr.mse")
      weights = rep(1L, length(inputs))

      self$state = 0L
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
  ),

  private = list(
    merge_predictions = function() {}
  )
)

# See issue #117
# #' @include mlr_pipeops.R
# mlr_pipeops$add("modelavg", PipeOpModelAvg)


truth = rnorm(1000)
prd = data.table(
  truth + rnorm(1000, 0, .01),
  truth + rnorm(1000, 0, .1),
  truth + rnorm(1000, 1, 1)
)

objfun = function(wts) {
  predicted = apply(prd, 1, weighted.mean, wts)
  Metrics::mae(truth, predicted)
}

nloptr::nloptr(
  x0 = rep(1, 3),
  eval_f = objfun,
  opts = list("algorithm" = "NLOPT_LN_COBYLA")
)