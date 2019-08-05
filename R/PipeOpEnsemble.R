#' @title PipeOpEnsemble
#'
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Parent class for PipeOps that aggregate a list of predictions.
#'
#' @section Methods:
#' * `PipeOpEnsemble$new(innum = 0, id)` \cr
#'   (`numeric(1)`, `character(1)`) -> `self` \cr
#'   Constructor. `innum` determines the number of input channels.
#'   If `innum` is 0 (default), a vararg input
#'   channel is created that can take an arbitrary number of inputs.
#'
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpEnsemble = R6Class("PipeOpEnsemble",
  inherit = PipeOp,
  public = list(
    initialize = function(innum = 0, id, param_set = ParamSet$new(), param_vals = list(), packages = character(0), prediction_type = "Prediction") {
      assert_integerish(innum, lower = 0)
      param_set$add(ParamUty$new("weights", custom_check = check_weights(innum)))
      param_set$values$weights = 1
      inname = if (innum) rep_suffix("input", innum) else "..."
      super$initialize(id, param_set = param_set, param_vals = param_vals, packages = packages,
        input = data.table(name = inname, train = "NULL", predict = prediction_type),
        output = data.table(name = "output", train = "NULL", predict = prediction_type))
    },
    train_internal = function(inputs) {
      self$state = list()
      list(NULL)
    },
    predict_internal = function(inputs) stop("abstract")
  ),
  active = list(
    weights = function(val) {
      if (!missing(val)) {
        self$param_set$values$weights = val
      }
      self$param_set$values$weights
    }
  )
)

#' @title PipeOpModelAvg
#'
#' @name mlr_pipeop_modelavg
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`].
#'
#' @description
#' Averages its input (a `list` of `PredictionRegr`).
#' Only used for regression `Prediction`s.
#' Weights can be set by the user, if none are provided, defaults to
#' equal weights `rep(1/innum, innum)` for each prediction.
#' Offers a `$weights` slot to set/get weights for each learner.
#' Returns a single `PredictionRegr`.
#' Defaults to equal weights for each model.
#'
#' @family PipeOps
#' @examples
#' op = PipeOpModelAvg$new(3)
#' @export
PipeOpModelAvg = R6Class("PipeOpModelAvg",
  inherit = PipeOpEnsemble,

  public = list(
    initialize = function(innum, id = "modelavg", param_vals = list(), ...) {
      super$initialize(innum, id, param_vals = param_vals, prediction_type = "PredictionRegr", ...)
    },

    predict = function(inputs) {
      list(private$weighted_avg_predictions(inputs, self$weights))
    }),

  private = list(
  )
)

register_pipeop("modelavg", PipeOpModelAvg, list("N"))


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
#' equal weights `rep(1/innum, innum)` for each prediction.
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
    initialize = function(innum, id = "majorityvote", param_vals = list(), ...) {
      super$initialize(innum, id, param_vals = param_vals, prediction_type = "PredictionClassif", ...)
    },

    predict = function(inputs) {
      list(private$weighted_avg_predictions(inputs, self$weights))
    }
  )
)

check_weights = function(innum) {
  if (innum > 0) {
    function(x) assert(
      check_numeric(x, len = innum, any.missing = FALSE),
      check_numeric(x, len = 1, any.missing = FALSE)
    )
  } else {
    function(x) assert_numeric(x, min.len = 1, any.missing = FALSE)
  }
}

