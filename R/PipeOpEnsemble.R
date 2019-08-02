#' @title PipeOpEnsemble
#'
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Parent class for PipeOps that aggregate a list of predictions.
#' @section Methods:
#' * `PipeOpEnsemble$new(innum, id)` \cr
#'   (`numeric(1)`, `character(1)`) -> `self` \cr
#'   Constructor. `innum` determines the number of input channels.
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpEnsemble = R6Class("PipeOpEnsemble",
  inherit = PipeOp,
  public = list(
    initialize = function(innum, id, param_set = ParamSet$new(), param_vals = list(), packages = character(0), prediction_type = "Prediction") {
      assert_integerish(innum, lower = 1)
      param_set$add(ParamDbl$new("weight")$rep(innum))
      weightids = param_set$ids(tags = "weight_rep")
      param_set$values[weightids] = 1 / innum
      super$initialize(id, param_set = param_set, param_vals = param_vals, packages = packages,
        input = data.table(name = rep_suffix("input", innum), train = "NULL", predict = prediction_type),
        output = data.table(name = "output", train = "NULL", predict = prediction_type))
    },
    train = function(inputs) {
      self$state = list()
      list(NULL)
    },
    predict = function(inputs) stop("abstract")
  ),
  active = list(
    weights = function(val) {
      weightids = self$param_set$ids(tags = "weight_rep")
      if (!missing(val)) {
        assert_numeric(val, len = nrow(self$input), any.missing = FALSE)
        self$param_set$values[weightids] = val
      }
      as.numeric(self$param_set$values[weightids])
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

register_pipeop("majorityvote", PipeOpMajorityVote, list("N"))

