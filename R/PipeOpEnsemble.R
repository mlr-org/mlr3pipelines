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
