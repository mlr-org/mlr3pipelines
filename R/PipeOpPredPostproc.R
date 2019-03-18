#' @title PipeOpPredPostproc
#'
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Base class for handling most "postprocessing" operations on predictions.
#' These are operations that have exactly one prediction object as input and one
#' prediction object as output.
#'
#' Users must implement `$train()` and `$predict()`, which have a [`Prediction`]
#' input and should return that `Prediction`. The `Prediction` should, if possible, be
#' manipulated in-place, and should not be cloned.
#'
#' @section Methods:
#' * `PipeOpPredPostproc$new(id, param_set = ParamSet$new())` \cr
#'   (`character(1)`, `ParamSet`, `logical(1)`) -> `self` \cr
#'   Constructor.
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpPredPostproc = R6Class("PipeOpPredPostproc",
  inherit = PipeOp,

  public = list(
    threshold = NULL,
    measure = NULL,
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(), packages = character(0)) {
      super$initialize(id, param_set = param_set, param_vals = param_vals, packages = packages,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction")
      )
    },
    train = function(input) {
      self$state = list()
      list(NULL)
    },
    predict = function(input) stop("abstract")
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpPredPostproc", PipeOpPredPostproc)


