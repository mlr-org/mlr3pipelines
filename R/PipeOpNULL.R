#' @title PipeOpNOP
#'
#' @name mlr_pipeop_NULL
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Simply pushes the input forward.
#' Can be useful during graph construction to keep the original task in conjunction with `gunion()`.
#'
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpNOP = R6Class("PipeOpNOP",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "nop", param_vals = list()) {
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = "output", train = "*", predict = "*")
      )
    },

    train_internal = function(inputs) {
      self$state = list()
      inputs
    },

    predict_internal = function(inputs) {
      inputs
    }
  )
)

mlr_pipeops$add("nop", PipeOpNOP)
