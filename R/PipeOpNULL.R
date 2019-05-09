#' @title PipeOpNULL
#'
#' @name mlr_pipeop_NULL
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Simply pushes the input forward unchanged.
#' Can be usefull to keep the original task in conjunction with
#' `gunion()`.
#'
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpNULL = R6Class("PipeOpNULL",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "null", param_vals = list()) {
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = "output", train = "*", predict = "*")
      )
    },

    train = function(inputs) {
      self$state = list()
      inputs
    },

    predict = function(inputs) {
      inputs
    })
)
