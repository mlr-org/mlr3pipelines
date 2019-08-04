#' @title PipeOpCopy
#'
#' @name mlr_pipeop_copy
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#'   Copies its input `outnum` times. This should usually not be needed,
#'   because copying happens automatically.
#' @section Methods:
#' * `PipeOpEnsemble$new(outnum, id)` \cr
#'   (`numeric(1)`, `character(1)`) -> `self` \cr
#'   Constructor. `outnum` determines the number of output channels and
#'   copies that will be made.
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpCopy = R6Class("PipeOpCopy",
  inherit = PipeOp,
  public = list(
    initialize = function(outnum, id = "copy", param_vals = list()) {
      assert_int(outnum, lower = 1)
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = rep_suffix("output", outnum), train = "*", predict = "*")
      )
    },

    train_internal = function(inputs) {
      self$state = list()
      rep_len(inputs, self$outnum)
    },

    predict_internal = function(inputs) {
      rep_len(inputs, self$outnum)
    }
  )
)

mlr_pipeops$add("copy", PipeOpCopy, list("N"))
