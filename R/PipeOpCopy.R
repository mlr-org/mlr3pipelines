#' @title PipeOpCopy
#'
#' @name mlr_pipeop_copy
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#'   Copies its input `outnum` times.
#' @section Usage:
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
    initialize = function(outnum, id = "copy") {
      assert_int(outnum, lower = 1)
      super$initialize(id,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = rep_suffix("output", outnum), train = "*", predict = "*")
      )
    },

    train = function(inputs) {
      self$state = list()
      rep(inputs, self$outnum)
    },

    predict = function(inputs) {
      rep(inputs, self$outnum)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpCopy", PipeOpCopy)
