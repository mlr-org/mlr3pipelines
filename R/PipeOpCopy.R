#' @title PipeOpCopy
#'
#' @name PipeOpCopy
#' @format [R6Class] PipeOpCopy
#'
#' @description
#'   Copies its input `outnum` times.
#' @section Usage:
#' Inherits from [PipeOp]
#' * `f = PipeOpCopy$new(outnum, id)` \cr
#'     `integer(1)`, `character(1)` -> [PipeOpCopy]
#' @section Details:
#' * `outnum`: `integer(1)` Number of times the input is copied.
#' @family PipeOp
#' @family PipeOpBroadcast
NULL

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
