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
    outnum = NULL,
    initialize = function(outnum, id = "scatter") {
      assert_count(outnum)
      super$initialize(id,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = rep_suffix("input", outnum), train = "*", predict = "*")
      )
      self$outnum = outnum
    },

    train = function(inputs) {
      self$state = list()
      map(seq_len(self$outnum), function(x) inputs[[1]]$clone())
    },

    predict = function(inputs) {
      map(seq_len(self$outnum), function(x) inputs[[1]]$clone())
    }
  )
)
