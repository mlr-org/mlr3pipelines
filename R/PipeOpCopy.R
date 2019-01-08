#' @title PipeOpCopy
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
#' @name PipeOpCopy
#' @family PipeOp, PipeOpBroadcast, PipeOpCopy
#' @export
PipeOpCopy = R6::R6Class("PipeOpCopy",
  inherit = PipeOp,
  public = list(
    initialize = function(outnum, id = "scatter") {
      assert_integerish(outnum)
      super$initialize(id)
      self$train_intypes = "any"
      self$train_outtypes = rep("any", outnum)
      self$predict_intypes = "any"
      self$predict_outtypes = rep("any", outnum)
      private$.outnum = outnum
    },
    train = function(input) {
      self$state = list()
      rep(input, self$outnum)
    },
    predict = function(input) {
      rep(input, self$outnum)
    }
  ),
  private = list(
    .outnum = NULL
  ),
  active = list(
    outnum = function() private$.outnum
  )
)
