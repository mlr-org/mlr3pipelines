#' @title PipeOpNULL
#'
#' @name PipeOpNULL
#' @format [R6Class] PipeOpNULL
#'
#' @description
#'   Simply pushes the input forward unchanged.
#'   Can be usefull for example to keep the original task in conjunction with
#'   `gunion()` to keep a copy of the original data.
#'
#' @section Usage:
#' Inherits from [PipeOp]
#' * `f = pipeOpNULL$new(id)` \cr
#'     `character(1)` -> [PipeOpNULL]
#' @family PipeOp
NULL

#' @include PipeOp.R
#' @export
PipeOpNULL = R6Class("PipeOpNULL",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "PipeOpNULL") {
      super$initialize(id,
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
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpNULL", PipeOpNULL)
