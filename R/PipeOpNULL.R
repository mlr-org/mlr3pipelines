#' @title PipeOpNULL
#' @format [R6Class] PipeOpNULL
#' 
#' @description
#'   Simply pushes the input forward unchanged.
#'   Can be usefull for example to keep the original task in conjunction with
#'   `gunion()` to keep a copy of the original data.
#' 
#' @section Usage:
#' * `f = pipeOpNULL$new(id)` \cr
#'     `character(1)` -> [PipeOpNULL]
#' @name pipeOpNULL
#' @family pipeOp
#' @examples
#' # Do PCA on input data, but also keep a copy of the original input.
#' op1 = PipeOpNULL$new()
#' op2 = PipeOpPCA$new()
#' g = gunion(op1, op2) %>>% pipeOpFeatureUnion()
#' @export
PipeOpNULL = R6Class("PipeOpNULL",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "OpNULL") {
      super$initialize(id)
      private$.intype = list("any")
      private$.outtype = list("any")
    },

    train = function(inputs) {
      assert_list(inputs, len = 1L, type = "Task")
      self$state = list()
      inputs
    },

    predict = function(inputs) {
      return(inputs)
    }
  )
)