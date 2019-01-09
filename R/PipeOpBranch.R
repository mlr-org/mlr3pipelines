#' @title PipeOpBranch
#'
#' @name PipeOpBranch
#' @format [R6Class] PipeOpBranch
#'
#' @description
#' This pipeop is used for multiplexing between different possible paths.
#'
#' @section Methods:
#' * `new(options = 1, id = "choice")` \cr
#'   (`integer(1)` | `character`), `character(1)` -> [`PipeOpBranch`]
#'
#' @section Parameters:
#' * `selection`: integer or discrete
#'
#' @section Details:
#' Creates a PipeOp with multiple output channels that can be used to
#' create a Graph network with alternative paths. If `options` is an `integer(1)`,
#' it determines the number of out-paths and `selection` is an integer parameter
#' choosing between these paths. If `options` is a `character`, then `length(options)`
#' out channels are created, each named according to `options`.
#'
#' To create a usable graph, the branching paths need to be brought together
#' using [`PipeOpUnbranch`].
#'
#' @family PipeOp
#' @family PipeOpBroadcast
#' @examples
#' pca = PipeOpPCA$new()
#' nop = PipeOpNULL$new()
#' choices = c("pca", "nothing")
#' PipeOpBranch$new(choices) %>>% gunion(list(pca, nop)) %>>% PipeOpUnbranch$new(choices)
NULL

#' @include PipeOp.R
#' @export
PipeOpBranch = R6Class("PipeOpBranch",
  inherit = PipeOp,
  public = list(
    outnum = NULL,
    initialize = function(options, id = "branch") {
      assert(
        check_int(options, lower = 1),
        check_character(options, min.len = 1, any.missing = FALSE)
      )
      if (is.numeric(options)) {
        options = round(options)
        param = ParamInt$new("selection", lower = 1, upper = options, default = 1)
        outnum = options
        options = rep_suffix("output", outnum)
      } else {
        param = ParamFct$new("selection", values = options, default = options[1])
        outnum = length(options)
      }
      super$initialize(id,
        param_set = ParamSet$new(params = list(param)),
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = options, train = "*", predict = "*")
      )
      self$outnum = outnum
    },

    train = function(inputs) {
      assert_list(inputs)
      self$state = list()
      ret = named_list(self$output$name)
      ret[[self$param_vals[[1L]]]] = inputs[[1L]]
      return(ret)
    },

    predict = function(inputs) {
      assert_list(inputs)
      ret = named_list(self$output$name)
      ret[[self$param_vals[[1L]]]] = inputs[[1L]]
      return(ret)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpBranch", PipeOpBranch)

