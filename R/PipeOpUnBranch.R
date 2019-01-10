#' @title PipeOpUnbranch
#'
#' @name PipeOpUnbranch
#' @format [R6Class] PipeOpUnbranch
#'
#' @description
#' Used to bring together different paths created by [`PipeOpBranch`].
#'
#' @section Methods:
#' * `new(options = 1)` \cr
#'   (`integer(1)` | `character`) -> [`PipeOpBranch`]
#'
#' @section Details:
#' Creates a PipeOp with multiple input channels that can be used to
#' create a Graph network with alternative paths. `options` works as in [`PipeOpBranch`]
#' and should probably be the same value as the `options` given to the corresponding
#' [`PipeOpBranch`] instance.
#'
#' @examples
#' pca = PipeOpPCA$new()
#' nop = PipeOpNULL$new()
#' choices = c("pca", "nothing")
#' PipeOpBranch$new(choices) %>>% gunion(list(pca, nop)) %>>% PipeOpUnbranch$new(choices)
#'
#' @family PipeOp
#' @family PipeOpAggregate
NULL

#' @include PipeOp.R
#' @export
PipeOpUnbranch = R6Class("PipeOpUnbranch",
  inherit = PipeOp,
  public = list(
    initialize = function(options, id = "unbranch") {
      assert(
        check_int(options, lower = 1),
        check_character(options, min.len = 1, any.missing = FALSE)
      )
      if (is.numeric(options)) {
        options = round(options)
        innum = options
      } else {
        innum = length(options)
      }
      super$initialize(id,
        input = data.table(name = rep_suffix("input", innum), train = "*", predict = "*"),
        output = data.table(name = "output", train = "*", predict = "*")
      )
    },

    train = function(inputs) {
      assert_list(inputs, len = self$innum)
      self$state = list()
      result = filter_noop(inputs)
      assert_list(result, len = 1)
      return(result)
    },

    predict = function(inputs) {
      assert_list(inputs, len = self$innum)
      result = filter_noop(inputs)
      assert_list(result, len = 1)
      return(result)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpUnbranch", PipeOpUnbranch)
