#' @title PipeOpUnbranch
#'
#' @name mlr_pipeop_unbranch
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Used to bring together different paths created by [`PipeOpBranch`].
#'
#' @section Methods:
#' * `PipeOpUnbranch$new(options = 0, id = "unbranch")` \cr
#'   (`numeric(1)` | `character`, `character(1)`) -> `self` \cr
#'   Constructor. If `options` is 0, only one vararg input channel is created that can
#'   be connected to an arbitrary number of branches. If `options` is a positive integer
#'   number, it determines the number of input channels that are created, named
#'   `input1`...`input<n>`. If `options` is a `character`, it determines the names of channels directly.
#'
#' @section Details:
#' Creates a PipeOp with multiple input channels that can be used to
#' create a Graph network with alternative paths. `options` works as in [`PipeOpBranch`]
#' and should probably be the same value as the `options` given to the corresponding
#' [`PipeOpBranch`] instance.
#'
#' @examples
#' pca = PipeOpPCA$new()
#' nop = PipeOpNOP$new()
#' choices = c("pca", "nothing")
#' PipeOpBranch$new(choices) %>>% gunion(list(pca, nop)) %>>% PipeOpUnbranch$new(choices)
#' @family PipeOps
#' @family Path Branching
#' @include PipeOp.R
#' @export
PipeOpUnbranch = R6Class("PipeOpUnbranch",
  inherit = PipeOp,
  public = list(
    initialize = function(options = 0, id = "unbranch", param_vals = list()) {
      assert(
        check_int(options, lower = 0),
        check_character(options, min.len = 1, any.missing = FALSE)
      )
      if (is.numeric(options)) {
        if (options) {
          options = rep_suffix("input", round(options))
        } else {
          options = "..."
        }
      }
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = options, train = "*", predict = "*"),
        output = data.table(name = "output", train = "*", predict = "*")
      )
    },

    train_internal = function(inputs) {
      self$state = list()
      filter_noop(inputs)
    },

    predict_internal = function(inputs) {
      filter_noop(inputs)
    }
  )
)

mlr_pipeops$add("unbranch", PipeOpUnbranch)
