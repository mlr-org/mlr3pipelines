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
#' Not to be confused with [`PipeOpCopy`], the naming scheme is a bit unfortunate.
#'
#' @family PipeOp
#' @family PipeOpBroadcast
#' @examples
#' pca = PipeOpPCA$new()
#' nop = PipeOpNULL$new()
#' choices = c("pca", "nothing")
#' PipeOpBranch$new(choices) %>>% gunion(list(pca, nop)) %>>% PipeOpUnbranch$new(choices)
NULL

#' @title No-Op Sentinel Used for Alternative Branching
#'
#' @description
#' Special data type for no-ops. Distinct from NULL for easier
#' and distinction from unintentional NULL returns.
#'
#' @export
NO_OP = R6Class("NO_OP",
  public = list(
    initialize = function() {},
    print = function() cat("mlr3pipelines NO_OP indicator\n")
  ),
)$new()
is_noop = function(x) test_r6(x, "NO_OP")
filter_noop = function(x) Filter(Negate(is_noop), x)

#' @include PipeOp.R
#' @export
PipeOpBranch = R6Class("PipeOpBranch",
  inherit = PipeOp,
  public = list(
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
      }
      super$initialize(id,
        param_set = ParamSet$new(params = list(param)),
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = options, train = "*", predict = "*")
      )
      self$param_vals$selection = self$param_set$params$selection$default
    },

    train = function(inputs) {
      self$state = list()
      ret = named_list(self$output$name, NO_OP)
      ret[[self$param_vals$selection]] = inputs[[1]]
      ret
    },

    predict = function(inputs) {
      assert_list(inputs)
      ret = named_list(self$output$name, NO_OP)
      ret[[self$param_vals$selection]] = inputs[[1]]
      ret
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("PipeOpBranch", PipeOpBranch)

