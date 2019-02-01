#' @title PipeOpBranch
#'
#' @name mlr_pipeop_branch
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' This pipeop is used for multiplexing between different possible paths and
#' should be used in conjunction with [`PipeOpUnbranch`].
#'
#' @section Methods:
#' * `PipeOpBranch$new(options, id = "branch")` \cr
#'   (`numeric(1)` | `character`, `character(1)`) -> `self` \cr
#'   Constructor. If `options` is an integer number, it determines the number of
#'   output channels / options that are created, named `output1`...`output<n>`. The
#'   `$selection` parameter will then be a [`ParamInt`].
#'   If `options` is a `character`, it determines the names of channels directly.
#'   The `$selection` parameter will then be a [`ParamFct`].
#'
#' @section Parameters:
#' * `selection`: (`numeric(1)` | `character(1)`) \cr
#'   Selection of branching path to take. Is a `ParamInt` if the `options` parameter
#'   during construction was a `numeric(1)`, and ranges from 1 to `options`. Is a
#'   `ParamFct` if the `options` parameter was a `character` and its possible values
#'   are the `options` values.
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
#' @examples
#' pca = PipeOpPCA$new()
#' nop = PipeOpNULL$new()
#' choices = c("pca", "nothing")
#' PipeOpBranch$new(choices) %>>% gunion(list(pca, nop)) %>>% PipeOpUnbranch$new(choices)
#' @family PipeOps
#' @family Path Branching
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

# This doesn't work, because there is no "default" number of options
# (and there really shouldn't be!)
# #' @include mlr_pipeops.R
# mlr_pipeops$add("branch", PipeOpBranch)

