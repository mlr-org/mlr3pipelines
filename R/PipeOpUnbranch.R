#' @title Unbranch Different Paths
#'
#' @usage NULL
#' @name mlr_pipeops_unbranch
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Used to bring together different paths created by [`PipeOpBranch`].
#'
#' @section Construction:
#' ```
#' PipeOpUnbranch$new(options, id = "unbranch", param_vals = list())
#' ```
#' * `options` :: `numeric(1)` | `character`\cr
#'   If `options` is 0, a vararg input channel is created that can take
#'   any number of inputs.
#'   If `options` is a nonzero integer number, it determines the number of
#'   input channels / options that are created, named `input1`...`input<n>`. The
#'   If `options` is a `character`, it determines the names of channels directly.
#'   The difference between these three is purely cosmetic if the user chooses
#'   to produce channel names matching with the corresponding [`PipeOpBranch`].
#'   However, it is not necessary to have matching names and the *vararg* option
#'   is always viable.
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"unbranch"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output:
#' [`PipeOpUnbranch`] has multiple input channels depending on the `options` construction argument, named `"input1"`, `"input2"`, ...
#' if `options` is a nonzero integer and named after each `options` value if `options` is a `character`; if `options` is 0, there is only one
#' *vararg* input channel named `"..."`.
#' All input channels take any argument (`"*"`) both during training and prediction.
#'
#' [`PipeOpUnbranch`] has one output channel named `"output"`, producing the only [`NO_OP`] object received as input (`"*"`),
#' both during training and prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' [`PipeOpUnbranch`] has no parameters.
#'
#' @section Internals:
#' See [`PipeOpBranch`] Internals on how alternative path branching works.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @examples
#' # See PipeOpBranch for a complete branching example
#' pou = po("unbranch")
#'
#' pou$train(list(NO_OP, NO_OP, "hello", NO_OP, NO_OP))
#' @family PipeOps
#' @family Path Branching
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
PipeOpUnbranch = R6Class("PipeOpUnbranch",
  inherit = PipeOp,
  public = list(
    initialize = function(options = 0, id = "unbranch", param_vals = list()) {
      assert(
        check_int(options, lower = 0L),
        check_character(options, min.len = 1L, any.missing = FALSE)
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
        output = data.table(name = "output", train = "*", predict = "*"),
        tags = "meta"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      filter_noop(inputs)
    },
    .predict = function(inputs) {
      filter_noop(inputs)
    },
    .additional_phash_input = function() self$input$name
  )
)

mlr_pipeops$add("unbranch", PipeOpUnbranch)
