#' @title Path Branching
#'
#' @usage NULL
#' @name mlr_pipeops_branch
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Perform alternative path branching: [`PipeOpBranch`] has multiple output channels
#' that connect to different paths in a [`Graph`]. At any time, only one of these
#' paths will be taken for execution. At the end of the different paths, the
#' [`PipeOpUnbranch`] `PipeOp` must be used to indicate the end of alternative paths.
#'
#' Not to be confused with [`PipeOpCopy`], the naming scheme is a bit unfortunate.
#'
#' @section Construction:
#' ```
#' PipeOpBranch$new(options, id = "branch", param_vals = list())
#' ```
#' * `options` :: `numeric(1)` | `character`\cr
#'   If `options` is an integer number, it determines the number of
#'   output channels / options that are created, named `output1`...`output<n>`. The
#'   `$selection` parameter will then be a [`ParamInt`].
#'   If `options` is a `character`, it determines the names of channels directly.
#'   The `$selection` parameter will then be a [`ParamFct`].
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"branch"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpBranch`] has one input channel named `"input"`, taking any input (`"*"`) both during training and prediction.
#'
#' [`PipeOpBranch`] has multiple output channels depending on the `options` construction argument, named `"output1"`, `"output2"`, ...
#' if `options` is `numeric`, and named after each `options` value if `options` is a `character`.
#' All output channels produce the object given as input (`"*"`) or [`NO_OP`], both during training and prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' * `selection` :: `numeric(1)` | `character(1)`\cr
#'   Selection of branching path to take. Is a `ParamInt` if the `options` parameter
#'   during construction was a `numeric(1)`, and ranges from 1 to `options`. Is a
#'   `ParamFct` if the `options` parameter was a `character` and its possible values
#'   are the `options` values. Initialized to either 1 (if the `options` construction argument is `numeric(1)`)
#'   or the first element of `options` (if it is `character`).
#'
#' @section Internals:
#' Alternative path branching is handled by the [`PipeOp`] backend. To indicate that
#' a path should not be taken, [`PipeOpBranch`] returns the [`NO_OP`] object on its
#' output channel. The [`PipeOp`] handles each [`NO_OP`] input by automatically
#' returning a [`NO_OP`] output without calling `private$.train()` or `private$.predict()`,
#' until [`PipeOpUnbranch`] is reached. [`PipeOpUnbranch`] will then take multiple inputs,
#' all except one of which must be a [`NO_OP`], and forward the only non-[`NO_OP`]
#' object on its output.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Path Branching
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
#' @examples
#' library("mlr3")
#'
#' pca = po("pca")
#' nop = po("nop")
#' choices = c("pca", "nothing")
#' gr = po("branch", choices) %>>%
#'   gunion(list(pca, nop)) %>>%
#'   po("unbranch", choices)
#'
#' gr$param_set$values$branch.selection = "pca"
#' gr$train(tsk("iris"))
#'
#' gr$param_set$values$branch.selection = "nothing"
#' gr$train(tsk("iris"))
PipeOpBranch = R6Class("PipeOpBranch",
  inherit = PipeOp,
  public = list(
    initialize = function(options, id = "branch", param_vals = list()) {
      assert(
        check_int(options, lower = 1L),
        check_character(options, min.len = 1L, any.missing = FALSE)
      )
      if (is.numeric(options)) {
        options = round(options)
        param = p_int(lower = 1L, upper = options, tags = c("train", "predict", "required"))
        options = rep_suffix("output", options)
        initval = 1
      } else {
        param = p_fct(options, tags = c("train", "predict", "required"))
        initval = options[1]
      }
      ps = ps(selection = param)
      ps$values$selection = initval
      super$initialize(id, ps, param_vals,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = options, train = "*", predict = "*"),
        tags = "meta"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      ret = named_list(self$output$name, NO_OP)
      ret[[self$param_set$values$selection]] = inputs[[1]]
      ret
    },
    .predict = function(inputs) {
      assert_list(inputs)
      ret = named_list(self$output$name, NO_OP)
      ret[[self$param_set$values$selection]] = inputs[[1]]
      ret
    },
    .additional_phash_input = function() c(class(self$param_set$params$selection), self$output$name)
  )
)

mlr_pipeops$add("branch", PipeOpBranch, list("N"))

