#' @title Simply Push Input Forward
#'
#' @usage NULL
#' @name mlr_pipeops_nop
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`].
#'
#' @description
#' Simply pushes the input forward.
#' Can be useful during [`Graph`] construction using the [`%>>%`]-operator to specify which [`PipeOp`] gets connected to which.
#'
#' @section Construction:
#' ```
#' PipeOpNOP$new(id = "nop", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"nop"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpNOP`] has one input channel named `"input"`, taking any input (`"*"`) both during training and prediction.
#'
#' [`PipeOpNOP`] has one output channel named `"output"`, producing the object given as input (`"*"`) without changes.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' [`PipeOpNOP`] has no parameters.
#'
#' @section Internals:
#' [`PipeOpNOP`] is a useful "default" stand-in for a [`PipeOp`]/[`Graph`] that does nothing.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' nop = po("nop")
#'
#' nop$train(list(1))
#'
#' # use `gunion` and `%>>%` to create a "bypass"
#' # next to "pca"
#' gr = gunion(list(
#'   po("pca"),
#'   nop
#' )) %>>% po("featureunion")
#'
#' gr$train(tsk("iris"))[[1]]$data()
#' @family PipeOps
#' @family Placeholder Pipeops
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
PipeOpNOP = R6Class("PipeOpNOP",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "nop", param_vals = list()) {
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = "output", train = "*", predict = "*"),
        tags = "meta"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      inputs
    },

    .predict = function(inputs) {
      inputs
    }
  )
)

mlr_pipeops$add("nop", PipeOpNOP)
