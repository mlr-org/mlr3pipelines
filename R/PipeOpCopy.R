#' @title Copy Input Multiple Times
#'
#' @usage NULL
#' @name mlr_pipeops_copy
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`].
#'
#' @description
#' Copies its input `outnum` times. This [`PipeOp`] usually not needed, because copying happens automatically when one
#' [`PipeOp`] is followed by multiple different [`PipeOp`]s. However, when constructing big Graphs using the
#' [`%>>%`]-operator, `PipeOpCopy` can be helpful to specify which [`PipeOp`] gets connected to which.
#'
#' @section Construction:
#' ```
#' PipeOpCopy$new(outnum, id = "copy", param_vals = list())
#' ```
#'
#' * `outnum` :: `numeric(1)`\cr
#'   Number of output channels, and therefore number of copies being made.
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"copy"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' `PipeOpCopy` has one input channel named `"input"`, taking any input (`"*"`) both during training and prediction.
#'
#' `PipeOpCopy` has multiple output channels depending on the `outnum` construction argument, named `"output1"`, `"output2"`, ...
#' All output channels produce the object given as input (`"*"`).
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' `PipeOpCopy` has no parameters.
#'
#' @section Internals:
#' Note that copies are not clones, but only reference copies. This affects R6-objects: If R6 objects are copied using
#' `PipeOpCopy`, they must be cloned beforehand.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @examples
#' # The following copies the output of 'scale' automatically to both
#' # 'pca' and 'nop'
#' po("scale") %>>%
#'   gunion(list(
#'     po("pca"),
#'     po("nop")
#'   ))
#'
#' # The following would not work: the '%>>%'-operator does not know
#' # which output to connect to which input
#' # > gunion(list(
#' # >   po("scale"),
#' # >   po("select")
#' # > )) %>>%
#' # >   gunion(list(
#' # >     po("pca"),
#' # >     po("nop"),
#' # >     po("imputemean")
#' # >   ))
#' # Instead, the 'copy' operator makes clear which output gets copied.
#' gunion(list(
#'   po("scale") %>>% po("copy", outnum = 2),
#'   po("select")
#' )) %>>%
#'   gunion(list(
#'     po("pca"),
#'     po("nop"),
#'     po("imputemean")
#'   ))
#' @family PipeOps
#' @family Placeholder Pipeops
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
PipeOpCopy = R6Class("PipeOpCopy",
  inherit = PipeOp,
  public = list(
    initialize = function(outnum, id = "copy", param_vals = list()) {
      assert_int(outnum, lower = 1)
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = rep_suffix("output", outnum), train = "*", predict = "*"),
        tags = "meta"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      rep_len(inputs, self$outnum)
    },
    .predict = function(inputs) {
      rep_len(inputs, self$outnum)
    },
    .additional_phash_input = function() self$output$name
  )
)

mlr_pipeops$add("copy", PipeOpCopy, list("N"))
