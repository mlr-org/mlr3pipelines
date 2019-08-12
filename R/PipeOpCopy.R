#' @title PipeOpCopy
#'
#' @name mlr_pipeop_copy
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#'   Copies its input `outnum` times. This is usually not needed,
#'   because copying happens automatically when one `PipeOp` is followed
#'   by multiple different `PipeOp`s. However, when constructing big
#'   Graphs using the `%>>%`-operator, `PipeOpCopy` can be helpful to
#'   specify which PipeOp gets connected to which.
#' @section Methods:
#' * `PipeOpEnsemble$new(outnum, id)` \cr
#'   (`numeric(1)`, `character(1)`) -> `self` \cr
#'   Constructor. `outnum` determines the number of output channels and
#'   copies that will be made.
#' @examples
#' # The following copies the output of 'scale' automatically to both
#' # 'pca' and 'nop'
#' mlr_pipeops$get("scale") %>>%
#'   gunion(list(
#'     mlr_pipeops$get("pca"),
#'     mlr_pipeops$get("nop")
#'   ))
#'
#' # The following would not work: the '%>>%'-operator does not know
#' # which output to connect to which input
#' # > gunion(list(
#' # >   mlr_pipeops$get("scale"),
#' # >   mlr_pipeops$get("select")
#' # > )) %>>%
#' # >   gunion(list(
#' # >     mlr_pipeops$get("pca"),
#' # >     mlr_pipeops$get("nop"),
#' # >     mlr_pipeops$get("impute")
#' # >   ))
#' # Instead, the 'copy' operator makes clear which output gets copied.
#' gunion(list(
#'   mlr_pipeops$get("scale") %>>% mlr_pipeops$get("copy", outnum = 2),
#'   mlr_pipeops$get("select")
#' )) %>>%
#'   gunion(list(
#'     mlr_pipeops$get("pca"),
#'     mlr_pipeops$get("nop"),
#'     mlr_pipeops$get("impute")
#'   ))
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpCopy = R6Class("PipeOpCopy",
  inherit = PipeOp,
  public = list(
    initialize = function(outnum, id = "copy", param_vals = list()) {
      assert_int(outnum, lower = 1)
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = "input", train = "*", predict = "*"),
        output = data.table(name = rep_suffix("output", outnum), train = "*", predict = "*")
      )
    },

    train_internal = function(inputs) {
      self$state = list()
      rep_len(inputs, self$outnum)
    },

    predict_internal = function(inputs) {
      rep_len(inputs, self$outnum)
    }
  )
)

mlr_pipeops$add("copy", PipeOpCopy, list("N"))
