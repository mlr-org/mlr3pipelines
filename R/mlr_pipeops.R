#' @title Dictionary of PipeOps
#'
#' @format [R6Class] object
#' @description
#' A simple [Dictionary] storing objects of class [PipeOp].
#' Each learner has an associated help page, see `mlr_pipeops_[id]`.
#'
#' @section Usage:
#'
#' See [mlr3::Dictionary].
#'
#' @family Dictionary
#' @family PipeOps
#' @name mlr_pipeops
NULL

DictionaryPipeOp = R6Class("DictionaryPipeOp", inherit = mlr3::Dictionary,
  cloneable = FALSE
)

#' @export
mlr_pipeops = DictionaryPipeOp$new()

#' @export
as.data.table.DictionaryPipeOp = function(x, ...) {
  setkeyv(map_dtr(x$ids(), function(id) {
    l = x$get(id)
    list(id = id, packages = list(l$packages))
  }), "id")[]
}
