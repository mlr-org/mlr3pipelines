#' @title Dictionary of PipeOps
#'
#' @format [`R6Class`] object inheriting from [`mlr3::Dictionary`]
#' @description
#' A simple [`Dictionary`][mlr3::Dictionary] storing objects of class [`PipeOp`].
#' Each `PipeOp` has an associated help page, see `mlr_pipeop_[id]`.
#'
#' @section Usage:
#'
#' See [`mlr3::Dictionary`].
#'
#' @family mlr3pipelines backend related
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
