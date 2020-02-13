#' @title Shorthand Graph Constructor
#'
#' @description
#' Creates a `Graph` from `mlr_graphs` from given ID
#'
#' @param key `[character]`\cr
#'   The object from which to construct a `PipeOp`. If this is a
#'   `character(1)`, it is looked up in the [`mlr_pipeops`] dictionary.
#'   Otherwise, it is converted to a `PipeOp`.
#' @param ... `any`\cr
#'   Additional parameters to give to constructed object.
#'   This may be an argument of the constructor of the
#'   `underlying function`.
#' @return [`Graph`]
#' @export
#' @examples
#' library("mlr3")
#'
#' gr = pipe("bagging_pipeline", graph = po(lrn("regr.rpart")), averager = po("regravg"))
pipe = function(key, ...) {
  dictionary_sugar(dict = mlr_graphs, .key = key, ...)
}
