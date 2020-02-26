#' @title Shorthand Graph Constructor
#'
#' @description
#' Creates a [`Graph`] from [`mlr_graphs`] from given ID
#'
#' @param key `[character(1)]`\cr
#'   The key of the [`Graph`] in [`mlr_graphs`].
#' @param ... `any`\cr
#'   Additional parameters to give to constructed object.
#'   This may be an argument of the constructor of the
#'   underlying function.
#' @return [`Graph`]
#' @export
#' @examples
#' library("mlr3")
#'
#' gr = ppl("bagging", graph = po(lrn("regr.rpart")), averager = po("regravg"))
ppl = function(key, ...) {
  dictionary_sugar(dict = mlr_graphs, .key = key, ...)
}
