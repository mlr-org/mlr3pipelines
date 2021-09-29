#' @title Shorthand Graph Constructor
#'
#' @description
#' Creates a [`Graph`] from [`mlr_graphs`] from given ID
#'
#' `ppl()` taks a `charcter(1)` and returns a [`Graph`]. `ppls()` takes a `character`
#' vector of any list and returns a `list` of possibly muliple [`Graph`]s.
#'
#' @param .key `[character(1)]`\cr
#'   The key of the [`Graph`] in [`mlr_graphs`].
#' @param ... `any`\cr
#'   Additional parameters to give to constructed object.
#'   This may be an argument of the constructor of the
#'   underlying function.
#' @return [`Graph`] (for `ppl()`) or `list` of [`Graph`]s (for `ppls()`).
#' @export
#' @examples
#' library("mlr3")
#'
#' gr = ppl("bagging", graph = po(lrn("regr.rpart")),
#'   averager = po("regravg", collect_multiplicity = TRUE))
ppl = function(.key, ...) {
  dictionary_sugar(dict = mlr_graphs, .key = .key, ...)
}

#' @export
#' @rdname ppl
ppls = function(.keys, ...) {
  dictionary_sugar_mget(dict = mlr_graphs, .keys = .keys, ...)
}
