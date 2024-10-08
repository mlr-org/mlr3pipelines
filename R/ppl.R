#' @title Shorthand Graph Constructor
#'
#' @description
#' Creates a [`Graph`] from [`mlr_graphs`] from given ID
#'
#' `ppl()` taks a `character(1)` and returns a [`Graph`]. `ppls()` takes a `character`
#' vector of any list and returns a `list` of possibly muliple [`Graph`]s.
#'
#' @param .key `[character(1)]`\cr
#'   The key of the [`Graph`] in [`mlr_graphs`].
#' @param .keys `[character]`\cr
#'   The key of possibly multiple [`Graph`]s in [`mlr_graphs`]. If this is named, a
#'   named `list` is returned, but unlike [`pos()`] it will not set any `$id` slots.
#' @param ... `any`\cr
#'   Additional parameters to give to constructed object.
#'   This may be an argument of the constructor of the
#'   underlying function.
#' @return [`Graph`] (for `ppl()`) or `list` of [`Graph`]s (for `ppls()`).
#' @export
#' @examplesIf requireNamespace("rpart")
#' library("mlr3")
#'
#' gr = ppl("bagging", graph = po(lrn("regr.rpart")),
#'   averager = po("regravg", collect_multiplicity = TRUE))
ppl = function(.key, ...) {
  dictionary_sugar_get(dict = mlr_graphs, .key = .key, ..., .dicts_suggest = list("po()" = mlr_pipeops))
}

#' @export
#' @rdname ppl
ppls = function(.keys, ...) {
  if (missing(.keys)) return(mlr_graphs)
  map(.x = .keys, .f = dictionary_sugar_get, dict = mlr_graphs, ..., .dicts_suggest = list("pos()" = mlr_pipeops))
}
