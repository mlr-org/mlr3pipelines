#' @title Shorthand PipeOp Constructor
#'
#' @description
#' Create
#'  - a `PipeOp` from `mlr_pipeops` from given ID
#'  - a `PipeOpLearner` from a `Learner` object
#'  - a `PipeOpFilter` from a `Filter` object
#'  - a `PipeOpSelect` from a `Selector` object
#'
#' The object is initialized with given parameters and `param_vals`.
#'
#' `po()` taks a single `obj` (`PipeOp` id, [`Learner`][mlr3::Learner], ...) and converts
#' it to a [`PipeOp`]. `pos()` (with plural-s) takes either a `character`-vector, or a
#' list of objects, and creates a `list` of [`PipeOp`]s.
#'
#' @param .obj `[any]`\cr
#'   The object from which to construct a `PipeOp`. If this is a
#'   `character(1)`, it is looked up in the [`mlr_pipeops`] dictionary.
#'   Otherwise, it is converted to a [`PipeOp`].
#' @param .objs `character` | `list`\cr
#'   Either a `character` of [`PipeOp`]s to look up in [`mlr_pipeops`],
#'   or a list of other objects to be converted to a [`PipeOp`].
#' @param ... `any`\cr
#'   Additional parameters to give to constructed object.
#'   This may be an argument of the constructor of the
#'   `PipeOp`, in which case it is given to this constructor;
#'   or it may be a parameter value, in which case it is
#'   given to the `param_vals` argument of the constructor.
#' @return A [`PipeOp`] (for `po()`), or a `list` of [`PipeOp`]s (for `pos()`).
#' @export
#' @examples
#' library("mlr3")
#'
#' po("learner", lrn("classif.rpart"), cp = 0.3)
#'
#' po(lrn("classif.rpart"), cp = 0.3)
#'
#' # is equivalent with:
#' mlr_pipeops$get("learner", lrn("classif.rpart"),
#'   param_vals = list(cp = 0.3))
#'
#' pos(c("pca", "nop"))
po = function(.obj, ...) {
  UseMethod("po")
}

#' @rdname po
#' @export
pos = function(.objs, ...) {
  UseMethod("pos")
}

#' @export
po.NULL = function(.obj, ...) {
  # class is NULL if .obj is missing
  dictionary_sugar(dict = mlr_pipeops)
}

#' @export
po.character = function(.obj, ...) {
  dictionary_sugar(dict = mlr_pipeops, .key = .obj, ...)
}

#' @export
po.Learner = function(.obj, ...) {
  # we use po() because that makes it possible to set hyperpars via `...`
  po(.obj = "learner", learner = .obj, ...)
}

#' @export
po.Filter = function(.obj, ...) {
  # we use po() because that makes it possible to set hyperpars via `...`
  po(.obj = "filter", filter = .obj, ...)
}

#' @export
po.Selector = function(.obj, ...) {
  po(.obj = "select", selector = .obj, ...)
}

#' @export
pos.NULL = function(.objs, ...) {
  # class is NULL if .obj is missing
  dictionary_sugar_mget(dict = mlr_pipeops)
}

#' @export
pos.character = function(.objs, ...) {
  dictionary_sugar_mget(dict = mlr_pipeops, .key = .objs, ...)
}

#' @export
pos.list = function(.objs, ...) {
  map(.x = .objs, .f = po, ...)
}
