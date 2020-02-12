#' @title Shorthand Graph Constructor
#'
#' @description
#' Creates a `Graph` from `mlr_graphs` from given ID
#'
#' @param .obj `[any]`\cr
#'   The object from which to construct a `PipeOp`. If this is a
#'   `character(1)`, it is looked up in the [`mlr_pipeops`] dictionary.
#'   Otherwise, it is converted to a `PipeOp`.
#' @param ... `any`\cr
#'   Additional parameters to give to constructed object.
#'   This may be an argument of the constructor of the
#'   `PipeOp`, in which case it is given to this constructor;
#'   or it may be a parameter value, in which case it is
#'   given to the `param_vals` argument of the constructor.
#' @export
#' @examples
#' library("mlr3")
#'
#' gr = pipe("bagging_pipeline", graph = po(lrn("regr.rpart")), averager = po("regravg"))
pipe = function(.obj, ...) {
  UseMethod("pipe")
}

#' @export
pipe.character = function(.obj, ...) {
  dictionary_sugar(dict = mlr_graphs, .key = .obj, ...)
}
