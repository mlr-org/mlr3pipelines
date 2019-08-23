


#' @title Shorthand for mlr3pipelines::mlr_pipeops$get
#'
#' @description
#' Create a `PipeOp` from `mlr_pipeops` from given ID, with
#' given parameters and `param_vals`.
#'
#' @param .key `[character(1)]`\cr
#'   The pipeop to construct
#' @param ... `any`\cr
#'   Additional parameters to give to constructed object.
#'   This may be an argument of the constructor of the
#'   `PipeOp`, in which case it is given to this constructor;
#'   or it may be a parameter value, in which case it is
#'   given to the `param_vals` argument of the constructor.
#' @examples
#' po("learner", "classif.rpart", cp = 0.3)
#' # is equivalent with:
#' mlr3pipelines::mlr_pipeops$get("learner", "classif.rpart",
#'   param_vals = list(cp = 0.3))
#' @export
po = function(.key, ...) {
  dictionary_sugar(mlr_pipeops, .key, ...)
}
