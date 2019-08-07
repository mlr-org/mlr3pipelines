


#' @title Shorthand for mlr3pipelines::mlr_pipeops$get
#'
#' @description
#' Create a `PipeOp` from `mlr_pipeops` from given ID, with
#' given parameters and `param_vals`.
#'
#' @param .key `[character(1)]`\cr
#'   The pipeop to construct
#' @param ... \cr
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
  assert_string(.key)

  args = list(...)
  given_argnames = names2(args)

  resulting_constructor = get0(.key, mlr_pipeops$items, ifnotfound = NULL)$value$public_methods$initialize
  signature_argnames = setdiff(names2(suppressWarnings(formals(args(resulting_constructor)))), ".key")

  args_not_in_sig = which(
    !is.na(given_argnames) &
    given_argnames %nin% signature_argnames
  )

  param_vals = args[args_not_in_sig]
  args[args_not_in_sig] = NULL
  args[["param_vals"]] = c(param_vals, args[["param_vals"]])
  do.call(mlr_pipeops$get, c(key = .key, args))
}
