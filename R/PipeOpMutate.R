#' @title PipeOpMutate
#'
#' @name mlr_pipeop_mutate
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`].
#'
#' @description
#' Adds features according to expressions.
#'
#' @section Parameter Set:
#' * `mutation` :: named `list` \cr
#'   Expressions for new features to create. Can be created with `alist()`.
#'   Can contain multiple expressions, that directly reference task features.
#' * `env` :: `environment` \cr
#'   Environment in which expressions are evaluated. This is the enclosing
#'   environment *after* the `Task`'s features are considered.
#' * `delete_originals` :: `logical(1)` \cr
#'   Whether to delete original features. If this is `FALSE` (the default),
#'   then features may still be overwritten.
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpMutate = R6Class("PipeOpMutate",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "mutate", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("mutation", custom_check = function(x) check_list(x, names = "unique"), tags = "required"),
        ParamUty$new("env", custom_check = function(x) check_environment, tags = "required"),
        ParamLgl$new("delete_originals", tags = "required")
      ))
      ps$values = list(mutation = named_list(), env = globalenv(), delete_originals = FALSE)
      super$initialize(id, ps, param_vals = param_vals)
    },

    transform = function(task) {
      taskdata = task$data(cols = task$feature_names)
      newdata = as.data.table(lapply(self$param_set$values$mutation, function(expr) {
        eval(expr, envir = taskdata, enclos = self$param_set$values$env)
      }))
      keep_feats = character(0)
      if (!self$param_set$values$delete_originals) {
        keep_feats = setdiff(task$feature_names, colnames(newdata))
      }
      task = task$select(keep_feats)
      if (ncol(newdata) && nrow(newdata) != task$nrow) {
        stopf("PipeOpMutate expression result has %s rows but must have %s rows.", nrow(newdata), task$nrow)
      }
      if (ncol(newdata)) task$cbind(newdata) # TODO: test if we can live without the `if()` here, but there seems to be a problem with 0-row data.tables
      task
    }
  )
)

mlr_pipeops$add("mutate", PipeOpMutate)
