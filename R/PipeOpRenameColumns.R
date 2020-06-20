#' @title PipeOpRenameColumns
#'
#' @usage NULL
#' @name mlr_pipeops_renamecolumns
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOp`].
#'
#' @description
#' Renames the columns of a [`Task`][mlr3::Task] both during training and prediction.
#' Uses the `$rename()` mutator of the [`Task`][mlr3::Task].
#'
#' @section Construction:
#' ```
#' PipeOpRenameColumns$new(id = "renamecolumns", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"renamecolumns"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreprocSimple`].
#'
#' The output is the input [`Task`][mlr3::Task] with the old column names changed to the new ones.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreprocSimple`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreprocSimple`], as well as:
#' * `renaming` :: named `character`\cr
#'   Named `character` vector. The names of the vector specify the old column names that should be
#'   changed to the new column names as given by the elements of the vector. Initialized to the empty
#'   character vector.
#' * `ignore_missing` :: `logical(1)`\cr
#'   Ignore if columns named in `renaming` are not found in the input [`Task`][mlr3::Task]. If this is
#'   `FALSE`, then names found in `renaming` not found in the [`Task`][mlr3::Task] cause an error.
#'   Initialized to `FALSE`.
#'
#' @section Internals:
#' Uses the `$rename()` mutator of the [`Task`][mlr3::Task] to set the new column names.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pop = po("renamecolumns", param_vals = list(renaming = c("Petal.Length" = "PL")))
#' pop$train(list(task))
PipeOpRenameColumns = R6Class("PipeOpRenameColumns",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "renamecolumns", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("renaming", tags = c("train", "predict", "required"), custom_check = function(x) {
          check_character(x, any.missing = FALSE, names = "strict") %check&&%
            check_names(x, type = "strict")
        }),
        ParamLgl$new("ignore_missing", tags = c("train", "predict", "required"))
      ))
      ps$values = list(renaming = character(0), ignore_missing = FALSE)
      super$initialize(id, ps, param_vals = param_vals, can_subset_cols = FALSE)
    }
  ),
  private = list(
    .transform = function(task) {
      if (!length(self$param_set$values$renaming)) {
        return(task)  # early exit
      }
      innames = names(self$param_set$values$renaming)
      nontargets = task$col_roles
      nontargets$target = NULL
      takenames = innames %in% unlist(nontargets)
      if (!self$param_set$values$ignore_missing && !all(takenames)) {
        # we can't rely on task$rename because it could also change the target name, which we don't want.
        stopf("The names %s from `renaming` parameter were not found in the Task.", str_collapse(innames[!takenames]))
      }
      task$rename(old = innames[takenames], new = self$param_set$values$renaming[takenames])
    }
  )
)

mlr_pipeops$add("renamecolumns", PipeOpRenameColumns)
