#' @title Change Column Roles of a Task
#'
#' @usage NULL
#' @name mlr_pipeops_colroles
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Changes the column roles of the input [`Task`][mlr3::Task] according to `new_role`.
#'
#' @section Construction:
#' ```
#' PipeOpColRoles$new(id = "colroles", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"colroles"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with transformed column roles according to `new_role`.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `new_role` :: `list`\cr
#'   Named list of new column roles. The names must match the column names of the input task that
#'   will later be trained/predicted on. Each entry of the list must contain a character vector with
#'   possible values of [`mlr_reflections$task_col_roles`][mlr3::mlr_reflections]. If the value is
#'   given as `character()`, the column will be dropped from the input task. Changing the role of a
#'   column results in this column loosing its previous role(s). Setting a new target variable or
#'   changing the role of an existing target variable is not supported.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("boston_housing")
#' pop = po("colroles", param_vals = list(
#'   new_role = list(town = c("order", "feature"))
#' ))
#'
#' pop$train(list(task))
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpColRoles = R6Class("PipeOpColRoles",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "colroles", param_vals = list()) {
      ps = ParamSet$new(params = list(
        # named list, each entry with a vector of roles
        new_role = p_uty(tags = c("train", "predict"), custom_check = function(x) {
          first_check = check_list(x, types = "character", any.missing = FALSE, min.len = 1L, names = "named")
          # return the error directly if this failed
          if (is.character(first_check)) {
            return(first_check)
          }
          # changing anything target related is not supported
          # a value of "character()" will lead to the column being dropped
          all_col_roles = unique(unlist(mlr3::mlr_reflections$task_col_roles))
          check_subset(unlist(x), all_col_roles[all_col_roles != "target"])
        })
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE)
    }
  ),
  private = list(
    .transform = function(task) {
      new_role = self$param_set$values$new_role

      if (is.null(new_role)) {
        return(task)  # early exit
      }

      new_role_names = names(new_role)
      ids = task$col_info$id
      ids = ids[ids != "..row_ids"]
      # names of "new_role" must be a subset of the column names of the task
      assert_subset(new_role_names, choices = ids, empty.ok = FALSE)

      # changing the role of a target is not supported
      if (any(task$col_roles$target %in% new_role_names)) {
        stopf("Cannot change the role of a target.")
      }

      # drop (all) old role(s)
      task$col_roles = map(task$col_roles, .f = function(x) x[x %nin% new_role_names])

      # add the new role(s)
      all_col_roles = unique(unlist(mlr3::mlr_reflections$task_col_roles))
      for(role in all_col_roles) {
        task$col_roles[[role]] = union(task$col_roles[[role]],
          y = names(which(unlist(map(new_role, .f = function(x) role %in% x)))))
      }

      task
    }
  )
)

mlr_pipeops$add("colroles", PipeOpColRoles)
