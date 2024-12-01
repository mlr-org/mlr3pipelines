#' @title Change Column Roles of a Task
#'
#' @usage NULL
#' @name mlr_pipeops_colroles
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Changes the column roles of the input [`Task`][mlr3::Task] according to `new_role` or its inverse `new_role_direct`.
#'
#' Setting a new target variable or changing the role of an existing target variable is not supported.
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
#' The output is the input [`Task`][mlr3::Task] with transformed column roles according to `new_role` or its inverse `new_role_direct`.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `new_role` :: named `list`\cr
#'   Named list of new column roles by column. The names must match the column names of the input task that
#'   will later be trained/predicted on. Each entry of the list must contain a character vector with
#'   possible values of [`mlr_reflections$task_col_roles`][mlr3::mlr_reflections].
#'   If the value is given as `character()`, the column will be dropped from the input task. Changing the role
#'   of a column results in this column loosing its previous role(s).
#' * `new_role_direct` :: named `list`\cr#
#'   Named list of new column roles by role. The names must match the possible column roles, i.e. values of
#'   [`mlr_reflections$task_col_roles`][mlr3::mlr_reflections]. Each entry of the list must contain a character
#'   vector with column names of the input task that will later be trained/predicted on.
#'   If the value is given as `character()` or `NULL`, all columns will be dropped from the role given in the element
#'   name. The value given for a role overwrites the previous entry in `task$col_roles` for that role, completely.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("penguins")
#' pop = po("colroles", param_vals = list(
#'   new_role = list(body_mass = c("order", "feature"))
#' ))
#'
#' train_out1 = pop$train(list(task))
#' train_out1$col_roles
#'
#' pop$param_set$set_values(
#'  new_role = NULL,
#'  new_role_direct = list(order = character(), group = "island")
#' )
#'
#' train_out2 = pop$train(list(train_out))
#' train_out2$col_roles
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpColRoles = R6Class("PipeOpColRoles",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "colroles", param_vals = list()) {
      ps = ps(
        # named list, each entry with a vector of roles, names are columns
        new_role = p_uty(
          tags = c("train", "predict"),
          custom_check = crate(function(x) {
            first_check = check_list(x, types = "character", any.missing = FALSE, min.len = 1L, names = "named")
            # Return the error directly if this failed
            if (is.character(first_check)) return(first_check)
            # Changing anything target related is not supported.
            # A value of "character()" is accepted.
            all_col_roles = unique(unlist(mlr3::mlr_reflections$task_col_roles))
            check_subset(unlist(x), all_col_roles[all_col_roles != "target"])
          }, .parent = topenv())
        ),
        # named list, each with a vector of columns, names are column roles
        new_role_direct = p_uty(
          tags = c("train", "predict"),
          custom_check = crate(function(x) {
            first_check = check_list(x, types = "character", any.missing = FALSE, min.len = 1L, unique = TRUE, names = "named", null.ok = TRUE)
            # Return the error directly if this failed
            if (is.character(first_check)) return(first_check)
            # Changing anything target related is not supported.
            # A value of "character()" is accepted.
            all_col_roles = unique(unlist(mlr3::mlr_reflections$task_col_roles))
            check_subset(names(x), all_col_roles[all_col_roles != "target"])
          }, .parent = topenv())
        )
      )
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE)
    }
  ),
  private = list(
    .transform = function(task) {
      new_role = self$param_set$values[["new_role"]]
      new_role_direct = self$param_set$values[["new_role_direct"]]

      if (is.null(new_role) && is.null(new_role_direct)) {
        return(task)  # early exit
      }
      if (!is.null(new_role) && !is.null(new_role_direct)) {
        stop("Both parameters, 'new_role' and 'new_role_direct', are set. Provide only one parameter at a time.")
      }

      # Get the columns for which we change roles
      # Replace NULLs with character(0)
      if (!is.null(new_role)) {
        new_role_cols = names(new_role)
      } else if (!is.null(new_role_direct)) {
        new_role_cols = unique(unlist(new_role_direct))
        # Replace NULL with empty string, to allow option with NULL (might throw this out)
        # would make sense to add NULL to new_role as well if we keep it
        new_role_direct <- lapply(new_role_direct, function(x) if (is.null(x)) character(0) else x)
      }

      # Changing the role of a target is not supported
      if (any(task$col_roles$target %in% new_role_cols)) {
        stop("Cannot change the role of a target.")
      }

      if (!is.null(new_role)) {
        # Drop (all) column(s) for which we change the role
        task$col_roles = map(task$col_roles, .f = function(x) x[x %nin% new_role_cols])

        # Add new role(s) for column(s) for which we change the role
        all_col_roles = unique(unlist(mlr3::mlr_reflections$task_col_roles))
        for(role in all_col_roles) {
          task$col_roles[[role]] = union(task$col_roles[[role]],
            names(which(unlist(map(new_role, .f = function(x) role %in% x)))))
        }
      } else if (!is.null(new_role_direct)) {
        # Update roles to match new_role_direct
        task$col_roles[names(new_role_direct)] = lapply(names(new_role_direct), function(role) new_role_direct[[role]])
      }

      task
    }
  )
)

mlr_pipeops$add("colroles", PipeOpColRoles)
