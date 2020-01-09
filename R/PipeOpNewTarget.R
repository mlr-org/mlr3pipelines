#' @title PipeOpNewTarget
#'
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Substitutes the current target with a new target.
#' The new target must exist as a column in the Task.
#' The previous target is set to row_role: `unused'.
#'
#'
#' @section Public Members / Active Bindings:
#' * `new_target` :: `character`| `function` \cr
#' New target variable name for the task. The previous target variable is set to 'unused'.
#' * `new_task_type` :: `NULL` | `character` \cr
#' New task type for the resulting task. Defaults to 'classif' if `new_target`is `character` | `factor`,
#' else `regr`.
#'
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpNewTarget = R6Class("PipeOpNewTarget",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "new_target", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("new_target", tags = c("train", "predict", "required"), custom_check = check_string),
        ParamUty$new("new_task_type", tags = c("train", "predict", "required"), custom_check = function(x) {check_choice(x, mlr_reflections$task_types$type)})
      ))
      super$initialize(id = id, param_set = ps, param_vals = param_vals,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    },
    train_internal = function(inputs) {
      outputs = private$convert_task_type(inputs)
      self$state = list()
      return(outputs)
    },
    predict_internal = function(inputs) {
      private$convert_task_type(inputs)
    }
  ),
  private = list(
    convert_task_type = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      assert_choice(self$param_set$values$new_target, intask$col_info$id)
      task = convert_task(intask, self$param_set$values$new_task_type, self$param_set$values$new_target)
      if (all(self$param_set$values$new_target != intask$target_names))
        task$set_col_role(intask$col_roles$target, "unused")
      list(task)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("new_target", PipeOpNewTarget)


#' Convert a task from its type to another.
#'
#' The task's target is replaced by a different column from the data.
#' The previous target is added as a feature.
#' @param intask [`Task`]\cr
#'   A [`Task`] to be converted.
#' @param new_type `character(1)`\cr
#'   The new task type. Must be in  `mlr_reflections$task_types`.
#' @param new_target `character(1)|NULL`\cr
#'   New target to be set, must be a column in the `intask` data.
#'   If NULL, no new target is set.
#' @return [`Task`]
convert_task = function(intask, new_type, new_target = NULL) {
  assert_task(intask)
  assert_choice(new_target, intask$col_info$id, null.ok = TRUE)
  assert_choice(new_type, mlr_reflections$task_types$type)
  if (is.null(new_target)) new_target = intask$target_names
  if (new_type == intask$task_type & intask$target_names == new_target) return(intask)
  # Get task_type from mlr_reflections and call constructor.
  tsk = get(mlr_reflections$task_types[mlr_reflections$task_types$type == new_type,]$task)
  newtsk = tsk$new(id = intask$id, backend = intask$backend, target = new_target)
  # Copy row_roles
  newtsk$row_roles = intask$row_roles
  # Copy col_roles
  props = intersect(mlr_reflections$task_col_roles[[intask$task_type]],
    mlr_reflections$task_col_roles[[new_type]])
  newtsk$col_roles[props] = intask$col_roles[props]
  newtsk$set_col_role(new_target, "target")
  if (!all(intask$target_names == new_target))
    newtsk$set_col_role(intask$col_roles$target, "feature")
  return(newtsk)
}
