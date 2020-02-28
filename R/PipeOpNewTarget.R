#' @title PipeOpNewTarget
#' @name mlr_pipeops_newtarget
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#'   Substitutes the current target with a new target.
#'   The new target must be an existing column in the Task.
#'   The previous target is set to row_role: `character(0)'.
#'
#' @section Construction:
#'   ```
#'   PipeOpNewTarget$new(id = "mutate", param_vals = list(...))
#'   ```
#'   * `id` :: `character(1)`\cr
#'     Identifier of resulting object, default `"mutate"`.
#'   * `param_vals` :: named `list`\cr
#'     List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOp`].
#'
#' The output is the input [`Task`][mlr3::Task] of task_type specified in `new_task_type` and
#' new target specified in `new_target`.
#'
#' @section Parameters:
#' The parameters are:
#'   * `new_target` :: `character` \cr
#'   New target variable name for the task. The previous target variable's col_role is set to
#'   `character(0)`.
#'   * `new_task_type` :: `character` \cr
#'   New task type for the resulting task.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Methods inherited from [`PipeOp`]. Internally calls `convert_task` for task conversion.
#'
#' @family PipeOps
#' @include PipeOp.R
#' @export
#' @examples
#' library("mlr3")
#'
#' pom = po("new_target")
#' pom$param_set$values = list(
#'   new_target = "Sepal.Length",
#'   new_task_type = "regr"
#' )
#'
#' pom$train(list(tsk("iris")))[[1]]
PipeOpNewTarget = R6Class("PipeOpNewTarget",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "new_target", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("new_target", tags = c("train", "predict", "required"),
          custom_check = function(x) check_character(x, null.ok = TRUE)),
        ParamUty$new("new_task_type", tags = c("train", "predict", "required"),
          custom_check = function(x) check_choice(x, mlr_reflections$task_types$type, null.ok = TRUE))
      ))
      ps$values = list(new_target = NULL, new_task_type = NULL)
      super$initialize(id = id, param_set = ps, param_vals = param_vals,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    },
    train_internal = function(inputs) {
      self$state = list()
      private$convert_task_type(inputs)
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
        task$set_col_role(intask$col_roles$target, character(0))
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
#'   If `NULL` (default), a new task with the same task_type is created.
#' @param new_target `character(1)|NULL`\cr
#'   New target to be set, must be a column in the `intask` data.
#'   If `NULL`, no new target is set, and task is converted as-is.
#' @return [`Task`]
convert_task = function(intask, new_type = NULL, new_target = NULL) {
  assert_task(intask)
  assert_subset(new_target, intask$col_info$id)
  assert_choice(new_type, mlr_reflections$task_types$type, null.ok = TRUE)

  if (is.null(new_target)) new_target = intask$target_names
  if (is.null(new_type)) new_type = intask$task_type

  # Get task_type from mlr_reflections and call constructor.
  tsk = get(mlr_reflections$task_types[mlr_reflections$task_types$type == new_type,]$task)
  newtsk = tsk$new(id = intask$id, backend = intask$backend, target = new_target)
  # Copy row_roles / col_roles / properties
  newtsk$row_roles = intask$row_roles
  props = intersect(mlr_reflections$task_col_roles[[intask$task_type]],
    mlr_reflections$task_col_roles[[new_type]])
  newtsk$col_roles[props] = intask$col_roles[props]
  newtsk$set_col_role(new_target, "target")
  if (!all(intask$target_names == new_target))
    newtsk$set_col_role(intask$col_roles$target, "feature")
  newtsk$droplevels()
  return(newtsk)
}
