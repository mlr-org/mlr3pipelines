#' Convert a task from its type to another.
#'
#' The task's target is replaced by a different column from the data.
#' #FIXME: This needs detailed tests.
#' @param intask [`Task`][mlr3::Task]\cr
#'   A [`Task`][mlr3::Task] to be converted.
#' @param new_target `character(1)|NULL`\cr
#'   New target to be set, must be a column in the `intask` data.
#'   If `NULL`, no new target is set, and task is converted as-is.
#' @param new_type `character(1)`\cr
#'   The new task type. Must be in `mlr_reflections$task_types`.
#'   If `NULL` (default), a new task with the same task_type is created.
#' @param drop_original_target `logical(1)`\cr
#'   If `FALSE` (default), the original target is added as a feature.
#'   Otherwise the original target is assigned no col_role, i.e. dropped.
#' @param \dots\r
#'  Further arguments passed to the constructor of the task.
#' @return [`Task`][mlr3::Task]
convert_task = function(intask, new_target = NULL, new_type = NULL, drop_original_target = FALSE, ...) {
  assert_task(intask)
  assert_subset(new_target, choices = intask$col_info$id)
  assert_choice(new_type, choices = mlr_reflections$task_types$type, null.ok = TRUE)
  assert_logical(drop_original_target, any.missing = FALSE, len = 1L)

  if (is.null(new_target)) new_target = intask$target_names
  if (is.null(new_type)) new_type = intask$task_type

  # get task_type from mlr_reflections and call constructor
  newtask = get(mlr_reflections$task_types[new_type, ]$task)$new(id = intask$id, backend = intask$backend, target = new_target, ...)
  # copy row_roles / col_roles / properties
  newtask$row_roles = intask$row_roles
  props = intersect(mlr_reflections$task_col_roles[[intask$task_type]], mlr_reflections$task_col_roles[[new_type]])
  newtask$col_roles[props] = intask$col_roles[props]
  newtask$set_col_role(new_target, "target")
  if (!all(intask$target_names == new_target)) newtask$set_col_role(intask$col_roles$target, "feature")
  # during prediction, when target is NA, we do not call droplevels
  if (!all(is.na(newtask$data()[, newtask$target_names, with = FALSE]))) newtask$droplevels()
  # if drop_original_target, remove the original target from the col_roles
  # FIXME: use unused col_role
  if (drop_original_target) newtask$col_roles$feature = setdiff(newtask$col_roles$feature, intask$col_roles$target)

  newtask
}
