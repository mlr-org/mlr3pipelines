#' @title PipeOpNewTarget
#'
#' @format [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Substitutes the current target with a new target.
#' The new target must already exist as a column in the [`Backend`].
#' The Task is also automatically converted to a Task of a different type, see
#' `new_task_type` below.
#' The previous target is set to row_role: `unused`.
#'
#' @section Public Members / Active Bindings:
#' * `new_target` :: `character` \cr
#' New target variable name for the task. The previous target variable is set to 'unused'.
#' * `new_task_type` :: `NULL` | `character` \cr
#' New task type for the resulting task. Defaults to 'classif' if `new_target`is `character` | `factor`,
#' else `regr`. Uses constructors from `mlr_reflections` to instantiate the new Task.
#'
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpNewTarget = R6Class("PipeOpNewTarget",

  inherit = PipeOp,

  public = list(
    initialize = function(id = "new_target", new_target = NULL, new_task_type = NULL) {
      super$initialize(id = id, param_set = ParamSet$new(), param_vals = list(),
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
      if (!is.null(new_target)) self$new_target = new_target
      if (!is.null(new_task_type)) self$new_task_type = new_task_type
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
  active = list(
    new_target = function(val) {
      if (!missing(val)) {
        assert_string(val)
        private$.new_target = val
      }
      private$.new_target
    },
    new_task_type = function(val) {
      if (!missing(val)) {
        assert_choice(val, mlr3::mlr_reflections$task_types$type)
        private$.new_task_type = val
      }
      private$.new_task_type
    }
  ),
  private = list(
    .new_target = NULL,
    .new_task_type = NULL,
    get_task_type = function(new_target_type) {
      # FIXME: This is currently not extensible. Would require changing mlr_reflections.
      ifelse(new_target_type %in% c("factor", "character"), "classif", "regr")
    },
    convert_task_type = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      assert_choice(private$.new_target, intask$col_info$id)
      if (is.null(private$.new_task_type)) {
        new_target_type = intask$col_info$type[intask$col_info$id == private$.new_target]
        private$.new_task_type = private$get_task_type(new_target_type)
      }
      if (private$.new_task_type == intask$task_type & intask$target_names == private$.new_target) return(list(intask))
      task = convert_task(intask, private$.new_task_type, private$.new_target)
      task$set_col_role(intask$col_roles$target, "unused")
      list(task)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("new_target", PipeOpNewTarget)


#' Convert a task from one type to another.
#' Requires for the task type to be in `mlr_reflections$task_types`.
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
  # Get task_type from mlr_reflections and call constructor.
  tsk = get(mlr_reflections$task_types[mlr_reflections$task_types$type == new_type,]$task)
  tsk$new(id = intask$id, backend = intask$backend, target = new_target)
}
