#' @title PipeOpNewTarget
#'
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Substitutes the current target with a new target.
#' Changes Task class if required.
#'
#'
#' @section Public Members / Active Bindings:
#' * `new_target` :: `character` \cr
#' New target variable for the task. The previous target variable is set to 'unused'.
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
    initialize = function(id = "new_target") {
      super$initialize(id = id, param_set = ParamSet$new(), param_vals = list(),
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
convert_task = function(intask, new_type, new_target = NULL) {
  assert_task(intask)
  assert_choice(new_target, intask$col_info$id, null.ok = TRUE)
  assert_choice(new_type, mlr_reflections$task_types$type)
  if (is.null(new_target)) new_target = intask$target_names
  if (new_type == intask$task_type & intask$target_names == new_target) return(intask)
  # Get task_type from mlr_reflections and call constructor.
  encapsulate("none",
    .f = eval(parse(text = paste0(mlr_reflections$task_types[type == new_type, task], "$new"))),
    list(id = intask$id, backend = intask$backend, target = new_target))$result
}
