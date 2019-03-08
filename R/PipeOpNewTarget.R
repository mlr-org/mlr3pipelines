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
    train = function(inputs) {
      outputs = private$convert_task(inputs)
      self$state = list()
      return(outputs)
    },
    predict = function(inputs) {
      private$convert_task(inputs)
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
        assert_choice(val, mlr3::mlr_reflections$task_types)
        private$.new_task_type = val
      }
      private$.new_task_type
    }
  ),
  private = list(
    .new_target = NULL,
    .new_task_type = NULL,
    convert_task = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      assert_choice(private$.new_target, intask$col_info$id)
      if (is.null(private$.new_task_type)) {
        new_target_type = intask$col_info$type[intask$col_info$id == private$.new_target]
        private$.new_task_type = ifelse(new_target_type %in% c("factor", "character"), "classif", "regr")
      }
      assert_choice(private$.new_task_type, mlr3::mlr_reflections$task_types)
      intask$set_col_role(intask$col_roles$target, "unused")
      intask$set_col_role(self$new_target, "target")

      if (private$.new_task_type != intask$task_type) {
        if (private$.new_task_type == "regr") {
          intask = TaskRegr$new(intask$id, backend = intask$backend, target = private$.new_target)
        } else {
          intask = TaskClassif$new(intask$id, backend = intask$backend, target = private$.new_target)
        }
      }
      list(intask)
    }
  )
)

#' @include mlr_pipeops.R
mlr_pipeops$add("new_target", PipeOpNewTarget)
