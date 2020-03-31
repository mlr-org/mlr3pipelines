#' @title PipeOpInvertiblePreproc
#'
#' @usage NULL
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' #FIXME:
#'
#' @section Construction:
#' #FIXME:
#' ```
#' PipeOpInvertiblePreproc$new(id)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. See `$id` slot of [`PipeOp`].
#'
#' @section Input and Output Channels:
#' #FIXME:
#'
#' @section State:
#' #FIXME:
#'
#' @section Internals:
#' #FIXME:
#'
#' @section Methods:
#' Methods inherited from [`PipeOp`], as well as:
#' #FIXME:
#'
#' @family mlr3pipelines backend related
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpInvertiblePreproc = R6Class("PipeOpInvertiblePreproc",
  inherit = PipeOp,
  public = list(
    initialize = function(id) {
      super$initialize(id = id,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = c("fun", "output"), train = c("NULL", "Task"), predict = c("function", "Task"))
      )
    },

    train_internal = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      intask = self$train_target(intask)
      list(NULL, intask)
    },

    predict_internal = function(inputs) {
      predict_phase_control = self$train_invert(inputs[[1L]])
      list(
        fun = private$invert_help(predict_phase_control),
        task = private$set_target_to_NA_and_possibly_convert_task_type(inputs[[1L]])
      )
    },

    train_target = function(task) {
      # return a modified task and set the state
      # make use of private$update_target
      # therefore, typically calculate new_target (name it appropriately), do task$cbind(new_target)
      # and provide names(new_target) as a character to update_target, e.g.:
      #
      # self$state = list()
      # new_target = foo
      # names(new_target) = "foo"
      # task$cbind(new_target)
      # private$update_target(task, new_target = names(new_target), new_type = NULL, ...)
      stop("Abstract.")
    },

    train_invert = function(task) {
      # return a predict_phase_control object (can be anything)
      stop("Abstract.")
    },

    inverter = function(prediction, predict_phase_control) {
      # function that inverts the predictions and returns a Prediction object
      stop("Abstract.")
    }
  ),
  private = list(
    update_target = function(task, new_target, new_type = NULL, ...) {
      type = if (is.null(new_type)) task$task_type else assert_subset(new_type, mlr_reflections$task_types$type, empty.ok = FALSE)
      get(mlr_reflections$task_types[type]$task)$new(id = task$id, backend = task$backend, target = new_target, ...)
    },

    invert_help = function(predict_phase_control) {
      function(inputs) {
        assert_list(inputs, len = 1L, types = "Prediction")
        self$inverter(inputs[[1L]], predict_phase_control)
      }
    },

    set_target_to_NA_and_possibly_convert_task_type = function(task) {
      #FIXME: convert the task target to NA and convert the task type?
      # or just call train_target again?
      intask = task$clone(deep = TRUE)
      self$train_target(intask)
    }
  )
)

#' @title PipeOpInverter
#'
#' @usage NULL
#' @name mlr_pipeops_inverter
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' #FIXME:
#'
#' @section Construction:
#' #FIXME:
#' ```
#' PipeOpInverter$new(id)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. See `$id` slot of [`PipeOp`].
#'
#' @section Input and Output Channels:
#' #FIXME:
#'
#' @section State:
#' #FIXME:
#'
#' @section Internals:
#' #FIXME:
#'
#' @section Methods:
#' Methods inherited from [`PipeOp`], as well as:
#' #FIXME:
#'
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpInverter = R6Class("PipeOpInverter",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "inverter") {
      super$initialize(id = id,
        input = data.table(name = c("fun", "prediction"), train = c("NULL", "NULL"), predict = c("function", "Prediction")),
        output = data.table(name = "output", train = "NULL", predict = "Prediction")
      )
    },

    train_internal = function(inputs) {
      self$state = list()
      list(NULL)
    },

    predict_internal = function(inputs) {
      list(inputs[[1L]](inputs[-1L]))
    }
  )
)

mlr_pipeops$add("inverter", PipeOpInverter)

#' @title PipeOpLogtrafo
#'
#' @usage NULL
#' @name mlr_pipeops_logtrafo
#' @format [`R6Class`] object inheriting from [`PipeOpInvertiblePreproc`]/[`PipeOp`]
#'
#' @description
#' #FIXME: Just some toy example for regression tasks. Needs more checks etc.
#'
#' @section Construction:
#' #FIXME:
#' ```
#' PipeOpLogtrafo$new(id)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. See `$id` slot of [`PipeOp`].
#'
#' @section Input and Output Channels:
#' #FIXME:
#'
#' @section State:
#' #FIXME:
#'
#' @section Internals:
#' #FIXME:
#'
#' @section Methods:
#' Methods inherited from [`PipeOpInvertiblePreproc`]/[`PipeOp`], as well as:
#' #FIXME:
#'
#' @examples
#'library(mlr3)
#'task = tsk("boston_housing")
#'po1 = PipeOpLogtrafo$new()
#'po1$train(list(task))
#'po1$predict(list(task))
#'
#'g = Graph$new()
#'g$add_pipeop(PipeOpLogtrafo$new())
#'g$add_pipeop(LearnerRegrRpart$new())
#'g$add_pipeop(PipeOpInverter$new())
#'g$add_edge(src_id = "logtrafo", dst_id = "inverter", src_channel = 1, dst_channel = 1)
#'g$add_edge(src_id = "logtrafo", dst_id = "regr.rpart", src_channel = 2, dst_channel = 1)
#'g$add_edge(src_id = "regr.rpart", dst_id = "inverter", src_channel = 1, dst_channel = 2)
#'
#'g$train(task)
#'g$predict(task)
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpLogtrafo = R6Class("PipeOpLogtrafo",
  inherit = PipeOpInvertiblePreproc,
  public = list(
    initialize = function(id = "logtrafo") {
      super$initialize(id = id)
    },

    train_target = function(task) {
      self$state = list()
      new_target = log(task$data(cols = task$target_names))
      names(new_target) = paste0("log.", task$target_names)
      task$cbind(new_target)
      private$update_target(task, new_target = names(new_target), new_type = NULL)
    },

    train_invert = function(task) {
      exp
    },

    inverter = function(prediction, predict_phase_control) {
      type = prediction$task_type
      get(mlr_reflections$task_types[type]$prediction)$new(row_ids = prediction$row_ids,
        truth = predict_phase_control(prediction$truth), response = predict_phase_control(prediction$response))
    }
  )
)

mlr_pipeops$add("logtrafo", PipeOpLogtrafo)
