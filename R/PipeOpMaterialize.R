#' @title Materialize Task View
#'
#' @usage NULL
#' @name mlr_pipeops_materialize
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`].
#'
#' @description
#' Materializes the active view of a [`Task`][mlr3::Task] by replacing its
#' [`DataBackend`][mlr3::DataBackend] with a new backend containing only the
#' rows and columns currently used by the task.
#'
#' This can be useful after operations that create virtual task views, such as
#' [`Task`][mlr3::Task] `$filter()`, `$select()`, or `$cbind()`. In particular, many
#' [`PipeOpTaskPreproc`] operations use [`Task`][mlr3::Task] `$cbind()` internally,
#' which can create nested virtual backends. Materializing the view can reduce backend
#' nesting and may free memory or speed up later data access.
#'
#' Note that [`Task`][mlr3::Task] `$materialize_view()` only materializes the currently
#' active view. Columns without any column role are dropped, and observations occuring more 
#' than once (duplicates in `$row_ids`), the resulting backend contains it only once, 
#' but the new task view will still contain it multiple times (duplicates in `$row_ids` are preserved).
#' 
#' @section Construction:
#' ```
#' PipeOpMaterialize$new(id = "materialize")
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object. See `$id` slot of [`PipeOp`].
#'
#' @section Input and Output Channels:
#' `PipeOpMaterialize` has one input channel named `"input"`, taking a [`Task`][mlr3::Task] both during training and prediction.
#'
#' `PipeOpMaterialize` has one output channel named `"output"`, producing a [`Task`][mlr3::Task] both during training and prediction.
#'
#' The output is the input [`Task`][mlr3::Task] with the active view materialized.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' `PipeOpMaterialize` has no parameters.
#'
#' @section Internals:
#' `PipeOpMaterialize` calls [`Task`][mlr3::Task] `$materialize_view()` on a clone of the input task,
#' both during training and prediction. During training, the internal validation task is also materialized 
#' using `$materialize_view(internal_valid_task = TRUE)`, but not during prediction.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' task$select("Petal.Length")$filter(1:10)
#' task$backend$colnames
#' task$backend$nrow
#' 
#' pom = PipeOpMaterialize$new("materialize")
#' materialized = pom$train(list(task))[[1]]
#' materialized$backend$colnames
#' materialized$backend$nrow
#'
#' @family mlr3pipelines backend related
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOp.R
#' @export
PipeOpMaterialize = R6Class("PipeOpMaterialize",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "materialize") {
      super$initialize(id = id, param_set = ps(), param_vals = list(),
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task"),
        packages = character(0), tags = "meta"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)
      
      # materialize_view() handles internal validation task
      intask$materialize_view(internal_valid_task = TRUE)

      # Set state to show that PipeOp is trained
      self$state = list()

      list(intask)
    },

    .predict = function(inputs) {
      intask = inputs[[1]]$clone(deep = TRUE)

      # No need to materialize internal validation task as they are not used in prediction path 
      intask$materialize_view(internal_valid_task = FALSE)

      list(intask)
    }
  )
)

mlr_pipeops$add("materialize", PipeOpMaterialize)
