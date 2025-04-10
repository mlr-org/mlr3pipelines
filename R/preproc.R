#' @title Simple Pre-processing
#'
#' @description
#' Function that offers a simple and direct way to train or predict [`PipeOp`]s and [`Graph`]s.
#'
#' Training happens if `predict` is set to `FALSE` and if the passed `Graph` or `PipeOp` is untrained, i.e. its state
#' is `NULL`.
#' Prediction happens if `predict` is set to `TRUE` and if the passed `Graph` or `PipeOp` is either trained or a `state`
#' is explicitly passed to this function.
#'
#' Note, that the passed `Graph` or `PipeOp` is modified-by-reference during training or if a `state` is passed to this
#' function.
#'
#' add different overwritings here or in params
#'
#' @param indata ([`Task`][mlr3::Task] | [`data.table`][data.table::data.table] )\cr
#'   Data to be pre-processed. If `indata` is a `data.table`, it is used to construct a
#'   [`TaskUnsupervised`][mlr3::TaskUnsupervised] internally, meaning that training or prediction with [`Graph`]s or
#'   [`PipeOp`]s that use the columns of role `target` will not work.
#' @param processor ([`Graph`] | [`PipeOp`])\cr
#'   `Graph` or `PipeOp` to be converted into a `Graph` internally. This gets modified-by-reference during training
#'   or if `state` is given as non-`NULL`.
#'   The `processor` must have exactly one output channel to allow piping of `preproc` calls.
#' @param state (named `list` | `NULL`)\cr
#'   Optional state to be used for prediction. Must be a complete and correct state for the respective processor, i.e.
#'   for a `Graph` a list named according to the `Graph`'s `PipeOp`s containing the `PipeOp`'s states.
#'   Note, that passing a non-`NULL` `state` modifies the `processor` by-reference.
#' @param predict (`logical(1)`)\cr
#'   Whether to predict (`TRUE`) or train (`FALSE`). By default, this is `FALSE` if `state` is `NULL` (`state`'s default),
#'   and `TRUE` otherwise.
#' @return any
#'
#' only single output channel
#' any type
#' if df input, then output open as well, or do we want to return a df for piping and consistency?
#'
#' @section Internals:
#' If `processor` is a [`PipeOp`], the S3 method `preproc.PipeOp` gets called first, converting the `PipeOp` into a
#' [`Graph`] and wrapping the `state` appropriately, before calling the S3 method `preproc.Graph` with the modified objects.
#'
#' @export
#' @examples
#' # example code
#'
preproc = function(indata, processor, state = NULL, predict = !is.null(state)) {
  assert(
    check_data_frame(indata, col.names = "unique"),
    check_class(indata, "Task")  # effectively the only thing assert_task checks with default args
  )
  assert_list(state, names = "unique", null.ok = TRUE)  # from Graph
  assert_flag(predict)

  UseMethod("preproc", processor)
}

#' @export
preproc.PipeOp = function(indata, processor, state = NULL, predict = !is.null(state)) {
  # Wrap PipeOp's state passed by user to look like a Graph's state
  if (!is.null(state)) state = named_list(processor$id, state)
  # Convert PipeOp into a Graph
  processor = as_graph(processor, clone = FALSE)
  # Call S3 method for Graph
  preproc(indata, processor, state, predict)
}

#' @export
preproc.Graph = function(indata, processor, state = NULL, predict = !is.null(state)) {
  if (nrow(processor$output) != 1) {
    stop("'processor' must have exactly one output channel.")
  }

  # Construct a Task from data.frame
  # TODO: Check what pos could potentially not handle targetless graphs
  #       probably all POs that don't allow Task, but TaskClassif or TaskRegr specifically, including POLearner
  #       $task_type only exists for POTaskPreproc, or POImpute, graph does not have this ...
  # Would be good to have a tag / property for PipeOp whether they work without targets
  if (is.data.frame(indata)) {
    task = TaskUnsupervised$new(id = "preproc_task", backend = indata)
  }

  if (predict) {
    # If a state is passed, we overwrite graph's state by assignment, should it already be trained
    if (!is.null(state)) {
      if (processor$is_trained) warning("'processor' is trained, but 'state' is explicitly given. Using passed 'state' for prediction.")
      processor$state = state
    }
    outtask = processor$predict(indata)[[1L]]
  } else if (is.null(state)) {
    # If a trained graph is passed, we overwrite its state by re-training the graph.
    if (processor$is_trained) warning("'processor' is trained, but preproc re-trains it, overwriting its original state.")
    outtask = processor$train(indata)[[1L]]
  } else {
    stop("Inconsistent function arguments. 'predict' is given as 'FALSE' while 'state' is given as not-NULL.")
  }

  # Conflict: Want to simplify preprocessing, i.e. handle possible extractions for user, but still be open to
  #           processors that return non-Task objects
  if (is.data.frame(indata)) {
    outtask$data()
  } else {
    outtask
  }
}
