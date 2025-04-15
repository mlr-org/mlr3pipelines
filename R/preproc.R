#' @title Simple Pre-processing
#'
#' @description
#' Function that offers a simple and direct way to train or predict [`PipeOp`]s and [`Graph`]s on [`Task`][mlr3::Task]s,
#' [`data.frame`]s or [`data.table`][data.table::data.table]s.
#'
#' Training happens if `predict` is set to `FALSE` and no `state` is passed to this function.
#' Prediction happens if `predict` is set to `TRUE` and if the passed `Graph` or `PipeOp` is either trained or a `state`
#' is explicitly passed to this function.
#'
#' The passed  `PipeOp` or `Graph` gets modified-by-reference.
#'
#' @param indata ([`Task`][mlr3::Task] | [`data.frame`] | [`data.table`][data.table::data.table] )\cr
#'   Data to be pre-processed.
#' @param processor ([`Graph`] | [`PipeOp`])\cr
#'   `Graph` or `PipeOp` which has exactly one input channel accepting a [`Task`][mlr3::Task] and one output channel.\cr
#'   If `indata` is a [`data.frame`] or [`data.table`][data.table::data.table], the output channel must give a `Task` to be converted back into a
#'   `data.frame` or `data.table`. Additionally, `processor`s which only work on sub-classes of
#'   [`TaskSupervised`][mlr3::TaskSupervised] will not be accepted, as it would be unclear which column was the `target`.\cr
#'   Be aware that the `processor` gets modified-by-reference during training or if a `state` is passed to
#'   this function. This especially means that the state of a trained `processor` will get overwritten.\cr
#'   You may want to use dictionary sugar functions to select a `processor` and to set its hyperparameters, e.g.
#'   [`po()`][po] or [`ppl()`][ppl].
#' @param state (named `list` | `NULL`)\cr
#'   Optional state to be used for prediction, if the `processor` is untrained or if the current `state` of the `processor`
#'   should be overwritten. Must be a complete and correct state for the respective `processor`.
#' @param predict (`logical(1)`)\cr
#'   Whether to predict (`TRUE`) or train (`FALSE`). By default, this is `FALSE` if `state` is `NULL` (`state`'s default),
#'   and `TRUE` otherwise.
#' @return (`any` | [`data.frame`] | [`data.table`][data.table::data.table])\cr
#' If `indata` is a `Task`, whatever is stored in the `processor`'s single output channel is returned.
#' If `indata` is a [`data.frame`] or [`data.table`][data.table::data.table], an object of the same class is returned, or
#' if the `processor`'s output channel does not return a `Task`, an error is thrown.
#'
#' @section Internals:
#' If `processor` is a [`PipeOp`], the S3 method `preproc.PipeOp` gets called first, converting the `PipeOp` into a
#' [`Graph`] and wrapping the `state` appropriately, before calling the S3 method `preproc.Graph` with the modified objects.
#'
#' If `indata` is a [`data.frame`] or [`data.table`][data.table::data.table], it is used to construct a
#' [`TaskUnsupervised`][mlr3::TaskUnsupervised] internally. This implies that `processor`s which only work on sub-classes
#' of [`TaskSupervised`][mlr3::TaskSupervised] will not work with these input types for `indata`.
#'
#' @export
#' @examples
#' task = tsk("iris")
#' pop = po("pca")
#'
#' # Training
#' preproc(task, pop)
#' # Note, that the PipeOp gets trained through this
#' pop$is_trained
#'
#' # Predicting a trained PipeOp (trained through previous call to preproc)
#' preproc(task, pop, predict = TRUE)
#'
#' # Predicting using a given state
#' # We use the state of the PipeOp from the last example and then reset it
#' state = pop$state
#' pop$state = NULL
#' preproc(task, pop, state)
#'
#' # Note that the PipeOp's state may get overwritten inadvertently during training or if a state is given
#' pop$state$sdev
#' preproc(tsk("wine"), pop)
#' pop$state$sdev
#'
#' # Piping multiple preproc() calls, using dictionary sugar to set parameters
#' outdata = preproc(tsk("penguins"), po("imputemode", affect_columns = selector_name("sex"))) |>
#'   preproc(po("imputemean"))
#' outdata$missings()
#'
#' # Use preproc with a Graph
#' gr = po("pca", rank. = 4) %>>% po("learner", learner = lrn("classif.rpart"))
#' preproc(tsk("sonar"), gr) # returns NULL because of the learner
#' preproc(tsk("sonar"), gr, predict = TRUE)
#'
#' # Training with a data.table input
#' # Note that this treats the target column as if it were a feature ...
#' dt = tsk("iris")$data()
#' preproc(dt, pop)
#'
#' # Predicting with a data.table input
#' preproc(dt, pop)
#'
preproc = function(indata, processor, state = NULL, predict = !is.null(state)) {
  assert(
    check_data_frame(indata, col.names = "unique"),
    check_class(indata, "Task")
  )
  assert_list(state, names = "unique", null.ok = TRUE)

  UseMethod("preproc", processor)
}

#' @export
preproc.PipeOp = function(indata, processor, state = NULL, predict = !is.null(state)) {
  assert_flag(predict)  # to force evaluation since S3 evaluates default arguments in every method call
  # Wrap PipeOp's state passed by user to look like a Graph's state
  state = named_list(processor$id, state)
  # Convert PipeOp into a Graph
  processor = as_graph(processor, clone = FALSE)
  # Call S3 method for Graph
  preproc(indata, processor, state, predict)
}

#' @export
preproc.Graph = function(indata, processor, state = NULL, predict = !every(state, is.null)) {
  assert_flag(predict)
  if (nrow(processor$output) != 1) {
    stop("'processor' must have exactly one output channel.")
  }
  # Note: We also expect the processor to only have a single input channel which only accepts Tasks. However, we put the
  #       burden of checking against this on Graph's check_types.

  # Construct a Task from data.frame indata
  if (is.data.frame(indata)) {
    task = TaskUnsupervised$new(id = "preproc_task", backend = indata)
  } else {
    task = indata
  }

  if (predict) {
    # If a state is passed, we overwrite graph's state by assignment, should it already be trained
    if (!every(state, is.null)) {  # state of untrained Graph is named list of NULLs
      processor$state = state
    }
    outtask = processor$predict(task)[[1L]]
  } else if (every(state, is.null)) {
    # If a trained graph is passed, we overwrite its state by re-training the graph.
    outtask = processor$train(task)[[1L]]
  } else {
    stop("Inconsistent function arguments. 'predict' is given as 'FALSE' while 'state' is given as not-NULL.")
  }

  if (is.data.frame(indata)) {
    if (!inherits(outtask, "Task")) {
      stop("Output channel of 'processor' does not contain a Task. For data.frame or data.table inputs to 'indata', training or predicting with 'processor' must return a Task or sub-class thereof.")
    }
    if (is.data.table(indata)) {
      outtask$data()
    } else {
      as.data.frame(outtask$data())
    }
  } else {
    outtask
  }
}
