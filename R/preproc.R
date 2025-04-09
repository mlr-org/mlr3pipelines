#' @title Simple Preprocessing
#'
#' @description
#'a
#'
#' @param indata ([`Task`][mlr3::Task] | [`data.table`][data.table::data.table] )\cr
#'   Data to be pre-processed. If `indata` is a `data.table`, it is used to
#'   construct a [`TaskUnsupervised`][mlr3::TaskUnsupervised] internally, meaning
#'   that it would not work with [`Graph`]s or [`PipeOp`]s that use the columns
#'   of role `target`.
#' @param graph ([`Graph`] | [`PipeOp`])\cr
#'   modified-by-reference
#'   could also be trained for predict (if state is given, original state gets overwirrten)
#'   if trained and train is run, state gets overwritten
#' @param state (named `list` | `NULL`)\cr
#'   a
#' @param predict (`logical(1)`)\cr
#'   a
#' @return
#' only single output channel
#' - makes sense for piping and nature of this function to simplify things (i.e. should be used in simple context)
#' - we couldn't identify the proper output channel (e.g. targetmutate)
#'
#' @section Internals
#'
#'
#' @export
#' @examples
#' # example code
#'
preproc <- function(indata, graph, state = NULL, predict = !is.null(state)) {
  assert(
    check_data_frame(indata, col.names = "unique"),
    check_class(indata, "Task")  # effectively the only thing assert_task checks with default args
  )
  assert_list(state, names = "unique", null.ok = TRUE)  # from Graph
  assert_flag(predict)
  assert(
    check_class(graph, "PipeOp"),
    check_class(graph, "Graph")
  )
  graph = as_graph(graph, clone = FALSE)

  if (nrow(graph$output) != 1) {
    stop("'graph' must have exactly one output channel.")
  }
  # Check: Graph must return a task (see discussion below @return)

  # Construct a Task from data.frame
  # TODO: Check what pos could potentially not handle targetless graphs
  #       can we test this here already?
  #       probably all POs that don't allow Task, but TaskClassif or TaskRegr specifically? -> also learner
  #       $task_type only exists for POTaskPreproc, or POImpute
  #       graph does not have this ...
  # Would be good to have a tag / property for PipeOp whether they work without targets
  if (is.data.frame(indata)) {
    task = TaskUnsupervised$new(id = "preproc_task", backend = indata)
  } else {
    task = indata
  }
  # Check: graph accepts task$task_type in graph$input (if not a task but a df, would need to add class of supervised; or do it later; potentially more expensive)
  # maybe not necessary since graph checks this itself

  if (predict) {
    # Do we need any checks on state? Graph does most checks for us
    # If a state is passed, we overwrite graph's state by assignment, should it already be trained
    if (!is.null(state)) {
      if (graph$is_trained) warning("'graph' is trained, but 'state' is explicitly given to preproc. Using passed 'state' for Prediction.")
      graph$state = state
    }
    outtask = graph$predict(task)[[1L]]
  } else if (is.null(state)) {
    # If a trained graph is passed, we overwrite its state by re-training the graph.
    if (graph$is_trained) warning("'graph' is trained, but preproc re-trains the Graph, overwriting its original state.")
    outtask = graph$train(task)[[1L]]
  } else {
    stop("Inconsistent function arguments. 'predict' is given as 'FALSE' while 'state' is given as not-NULL.")
  }

  # What if Graph has a PipeOpLearner in it? Return Prediction?
  # Or do we want to force a return type Task above?
  if (is.data.frame(indata)) {
    # Especially for df-input, PipeOpLearner in graph not possible
    outtask$data()
  } else {
    outtask
  }
}

# Examples
# preproc(tsk("iris"), graph = po("histbin")) # extracting infos from po/graph should work since we modify it by-reference
# preproc(tsk("iris"), graph = po("histbin"), predict = TRUE)
#
# preproc(tsk("iris"), graph = ppl("robustify"))
# preproc(tsk("iris"), graph = ppl("robustify"), predict = TRUE)
#
filter = flt("mim")
op = po("filter", filter, filter.nfeat = 2)
preproc(tsk("iris"), op)
preproc(tsk("iris"), flt("auc"))

gr = po("smote") %>>% po("pca")
preproc(tsk("iris"), gr)

learner = lrn("regr.rpart")
preproc(tsk("mtcars"), learner)
# works but only returns NULL
preproc(tsk("mtcars"), learner, state = state)
# learner does not get trained here

df = tsk("iris")$data(cols = task$feature_names)
op = po("pca")
preproc(df, op)
state = list(pca = op$state)
preproc(df, op, state)
