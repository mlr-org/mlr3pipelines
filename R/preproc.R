#' @title Simple Preprocessing
#'
#' @description
#'
#' calling train and predict with single_input = TRUE -> input task is copied and given to each unconnected input channel
#'
#'
#' @param indata (`Task` | `data.frame`-like object)\cr
#'   probably just write data.table. I think this is our convention.
#'
#'   If `indata` is a `data.frame`-like object, it is used to contruct a `TaskUnsupervised` internally.
#' @param graph `Graph` | `PipeOp`\cr
#'   a
#' @param state named `list` | `NULL`\cr
#'   a
#' @param predict `logical(1)`\cr
#'   a
#' @return description
#' only single output channel
#' - makes sense for piping and nature of this function to simplify things (i.e. should be used in simple context)
#' - we couldn't identify the proper output channel (e.g. targetmutate)
#'
#' @export
#' @examples
#' # example code
#'
preproc <- function(indata, graph, state = NULL, predict = !is.null(state)) {
  assert(
    # could also accept backends here? anything that is acceptable for task constructor would be OK
    # or we just later call task_constructor on anything that is not task, and let it throw relevant errors?
    check_data_frame(indata, col.names = "unique"),
    check_class(indata, "Task")  # effectively the only thing assert_task checks with default args
  )
  assert_list(state, names = "unique", null.ok = TRUE)  # from Graph
  assert_flag(predict)
  # don't need to assert on graph, since S3 handles it

  UseMethod("preproc", graph)
}

# is it acceptable to rename the second argument here? or do we just call it graph?
#
# using export instead of exportS3method since we mostly do this in mlr3 packages
#' @export
preproc.PipeOp <- function(indata, graph, state = NULL, predict = !is.null(state)) {
  if (nrow(graph$output) != 1) {
    stop("'graph' must have exactly one output channel.")
  }

  # Construct a Task from data.frame
  # TODO: Check what pos could potentially not handle targetless graphs
  #       can we test this here already?
  #       probably all POs that don't allow Task, but TaskClassif or TaskRegr specifically?
  if (is.data.frame(indata)) {
    task = TaskUnsupervised$new(id = "preproc_task", backend = indata)
  } else {
    task = indata
  }

  if (predict) {
    # if no state given and predict is true: predict might work
    # do we need any checks on state?
    graph$state = state
    outtask = graph$predict(list(task))[[1L]]
  } else if (is.null(state)) {
    outtask = graph$train(list(task))[[1L]]
  } else {
    stopf("Giving a state makes no sense if predict=FALSE ...")
  }

  if (is.data.frame(indata)) {
    outtask$data()
  } else {
    outtask
  }
}

# Should return a task (only allow pipelines that return a single task? but that is not the same as only one output channel, e.g. TargetMutate)
#' @export
preproc.Graph <- function(indata, graph, state = NULL, predict = !is.null(state)) {

  if (nrow(graph$output) != 1) {
    stop("'graph' must have exactly one output channel.")
  }

  # Construct a Task from data.frame
  # TODO: see preproc.PipeOp
  if (is.data.frame(indata)) {
    task = TaskUnsupervised$new(id = "preproc_task", backend = indata)
  }

}

# Examples
# preproc(tsk("iris"), graph = po("histbin")) # extracting infos from po/graph should work since we modify it by-reference
# preproc(tsk("iris"), graph = po("histbin"), predict = TRUE)
#
# preproc(tsk("iris"), graph = ppl("robustify"))
# preproc(tsk("iris"), graph = ppl("robustify"), predict = TRUE)
