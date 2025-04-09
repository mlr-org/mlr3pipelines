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
#'   modified-by-reference
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
    check_data_frame(indata, col.names = "unique"),
    check_class(indata, "Task")  # effectively the only thing assert_task checks with default args
  )
  assert_list(state, names = "unique", null.ok = TRUE)  # from Graph
  assert_flag(predict)

  graph = as_graph(graph, clone = FALSE)

  if (nrow(graph$output) != 1) {
    stop("'graph' must have exactly one output channel.")
  }
  # Check: graph accepts task_type in input (if not a task but a df, would need to add class of supervised; or do it later; potentially more expensive)

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
    outtask = graph$predict(task)[[1L]]
  } else if (is.null(state)) {
    outtask = graph$train(task)[[1L]]
  } else {
    stopf("Giving a state makes no sense if predict=FALSE ...")
  }

  if (is.data.frame(indata)) {
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
