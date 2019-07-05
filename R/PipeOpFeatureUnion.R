#' @title PipeOpFeatureUnion
#'
#' @name mlr_pipeop_featureunion
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#'   Aggregates features from all input tasks by cbinding them together
#'   into a [`data.table`].
#'   [`DataBackend`] primary keys and [`Task`] targets have to be equal across each
#'   `Task`. Only the target column(s) of the first task are kept.
#'   If `assert_targets_equal` is `TRUE`, then an error is thrown if target column name(s)
#'   disagree.
#'
#' @section Methods:
#' * `PipeOpFeatureUnion$new(innum, id = "featureunion", param_vals = list(), assert_targets_equal = TRUE)` \cr
#'   (`numeric(1)`, `character(1)`, named `list`, `logical(1)`) -> `self` \cr
#'   Constructor. `innum` determines the number of input channels. If `assert_targets_equal` is `TRUE` (Default),
#'   task target column names are checked for agreement. Disagreeing target column names are usually a
#'   bug, so this should often be left at the default.
#' @family PipeOps
#' @include PipeOp.R
#' @export
PipeOpFeatureUnion = R6Class("PipeOpFeatureUnion",
  inherit = PipeOp,
  public = list(
    assert_targets_equal = NULL,
    initialize = function(innum, id = "featureunion", param_vals = list(), assert_targets_equal = TRUE) {
      assert_int(innum, lower = 1)
      assert_flag(assert_targets_equal)
      self$assert_targets_equal = assert_targets_equal
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = rep_suffix("input", innum), train = "Task", predict = "Task"),
        output = data.table(name = "output", train = "Task", predict = "Task")
      )
    },

    train = function(inputs) {
      self$state = list()
      list(cbind_tasks(inputs, self$assert_targets_equal))
    },

    predict = function(inputs) {
      list(cbind_tasks(inputs, self$assert_targets_equal))
    }
  )
)

register_pipeop("featureunion", PipeOpFeatureUnion, list("N"))

cbind_tasks = function(inputs, assert_targets_equal) {
  task = inputs[[1L]]
  ids = task$row_ids
  inputs = discard(inputs, is.null)

  targets = unique(unlist(map(inputs, function(x) x$target_names), use.names = FALSE))
  if (assert_targets_equal && !setequal(targets, task$target_names)) {
    stopf("All tasks must have the same target columns")
  }

  if (is.null(names(inputs))) names(inputs) = paste0("input", seq_along(inputs))
  input_names = map(inputs, function(input) input$feature_names)
  if (any(duplicated(unlist(input_names)))) {
    # Treat duplicate names: Suffix with name of the input
    new_names = imap(input_names, function(input, name) {
      dupe = input %in% unlist(input_names[!(names(input_names) == name)])
      input[dupe] = paste(input[dupe], name, sep = "_")
      if (any(dupe)) return(input) else return(NULL)
    })
    new_names = discard(new_names, is.null)
    # Use replace_features on renamed data. This seems very inefficient
    inputs[names(new_names)] = imap(inputs[names(new_names)], function(input, name) {
      browser()
      dt = input$data(ids, input$feature_names)
      colnames(dt) = new_names[[name]]
      input$replace_features(dt)
      return(input)
    })
  }

  new_cols = Reduce(function(x, y) rcbind(x, y$data(ids, y$feature_names)), tail(inputs, -1L), init = data.table())
  task$clone(deep = TRUE)$cbind(new_cols)
}
