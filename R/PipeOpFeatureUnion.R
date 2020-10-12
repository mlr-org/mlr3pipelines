#' @title PipeOpFeatureUnion
#'
#' @usage NULL
#' @name mlr_pipeops_featureunion
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Aggregates features from all input tasks by [cbind()]ing them together into a single
#' [`Task`][mlr3::Task].
#'
#' [`DataBackend`][mlr3::DataBackend] primary keys and [`Task`][mlr3::Task] targets have to be equal
#' across all [`Task`][mlr3::Task]s. Only the target column(s) of the first [`Task`][mlr3::Task]
#' are kept.
#'
#' If `assert_targets_equal` is `TRUE` then target column names are compared and an error is thrown
#' if they differ across inputs.
#'
#' If input tasks share some feature names but these features are not identical an error is thrown.
#' This check is performed by first comparing the features names and if duplicates are found, also
#' the values of these possibly duplicated features. True duplicated features are only added a
#' single time to the output task.
#'
#' @section Construction:
#' ```
#' PipeOpFeatureUnion$new(innum = 0, collect_multiplicity = FALSE, id = "featureunion", param_vals = list(),
#'   assert_targets_equal = TRUE)
#' ```
#'
#' * `innum` :: `numeric(1)` | `character`\cr
#'   Determines the number of input channels.
#'   If `innum` is 0 (default), a vararg input channel is created that can take an arbitrary number
#'   of inputs. If `innum` is a `character` vector, the number of input channels is the length of
#'   `innum`, and the columns of the result are prefixed with the values.
#' * `collect_multiplicity` :: `logical(1)`\cr
#'   If `TRUE`, the input is a [`Multiplicity`] collecting channel. This means, a
#'   [`Multiplicity`] input, instead of multiple normal inputs, is accepted and the members are aggregated. This requires `innum` to be 0.
#'   Default is `FALSE`.
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, default `"featureunion"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#' * `assert_targets_equal` :: `logical(1)`\cr
#'   If `assert_targets_equal` is `TRUE` (Default), task target column names are checked for
#'   agreement. Disagreeing target column names are usually a bug, so this should often be left at
#'   the default.
#'
#' @section Input and Output Channels:
#' [`PipeOpFeatureUnion`] has multiple input channels depending on the `innum` construction
#' argument, named `"input1"`, `"input2"`, ... if `innum` is nonzero; if `innum` is 0, there is
#' only one *vararg* input channel named `"..."`. All input channels take a [`Task`][mlr3::Task]
#' both during training and prediction.
#'
#' [`PipeOpFeatureUnion`] has one output channel named `"output"`, producing a [`Task`][mlr3::Task]
#' both during training and prediction.
#'
#' The output is a [`Task`][mlr3::Task] constructed by `cbind()`ing all features from all input
#' [`Task`][mlr3::Task]s, both during training and prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' [`PipeOpFeatureUnion`] has no Parameters.
#'
#' @section Internals:
#' [`PipeOpFeatureUnion`] uses the [`Task`][mlr3::Task] `$cbind()` method to bind the input values
#' beyond the first input to the first [`Task`][mlr3::Task]. This means if the [`Task`][mlr3::Task]s
#' are database-backed, all of them except the first will be fetched into R memory for this. This
#' behaviour may change in the future.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Multiplicity PipeOps
#' @include PipeOp.R
#' @export
#' @examples
#' library("mlr3")
#'
#' task1 = tsk("iris")
#' gr = gunion(list(
#'   po("nop"),
#'   po("pca")
#' )) %>>% po("featureunion")
#'
#' gr$train(task1)
#'
#' task2 = tsk("iris")
#' task3 = tsk("iris")
#' po = po("featureunion", innum = c("a", "b"))
#'
#' po$train(list(task2, task3))
PipeOpFeatureUnion = R6Class("PipeOpFeatureUnion",
  inherit = PipeOp,
  public = list(
    assert_targets_equal = NULL,
    inprefix = NULL,
    initialize = function(innum = 0L, collect_multiplicity = FALSE, id = "featureunion", param_vals = list(), assert_targets_equal = TRUE) {
      assert(
        check_int(innum, lower = 0L),
        check_character(innum, min.len = 1L, any.missing = FALSE)
      )
      if (is.numeric(innum)) {
        self$inprefix = rep("", innum)
      } else {
        self$inprefix = innum
        innum = length(innum)
      }
      assert_flag(assert_targets_equal)
      self$assert_targets_equal = assert_targets_equal
      inname = if (innum) rep_suffix("input", innum) else "..."
      intype = "Task"
      private$.collect = assert_flag(collect_multiplicity)
      if (collect_multiplicity) {
        if (innum) {
          stop("collect_multiplicity only works with innum == 0.")
        }
        inname = "[...]"
        intype = sprintf("[%s]", intype)
      }
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = inname, train = intype, predict = intype),
        output = data.table(name = "output", train = "Task", predict = "Task"),
        tags = "ensemble"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      if (private$.collect) inputs = unclass(inputs[[1]])
      list(cbind_tasks(inputs, self$assert_targets_equal, self$inprefix))
    },
    .predict = function(inputs) {
      if (private$.collect) inputs = unclass(inputs[[1]])
      list(cbind_tasks(inputs, self$assert_targets_equal, self$inprefix))
    },
    .collect = NULL
  )
)

mlr_pipeops$add("featureunion", PipeOpFeatureUnion)

cbind_tasks = function(inputs, assert_targets_equal, inprefix) {
  task = inputs[[1L]]
  ids = task$row_ids

  if (length(inprefix)) {  # inprefix has length 0 if innum is 0
    names(inputs) = inprefix
    if (inprefix[1L] != "") {
      task$rename(task$feature_names, sprintf("%s.%s", inprefix[1L], task$feature_names))
    }
  } else {
    names(inputs) = NULL
  }
  inputs = discard(inputs, is.null)

  targets = unique(unlist(map(inputs, .f = function(x) x$target_names), use.names = FALSE))
  if (assert_targets_equal && !setequal(targets, task$target_names)) {
    stopf("All tasks must have the same target columns.")
  }

  # check for duplicated feature names
  feature_names = if (length(inprefix)) {
    inprefix = ifelse(inprefix == "", yes = "", no = paste0(inprefix, "."))
    c(task$feature_names, unlist(pmap(list(as.list(inprefix[-1L]), tail(inputs, -1L)),
      .f = function(prefix, x) sprintf("%s%s", prefix, x$feature_names))))
  } else {
    unlist(map(inputs, function(x) x$feature_names))
  }
  duplicates = unique(feature_names[which(duplicated(feature_names))])
  # check whether the duplicated feature names are actually true duplicates (by value)
  if (length(duplicates)) {
    real_duplicates = logical(length(duplicates))
    for(i in seq_along(duplicates)) {
      # this is done by reference and should have good performance
      real_duplicates[i] = sum(duplicated(t(setDT(unlist(map(inputs,
        .f = function(x) {
          if (duplicates[i] %in% x$feature_names) {
            x$data(cols = duplicates[i])
          } else {
            NULL  # if the duplicated column is not present, explicitly return NULL
          }
        }), recursive = FALSE))))) > 0L
    }
    if (any(!real_duplicates)) {
      # FIXME: sprintf may not be able to handle large error messages here?
      stopf(sprintf("PipeOpFeatureUnion cannot aggregate different features sharing the same feature name. This applies to the following features: '%s'",
        paste0(duplicates[!real_duplicates], collapse = "', '")))
    }
  }

  # cbind() with only empty data.tables is problematic, so we have to do voodoo magic here:
  # cbind at least one data.table that is guaranteed not to be empty and subtract that column later
  # again done by reference
  new_features = unlist(c(list(data.table(x = vector(length = task$nrow))),
    map(tail(inputs, -1L), .f = function(y) y$data(ids, cols = y$feature_names))), recursive = FALSE)

  # we explicitly have to subset to the unique column names, otherwise task$cbind() complains for data.table backends
  new_features = new_features[unique(names(new_features))]

  task$clone(deep = TRUE)$cbind(setDT(new_features)[, -1L])
}
