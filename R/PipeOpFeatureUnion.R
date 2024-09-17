#' @title Aggregate Features from Multiple Inputs
#'
#' @usage NULL
#' @name mlr_pipeops_featureunion
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOp`].
#'
#' @description
#' Aggregates features from all input tasks by [cbind()]ing them together into a single
#' [`Task`][mlr3::Task].
#'
#' [`DataBackend`][mlr3::DataBackend] primary keys and [`Task`][mlr3::Task] targets have to be equal
#' across all [`Task`][mlr3::Task]s. Only the target column(s) of the first [`Task`][mlr3::Task]
#' are kept.
#'
#' `PipeOpFeatureUnion` tries to merge columns that are identical, while preventing accidental
#' overwrites of columns that contain differing data. This is controlled using the `feature_clash`
#' (for columns containing features, weights etc.) and `target_clash` (for tharget columns)
#' hyperparameters. The `assert_target_equal` construction parameter / field can still be used
#' as well but is deprecated and will generate a warning.
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
#'   DEPRECATED; use `target_clash` hyperparameter instead.\cr
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
#' * `target_clash` :: `character(1)`\cr
#'   How to handle target columns that differ between input [`Task`][mlr3::Task]s. `"allow_same_hash"`
#'   checks the names and `$col_hashes` and throws an error if they disagree. `"allow_same_content"` (default) is
#'   more permissive: If `$col_hashes` disagree, then it checks the target content, if the content of both
#'   columns agree, then merging of tasks is still allowed. This avoids some rare false-positives, but in cases
#'   where hashes *do* disagree this may be slow for [`Task`][mlr3::Task]s with many rows or targets.
#'   `"ignore"` does not check for target agreement and overwrites the target with the target of the *rightmost* /
#'   highest numbered input [`Task`][mlr3::Task]. Use with caution. This is the only option that allows feature-union of [`Task`][mlr3::Task]s
#'   that differ in the names of their target column (and all target columns except the rightmost / highest numbered input
#'   [`Task`][mlr3::Task]'s target are dropped in that case).\cr
#'   The deprecated field `assert_targets_equal` sets this value to `"allow_same_content"` (i.e. default) when `TRUE` and to
#'   `"ignore"` when `FALSE`.
#' * `feature_clash` :: `character(1)`\cr
#'   How to handle non-target columns that have the same name but differ between input [`Task`][mlr3::Task]s. `"allow_same_hash"`
#'   checks the names and `$col_hashes` and throws an error if they disagree. `"allow_same_content"` (default) is
#'   more permissive: If `$col_hashes` disagree, then it checks the column content, if the content of both
#'   columns agree, then merging of tasks is still allowed. This avoids some rare false-positives, but in cases
#'   where hashes *do* disagree this may be slow for large [`Task`][mlr3::Task]s.
#'   `"ignore"` does not check for column data agreement and overwrites columns of the same name with the values of the *rightmost* /
#'   highest numbered input [`Task`][mlr3::Task].\cr
#'   Some column roles (`"group"`, `"weight"`, `"name"`) do not allow more than one column role present in a [`Task`][mlr3::Task] (see
#'   `$col_roles` documentation there). When up to one [`Task`][mlr3::Task] has a column of these column role, it is taken for the
#'   resulting [`Task`][mlr3::Task] without any issue. When more than one [`Task`][mlr3::Task] has a column with one of these roles,
#'   but with the same name, the `feature_clash` policy applies as described above. When more than one [`Task`][mlr3::Task] has a
#'   column with one of these roles, but they have *different* names, then an error is thrown when `feature_clash` is not `"ignore"`.
#'   When it is `"ignore"`, the *rightmost* / highest numbered input [`Task`][mlr3::Task]'s column is used and all others of this
#'   role are discarded.
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
#' @template seealso_pipeopslist
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

    inprefix = NULL,
    initialize = function(innum = 0L, collect_multiplicity = FALSE, id = "featureunion", param_vals = list(), assert_targets_equal = TRUE) {
      assert(
        check_int(innum, lower = 0L),
        check_character(innum, min.len = 1L, any.missing = FALSE)
      )
      params = ps(
        target_clash = p_fct(c("allow_same_hash", "allow_same_content", "ignore")),
        feature_clash = p_fct(c("forbid", "allow_same_hash", "allow_same_content", "ignore"))
      )
      params$values = list(target_clash = "allow_same_content", feature_clash = "allow_same_content")

      if (is.numeric(innum)) {
        self$inprefix = rep("", innum)
      } else {
        self$inprefix = innum
        innum = length(innum)
      }

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

      # the following is DEPRECATED
      if (!missing(assert_targets_equal)) {
        # do this after init so the AB can modify self$param_set
        assert_flag(assert_targets_equal)
        self$assert_targets_equal = assert_targets_equal
      }
    }
  ),
  active = list(
    assert_targets_equal = function(rhs) {
      if (!missing(rhs)) private$.assert_targets_equal = rhs
      self$param_set$values$target_clash = if (private$.assert_targets_equal) "allow_same_content" else "ignore"
      warning("PipeOpFeatureUnion assert_targets_equal is deprecated. Use the 'target_clash' hyperparameter.")
      private$.assert_targets_equal
    }
  ),
  private = list(
    .assert_targets_equal = NULL,
    .train = function(inputs) {
      self$state = list()
      if (private$.collect) inputs = unclass(inputs[[1]])
      list(cbind_tasks(inputs, self$assert_targets_equal, self$inprefix))
    },
    .predict = function(inputs) {
      if (private$.collect) inputs = unclass(inputs[[1]])
      list(cbind_tasks(inputs, self$assert_targets_equal, self$inprefix))
    },
    .collect = NULL,
    .additional_phash_input = function() list(private$.collect, self$input$name)
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
  names(new_features)[1] = make.unique(rev(names(new_features)))[[length(new_features)]]

  # we explicitly have to subset to the unique column names, otherwise task$cbind() complains for data.table backends
  new_features = new_features[unique(names(new_features))]

  task$clone(deep = TRUE)$cbind(setDT(new_features)[, -1L])
}
