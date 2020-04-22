#' @title PipeOpFilterRows
#'
#' @usage NULL
#' @name mlr_pipeops_filterrows
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`].
#'
#' @description
#' Filter rows of the data of a task. Also directly allows for the removal of rows holding missing
#' values. If both filtering and missing value removal is performed, filtering is done after missing
#' value removal.
#'
#' @section Construction:
#' ```
#' PipeOpFilterRows$new(id = "filterrows", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"filterrows"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output during training is the input [`Task`][mlr3::Task] with rows kept according to the
#' filtering (see Parameters) and (possible) rows with missing values removed.
#' 
#' The output during prediction is the unchanged input [`Task`][mlr3::Task] if the parameter
#' `skip_during_predict` is `TRUE`. Otherwise it is analogously handled as the output during
#' training.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`],
#' as well as the following elements:
#' * `na_ids` :: `integer`\cr
#'   The row identifiers that had missing values during training and therefore were removed. See the
#'   parameter `na_column`.
#' * `row_ids` :: `integer`\cr
#'   The row identifiers that were kept during training according to the parameters `filter`,
#'   `na_column` and `invert`.
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `filter` :: `NULL` | `character(1)` | `expression` | `integer`\cr
#'   How the rows of the data of the input [`Task`][mlr3::Task] should be filtered. This can be a
#'   character vector of length 1 indicating a feature column of logicals in the data of the input
#'   [`Task`][mlr3::Task] which forms the basis of the filtering, i.e., all rows that are `TRUE`
#'   with respect to this column are kept in the data of the output [`Task`][mlr3::Task]. Moreover,
#'   this can be an expression that will result in a logical vector of length `$nrow` of the data of
#'   the input [`Task`][mlr3::Task] when evaluated withing the environment of the `$data()` of the
#'   input [`Task`][mlr3::Task]. Finally, this can also be an integerish vector that directly
#'   specifies the row identifiers of the rows of the data of the input [`Task`][mlr3::Task] that
#'   should be kept. Default is `NULL`, i.e., no filtering is done.
#' * `na_column` :: `character`\cr
#'   A character vector that specifies the columns of the data of the input [`Task`][mlr3::Task]
#'   that should be checked for missing values. If set to `_all_`, all columns of the data are used. A
#'   row is removed if at least one missing value is found with respect to the columns specified.
#'   Default is `character(0)`, i.e., no removal of missing values is done.
#' * `invert` :: `logical(1)`\cr
#'   Should the filtering rule be set-theoretically inverted? Note that this happens after
#'   (possible) missing values were removed if `na_column` is specified. Default is `FALSE`.
#' * `skip_during_predict` :: `logical(1)`\cr
#'   Should the filtering and missing value removal steps be skipped during prediction? Default is
#'   `TRUE`, i.e., the input [`Task`][mlr3::Task] is returned unaltered during prediction.
#'
#' @section Internals:
#' Uses the [`is.na()`][base::is.na] function for the checking of missing values.
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#' task = tsk("pima")
#' po = PipeOpFilterRows$new(param_vals = list(
#'   filter = expression(age < median(age) & mass > 30),
#'   na_column = "_all_")
#' )
#' po$train(list(task))
#' po$state
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpFilterRows = R6Class("PipeOpFilterRows",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "filterrows", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamUty$new("filter", default = NULL, tags = c("train", "predict"), custom_check = function(x) {
          ok = test_character(x, any.missing = FALSE, len = 1L) ||
            is.expression(x) ||
            test_integerish(x, lower = 1, min.len = 1L) ||
            is.null(x)
          if (!ok) return("Must either be a character vector of length 1, an expression, or an integerish object of row ids")
          TRUE
        }),
        ParamUty$new("na_column", default = character(0L), tags = c("train", "predict"), custom_check = function(x) {
          check_character(x, any.missing = FALSE, null.ok = TRUE)
        }),
        ParamLgl$new("invert", default = FALSE, tags = c("train", "predict")),
        ParamLgl$new("skip_during_predict", default = TRUE, tags = "predict"))
      )
      ps$values = list(filter = NULL, na_column = character(0L), invert = FALSE, skip_during_predict = TRUE)
      super$initialize(id, param_set = ps, param_vals = param_vals)
    }
  ),
  private = list(
    .na_and_filter = function(task, skip, set_state) {
      if (skip) {
        return(task)  # early exit if skipped (if skip_during_predict)
      }

      row_ids = task$row_ids

      # NA column(s) handling
      na = self$param_set$values$na_column
      if (length(na)) {
        assert_subset(na, choices = c("_all_", colnames(task$data())))
        if (na == "_all_") na = colnames(task$data())
        na_ids = which(rowSums(is.na(task$data(cols = na))) > 0L)
        row_ids = setdiff(row_ids, na_ids)
      } else {
        na_ids = integer(0L)
      }

      # filtering
      filter = self$param_set$values$filter
      filter_ids =
      if (is.null(filter)) {
        row_ids
      } else if (is.character(filter)) {
        assert_subset(filter, choices = task$feature_names)
        filter_column = task$data(cols = filter)[[1L]]
        assert_logical(filter_column)
        which(filter_column)
      } else if(is.expression(filter)) {
        filter_expression = eval(filter, envir = task$data())
        assert_logical(filter_expression, len = task$nrow)
        which(filter_expression)
      } else {
        filter = as.integer(filter)
        assert_subset(filter, choices = task$row_ids)
        filter
      }

      row_ids = if (self$param_set$values$invert) {
        setdiff(row_ids, filter_ids)
      } else {
        intersect(row_ids, filter_ids)
      }

      # only set the state if required (during training)
      if (set_state) { 
        self$state$na_ids = na_ids
        self$state$row_ids = row_ids
      }

      task$filter(row_ids)
    },

    .train_task = function(task) {
      private$.na_and_filter(task, skip = FALSE, set_state = TRUE)
    },

    .predict_task = function(task) {
      private$.na_and_filter(task, skip = self$param_set$values$skip_during_predict, set_state = FALSE)
    }
  )
)

mlr_pipeops$add("filterrows", PipeOpFilterRows)
