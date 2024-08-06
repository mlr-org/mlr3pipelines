#' @title Apply a Function to each Row of a Task
#'
#' @usage NULL
#' @name mlr_pipeops_rowapply
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Applies a function to each row of a task. Use the `affect_columns` parameter inherited from
#' [`PipeOpTaskPreprocSimple`] to limit the columns this function should be applied to.
#'
#' @section Construction:
#' ```
#' PipeOpColApply$new(id = "rowapply", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"rowapply"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreprocSimple`].
#'
#' The output is the input [`Task`][mlr3::Task] with the original affected columns replaced by the columns created by
#' applying `applicator` to each row.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreprocSimple`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreprocSimple`], as well as:
#' * `applicator` :: `function`\cr
#'   Function to apply to each row in the affected columns of the task.
#'   The return value should be a vector of the same length for every input.
#'   Initialized as [`identity()`][base::identity].
#' * `col_prefix` :: `character(1)`\cr
#'   If specified, prefix to be prepended to the column names of affected columns, separated by a dot (`.`). Initialized as `""`.
#'
#' @section Internals:
#' Calls [`apply`] on the data, using the value of `applicator` as `FUN`.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreprocSimple`]/[`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("iris")
#' pora = po("rowapply", applicator = scale)
#' pora$train(list(task))[[1]]  # rows are standardized
PipeOpRowApply = R6Class("PipeOpRowApply",
  inherit = PipeOpTaskPreprocSimple,
  public = list(
    initialize = function(id = "rowapply", param_vals = list()) {
      ps = ps(
        applicator = p_uty(custom_check = check_function, tags = c("train", "predict")),
        col_prefix = p_uty(custom_check = check_string, tags = c("train", "predict"))
      )
      ps$values = list(
        applicator = identity,
        col_prefix = ""
      )
      super$initialize(id, ps, param_vals = param_vals, feature_types = c("numeric", "integer"))
    }
  ),
  private = list(

    .get_state_dt = function(dt, levels, target) {
      list()
    },

    .transform_dt = function(dt, levels) {
      pv = self$param_set$values
      cnames = colnames(dt)

      # Handle data table with zero rows by adding filler content to emulate column creation later
      if (nrow(dt) == 0L) {
        dt = dt[NA_integer_]  # Adds empty row
        was_empty = TRUE
      } else {
        was_empty = FALSE
      }

      res = apply(dt, 1, pv$applicator)

      if (!length(res)) {
        return(matrix(numeric(0), nrow = nrow(dt)))
      }
      if (!(test_atomic_vector(res) || test_matrix(res))) {
        stop("Apply with FUN = applicator and simplified = TRUE should generate either atomic vector or matrix.")
      }
      # Convert result to a matrix for consistent column name handling
      if (test_atomic_vector(res)) {
        res = matrix(res, ncol = 1)
      } else {
        # Transpose the matrix for correct Task dimensions
        res = t(res)
      }
      # Assign column names if they are missing
      if (is.null(colnames(res))) {
        if (ncol(res) == ncol(dt)) {
          colnames(res) = cnames
        } else {
          colnames(res) = paste0("V", seq_len(ncol(res)))
        }
      }
      # Prepend column prefix if specified
      if (pv$col_prefix != "") {
        colnames(res) = paste(pv$col_prefix, colnames(res), sep = ".")
      }
      # Remove filler content if the original data.table had zero rows
      if (was_empty) {
        res = res[0L, ]
      }

      res
    }
  )
)

mlr_pipeops$add("rowapply", PipeOpRowApply)
