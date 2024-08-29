#' @title Nearmiss Down-Sampling
#'
#' @usage NULL
#' @name mlr_pipeops_nearmiss
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Generates a more balanced data set by ...
#'
#' The algorithm down-samples ...
#'
#' It can only be applied to tasks with numeric (or integer) features with no missing values.
#' The algorithm treats integer features as numeric features. To not change feature types, these are then rounded back to integer.
#'
#' See [`themis::nearmiss`] for details.
#'
#' @section Construction:
#' ```
#' PipeOpNearmiss$new(id = "nearmiss", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"nearmiss"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output during training is the input [`Task`][mlr3::Task] with
#' The output during prediction is the unchanged input.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as
#' * `k` :: `integer(1)`\cr
#'   Number of nearest neighbors used for generating new values from the minority class. Default is `5`.
#' * `under_ratio` :: `numeric(1)`\cr
#'   Ratio of the minority to majority class. Default is `1`. For details, see [`themis::nearmiss`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @references
#' `r format_bib("zhang2003")`
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' \dontshow{ if (requireNamespace("themis")) \{ }
#' library("mlr3")
#'
#' # Create example task
#' data = data.frame(
#'   target = factor(sample(c("c1", "c2"), size = 200, replace = TRUE, prob = c(0.1, 0.9))),
#'   feature = rnorm(200)
#' )
#' task = TaskClassif$new(id = "example", backend = data, target = "target")
#' task$head()
#' table(task$data()$target)
#'
#' # Generate synthetic data for minority class
#' pop = po("nearmiss")
#' nearmiss_result = pop$train(list(task))[[1]]$data()
#' table(nearmiss_result$target)
#' \dontshow{ \} }
PipeOpNearmiss = R6Class("PipeOpNearmiss",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "nearmiss", param_vals = list()) {
      ps = ps(
        k = p_int(lower = 1, default = 5, tags = c("train", "nearmiss")),
        over_ratio = p_dbl(lower = 0, default = 1, tags = c("train", "nearmiss"))
      )
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "themis", can_subset_cols = FALSE,
                       task_type = "TaskClassif", tags = "imbalanced data")
    }
  ),
  private = list(

    .train_task = function(task) {
      cols = task$feature_names

      # Return task unchanged, if no feature columns exist
      if (!length(cols)) {
        return(task)
      }
      # PipeOp does not know how to handle non-feature columns
      unsupported_cols = setdiff(unlist(task$col_roles), union(cols, task$target_names))
      if (length(unsupported_cols)) {
        stopf("Nearmiss cannot generate synthetic data for the following columns since they are neither features nor targets: '%s'",
              paste(unsupported_cols, collapse = "', '"))
      }
      # Only numeric and integer features allowed
      if (!all(task$feature_types$type %in% c("numeric", "integer"))) {
        stop("Nearmiss does only accept numeric and integer features. Use PipeOpSelect to select the appropriate features.")
      }

      # Down-sample Data
      dt = setDT(invoke(themis::tomek, df = task$data(), var = task$target_names))

      # Return task unchanged if no synthetic data was generated
      if (nrow(dt) == task$nrow) {
        return(task)
      }

      # Filter snc to only contain the generated synthetic data
      # dt <- dt[seq(task$nrow + 1L, nrow(snc))]
      # Might need a better solution here, since we are reducing the number of rows

      # Convert originally integer columns back to integer as SMOTENC treats them as numeric
      int_cols = task$feature_names[task$feature_types$type == "integer"]
      dt[, (int_cols) := lapply(.SD, function(x) as.integer(round(x))), .SDcols = int_cols]

      task$rbind(dt)
    }
  )
)

mlr_pipeops$add("nearmiss", PipeOpNearmiss)
