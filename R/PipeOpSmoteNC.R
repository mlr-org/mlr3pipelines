#' @title SMOTENC Balancing
#'
#' @usage NULL
#' @name mlr_pipeops_smotenc
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Generates a more balanced data set by creating synthetic instances of the minority class
#' for nominal and continuous data using the SMOTENC algorithm.
#'
#' The algorithm generates for each minority instance a new data point based on the `k` nearest
#' neighbors of that data point.
#' It treats integer features as numeric. To not change feature types, the numeric, synthetic data
#' generated for these features are rounded back to integer.
#' Because of this, data generated through usage of this [`PipeOp`] is not exactly equal to data generated by
#' calling [`themis::smotenc`] directly on the same data set.
#'
#' It can only be applied to [classification tasks][mlr3::TaskClassif] with factor (or ordered) features
#' and at least one numeric (or integer) feature that have no missing values.
#'
#' See [`themis::smotenc`] for details.
#'
#' @section Construction:
#' ```
#' PipeOpSmoteNC$new(id = "smotenc", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"smotenc"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`]. Instead of a [`Task`][mlr3::Task], a
#' [`TaskClassif`][mlr3::TaskClassif] is used as input and output during training and prediction.
#'
#' The output during training is the input [`Task`][mlr3::Task] with added synthetic rows for the minority class.
#' The output during prediction is the unchanged input.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `k` :: `integer(1)`\cr
#'   Number of nearest neighbors used for generating new values from the minority class. Default is `5`.
#' * `over_ratio` :: `numeric(1)`\cr
#'   Ratio of the majority to minority class. Default is `1`. For details, see [`themis::smotenc`].
#'
#' @section Internals:
#' If a target level is unobserved during training, no synthetic data points will be generated for that class.
#' No error is raised; the unobserved class is simply ignored.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @references
#' `r format_bib("chawla_2002")`
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examplesIf requireNamespace("themis")
#' library("mlr3")
#'
#' # Create example task
#' data = data.frame(
#'   target = factor(sample(c("c1", "c2"), size = 200, replace = TRUE, prob = c(0.1, 0.9))),
#'   feature = rnorm(200)
#' )
#' task = TaskClassif$new(id = "example", backend = data, target = "target")
#' task$head()
#' table(task$data(cols = "target"))
#'
#' # Generate synthetic data for minority class
#' pop = po("smotenc")
#' smotenc_result = pop$train(list(task))[[1]]$data()
#' nrow(smotenc_result)
#' table(smotenc_result$target)
PipeOpSmoteNC = R6Class("PipeOpSmoteNC",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "smotenc", param_vals = list()) {
      ps = ps(
        k = p_int(lower = 1, default = 5, tags = c("train", "smotenc")),
        over_ratio = p_dbl(lower = 0, default = 1, tags = c("train", "smotenc"))
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
      # SmoteNC cannot generate synthetic data for non-feature columns
      unsupported_cols = setdiff(unlist(task$col_roles), union(cols, task$target_names))
      if (length(unsupported_cols)) {
        stopf("SMOTENC cannot generate synthetic data for the following columns since they are neither features nor targets: %s.",
              str_collapse(unsupported_cols, quote = '"'))
      }
      # Only factor, ordered, numeric and integer features allowed
      if (!all(task$feature_types$type %in% c("numeric", "integer", "factor", "ordered"))) {
        stop("SmoteNC does only accept factor, ordered, numeric and integer features. Use PipeOpSelect to select the appropriate features.")
      }
      # At least one numeric or integer feature required
      if (!any(task$feature_types$type %in% c("numeric", "integer"))) {
        stop("SmoteNC requires at least one numeric or integer feature.")
      }

      # Remove unseen target levels, see #881
      # Save original levels to re-add them later
      levels = task$levels()
      task$droplevels(cols = task$target_names)

      # Calculate synthetic data
      snc = setDT(invoke(themis::smotenc, df = task$data(), var = task$target_names,
        .args = self$param_set$get_values(tags = "smotenc")))

      # Filter snc to only contain the generated synthetic data
      snc = utils::tail(snc, n = -task$nrow)

      # Convert originally integer columns back to integer as SMOTENC treats them as numeric
      int_cols = task$feature_names[task$feature_types$type == "integer"]
      snc[, (int_cols) := lapply(.SD, function(x) as.integer(round(x))), .SDcols = int_cols]

      # Re-add empty target levels
      task$set_levels(levels)

      task$rbind(snc)
    }
  )
)

mlr_pipeops$add("smotenc", PipeOpSmoteNC)
