#' @title SMOTE Balancing
#'
#' @usage NULL
#' @name mlr_pipeops_smote
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Generates a more balanced data set by creating
#' synthetic instances of the minority class using the SMOTE algorithm.
#' The algorithm samples for each minority instance a new data point based on the `K` nearest
#' neighbors of that data point.
#' It can only be applied to tasks with purely numeric features.
#' See [`smotefamily::SMOTE`] for details.
#'
#' @section Construction:
#' ```
#' PipeOpSmote$new(id = "smote", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"smote"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output during training is the input [`Task`][mlr3::Task] with added synthetic rows for the minority class.
#' The output during prediction is the unchanged input.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as:
#' * `K` :: `numeric(1)` \cr
#'   The number of nearest neighbors used for sampling new values.
#'   See [`SMOTE()`][`smotefamily::SMOTE`].
#' * `dup_size` :: `numeric` \cr
#'   Desired times of synthetic minority instances over the original number of
#'   majority instances. See [`SMOTE()`][`smotefamily::SMOTE`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
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
#' @examplesIf requireNamespace("smotefamily")
#' library("mlr3")
#'
#' # Create example task
#' data = smotefamily::sample_generator(1000, ratio = 0.80)
#' data$result = factor(data$result)
#' task = TaskClassif$new(id = "example", backend = data, target = "result")
#' task$data()
#' table(task$data()$result)
#'
#' # Generate synthetic data for minority class
#' pop = po("smote")
#' smotedata = pop$train(list(task))[[1]]$data()
#' table(smotedata$result)
PipeOpSmote = R6Class("PipeOpSmote",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "smote", param_vals = list()) {
      ps = ps(
        K = p_int(lower = 1, default = 5, tags = c("train", "smote")),
        # dup_size = 0 leads to behaviour different from 1, 2, 3, ..., because it means "autodetect",
        # so it is a 'special_vals'.
        dup_size = p_int(lower = 1, default = 0, special_vals = list(0), tags = c("train", "smote"))
      )
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE,
        packages = "smotefamily", task_type = "TaskClassif", tags = "imbalanced data")
    }
  ),
  private = list(

    .train_task = function(task) {
      cols = task$feature_names
      unsupported_cols = setdiff(unlist(task$col_roles), union(cols, task$target_names))
      if (length(unsupported_cols)) {
        stopf("SMOTE cannot generate synthetic data for the following columns since they are neither features nor targets: %s.",
              str_collapse(unsupported_cols, quote = '"'))
      }
      if (!length(cols)) {
        return(task)
      }
      if (!all(task$feature_types$type %in% c("numeric"))) {
        stop("Smote does only accept numeric features. Use PipeOpSelect to select the appropriate features.")
      }

      # Calculate synthetic data
      dt = task$data(cols = cols)
      # Remove unseen factor levels, see #881
      # Don't need to re-add them later since we don't touch task here
      target = droplevels(task$truth())

      st = setDT(invoke(smotefamily::SMOTE, X = dt, target = target,
        .args = self$param_set$get_values(tags = "smote"),
        .opts = list(warnPartialMatchArgs = FALSE))$syn_data)

      # Rename target column and fix character conversion
      # We index by position (target should be last column) instead of indexing by name, which would lead to problems if a feature were called "class"
      st[[ncol(st)]] = as_factor(st[[ncol(st)]], levels = task$class_names)
      setnames(st, ncol(st), task$target_names)

      task$rbind(st)
    }
  )
)

mlr_pipeops$add("smote", PipeOpSmote)
