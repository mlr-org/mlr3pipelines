#' @title BLSMOTE Balancing
#'
#' @usage NULL
#' @name mlr_pipeops_blsmote
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Adds new data points by generating synthetic instances for the minority class using the Borderline-SMOTE algorithm.
#' This can only be applied to [classification tasks][mlr3::TaskClassif] with numeric features that have no missing values.
#' See [`smotefamily::BLSMOTE`] for details.
#'
#' @section Construction:
#' ```
#' PipeOpBLSmote$new(id = "blsmote", param_vals = list())
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
#'   The number of nearest neighbors used for sampling from the minority class. Default is `5`.
#'   See [`BLSMOTE()`][`smotefamily::BLSMOTE`].
#' * `C` :: `numeric(1)` \cr
#'   The number of nearest neighbors used for classifying sample points as SAFE/DANGER/NOISE. Default is `5`.
#'   See [`BLSMOTE()`][`smotefamily::BLSMOTE`].
#' * `dup_size` :: `numeric` \cr
#'   Desired times of synthetic minority instances over the original number of majority instances. `0` leads to balancing minority and majority class.
#'   Default is `0`. See [`BLSMOTE()`][`smotefamily::BLSMOTE`].
#' * `method` :: `character(1)` \cr
#'   The type of Borderline-SMOTE algorithm to use. Default is `"type1"`.
#'   See [`BLSMOTE()`][`smotefamily::BLSMOTE`].
#' * `quiet` :: `logical(1)` \cr
#'   Whether to suppress printing status during training. Initialized to `TRUE`.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @references
#' `r format_bib("han_2005")`
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examplesIf requireNamespace("smotefamily")
#' library("mlr3")
#'
#' # Create example task
#' data = smotefamily::sample_generator(500, 0.8)
#' data$result = factor(data$result)
#' task = TaskClassif$new(id = "example", backend = data, target = "result")
#' task$head()
#' table(task$data(cols = "result"))
#'
#' # Generate synthetic data for minority class
#' pop = po("blsmote")
#' bls_result = pop$train(list(task))[[1]]$data()
#' nrow(bls_result)
#' table(bls_result$result)
PipeOpBLSmote = R6Class("PipeOpBLSmote",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "blsmote", param_vals = list()) {
      ps = ps(
        K = p_int(lower = 1, default = 5, tags = c("train", "blsmote")),
        C = p_int(lower = 1, default = 5, tags = c("train", "blsmote")),
        # dup_size = 0 leads to behaviour different from 1, 2, 3, ..., because it means "duplicating until balanced", so it is a "special_vals".
        dupSize = p_int(lower = 1, default = 0, special_vals = list(0), tags = c("train", "blsmote")),
        # Default of `method` is derived from the source code of smotefamily::BLSMOTE(), not documented there.
        method = p_fct(levels = c("type1", "type2"), default = "type1", tags = c("train", "blsmote")),
        quiet = p_lgl(tags = c("train", "required"))
      )
      ps$set_values(quiet = TRUE)
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE,
        packages = "smotefamily", task_type = "TaskClassif", tags = "imbalanced data")
    }
  ),
  private = list(

    .train_task = function(task) {
      cols = task$feature_names
      unsupported_cols = setdiff(unlist(task$col_roles), union(cols, task$target_names))
      if (length(unsupported_cols)) {
        stopf("BLSMOTE cannot generate synthetic data for the following columns since they are neither features nor targets: %s.",
              str_collapse(unsupported_cols, quote = '"'))
      }
      if (!length(cols)) {
        return(task)
      }
      if (!all(task$feature_types$type %in% c("numeric"))) {
        stop("BLSmote does only accept numeric features. Use PipeOpSelect to select the appropriate features.")
      }

      # Calculate synthetic data
      dt = task$data(cols = cols)
      if (self$param_set$get_values()$quiet) {
        utils::capture.output({
          st = setDT(invoke(smotefamily::BLSMOTE, X = dt, target = task$truth(),
                            .args = self$param_set$get_values(tags = "blsmote"),
                            .opts = list(warnPartialMatchArgs = FALSE))$syn_data)  # BLSMOTE uses partial arg matching internally
        })
      } else {
        st = setDT(invoke(smotefamily::BLSMOTE, X = dt, target = task$truth(),
                          .args = self$param_set$get_values(tags = "blsmote"),
                          .opts = list(warnPartialMatchArgs = FALSE))$syn_data)
      }

      # Rename target column and fix character conversion
      # We index by position (target should be last column) instead of indexing by name, which would lead to problems if a feature were called "class"
      st[[ncol(st)]] = as_factor(st[[ncol(st)]], levels = task$class_names)
      setnames(st, ncol(st), task$target_names)

      task$rbind(st)
    }
  )
)

mlr_pipeops$add("blsmote", PipeOpBLSmote)
