#' @title ADAS Balancing
#'
#' @usage NULL
#' @name mlr_pipeops_adas
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Generates a more balanced data set by creating synthetic instances of the minority class using the ADASYN algorithm.
#'
#' The algorithm generates for each minority instance new data points based on its `K` nearest neighbors and the difficulty of learning for that data point.
#' It can only be applied to tasks with numeric features that have no missing values.
#'
#' See [`smotefamily::ADAS`] for details.
#'
#' @section Construction:
#' ```
#' PipeOpADAS$new(id = "adas", param_vals = list())
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
#'   The number of nearest neighbors used for sampling new values. Default is `5`.
#'   See [`ADAS()`][`smotefamily::ADAS`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @references
#' `r format_bib("he_2008")`
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' \dontshow{ if (requireNamespace("smotefamily")) \{ }
#' library("mlr3")
#'
#' # Create example task
#' data = data.frame(
#'   target = factor(sample(c("c1", "c2"), size = 300, replace = TRUE, prob = c(0.1, 0.9))),
#'   x1 = rnorm(300),
#'   x2 = rnorm(300)
#' )
#' task = TaskClassif$new(id = "example", backend = data, target = "target")
#' task$head()
#' table(task$data(cols = "target"))
#'
#' # Generate synthetic data for minority class
#' pop = po("adas")
#' adas_result = pop$train(list(task))[[1]]$data()
#' nrow(adas_result)
#' table(adas_result$target)
#' \dontshow{ \} }
PipeOpADAS = R6Class("PipeOpADAS",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "adas", param_vals = list()) {
      ps = ps(
        K = p_int(lower = 1, default = 5, tags = c("train", "adas"))
      )
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE,
        packages = "smotefamily", task_type = "TaskClassif", tags = "imbalanced data")
    }
  ),
  private = list(

    .train_task = function(task) {
      assert_true(all(task$feature_types$type == "numeric"))
      cols = task$feature_names

      unsupported_cols = setdiff(unlist(task$col_roles), union(cols, task$target_names))
      if (length(unsupported_cols)) {
        stopf("ADAS cannot generate synthetic data for the following columns since they are neither features nor targets: '%s'",
              paste(unsupported_cols, collapse = "', '"))
      }

      if (!length(cols)) {
        return(task)
      }
      dt = task$data(cols = cols)

      # calculate synthetic data
      st = setDT(invoke(smotefamily::ADAS, X = dt, target = task$truth(),
        .args = self$param_set$get_values(tags = "adas"),
        .opts = list(warnPartialMatchArgs = FALSE))$syn_data)  # ADAS uses partial arg matching internally

      # rename target column and fix character conversion
      st[["class"]] = as_factor(st[["class"]], levels = task$class_names)
      setnames(st, "class", task$target_names)

      task$rbind(st)
    }
  )
)

mlr_pipeops$add("adas", PipeOpADAS)
