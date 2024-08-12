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
#' It can only be applied to tasks with factor features and at least one numeric feature.
#' See [`themis::smotenc`] for details.
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
#' * `k` :: `integer(1)`\cr
#'   Number of nearest neighbors used for generating new values from the minority class. Default is `5`.
#' * `over_ratio` :: `numeric(1)`\cr
#'   Ratio of the majority to minority class. Default is `1`. For details, see [`themis::smotenc`].
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
#' @examples
#' \dontshow{ if (requireNamespace("themis")) \{ }
#' library("mlr3")
#'
#' # Create example task
#' data = data.frame(
#'   target = factor(sample(c("c1, "c2"), size = 200, replace = TRUE, prob = c(0.1, 0.9))),
#'   feature = rnorm(200)
#' )
#' task = TaskClassif$new(id = "example", backend = data, target = "target")
#' task$head()
#' table(task$data()$target)
#'
#' # Generate synthetic data for minority class
#' pop = po("smotenc")
#' smotenc_result = pop$train(list(task))[[1]]$data()
#' table(smotenc_result$target)
#' \dontshow{ \} }
PipeOpSmoteNC = R6Class("PipeOpSmoteNC",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "smotenc", param_vals = list()) {
      ps = ps(
        k = p_int(lower = 1, default = 5, tags = c("train", "smotenc")),
        over_ratio = p_dbl(lower = 0, default = 1, tags = c("train", "smotenc"))
      )
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "themis", can_subset_cols = FALSE,
                       feature_types = c("numeric", "factor"), tags = "imbalanced data")
    }
  ),
  private = list(

    .train_task = function(task) {
      # At least one numeric feature necessary for SmoteNC
      assert_true(any(task$feature_types$type == "numeric"))
      if (!task$task_type == "classif") {
        stop("SmoteNC only supported for TaskClassif")
      }
      # Check that only one target column exists (better way?)
      if (length(task$target_names) > 1L) {
        stop("SmoteNC does not work for Tasks with more than one target column")
      }

      dt_columns = private$.select_cols(task)
      if (!length(dt_columns)) {
        return(task)
      }
      dt = task$data(cols = c(task$target_names, dt_columns))

      # Calculate synthetic data
      st = setDT(invoke(themis::smotenc, df = dt, var = task$target_names,
                        .args = self$param_set$get_values(tags = "smotenc")))

      task$rbind(st)
    }
  )
)

mlr_pipeops$add("smotenc", PipeOpSmoteNC)
