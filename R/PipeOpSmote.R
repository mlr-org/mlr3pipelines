#' @title PipeOpSmote
#'
#' @usage NULL
#' @name mlr_pipeops_smote
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Generates a more balanced data set by creating
#' synthetic instances of the minority class using the SMOTE algorithm.
#' The algorithm samples for each minority instance a new data point based on the `K` nearest
#' neighbors of that data point.
#' It can only be applied to tasks with numeric features.
#' See [`smotefamily::SMOTE`] for details.
#'
#' @section Construction:
#' ```
#' PipeOpSmote$new(id = "smote", param_vals = list())
#' ```
#'
#" * `id` :: `character(1)`\cr
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
#' @section Internals:
#' For details see: \cr
#' Chawla, N., Bowyer, K., Hall, L. and Kegelmeyer, W. 2002.\cr
#' SMOTE: Synthetic minority oversampling technique.\cr
#' Journal of Artificial Intelligence Research. 16, 321-357.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' library("mlr3")
#'
#' # Create example task
#' data_example = smotefamily::sample_generator(1000, ratio = 0.80)
#' task = TaskClassif$new(id = "example", backend = data_example, target = "result")
#' task$data()
#' table(task$data()$result)
#'
#' # Generate synthetic data for minority class
#' pop = po("smote")
#' smotedata = pop$train(list(task))[[1]]$data()
#' table(smotedata$result)
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpSmote = R6Class("PipeOpSmote",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "smote", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamInt$new("K", lower = 1, default = 5, tags = c("train", "smote")),
        # dup_size = 0 leads to behaviour different from 1, 2, 3, ..., because it means "autodetect",
        # so it is a 'special_vals'.
        ParamInt$new("dup_size", lower = 1, default = 0, special_vals = list(0), tags = c("train", "smote"))
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals,
        packages = "smotefamily", can_subset_cols = FALSE)
    },

    train_task = function(task) {
      assert_true(all(task$feature_types$type == "numeric"))
      cols = self$select_cols(task)

      if (!length(cols)) {
        self$state = list(dt_columns = cols)
        return(task)
      }
      dt = task$data(cols = cols)

      # calculate synthetic data
      st = setDT(invoke(smotefamily::SMOTE, X = dt, target = task$truth(),
        .args = self$param_set$get_values(tags = "smote"),
        .opts = list(warnPartialMatchArgs = FALSE))$syn_data)

      # rename target column and fix character conversion for TaskClassif
      if (task$task_type == "classif") {
        st[["class"]] = as_factor(st[["class"]], levels = task$class_names)
      }
      setnames(st, "class", task$target_names)
      task$rbind(st)
    }
  )
)

mlr_pipeops$add("smote", PipeOpSmote)
