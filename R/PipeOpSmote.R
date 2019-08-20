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
#" * `id` :: `character(1)`\cr
#'   Identifier of resulting object, default `"smote"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`].
#'
#' The output is the input [`Task`][mlr3::Task] with added synthetic rows for the minority class.
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
#' Chawla, N., Bowyer, K., Hall, L. and Kegelmeyer, W. 2002.
#' SMOTE: Synthetic minority oversampling technique.
#' Journal of Artificial Intelligence Research. 16, 321-357.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @examples
#' pop = mlr_pipeops$get("smote")
#'
#' # Create example task
#' library(smotefamily)
#' data_example = sample_generator(1000,ratio = 0.80)
#' task = TaskClassif$new(id = "example", backend = data_example, target = "result")
#' task$data()
#' table(task$data()$result)
#'
#' # Generate synthetic data for minority class
#' smotedata = pop$train(list(task))[[1]]$data()
#' table(smotedata$result)
#'
#' @family PipeOps
#' @include PipeOpTaskPreproc.R
#' @export
PipeOpSmote = R6Class("PipeOpSmote",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "smote", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamInt$new("K", default = 5),
        ParamInt$new("dup_size", default = 0)
      ))
      super$initialize(id, param_set = ps, param_vals = param_vals,
        packages = "smotefamily")
    },

    train_task = function(task) {
      assert_true(all(task$feature_types$type == "numeric"))
      dt_columns = self$select_cols(task)
      cols = dt_columns
      if (!length(cols)) {
        self$state = list(dt_columns = dt_columns)
        return(task)
      }
      dt = task$data(cols = cols)

      # extract info to use smotefamily::SMOTE
      ps = self$param_set$values
      ps$X = as.data.frame(dt)
      ps$target = c(task$data(cols = task$target_names))[[1]]

      # calculate synthetic data
      st = invoke(smotefamily::SMOTE, .args = ps)$syn_data

      # add synthetic data to task data
      target.id = which(names(st) == "class")
      colnames(st)[target.id] = task$target_names
      task$rbind(st)
    }
  )
)

mlr_pipeops$add("smote", PipeOpSmote)
