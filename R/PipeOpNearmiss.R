#' @title Nearmiss Down-Sampling
#'
#' @usage NULL
#' @name mlr_pipeops_nearmiss
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Generates a more balanced data set by down-sampling the instances of non-minority classes using the NEARMISS algorithm.
#'
#' The algorithm down-samples by selecting instances from the non-minority classes that have the smallest mean distance
#' to their `k` nearest neighbors of different classes.
#' For this only numeric and integer features are taken into account. These must have no missing values.
#'
#' This can only be applied to [classification tasks][mlr3::TaskClassif]. Multiclass classification is supported.
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
#' The output during training is the input [`Task`][mlr3::Task] with the rows removed from the non-minority classes.
#' The output during prediction is the unchanged input.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`], as well as
#' * `k` :: `integer(1)`\cr
#'   Number of nearest neighbors used for calculating the mean distances. Default is `5`.
#' * `under_ratio` :: `numeric(1)`\cr
#'   Ratio of the minority-to-majority frequencies. This specifies the ratio to which the number of instances
#'   in the non-minority classes get down-sampled to, relative to the number of instances of the minority class.
#'   Default is `1`. For details, see [`themis::nearmiss`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
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
#' @examplesIf requireNamespace("themis")
#' library("mlr3")
#'
#' # Create example task
#' task = tsk("wine")
#' task$head()
#' table(task$data(cols = "type"))
#'
#' # Down-sample and balance data
#' pop = po("nearmiss")
#' nearmiss_result = pop$train(list(task))[[1]]$data()
#' nrow(nearmiss_result)
#' table(nearmiss_result$type)
PipeOpNearmiss = R6Class("PipeOpNearmiss",
  inherit = PipeOpTaskPreproc,
  public = list(
    initialize = function(id = "nearmiss", param_vals = list()) {
      ps = ps(
        k = p_int(lower = 1, default = 5, tags = c("train", "nearmiss")),
        under_ratio = p_dbl(lower = 0, default = 1, tags = c("train", "nearmiss"))
      )
      super$initialize(id, param_set = ps, param_vals = param_vals, packages = "themis", can_subset_cols = FALSE,
                       task_type = "TaskClassif", tags = "imbalanced data")
    }
  ),
  private = list(

    .train_task = function(task) {
      # Return task unchanged, if no feature columns exist
      if (!length(task$feature_names)) {
        return(task)
      }
      # At least one numeric or integer feature required
      if (!any(task$feature_types$type %in% c("numeric", "integer"))) {
        stop("Nearmiss needs at least one numeric or integer feature to work.")
      }
      # Subset columns to only include integer/numeric features and the target
      type = id = NULL
      cols = c(task$feature_types[type %in% c("integer", "numeric"), id], task$target_names)
      # Down-sample data
      dt = setDT(invoke(themis::nearmiss, df = task$data(cols = cols), var = task$target_names,
                        .args = self$param_set$get_values(tags = "nearmiss")))

      keep = task$row_ids[as.integer(row.names(dt))]
      task$filter(keep)
    }
  )
)

mlr_pipeops$add("nearmiss", PipeOpNearmiss)
