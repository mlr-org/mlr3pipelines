#' @title Class Weights for Sample Weighting
#'
#' @usage NULL
#' @name mlr_pipeops_classweights
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Adds a class weight column to the [`Task`][mlr3::Task], influencing how different [`Learner`][mlr3::Learner]s weight samples during training.
#' It is also possible to add a weight column to the [`Task`][mlr3::Task], which affects how samples are weighted during evaluation.
#' Sample weights are assigned to each observation according to its target class.
#'
#' Only binary [classification tasks][mlr3::TaskClassif] are supported.
#'
#' Caution: when constructed naively without parameter, the weights are all set to 1. The `minor_weight` parameter
#' must be adjusted for this [`PipeOp`] to be useful.
#'
#' It is possible to set either one of the `"weights_learner"` and `"weights_measure"` columns, both of them or none of them.
#' Thus, the behavior of subsequent [`Learner`][mlr3::Learner]s or evaluation metric weights can be determined.
#'
#' @section Construction:
#' ```
#' PipeOpClassWeights$new(id = "classweights", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)` \cr
#'   Identifier of the resulting  object, default `"classweights"`
#' * `param_vals` :: named `list` \cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpTaskPreproc`]. Instead of a [`Task`][mlr3::Task], a
#' [`TaskClassif`][mlr3::TaskClassif] is used as input and output during training and prediction.
#'
#' The output during training is the input [`Task`][mlr3::Task] with added weights column according to target class.
#' The output during prediction is the unchanged input.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`]; however, the `affect_columns` parameter is *not* present. Further parameters are:
#' * `minor_weight` :: `numeric(1)` \cr
#'   Weight given to samples of the minor class. Major class samples have weight 1. Initialized to 1.
#' * `weight_type` :: `character` \cr
#'   Determines whether `"weights_learner"`, `"weights_measure"`, both or none of the columns will be set. Defaults to `"learner"`. An empty
#'   vector leaves the task unchanged.
#'
#' @section Internals:
#' Adds a `.WEIGHTS` column to the [`Task`][mlr3::Task], which is removed from the feature role and mapped to the requested weight roles.
#' The [`Learner`][mlr3::Learner] must support weights for this to have an effect. There will be a naming conflict if this column already
#' exists and is *not* as weight column itself.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @family PipeOps
#' @template seealso_pipeopslist
#' @include PipeOpTaskPreproc.R
#' @export
#' @examples
#' library("mlr3")
#'
#' task = tsk("spam")
#' opb = po("classweights")
#'
#' # task weights
#' if ("weights_learner" %in% names(task)) {
#'   task$weights_learner  # recent mlr3-versions
#' } else {
#'   task$weights  # old mlr3-versions
#' }
#'
#' # double the instances in the minority class (spam)
#' opb$param_set$values$minor_weight = 2
#' result = opb$train(list(task))[[1L]]
#' if ("weights_learner" %in% names(result)) {
#'   result$weights_learner  # recent mlr3-versions
#' } else {
#'   result$weights  # old mlr3-versions
#' }
PipeOpClassWeights = R6Class("PipeOpClassWeights",
  inherit = PipeOpTaskPreproc,

  public = list(
    initialize = function(id = "classweights", param_vals = list()) {
      ps = ps(
        minor_weight = p_dbl(init = 1, lower = 0, upper = Inf, tags = "train"),
        weight_type = p_uty(init = "learner", tags = "train",
                            custom_check = crate(function(x) check_character(x, max.len = 2) %check&&% check_subset(x, choices = c("learner", "measure"))))
        )
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE, task_type = "TaskClassif", tags = "imbalanced data")
    }
  ),
  private = list(
    .train_task = function(task) {

      pv = self$param_set$get_values(tags = "train")

      if ("twoclass" %nin% task$properties) {
        stop("Only binary classification Tasks are supported.")
      }

      # return task as is, if weight_type is an empty list
      if (length(pv$weight_type) == 0)
      return(task)

      weightcolname = ".WEIGHTS"
      if (weightcolname %in% unlist(task$col_roles)) {
        stopf("Weight column '%s' is already in the Task", weightcolname)
      }

      truth = task$truth()
      minor = names(which.min(table(task$truth())))

      wcol = setnames(data.table(ifelse(truth == minor, pv$minor_weight, 1)), weightcolname)

      task$cbind(wcol)
      task$col_roles$feature = setdiff(task$col_roles$feature, weightcolname)

      classif_roles = mlr_reflections$task_col_roles$classif

      for (type in pv$weight_type) {
        preferred_role = paste0("weights_", type)
        final_role = if (preferred_role %in% classif_roles) preferred_role else "weight"
        task$col_roles[[final_role]] = weightcolname
      }
      task
    },

    .predict_task = identity
  )
)

mlr_pipeops$add("classweights", PipeOpClassWeights)
