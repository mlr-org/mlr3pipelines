#' @title Class Weights for Sample Weighting
#'
#' @usage NULL
#' @name mlr_pipeops_classweights
#' @format [`R6Class`] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Adds a class weight column to the [`Task`][mlr3::Task] that different [`Learner`][mlr3::Learner]s may be
#' able to use for sample weighting. Sample weights are added to each sample according to the target class.
#'
#' Only binary [classification tasks][mlr3::TaskClassif] are supported.
#'
#' Caution: when constructed naively without parameter, the weights are all set to 1. The `minor_weight` parameter
#' must be adjusted for this [`PipeOp`] to be useful.
#'
#' @section Construction:
#' ```
#' PipeOpClassWeights$new(id = "classweights", param_vals = list())
#' ```
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"classweights"`
#' * `param_vals` :: named `list`\cr
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
#' @section Internals:
#' Introduces, or overwrites, the "weights" column in the [`Task`][mlr3::Task]. However, the [`Learner`][mlr3::Learner] method needs to
#' respect weights for this to have an effect.
#'
#' The newly introduced column is named `.WEIGHTS`; there will be a naming conflict if this column already exists and is *not* a
#' weight column itself.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpTaskPreproc`]/[`PipeOp`].
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
#' task$weights
#'
#' # double the instances in the minority class (spam)
#' opb$param_set$values$minor_weight = 2
#' result = opb$train(list(task))[[1L]]
#' result$weights
PipeOpClassWeights = R6Class("PipeOpClassWeights",
  inherit = PipeOpTaskPreproc,

  public = list(
    initialize = function(id = "classweights", param_vals = list()) {
      ps = ParamSet$new(params = list(
        ParamDbl$new("minor_weight", lower = 0, upper = Inf, tags = "train")
      ))
      ps$values = list(minor_weight = 1)
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE, task_type = "TaskClassif", tags = "imbalanced data")
    }
  ),
  private = list(

    .train_task = function(task) {

      if ("twoclass" %nin% task$properties) {
        stop("Only binary classification Tasks are supported.")
      }

      weightcolname = ".WEIGHTS"
      if (weightcolname %in% unlist(task$col_roles)) {
        stopf("Weight column '%s' is already in the Task", weightcolname)
      }

      truth = task$truth()
      minor = names(which.min(table(task$truth())))

      wcol = setnames(data.table(ifelse(truth == minor, self$param_set$values$minor_weight, 1)), weightcolname)

      task$cbind(wcol)
      task$col_roles$feature = setdiff(task$col_roles$feature, weightcolname)
      task$col_roles$weight = weightcolname
      task
    },

    .predict_task = identity
  )
)

mlr_pipeops$add("classweights", PipeOpClassWeights)
