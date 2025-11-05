#' @title Class Weights for Sample  - Extended
#'
#' @usage NULL
#' @name mlr_pipeops_classweightsex
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Adds a class weight column to the [`Task`][mlr3::Task], influencing how different [`Learner`][mlr3::Learner]s weight samples during training.
#' It is also possible to add a weight column to the [`Task`][mlr3::Task], which affects how samples are weighted during evaluation.
#' Sample weights are assigned to each observation according to its target class.
#'
#' Binary as well as Multiclass [classification tasks][mlr3::TaskClassif] are supported.
#'
#' It is possible to set either one of the `"weights_learner"` and `"weights_measure"` columns, both of them or none of them.
#' Thus, the behavior of subsequent [`Learner`][mlr3::Learner]s or evaluation metric weights can be determined. (resampling techniques???)
#'
#' @section Construction:
#' ```
#' PipeOpClassWeightsEx$new(id = "classweightsex", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)` \cr
#'   Identifier of the resulting  object, default `"classweightsex"`
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
#' * `weight_type` :: `character` \cr
#'   Determines whether `"weights_learner"`, `"weights_measure"`, both or none of the columns will be set.
#' * `weight_method` :: `character(1)` \cr
#'   The method that is chosen to determine the weights of the samples. Methods encompass (`"inverse_class_frequency"`, `"inverse_square_root_of_frequency"`, `"median_frequency_balancing"`, `"explicit"`)
#' * `mapping`:: named `character` \cr
#'   Depends on `"weight_method" = "explicit"`. Must be a named character, that specifies for each target class the corresponding weight.
#'
#' The newly introduced column is named `.WEIGHTS`; there will be a naming conflict if this column already exists and is *not* a
#' weight column itself.
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
#'
#' poicf = po("classweightsex", param_vals = list(weight_type = c("learner", "measure"), weight_method = "inverse_class_frequency"))
#' result = poicf$train(list(task))[[1L]]
#'
#' if ("weights_learner" %in% names(result)) {
#'   result$weights_learner  # recent mlr3-versions
#' } else {
#'   result$weights  # old mlr3-versions
#' }
#'
#' result$weights_measure
#'
#'
#' if ("weights_measure" %in% names(result)) {
#'   result$weights_measure  # recent mlr3-versions
#' } else {
#'   result$weights  # old mlr3-versions
#' }

PipeOpClassWeightsEx = R6Class("PipeOpClassWeightsEx",
  inherit = PipeOpTaskPreproc,

  public = list(
    initialize = function(id = "classweightsex", param_vals = list()) {
      ps = ps(
        weight_type = p_uty(init = "learner", tags = "train",
                            custom_check = crate(function(x) check_character(x, max.len = 2) %check&&% check_subset(x, choices = c("learner", "measure")))),
        weight_method = p_fct(init = "explicit",
                              levels = c("inverse_class_frequency", "inverse_square_root_of_frequency", "median_frequency_balancing", "effective_number_of_samples", "explicit"), tags = c("train", "required")),
        mapping = p_uty(tags = "train",
                        custom_check = crate(function(x) check_character(names(x), any.missing = FALSE, unique = TRUE)),
                        depends = weight_method == "explicit")
      )
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE, task_type = "TaskClassif", tags = "imbalanced data")
    }
  ),
  private = list(

    .train_task = function(task) {
      pv = self$param_set$get_values(tags = "train")

      if (is.null(pv$weight_type) ||
          is.null(pv$weight_method) ||
          (pv$weight_method == "explicit" && is.null(pv$mapping))) {
        return(task)
      }

      weightcolname = ".WEIGHTS"
      if (weightcolname %in% unlist(task$col_roles)) {
        stopf("Weight column '%s' is already in the Task", weightcolname)
      }

      truth = task$truth()

      class_frequency = prop.table(table(truth))
      class_names = names(class_frequency)

      weights_by_class = switch(pv$weight_method,
        "inverse_class_frequency" = 1 / class_frequency,
        "inverse_square_root_of_frequency" = 1 / sqrt(class_frequency),
        "median_frequency_balancing" = median(class_frequency) / class_frequency,
        "explicit" = pv$mapping
      )

      weights_table = data.table(weights_by_class[truth])
      wcol = setnames(as.data.table(weights_table[[ncol(weights_table)]]), weightcolname)
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

mlr_pipeops$add("classweightsex", PipeOpClassWeightsEx)

