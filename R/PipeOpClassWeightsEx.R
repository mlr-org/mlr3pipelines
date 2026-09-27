#' @title Class Weights for Sample Weighting - Extended
#'
#' @usage NULL
#' @name mlr_pipeops_classweightsex
#' @format [`R6Class`][R6::R6Class] object inheriting from [`PipeOpTaskPreproc`]/[`PipeOp`].
#'
#' @description
#' Adds a class-dependent sample weights column to a [`Task`][mlr3::Task], allowing
#' [`Learner`][mlr3::Learner]s and [`Measure`][mlr3::Measure]s to weight observations
#' differently during training and evaluation. 
#' 
#' Weights are assigned per observation based on the target class and can be written
#' to the `"weights_learner"` column, the `"weights_measure"` column, both, or neither.
#'
#' Binary as well as multiclass classification tasks ([`TaskClassif`][mlr3::TaskClassif]) are supported.
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
#' The output during training is the input [`Task`][mlr3::Task] with an added weights column according to the target class.
#' The output during prediction is the unchanged input.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpTaskPreproc`].
#'
#' @section Parameters:
#' The parameters are the parameters inherited from [`PipeOpTaskPreproc`]; however, the `affect_columns` parameter is *not* present. Further parameters are:
#' * `weights_learner` :: `logical(1)` \cr
#'   Whether the created weights should be stored as a `weights_learner` column or not. Initialized to `TRUE`.
#' * `weights_measure` :: `logical(1)` \cr
#'   Whether the created weights should be stored as a `weights_measure` column or not. Initialized to `FALSE`.
#' * `weight_method` :: `character(1)` \cr
#'   The method that is chosen to determine the weights of the samples. Methods encompass `"inverse_class_frequency"`, `"inverse_square_root_of_frequency"`, `"median_frequency_balancing"` and `"explicit"`.
#'   In case of `"explicit"`, the `mapping` hyperparameter must be use. Initialized to `"explicit"`.
#' * `mapping` :: named `numeric` \cr
#'   A named numeric vector that specifies a finite weight for each target class in the task. This only has an effect if `weight_method` is `explicit`.
#'
#' @section Internals:
#' Adds a `.WEIGHTS` column to the [`Task`][mlr3::Task], which is removed from the feature role and mapped to the requested weight roles.
#' There will be a naming conflict if this column already exists and is *not* a weight column already. For potentially pre-existing weight columns, the weight 
#' column role gets dropped, but they remain in the [`DataBackend`][mlr3::DataBackend] of the `Task`. \cr
#' When `weight_method = "explicit"`, the mapping must cover every class present in the training data and may not contain additional classes. \cr
#' The [`Learner`][mlr3::Learner] must support weights for this `PipeOp` to have an effect.
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
#' poicf = po("classweightsex", param_vals = list(weights_learner = TRUE, weights_measure = TRUE, 
#'   weight_method = "inverse_class_frequency"))
#' result = poicf$train(list(task))[[1L]]
#'
#' if ("weights_learner" %in% names(result)) {
#'   result$weights_learner  # recent mlr3-versions
#' } else {
#'   result$weights  # old mlr3-versions
#' }
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
        weights_learner = p_lgl(init = TRUE, tags = c("train", "weights_indicator", "required")),
        weights_measure = p_lgl(init = FALSE, tags = c("train", "weights_indicator", "required")),
        weight_method = p_fct(init = "explicit", levels = c("inverse_class_frequency", "inverse_square_root_of_frequency", "median_frequency_balancing", "explicit"), 
          tags = c("train", "required")),
        mapping = p_uty(tags = "train", depends = quote(weight_method == "explicit"), custom_check = crate(function(x) {
          if (is.null(x)) return(TRUE)
          check_numeric(x, any.missing = FALSE, finite = TRUE) %check&&% check_character(names(x), any.missing = FALSE, unique = TRUE, min.chars = 1)
        }))
      )
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE, task_type = "TaskClassif", tags = "imbalanced data")
    }
  ),
  private = list(
    .train_task = function(task) {
      pv = self$param_set$get_values(tags = "train")

      if ((!pv$weights_learner && !pv$weights_measure) || (pv$weight_method == "explicit" && is.null(pv$mapping))) {
        return(task)
      }

      class_names = task$class_names
      if (pv$weight_method == "explicit") {
        mapping_names = names(pv$mapping)
        missing = setdiff(class_names, mapping_names)
        extra = setdiff(mapping_names, class_names)

        if (length(missing)) {
          stopf("Explicit class weights must cover every class in the task; missing: %s", paste(missing, collapse = ", "))
        }
        if (length(extra)) {
          stopf("Explicit class weights contain labels not present in the task: %s", paste(extra, collapse = ", "))
        }
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
        "median_frequency_balancing" = stats::median(class_frequency) / class_frequency,
        "explicit" = pv$mapping
      )

      weights_table = data.table(weights_by_class[truth])
      wcol = setnames(as.data.table(weights_table[[ncol(weights_table)]]), weightcolname)
      
      task$cbind(wcol)
      task$col_roles$feature = setdiff(task$col_roles$feature, weightcolname)

      weights_indicators = unlist(self$param_set$get_values(tags = "weights_indicator"))
      final_roles = names(weights_indicators)[weights_indicators]
      final_roles = if (all(final_roles %in% mlr_reflections$task_col_roles$classif)) final_roles else "weight"  # support for older mlr3 versions
      task$col_roles[final_roles] = weightcolname

      task
    },

    .predict_task = identity
  )
)

mlr_pipeops$add("classweightsex", PipeOpClassWeightsEx)
