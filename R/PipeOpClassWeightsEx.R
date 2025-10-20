#' library("mlr3")
#'
#' task = tsk("spam")
#' opb = po("classweightsex", param_vals = list(weight_method = "inverse class frequency"))
#' opb = po("classweightsex", param_vals = list(weight_method = "inverse square root of frequency"))
#' opb = po("classweightsex", param_vals = list(weight_method = "median frequency balancing"))
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

PipeOpClassWeightsEx = R6Class("PipeOpClassWeightsEx",
  inherit = PipeOpTaskPreproc,

  public = list(
    initialize = function(id = "classweightsex", param_vals = list()) {
      ps = ps(
        weight_type = p_uty(init = c("learner", "measure"), tags = "train"),
        weight_method = p_fct(init = "explicit", levels = c("inverse class frequency", "inverse square root of frequency", "median frequency balancing", "effective number of samples", "explicit"), tags = "train"),
        mapping = p_uty(tags = "train")
      )
      super$initialize(id, param_set = ps, param_vals = param_vals, can_subset_cols = FALSE, task_type = "TaskClassif", tags = "imbalanced data")
    }
  ),
  private = list(

    .train_task = function(task) {
      browser()
      #if ("twoclass" %nin% task$properties) {
      #  stop("Only binary classification Tasks are supported.")
      #}

      weightcolname = ".WEIGHTS"
      if (weightcolname %in% unlist(task$col_roles)) {
        stopf("Weight column '%s' is already in the Task", weightcolname)
      }

      truth = task$truth()
      minor = names(which.min(table(task$truth())))
      class_frequency = table(truth) / length(truth)
      if (self$param_set$values$weight_method == "inverse class frequency") {
        wcol = setnames(data.table(truth)[data.table(class_frequency^-1), on = .(truth)][, "N"], weightcolname)
      } else if (self$param_set$values$weight_method == "inverse square root of frequency") {
        wcol = setnames(data.table(truth)[data.table((class_frequency^0.5)^-1), on = .(truth)][, "N"], weightcolname)
      } else if (self$param_set$values$weight_method == "median frequency balancing") {
        wcol = setnames(data.table(truth)[data.table(median(class_frequency) / class_frequency), on = .(truth)][, "N"], weightcolname)
      } else if (self$param_set$values$weight_method == "explicit") {
        task$cbind(data.table(.WEIGHTS = mapping[task$truth()]))
      }
      task$cbind(wcol)
      task$col_roles$feature = setdiff(task$col_roles$feature, weightcolname)
      if ("learner" %in% self$param_set$values$weight_type) {
        if ("weights_learner" %in% mlr_reflections$task_col_roles$classif) {
          task$col_roles$weights_learner = weightcolname
        } else {
          task$col_roles$weight = weightcolname
        }
      }
      if ("measure" %in% self$param_set$values$weight_type) {
        if ("weights_measure" %in% mlr_reflections$task_col_roles$classif) {
          task$col_roles$weights_measure = weightcolname
        } else {
          task$col_roles$weight = weightcolname
        }
      }
      task
    },

    .predict_task = identity
  )
)

mlr_pipeops$add("classweightsex", PipeOpClassWeightsEx)

