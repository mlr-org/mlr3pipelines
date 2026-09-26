# library("mlr3")
#
#' task = tsk("spam")
#' opb = po("classweightsex", param_vals = list(weight_method = "inverse_class_frequency"))
#' opb = po("classweightsex", param_vals = list(weight_method = "inverse square root of frequency"))
#' opb = po("classweightsex", param_vals = list(weight_method = "median frequency balancing"))
# opb = po("classweightsex", param_vals = list(weight_method = "explicit", mapping = c("setosa" = 0.3, "virginica" = 0.5, "versicolor" = 0.4)))
#
# task weights
# if ("weights_learner" %in% names(task)) {
#   task$weights_learner  # recent mlr3-versions
# } else {
#   task$weights  # old mlr3-versions
# }
#
# double the instances in the minority class (spam)
# opb$param_set$values$minor_weight = 2
# result = opb$train(list(task))[[1L]]
# if ("weights_learner" %in% names(result)) {
#   result$weights_learner  # recent mlr3-versions
# } else {
#   result$weights  # old mlr3-versions
# }
