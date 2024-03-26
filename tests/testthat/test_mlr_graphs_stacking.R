context("ppl - pipeline_stacking")

test_that("Stacking Pipeline", {

  base_learners = list(
    lrn("classif.rpart", predict_type = "prob", id = "base.rpart")
  )
  super_learner = lrn("classif.rpart", id = "super.rpart")

  # default
  graph_stack = pipeline_stacking(base_learners, super_learner)
  expect_graph(graph_stack)
  expect_names(graph_stack$ids(), identical.to = c("base.rpart", "nop", "featureunion_stacking", "super.rpart"))
  graph_learner = as_learner(graph_stack)
  graph_learner$train(tsk("iris"))
  expect_class(graph_learner$model$super.rpart$model, "rpart")
  expect_class(graph_learner$model$base.rpart$model, "rpart")

  # no nop
  graph_stack = pipeline_stacking(base_learners, super_learner, use_features = FALSE)
  expect_graph(graph_stack)
  expect_names(graph_stack$ids(), identical.to = c("base.rpart", "featureunion_stacking", "super.rpart"))
  graph_learner = as_learner(graph_stack)
  graph_learner$train(tsk("iris"))
  expect_class(graph_learner$model$super.rpart$model, "rpart")
  expect_class(graph_learner$model$base.rpart$model, "rpart")

  # folds
  graph_stack = pipeline_stacking(base_learners, super_learner, folds = 5)
  expect_graph(graph_stack)
  expect_names(graph_stack$ids(), identical.to = c("base.rpart", "nop", "featureunion_stacking", "super.rpart"))
  graph_learner = as_learner(graph_stack)
  graph_learner$train(tsk("iris"))
  expect_equal(graph_learner$graph$pipeops$base.rpart$param_set$values$resampling.folds, 5)
  expect_class(graph_learner$model$super.rpart$model, "rpart")
  expect_class(graph_learner$model$base.rpart$model, "rpart")

  # insample
  graph_stack = pipeline_stacking(base_learners, super_learner, method = "insample")
  expect_graph(graph_stack)
  expect_names(graph_stack$ids(), identical.to = c("base.rpart", "nop", "featureunion_stacking", "super.rpart"))
  graph_learner = as_learner(graph_stack)
  graph_learner$train(tsk("iris"))
  expect_equal(graph_learner$graph$pipeops$base.rpart$param_set$values$resampling.method, "insample")
  expect_class(graph_learner$model$super.rpart$model, "rpart")
  expect_class(graph_learner$model$base.rpart$model, "rpart")
})
