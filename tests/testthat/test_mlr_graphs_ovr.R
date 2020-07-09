context("ppl - pipeline_ovr")

test_that("OVR Pipeline", {
  task = tsk("wine")

  # assertions on graph
  expect_error(ppl("ovr", lrn("regr.rpart")), regexp = "Graph should return 'NULL' during training and a 'PredictionClassif' during prediction")

  g = ppl("ovr", lrn("classif.rpart"))
  expect_graph(g)
  expect_true(length(g$pipeops) == 1 + 1 + 1)

  train_out = g$train(task)
  expect_null(train_out[[1]])

  predict_out = g$predict(task)
  expect_r6(predict_out[[1]], "PredictionClassif")
})
