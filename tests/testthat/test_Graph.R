context("Graph")

test_that("linear graph", {
  g = Graph$new()
  g$ids(sorted = TRUE)

  op_ds = PipeOpDownsample$new()
  op_pca = PipeOpPCA$new()
  op_lrn = PipeOpLearner$new(mlr_learners$get("classif.rpart"))
  g$add_pipeop(op_ds)
  g$add_pipeop(op_pca)
  # g$add_pipeop(op_lrn)
  g$add_channel("downsample", "1", "pca", "1")
  # g$add_channel("pca", "1", "classif.rpart", "1")

  expect_graph(g)

  input = mlr_tasks$mget("iris")
  x = g$train(input)
  expect_task(x[[1]])

  out = g$predict(input)
  expect_task(x[[1]])
})
