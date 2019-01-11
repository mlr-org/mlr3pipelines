context("Graph")

test_that("linear graph", {

  g = Graph$new()
  expect_equal(g$ids(sorted = TRUE), character(0))

  op_ds = PipeOpDownsample$new()
  op_pca = PipeOpPCA$new()
  op_lrn = PipeOpLearner$new(mlr_learners$get("classif.rpart"))
  g$add_pipeop(op_ds)
  g$add_pipeop(op_pca)

  g$add_edge("downsample", "1", "pca", "1")

  expect_graph(g)

  expect_output(print(g), "Graph with 2 PipeOps.*downsample.*UNTRAINED.*pca.*UNTRAINED")



  inputs = mlr_tasks$get("iris")
  x = g$train(inputs)
  expect_task(x[[1]])

  expect_output(print(g), "Graph with 2 PipeOps.*downsample.*list.*pca.*prcomp")

  out = g$predict(inputs)
  expect_task(x[[1]])

  g$add_pipeop(op_lrn)

  expect_error(g$add_edge("downsample", "1", "classif.rpart", "1"),
    "Channel.*1.*of node.*downsample.*already connected to channel.*1.*of node pca")

  expect_error(g$add_pipeop(op_lrn), "PipeOp with id.*classif\\.rpart.*already in Graph")

})
