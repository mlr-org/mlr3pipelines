test_that("Graph", {
  task = mlr_tasks$get("iris")

  op1 = PipeOpScaler$new()
  op2 = PipeOpPCA$new()
  lrn = mlr_learners$get("classif.rpart")
  lrn$predict_type <- "prob"

  op3 = PipeOpLearner$new(learner = lrn)

  root <- GraphNode$new(op1)
  root$set_next(GraphNode$new(op2))$set_next(GraphNode$new(op3))

  g = Graph$new(root)
  expect_class(g, "Graph")
  expect_output(print(g), regexp = "Graph: scaler->pca->classif.rpart")
  expect_false(g$is_learnt)

  # Test train/predict
  g$train(task)
  expect_true(g$is_learnt)
  # op3$params$prediction

  # Test active bindings
  expect_equal(names(g$ids), c(op1$id, op2$id, op3$id))
  expect_equal(g$param_vals, list())
  expect_equal(g$param_set, graph_gather_params(root))

  # Test `[[` operator
  expect_class(g[["scaler"]], "GraphNode")
  expect_equal(g[["scaler"]]$id, "scaler")
  expect_error(g[["foo"]], "Assertion on 'id' failed:")

  expect_equal(length(g), 3L)
})
