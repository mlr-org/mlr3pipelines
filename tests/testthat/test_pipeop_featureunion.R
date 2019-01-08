context("featureunion")


test_that("featureunion - basic properties", {
  # Test basic properties
  op = PipeOpFeatureUnion$new(3)
  test_basic_pipeop_props(op)
  expect_true(length(op$train_intypes) == 3L)
  expect_true(length(op$predict_intypes) == 3L)
})


test_that("featureunion - basic", {

  # Define PipeOp's
  scatter = PipeOpCopy$new(2)
  op2a = PipeOpPCA$new()
  op2b = PipeOpNULL$new()
  op3 = PipeOpFeatureUnion$new(2)
  expect_true(length(op3$intype) == 2L)

  task = mlr_tasks$get("iris")
  lrn = mlr_learners$get("classif.rpart")
  op4 = PipeOpLearner$new(learner = lrn)

  #  FIXME: Check param_set
  param_names_union = c(
    "OpNULL.Petal.Width", "OpNULL.Petal.Length",
    "pca.PC1", "OpNULL.Sepal.Length", "pca.PC2",
    "OpNULL.Sepal.Width", "pca.PC3")

  graph = scatter %>>% gunion(op2a, op2b)
  graph = graph %>>% op3
  expect_false(graph$is_trained)
  test_basic_graph_props(graph)
  expect_true(length(graph) == 4L)

  trained = graph$train(task)
  expect_equal(trained$feature_names, c("PC1", "PC2", "PC3", "PC4", "Petal.Length",
    "Petal.Width", "Sepal.Length", "Sepal.Width"))
  # expect_true(graph$is_trained)
})

