context("featureunion")
test_that("featureunion - basic", {

  task = mlr_tasks$get("iris")
  dd = iris[, -5]
  nd = iris[, -5]

  op1 = PipeOpScaler$new()

  op2a = PipeOpPCA$new()
  op2b = PipeOpNULL$new()

  op3 = PipeOpFeatureUnion$new()

  lrn = mlr_learners$get("classif.rpart")
  op4 = PipeOpLearner$new(learner = lrn)

  PipeOpCopy$new(2)


  root = GraphNode$new(op1)
  root$
    set_next(list(GraphNode$new(op2a), GraphNode$new(op2b)))$
    set_next(GraphNode$new(op3))$
    set_next(GraphNode$new(op4))

  graph = Graph$new(root)
  graph$train(task)

  model = op4$params$model

  param_names_union = c(
    "OpNULL.Petal.Width", "OpNULL.Petal.Length",
    "pca.PC1", "OpNULL.Sepal.Length", "pca.PC2",
    "OpNULL.Sepal.Width", "pca.PC3")

  expect_equal(names(model$variable.importance), param_names_union)

})

