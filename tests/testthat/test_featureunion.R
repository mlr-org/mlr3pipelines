context("featureunion")
test_that("featureunion - basic", {

  task = mlr_tasks$get("iris")
  dd = iris[, -5]
  nd = iris[, -5]

  op1 = PipeOpScale$new()

  op2a = PipeOpPCA$new()
  op2b = PipeOpNULL$new()

  op3 = PipeOpFeatureUnion$new(2)

  lrn = mlr_learners$get("classif.rpart")
  op4 = PipeOpLearner$new(learner = lrn)

  scatter = PipeOpCopy$new(2)

  graph = scatter %>>% gunion(op2a, op2b) %>>% op3
  graph$plot()

  trained = graph$train(task)

  graph[["pca"]]$pipeop$state



  param_names_union = c(
    "OpNULL.Petal.Width", "OpNULL.Petal.Length",
    "pca.PC1", "OpNULL.Sepal.Length", "pca.PC2",
    "OpNULL.Sepal.Width", "pca.PC3")

})

