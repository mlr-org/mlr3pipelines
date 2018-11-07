test_that("featureunion - basic", {
  
  set.seed(123)
  
  task = mlr_tasks$get("iris")
  dd = iris[, -5]
  nd = iris[, -5]
  
  op1 = PipeOpScaler$new()
  
  op2a = PipeOpPCA$new()
  op2b = PipeOpNULL$new()
  
  op3 = PipeOpFeatureUnion$new()
  
  lrn = mlr_learners$get("classif.rpart")
  op4 = PipeOpLearner$new(learner = lrn)
  
  root <- GraphNode$new(op1)
  root$set_next(list(op2a, op2b))
  
  n3 <- GraphNode$new(op3)
  
  n2a <- root$next_node(1)$set_next(n3)
  n2b <- root$next_node(2)$set_next(n3)
  
  # # warning!
  # n2a <- root$next_node(1)$set_next(op3)
  # n2b <- root$next_node(2)$set_next(op3)
  
  n3$set_next(op4)
  n4 <- n3$next_node()
  
  graph <- Graph$new(root)
  graph$train(task)
  
  model <- op4$params$model
  
  paramNamesFeatureUnion <- c(
    "OpNULL.Petal.Width", "OpNULL.Petal.Length", "pca.Sepal.Length", 
    "OpNULL.Sepal.Length", "pca.Sepal.Width", "OpNULL.Sepal.Width", 
    "pca.Petal.Length")
  
  expect_equal(names(model$variable.importance), paramNamesFeatureUnion)
  
})
