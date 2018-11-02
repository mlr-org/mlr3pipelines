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
  
  op1$set_next(list(op2a, op2b))
  
  op2a$set_next(list(op3))
  op2b$set_next(list(op3))
  
  op3$set_next(list(op4))
  
  trainGraph(op1, task)
  
  model <- op4$params$model
  
  paramNamesFeatureUnion <- c(
    "OpNULL.Petal.Width", "OpNULL.Petal.Length", "pca.Sepal.Length", 
    "OpNULL.Sepal.Length", "pca.Sepal.Width", "OpNULL.Sepal.Width", 
    "pca.Petal.Length")
  
  expect_equal(names(model$variable.importance), paramNamesFeatureUnion)
  
})

