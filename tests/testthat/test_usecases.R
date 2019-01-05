context("usecases for pipelines")

test_that("scale + pca", {
  task = mlr_tasks$get("iris")
  g = PipeOpScale$new() %>>% PipeOpPCA$new()
  res1 = g$train(task)
  res2 = g$predict(task)

})

test_that("scale + pca + PipeOpLearner", {
  task = mlr_tasks$get("iris")

  g = PipeOpScale$new() %>>%
    PipeOpPCA$new() %>>%
    PipeOpLearner$new(mlr_learners$get("classif.rpart"))
  res1 = g$train(task)
  res2 = g$predict(task)
})

