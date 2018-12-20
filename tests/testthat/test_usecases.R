context("usecases for pipelines")

test_that("scale + pca", {
  task = mlr_tasks$get("iris")
  g = PipeOpScale$new() %>>% PipeOpPCA$new()
  g$train(task)
})



