context("usecases for pipelines")

test_that("scale + pca", {
  task = mlr_tasks$get("iris")
  g = PipeOpScale$new() %>>% PipeOpPCA$new()
  res1 = g$train(task)
  assert_list(res1)
  res2 = g$predict(task)

})



