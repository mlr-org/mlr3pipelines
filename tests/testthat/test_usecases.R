context("usecases for pipelines")

test_that("scale + pca", {
  task = mlr_tasks$get("iris")
  g = PipeOpScale$new() %>>% PipeOpPCA$new()
  res1 = g$train(task)
  assert_task(res1)
  res2 = g$predict(task)
  assert_task(res2)
  expect_equal(res1$data(), res2$data())

})



