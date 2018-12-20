
context("PipeOp")


test_that("PipeOp - simple tests with PipeOpScale", {
  # task = mlr_tasks$get("iris")
  p = PipeOpScale$new()
  expect_class(p, "PipeOpScale")
  expect_false(p$is_trained)
  expect_class(p$param_set, "ParamSet")
})


