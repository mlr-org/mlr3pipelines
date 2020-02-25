
context("PipeOpChunk")

test_that("PipeOpChunk - basic properties", {
  pos = po("subsample")
  expect_pipeop(pos)

  pos = mlr_pipeops$get("subset", param_vals = list("condition" = ~ Species != "versicolor"))
  out = pos$train(list(tsk("iris")))[[1]]
  expect_true(out$nrow == 100)
  expect_true("multiclass" %nin% out$properties)
  expect_true("versicolor" %nin% out$class_names)
}
