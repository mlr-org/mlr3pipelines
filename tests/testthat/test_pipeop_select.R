context("select")

test_that("select", {
  op = PipeOpSelect$new()
  expect_pipeop(op)

  # Generic tests
  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpSelect, task = mlr_tasks$get("iris"))

  expect_datapreproc_pipeop_class(PipeOpSelect, task = mlr_tasks$get("iris"),
    constargs = list(param_vals = list(selector = selector_grep("^Petal"))))


  # Selects the columns we expect it to select
  po = PipeOpSelect$new()
  expect_equal(po$train(list(tsk("iris")))[[1]]$data(), mlr_tasks$get("iris")$data())

  po$param_set$values$selector = selector_grep("^Petal")
  expect_set_equal(po$train(list(tsk("iris")))[[1]]$feature_names, c("Petal.Width", "Petal.Length"))

  po$param_set$values$selector = selector_name("Petal.Length")
  expect_equal(po$train(list(tsk("iris")))[[1]]$feature_names, "Petal.Length")

  po$param_set$values$selector = selector_invert(selector_name("Petal.Length"))
  expect_set_equal(po$train(list(tsk("iris")))[[1]]$feature_names, c("Sepal.Length", "Sepal.Width", "Petal.Width"))

  po$param_set$values$selector = selector_type("factor")
  expect_set_equal(po$train(list(tsk("boston_housing")))[[1]]$feature_names, c("chas", "town"))

})


