context("mutate")

test_that("mutate", {
  op = PipeOpMutate$new()
  expect_pipeop(op)

  # Generic tests
  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpMutate, task = mlr_tasks$get("iris"))

  # Works with Task features
  al = alist(Sepal.Sum = Sepal.Length + Sepal.Width)
  op$param_set$values$mutation = al
  res = op$train(list(task))
  expect_true(op$is_trained)
  expect_true("Sepal.Sum" %in% res[[1]]$feature_names)
  expect_equal(res[[1]]$data("Sepal.Sum", rows = 1:10)$Sepal.Sum,
    (task$data("Sepal.Length", rows = 1:10) + task$data("Sepal.Width", rows = 1:10))$Sepal.Length)
  res2 = op$predict(list(task))
  expect_true("Sepal.Sum" %in% res2[[1]]$feature_names)
  expect_equal(res2[[1]]$data("Sepal.Sum", rows = 1:10)$Sepal.Sum,
    (task$data("Sepal.Length", rows = 1:10) + task$data("Sepal.Width", rows = 1:10))$Sepal.Length)

  # Works with variables from GlobalEnv
  some_test_val = 7
  al = alist(Sepal.PlusVal = Sepal.Length + some_val)
  op$param_set$values$mutation = al
  res3 = op$train(list(task))
  expect_true("Sepal.PlusVal" %in% res[[1]]$feature_names)
  expect_equal(res[[1]]$data("Sepal.PlusVal", rows = 1:10)$Sepal.PlusVal,
    (task$data("Sepal.Length", rows = 1:10) + some_test_val)$Sepal.Length)
  res4 = op$predict(list(task))
  expect_true("Sepal.PlusVal" %in% res4[[1]]$feature_names)
  expect_equal(res4[[1]]$data("Sepal.PlusVal", rows = 1:10)$Sepal.PlusVal,
    (task$data("Sepal.Length", rows = 1:10) + some_test_val)$Sepal.Length)


  # Errors if lengths don't match
  al = alist(Sepal.Sum = sum(Sepal.Length + Sepal.Width))
  op$param_set$values$mutation = al
  expect_error(op$train(list(task)), "Must have exactly 150 rows")
})
