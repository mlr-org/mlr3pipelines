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
  expect_task(res[[1]])
  expect_true("Sepal.Sum" %in% res[[1]]$feature_names)
  expect_equal(res[[1]]$data("Sepal.Sum", rows = 1:10)$Sepal.Sum,
    (task$data("Sepal.Length", rows = 1:10) + task$data("Sepal.Width", rows = 1:10))$Sepal.Length)
  res2 = op$predict(list(task))
  expect_task(res2[[1]])
  expect_true("Sepal.Sum" %in% res2[[1]]$feature_names)
  expect_equal(res2[[1]]$data("Sepal.Sum", rows = 1:10)$Sepal.Sum,
    (task$data("Sepal.Length", rows = 1:10) + task$data("Sepal.Width", rows = 1:10))$Sepal.Length)

  op$param_set$values$delete_originals = TRUE
  res = op$train(list(task))
  expect_true(op$is_trained)
  expect_task(res[[1]])
  expect_true(length(res[[1]]$feature_names) == 1L)

  # Works with variables from an env
  env = new.env()
  assign("some_test_val", 7, envir = env)
  op$param_set$values$env = env
  al = alist(Sepal.PlusVal = Sepal.Length + some_test_val)
  op$param_set$values$mutation = al
  res3 = op$train(list(task))
  expect_task(res3[[1]])
  expect_true("Sepal.PlusVal" %in% res3[[1]]$feature_names)
  expect_equal(res3[[1]]$data("Sepal.PlusVal", rows = 1:10)$Sepal.PlusVal,
    (task$data("Sepal.Length", rows = 1:10) + env$some_test_val)$Sepal.Length)
  res4 = op$predict(list(task))
  expect_task(res4[[1]])
  expect_true("Sepal.PlusVal" %in% res4[[1]]$feature_names)
  expect_equal(res4[[1]]$data("Sepal.PlusVal", rows = 1:10)$Sepal.PlusVal,
    (task$data("Sepal.Length", rows = 1:10) + env$some_test_val)$Sepal.Length)


  # Errors if lengths don't match
  al = alist(Sepal.Sum = sum(Sepal.Length + Sepal.Width))
  op$param_set$values$mutation = al
  expect_error(op$train(list(task)), "Must have exactly 150 rows")
})
