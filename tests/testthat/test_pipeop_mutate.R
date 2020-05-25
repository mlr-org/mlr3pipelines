context("PipeOpMutate")

test_that("mutate", {
  op = PipeOpMutate$new()
  expect_pipeop(op)

  # Generic tests
  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpMutate, task = mlr_tasks$get("iris"))

  # Works with Task features
  al = list(Sepal.Sum = ~ Sepal.Length + Sepal.Width)
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


  # Name clashes
  al = list(Sepal.Width = ~ Sepal.Width^2)
  op$param_set$values$mutation = al
  res_repl = op$train(list(task))
  expect_true("Sepal.Width" %in% res_repl[[1]]$feature_names)
  expect_equal(res_repl[[1]]$data("Sepal.Width", rows = 1:10)$Sepal.Width,
    task$data("Sepal.Width", rows = 1:10)$Sepal.Width^2)


  # Delete originals
  op$param_set$values$delete_originals = TRUE
  res = op$train(list(task))
  expect_true(op$is_trained)
  expect_task(res[[1]])
  expect_true(length(res[[1]]$feature_names) == 1L)

  # Works with variables from an env
  env = new.env()
  assign("some_test_val", 7, envir = env)
  some_test_val = -100  # this should not be taken!
  al = list(Sepal.PlusVal = ~ Sepal.Length + some_test_val)
  environment(al[[1]]) = env
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


  # Constant column
  al = list(Sepal.Sum = ~ sum(Sepal.Length + Sepal.Width))
  op$param_set$values$mutation = al
  expect_equal(op$train(list(task))[[1]]$data()$Sepal.Sum,
    rep(sum(iris[c("Sepal.Length", "Sepal.Width")]), 150))

  # non-constant column, mismatching number of rows
  al = list(Sepal.Sum = ~ c(0, sum(Sepal.Length + Sepal.Width)))
  op$param_set$values$mutation = al
  expect_error(op$train(list(task)), "150 items")

  # Can use just-created variables
  op$param_set$values$mutation = list(
    Petal.Length_half = ~ Petal.Length / 2,
    Petal.Length_quart = ~ Petal.Length_half / 2
  )
  res5 = op$train(list(task))
  expect_task(res5[[1]])
  expect_true(all(c("Petal.Length_half", "Petal.Length_quart") %in% res5[[1]]$feature_names))
  expect_equivalent(res5[[1]]$data(cols = "Petal.Length_quart"), res5[[1]]$data(cols = "Petal.Length_half") / 2)
})
