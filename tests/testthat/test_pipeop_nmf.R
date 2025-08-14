context("PipeOpNMF")

test_that("basic properties", {
  skip_if_not_installed("NMF")
  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpNMF, task = task, deterministic_train = FALSE)
})

test_that("feature selector", {
  skip_if_not_installed("NMF")
  op = PipeOpNMF$new()
  dat = iris
  dat$Sepal.Length[1L] <- -999
  dat$test <- rep.int(c(TRUE, FALSE), times = 75L)
  task = TaskClassif$new("test", backend = dat, target = "Species")
  train_out = op$train(list(task))[[1L]]
  expect_setequal(c("Sepal.Length", "test", paste0("NMF", 1:2)), train_out$feature_names)
})


test_that("parameters", {
  skip_if_not_installed("NMF")
  op = PipeOpNMF$new()
  op$param_set$values$rank = 3L
  op$param_set$values$nrun = 2L
  op$param_set$values$seed = 999

  task = mlr_tasks$get("iris")

  train_out1 = op$train(list(task))[[1L]]
  expect_subset(paste0("NMF", 1:3), train_out1$feature_names)
  expect_equal(op$state@nrun, 2L)
  expect_matrix(op$state@consensus)

  train_out2 = op$train(list(task))[[1L]]
  expect_equal(train_out1, train_out2)
})

test_that("PipeOpNMF - does not modify search path when NMF is not loaded, fix for #929", {
  skip_if_not_installed("NMF")

  orig_attached = search()

  op = po("nmf")
  op$train(list(tsk("iris")))
  expect_equal(search(), orig_attached)
  # Ideally, we'd want to restore the original environment but that is not easily possible. Testing predict in its own
  # test is also difficult since we'd need a correct PipeOp state for that.
  unloadNamespace("NMF")
  op$predict(list(tsk("iris")))
  expect_equal(search(), orig_attached)
})

test_that("PipeOpNMF - does not modify search path when NMF or its dependencies are loaded, fix for #929", {
  skip_if_not_installed("NMF")

  library("NMF")
  orig_attached = search()

  op = po("nmf")
  op$train(list(tsk("iris")))
  expect_equal(search(), orig_attached)
  op$predict(list(tsk("iris")))
  expect_equal(search(), orig_attached)

  # Test when only (some of) NMF's dependencies are loaded (e.g. through other packages)
  unloadNamespace("NMF")
  unloadNamespace("Biobase")
  orig_attached = search()

  op = po("nmf")
  op$train(list(tsk("iris")))
  expect_equal(search(), orig_attached)
  unloadNamespace("NMF")
  unloadNamespace("Biobase")
  op$predict(list(tsk("iris")))
  expect_equal(search(), orig_attached)
})

test_that("PipeOpNMF - Did NMF fix its issues?", {
  skip_if_not_installed("NMF")
  orig_attached = search()
  NMF::nmfModels()
  expect_failure(expect_equal(search(), orig_attached))
})
