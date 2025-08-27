context("PipeOpNMF")

test_that("PipeOpNMF - basic properties", {
  skip_if_not_installed("NMF")
  task = mlr_tasks$get("iris")
  expect_datapreproc_pipeop_class(PipeOpNMF, task = task, deterministic_train = FALSE)
})

test_that("PipeOpNMF - feature selector", {
  skip_if_not_installed("NMF")
  op = PipeOpNMF$new()
  dat = iris
  dat$Sepal.Length[1L] <- -999
  dat$test <- rep.int(c(TRUE, FALSE), times = 75L)
  task = TaskClassif$new("test", backend = dat, target = "Species")
  train_out = op$train(list(task))[[1L]]
  expect_setequal(c("Sepal.Length", "test", paste0("NMF", 1:2)), train_out$feature_names)
})


test_that("PipeOpNMF - parameters", {
  skip_if_not_installed("NMF")
  op = PipeOpNMF$new()
  op$param_set$values$rank = 3L
  op$param_set$values$nrun = 2L
  op$param_set$values$seed = 999

  task = mlr_tasks$get("iris")

  train_out1 = op$train(list(task))[[1L]]
  expect_subset(paste0("NMF", 1:3), train_out1$feature_names)
  expect_equal(op$state$nmf@nrun, 2L)
  expect_matrix(op$state$nmf@consensus)

  train_out2 = op$train(list(task))[[1L]]
  expect_equal(train_out1, train_out2)
})

test_that("PipeOpNMF - does not modify search path when NMF is not loaded, fix for #929", {
  skip_if_not_installed("NMF")
  # Cleanup to get as clean an environment and search path as possible. Necessary because skip_if_not_installed loads the package.
  unloadNamespace("NMF")
  unloadNamespace("Biobase")
  unloadNamespace("BiocGenerics")
  unloadNamespace("dplyr")  # necessary to unload generics
  unloadNamespace("generics")
  # Test that environment and search path are as we want them
  expect_true(all(c("NMF", "Biobase", "BiocGenerics", "generics") %nin% loadedNamespaces()))
  expect_true(all(paste0("package:", c("NMF", "Biobase", "BiocGenerics", "generics")) %nin% search()))

  orig_attached = search()

  op = po("nmf")
  op$train(list(tsk("iris")))
  expect_equal(search(), orig_attached)

  # Ideally, we'd want to restore the original environment but that is not easily possible.
  # Testing predict in its own test is also difficult since we'd need a correct state for that.
  unloadNamespace("NMF")
  op$predict(list(tsk("iris")))
  expect_equal(search(), orig_attached)

  # Test that printing the state does not modify the search path
  unloadNamespace("NMF")
  print(op$state)
  expect_equal(search(), orig_attached)

})

test_that("PipeOpNMF - does not modify search path when NMF is loaded, fix for #929", {
  skip_if_not_installed("NMF")  # does not attach NMF, but for our fix to work NMF only needs to be loaded
  # Test that environment is as we'd expect it
  expect_true(all(c("NMF", "Biobase", "BiocGenerics", "generics") %in% loadedNamespaces()))
  expect_true(all(paste0("package:", c("Biobase", "BiocGenerics", "generics")) %in% search()))

  orig_attached = search()

  op = po("nmf")
  op$train(list(tsk("iris")))
  expect_equal(search(), orig_attached)
  op$predict(list(tsk("iris")))
  expect_equal(search(), orig_attached)

})

test_that("PipeOpNMF - does not modify search path when some of NMF's dependencies are loaded, fix for #929", {
  skip_if_not_installed("NMF")
  unloadNamespace("NMF")
  unloadNamespace("Biobase")
  # Test that environment is as we'd expect it
  expect_true(all(c("NMF", "Biobase") %nin% loadedNamespaces()))
  expect_true(all(paste0("package:", c("NMF", "Biobase")) %nin% search()))
  expect_true(all(c("BiocGenerics", "generics") %in% loadedNamespaces()))
  expect_true(all(paste0("package:", c("BiocGenerics", "generics")) %in% search()))

  orig_attached = search()

  op = po("nmf")
  op$train(list(tsk("iris")))
  expect_equal(search(), orig_attached)

  unloadNamespace("NMF")
  unloadNamespace("Biobase")
  op$predict(list(tsk("iris")))
  expect_equal(search(), orig_attached)

  # Test that printing the state does not modify the search path
  unloadNamespace("NMF")
  unloadNamespace("Biobase")
  print(op$state)
  expect_equal(search(), orig_attached)

})

test_that("PipeOpNMF - Did NMF fix its .onload weirdness? See renozao/NMF#191", {
  skip_if_not_installed("NMF")  # loads NMF which attaches the packages as long as this is not fixed
  expect_true(all(paste0("package:", c("Biobase", "BiocGenerics", "generics")) %in% search()))

})
