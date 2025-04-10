context("RandomProjection")

test_that("basic properties", {
  task = mlr_tasks$get("iris")
  op = PipeOpRandomProjection$new()
  expect_datapreproc_pipeop_class(PipeOpRandomProjection, task = task, deterministic_train = FALSE)
})

test_that("projection properties", {
  task = mlr_tasks$get("iris")
  op = PipeOpRandomProjection$new()
  set.seed(1234)
  result = op$train(list(task))
  resdt = result[[1]]$data()
  expect_names(colnames(resdt), permutation.of = c("Species", "PR1"))

  expect_equal(resdt, op$predict(list(task))[[1]]$data())

  expect_equal(
    Reduce(`+`, Map(`*`, task$data(cols = task$feature_names), op$state$projection)),
    resdt$PR1
  )

  op$param_set$values$rank = 2
  result = op$train(list(task))
  resdt = result[[1]]$data()
  expect_names(colnames(resdt), permutation.of = c("Species", "PR1", "PR2"))

  expect_equal(as.matrix(resdt[, c("PR1", "PR2")]),
    as.matrix(task$data(cols = task$feature_names)) %*% op$state$projection)

  expect_equal(resdt, op$predict(list(task))[[1]]$data())

  op$param_set$values$rank = 0
  result = op$train(list(task))
  resdt = result[[1]]$data()
  expect_names(colnames(resdt), permutation.of = "Species")

  expect_equal(resdt, op$predict(list(task))[[1]]$data())

  op$param_set$values$rank = 6
  result = op$train(list(task))
  resdt = result[[1]]$data()
  expect_names(colnames(resdt), permutation.of = c("Species", paste0("PR", 1:6)))
  expect_equal(resdt, op$predict(list(task))[[1]]$data())

  pca = po("pca")
  pca$train(list(task))

  gr = (op %>>% pca)
  gr$train(task)

  # PCA on original data should equal PCA on data that was projected to higher space
  # when the same projection is done to it.
  expect_equal(abs(cbind(pca$state$rotation, PC5 = 0, PC6 = 0)),
    abs(gr$state$randomprojection$projection %*% gr$state$pca$rotation))

})
