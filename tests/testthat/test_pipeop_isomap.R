context("PipeOpIsomap")

test_that("PipeOpIsomap - basic properties", {
  op = po("isomap")
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  expect_datapreproc_pipeop_class(PipeOpIsomap, task = task, tolerance = 1)
})

test_that("compare to dimRed::isomap", {
  skip_if_not_installed("dimRed")

})

test_that("cannot handle missing values", {
  skip_if_not_installed("dimRed")
  po = po("isomap")
  task = tsk("penguins")
  task$filter(which(complete.cases(task$data())))
  expect_error(po$train(list(tsk("penguins"))))
})


# Argumentation für tolerance = 1
#' dat <- loadDataSet("Iris")
#' emb <- embed(dat, "Isomap", knn = 50)
#' emb2 <- predict(emb, dat)
#' plot(emb)
#' plot(emb2)

dat = na.omit(penguins[,3:7])
embed(dat, "Isomap", knn = 50)
