context("PipeOpIsomap")

test_that("PipeOpIsomap - basic properties", {
  op = po("isomap")
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  expect_datapreproc_pipeop_class(PipeOpIsomap, task = task, tolerance = 1)
})


# Argumentation für tolerance = 1
#' dat <- loadDataSet("Iris")
#' emb <- embed(dat, "Isomap", knn = 50)
#' emb2 <- predict(emb, dat)
#' plot(emb)
#' plot(emb2)
