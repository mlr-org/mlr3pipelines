context("PipeOpIsomap")

test_that("PipeOpIsomap - basic properties", {
  op = po("isomap")
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  expect_datapreproc_pipeop_class(PipeOpIsomap, task = task, tolerance = 1)
})

test_that("compare to dimRed::isomap", {
  skip_if_not_installed("dimRed")
  # Part 1 - Train-method
  # Version 1 - PipeOpIsomap
  task = tsk("iris")
  po = po("isomap", knn = 50)
  pipeop_result_train = po$train(list(task))[[1]]$data()
  pipeop_iso_train = pipeop_result_train[, grepl("^iso \\d", colnames(pipeop_result_train)), with = FALSE]
  pipeop_meta_train = pipeop_result_train[, !grepl("^iso \\d", colnames(pipeop_result_train)), with = FALSE]
  # Version 2 - dimRed package
  data = dimRed::loadDataSet("Iris")
  dimRed_result_train = dimRed::embed(data, "Isomap", knn = 50)
  dimRed_result_train@data@data = dimRed_result_train@data@data

  expect_equal(as.matrix(pipeop_iso_train), dimRed_result_train@data@data)
  expect_equal(as.data.frame(pipeop_meta_train), dimRed_result_train@data@meta)

  # Part 2 - Predict-method
  # Version 1
  pipeop_result_predict = po$predict(list(task))[[1]]$data()
  pipeop_iso_predict = pipeop_result_predict[, grepl("^iso \\d", colnames(pipeop_result_predict)), with = FALSE]
  pipeop_meta_predict = pipeop_result_predict[, !grepl("^iso \\d", colnames(pipeop_result_predict)), with = FALSE]
  # Version 2
  dimRed_result_predict = selectMethod("predict", "dimRedResult")(dimRed_result_train, data)

  expect_equal(as.matrix(pipeop_iso_predict), dimRed_result_predict@data, tolerance = 0.001)
  expect_equal(as.data.frame(pipeop_meta_predict), dimRed_result_predict@meta)
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
