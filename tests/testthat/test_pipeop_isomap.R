context("PipeOpIsomap")

test_that("PipeOpIsomap - basic properties", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("stats")
  op = po("isomap", .mute = "message")
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  expect_datapreproc_pipeop_class(PipeOpIsomap, task = task, predict_like_train = FALSE, deterministic_predict = FALSE)
})

test_that("compare to dimRed::isomap", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("stats")
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

  expect_equal(as.matrix(pipeop_iso_train), dimRed_result_train@data@data)
  expect_equal(as.data.frame(pipeop_meta_train), dimRed_result_train@data@meta)

  # Part 2 - Predict-method
  # Version 1 - PipeOpIsomap
  pipeop_result_predict = po$predict(list(task))[[1]]$data()
  pipeop_iso_predict = pipeop_result_predict[, grepl("^iso \\d", colnames(pipeop_result_predict)), with = FALSE]
  pipeop_meta_predict = pipeop_result_predict[, !grepl("^iso \\d", colnames(pipeop_result_predict)), with = FALSE]
  # Version 2 - dimRed package
  dimRed_result_predict = selectMethod("predict", "dimRedResult")(dimRed_result_train, data)

  expect_equal(as.matrix(pipeop_iso_predict), dimRed_result_predict@data, tolerance = 0.001)
  expect_equal(as.data.frame(pipeop_meta_predict), dimRed_result_predict@meta)
})

test_that("isomap handles non-numeric features by leaving them untouched", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("stats")
  po = po("isomap")
  task = tsk("penguins")
  task$filter(which(complete.cases(task$data())))
  non_numeric = task$feature_types[type %nin% c("numeric", "integer"), id]
  expect_true(length(non_numeric) > 0)

  trained_task = po$train(list(task))[[1]]
  predicted_task = po$predict(list(task))[[1]]

  expect_true(all(non_numeric %in% trained_task$feature_names))
  expect_true(all(non_numeric %in% predicted_task$feature_names))
  expect_equal(trained_task$data(cols = non_numeric), task$data(cols = non_numeric))
  expect_equal(predicted_task$data(cols = non_numeric), task$data(cols = non_numeric))
  expect_true(any(grepl("^iso ", trained_task$feature_names)))
  expect_true(any(grepl("^iso ", predicted_task$feature_names)))
})

test_that("isomap leaves non-numeric features untouched", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("stats")
  backend = data.table::as.data.table(tsk("iris")$data())
  backend$species_factor = factor(rep(letters[1:3], length.out = nrow(backend)))
  task = mlr3::TaskClassif$new("iris_factor", backend = backend, target = "Species")
  po = po("isomap", ndim = 3)
  trained_task = po$train(list(task))[[1]]
  predicted_task = po$predict(list(task))[[1]]

  expect_true("species_factor" %in% trained_task$feature_names)
  expect_equal(trained_task$data(cols = "species_factor"), task$data(cols = "species_factor"))
  expect_equal(predicted_task$data(cols = "species_factor"), task$data(cols = "species_factor"))
  expect_equal(sum(grepl("^iso ", trained_task$feature_names)), 3L)
  expect_equal(sum(grepl("^iso ", predicted_task$feature_names)), 3L)
})

test_that("hyperparameter ndim", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("stats")
  for (i in seq_len(length(tsk("iris")$feature_names))) {
    po = po("isomap", ndim = i)
    expect_equal(length(po$train(list(tsk("iris")))[[1]]$feature_names), i)
    expect_equal(length(po$predict(list(tsk("iris")))[[1]]$feature_names), i)
  }
})

test_that("hyperparameter get_geod", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("stats")
  # Check 1 - get_geod = FALSE behaves as expected
  po_no_geod = po("isomap", get_geod = FALSE)
  po_no_geod$train(list(tsk("iris")))
  expect_equal(po_no_geod$state$embed_result@other.data, list())

  # Check 2 - get_geod = TRUE behaves as expected
  po_geod = po("isomap", get_geod = TRUE)
  po_geod$train(list(tsk("iris")))

  # obtain geodistance matrix from original isomap embedding
  emb1 = dimRed::embed(dimRed::loadDataSet("Iris"), "Isomap", get_geod = TRUE)
  expect_equal(po_geod$state$embed_result@other.data, emb1@other.data)
})

test_that("hyperparameter .mute", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("RSpectra")
  skip_if_not_installed("stats")
  po = po("isomap", .mute = c("message", "output"))
#  expect_silent(po$train(list(tsk("iris")))) # does not work because of testthat #1480
})
