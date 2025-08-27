context("PipeOpIsomap")

test_that("PipeOpIsomap - basic properties", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("stats")
  op = po("isomap", .mute = "message")
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  expect_datapreproc_pipeop_class(PipeOpIsomap, task = task, predict_like_train = FALSE, deterministic_predict = FALSE)
})



# Argumentation für tolerance = 1
# dat <- dimRed::loadDataSet("Iris")
# iris[150:1,] --> in Datenformat S4/dimRed Format bringen ==> schauen ob das Resultat das gleiche ist
# falls es nicht gleich ist determ_pred auf FALSE
# emb <- dimRed::embed(dat, "Isomap", knn = 50)
# emb2 <- selectMethod("predict", "dimRedResult")(emb, dat)
# emb3 <- selectMethod("predict", "dimRedResult")(emb, dat)
# dimRed::plot(emb, type ="2vars")
# dimRed::plot(emb3, type = "2vars")

# datrev = dimRed::loadDataSet("Iris")
# datrev@data <- datrev@data[nrow(datrev@data):1, ]
# datrev@meta <- datrev@meta[nrow(datrev@meta):1, , drop = FALSE]

# embrev <- dimRed::embed(datrev, "Isomap", knn = 50)
# embrev2 <- selectMethod("predict", "dimRedResult")(embrev, dat)


test_that("compare to dimRed::isomap", {
  skip_if_not_installed("dimRed")
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
  # Version 1
  pipeop_result_predict = po$predict(list(task))[[1]]$data()
  pipeop_iso_predict = pipeop_result_predict[, grepl("^iso \\d", colnames(pipeop_result_predict)), with = FALSE]
  pipeop_meta_predict = pipeop_result_predict[, !grepl("^iso \\d", colnames(pipeop_result_predict)), with = FALSE]
  # Version 2
  dimRed_result_predict = selectMethod("predict", "dimRedResult")(dimRed_result_train, data)

  expect_equal(as.matrix(pipeop_iso_predict), dimRed_result_predict@data, tolerance = 0.001)
  expect_equal(as.data.frame(pipeop_meta_predict), dimRed_result_predict@meta)
})



test_that("isomap algorithm requires data to be numeric", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("stats")
  po = po("isomap")
  task = tsk("penguins")
  task$filter(which(complete.cases(task$data())))
  expect_error(po$train(list(task)))
})

test_that("hyperparameter ndim", {
  skip_if_not_installed("dimRed")
  skip_if_not_installed("stats")
  for (i in seq_len(length(tsk("iris")$feature_names))) {
    po = po("isomap", ndim = i)
    expect_equal(length(po$train(list(tsk("iris")))[[1]]$feature_names), i)
    expect_equal(length(po$predict(list(tsk("iris")))[[1]]$feature_names), i)
  }
})

test_that("hyperparameter get_geod", {
  skip_if_not_installed("dimRed")
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
  skip_if_not_installed("stats")
  po = po("isomap", .mute = c("message", "output"))
  expect_no_message(po$train(list(tsk("iris"))))
  # expect_no_message(po$predict(list(tsk("iris"))))
})


# why does it fail
po_message = po("isomap")
po_message$train(list(tsk("iris")))

po_no_message = po("isomap", .mute = c("message", "output"))
po_no_message$train(list(tsk("iris")))
