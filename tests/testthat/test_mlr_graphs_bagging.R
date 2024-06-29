context("ppl - pipeline_bagging")


test_that("Bagging Pipeline", {
  skip_if_not_installed("rpart")
  skip_on_cran()  # takes too long

  expect_error(ppl("bagging", graph = lrn("classif.rpart"), averager = po("classifavg", collect_multiplicity = FALSE)),
    regexp = "must collect multiplicities")


  # classif
  tsk = tsk("iris")
  lrn = lrn("classif.rpart")
  p = ppl("bagging", graph = po(lrn), averager = po("classifavg", collect_multiplicity = TRUE))
  expect_graph(p)

  # regr
  tsk = tsk("boston_housing_classic")
  lrn = lrn("regr.rpart")
  p = ppl("bagging", graph = po(lrn), iterations = 5L, averager = po("regravg", collect_multiplicity = TRUE))
  expect_graph(p)

  # graph instead of po(lrn)
  gr = po("pca") %>>% po(lrn)
  p = pipeline_bagging(graph = gr, iterations = 2L, averager = po("regravg", collect_multiplicity = TRUE))
  expect_graph(p)
  res = resample(tsk$filter(1:50), GraphLearner$new(p), rsmp("holdout"))
  expect_resample_result(res)

  # no averager
  tsk = tsk("iris")
  lrn = lrn("classif.rpart")
  p = pipeline_bagging(graph = po(lrn))
  expect_graph(p)
  train_out = p$train(tsk)[[1L]]
  predict_out = p$predict(tsk)[[1L]]
  expect_length(train_out, 10L)
  expect_length(predict_out, 10L)
  expect_true(all(map_lgl(predict_out, function(x) "PredictionClassif" %in% class(x))))
})

test_that("Bagging with replacement", {
  skip_if_not_installed("rpart")
  tsk = tsk("iris")
  lrn = lrn("classif.rpart")
  p = ppl("bagging", graph = po(lrn), replace = TRUE, averager = po("classifavg", collect_multiplicity = TRUE))
  expect_graph(p)
  res = resample(tsk, GraphLearner$new(p), rsmp("holdout"))
  expect_resample_result(res)

  tsk$filter(1:140)
  expect_equal(anyDuplicated(tsk$data()), 0)  # make sure no duplicates

  p = ppl("bagging", iterations = 2, frac = 1,
    graph = lrn("classif.debug", save_tasks = TRUE),
    replace = TRUE, averager = po("classifavg", collect_multiplicity = TRUE)
  )
  p$train(tsk)

  expect_true(anyDuplicated(p$pipeops$classif.debug$state[[1]]$model$task_train$data()) != 0)

  getOrigId = function(data) {
    tsk$data()[, origline := .I][data, on = colnames(tsk$data()), origline]
  }
  orig_id_1 = getOrigId(p$pipeops$classif.debug$state[[1]]$model$task_train$data())
  orig_id_2 = getOrigId(p$pipeops$classif.debug$state[[2]]$model$task_train$data())

  expect_equal(length(orig_id_1), 140)
  expect_equal(length(orig_id_2), 140)
  # if we sampled the same values twice, the all.equal() would just give TRUE
  expect_string(all.equal(orig_id_1, orig_id_2))

  expect_true(length(unique(orig_id_1)) < 140)
  expect_true(length(unique(orig_id_2)) < 140)
})
