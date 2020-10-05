context("ppl - pipeline_bagging")


test_that("Bagging Pipeline", {
  skip_on_cran()  # takes too long
  # classif
  tsk = tsk("iris")
  lrn = lrn("classif.rpart")
  p = ppl("bagging", graph = po(lrn), averager = po("classifavg", collect_multiplicity = TRUE))
  expect_graph(p)

  # regr
  tsk = tsk("boston_housing")
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

