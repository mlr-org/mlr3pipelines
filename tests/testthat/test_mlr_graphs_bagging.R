context("ppl - pipeline_bagging")


test_that("Bagging Pipeline", {
  # classif
  tsk = tsk("iris")
  lrn = lrn("classif.rpart")
  p = ppl("bagging", graph = po(lrn), averager = po("classifavg"))
  expect_graph(p)
  expect_true(length(p$pipeops) == 10 + 10 + 1)

  # regr
  tsk = tsk("boston_housing")
  lrn = lrn("regr.rpart")
  p = ppl("bagging", graph = po(lrn), iterations = 5L, averager = po("regravg"))
  expect_graph(p)
  expect_true(length(p$pipeops) == 5 + 5 + 1)

  # graph instead of po(lrn)
  gr = po("pca") %>>% po(lrn)
  p = pipeline_bagging(graph = gr, iterations = 2L, averager = po("regravg"))
  expect_graph(p)
  expect_true(length(p$pipeops) == 2 + (2*2) + 1)
  res = resample(tsk$filter(1:50), GraphLearner$new(p), rsmp("holdout"))
  expect_resample_result(res)

  # no averager
  tsk = tsk("iris")
  lrn = lrn("classif.rpart")
  p = pipeline_bagging(graph = po(lrn))
  expect_graph(p)
  expect_true(length(p$pipeops) == 10 + 10)
  expect_data_table(p$output, nrows = 10)
})

