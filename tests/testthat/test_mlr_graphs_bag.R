context("ppl - pipeline_bag")

test_that("Bag defaults and explicit sampling arguments", {
  p = ppl("bag", graph = po("nop"))
  expect_equal(p$param_set$values$replicate.reps, 10)
  expect_equal(p$param_set$values$subsample.frac, 1)
  expect_true(p$param_set$values$subsample.replace)

  task = tsk("iris")
  p = pipeline_bag(po("nop"), iterations = 3, frac = 0.5, replace = FALSE)
  train_out = p$train(task)[[1L]]
  predict_out = p$predict(task)[[1L]]
  expect_length(train_out, 3L)
  expect_length(predict_out, 3L)
  for (sample in train_out) {
    expect_equal(sample$nrow, 75)
    expect_subset(sample$row_ids, task$row_ids)
    expect_equal(anyDuplicated(sample$row_ids), 0L)
  }
  for (prediction_task in predict_out) {
    expect_equal(prediction_task$data(), task$data())
  }
})

test_that("Bag validates its arguments", {
  expect_error(pipeline_bag(po("nop"), iterations = -1), "iterations")
  expect_error(pipeline_bag(po("nop"), iterations = 1.5), "iterations")
  expect_error(pipeline_bag(po("nop"), frac = -0.1), "frac")
  expect_error(pipeline_bag(po("nop"), frac = 1.1), "frac")
  expect_error(pipeline_bag(po("nop"), replace = NA), "replace")
})

test_that("Bag clones the supplied graph and averager", {
  graph = as_graph(lrn("classif.debug"))
  averager = as_graph(po("classifavg", collect_multiplicity = TRUE))
  p = pipeline_bag(graph, iterations = 2, averager = averager)
  p$train(tsk("iris"))
  expect_true(p$is_trained)
  expect_false(graph$is_trained)
  expect_false(averager$is_trained)
  p$pipeops$classif.debug$param_set$values$x = 0.5
  expect_null(graph$pipeops$classif.debug$param_set$values$x)
})


test_that("Bagging Pipeline", {
  skip_if_not_installed("rpart")
  skip_on_cran()  # takes too long

  expect_error(ppl("bag", graph = lrn("classif.rpart"), averager = po("classifavg", collect_multiplicity = FALSE)),
    regexp = "must collect multiplicities")


  # classif
  tsk = tsk("iris")
  lrn = lrn("classif.rpart")
  p = ppl("bag", graph = po(lrn), averager = po("classifavg", collect_multiplicity = TRUE))
  expect_graph(p)

  # regr
  tsk = tsk("boston_housing_classic")
  lrn = lrn("regr.rpart")
  p = ppl("bag", graph = po(lrn), iterations = 5L, averager = po("regravg", collect_multiplicity = TRUE))
  expect_graph(p)

  # graph instead of po(lrn)
  gr = po("pca") %>>% po(lrn)
  p = pipeline_bag(graph = gr, iterations = 2L, averager = po("regravg", collect_multiplicity = TRUE))
  expect_graph(p)
  res = resample(tsk$filter(1:50), GraphLearner$new(p), rsmp("holdout"))
  expect_resample_result(res)

  # no averager
  tsk = tsk("iris")
  lrn = lrn("classif.rpart")
  p = pipeline_bag(graph = po(lrn))
  expect_graph(p)
  train_out = p$train(tsk)[[1L]]
  predict_out = p$predict(tsk)[[1L]]
  expect_length(train_out, 10L)
  expect_length(predict_out, 10L)
  expect_true(all(map_lgl(predict_out, function(x) "PredictionClassif" %in% class(x))))
})

test_that("Bagging with replacement by default", {
  set.seed(727)
  skip_if_not_installed("rpart")
  tsk = tsk("iris")
  lrn = lrn("classif.rpart")
  p = ppl("bag", graph = po(lrn), replace = TRUE, averager = po("classifavg", collect_multiplicity = TRUE))
  expect_graph(p)
  res = resample(tsk, GraphLearner$new(p), rsmp("holdout"))
  expect_resample_result(res)

  tsk$filter(1:140)
  expect_equal(anyDuplicated(tsk$data()), 0)  # make sure no duplicates

  p = ppl("bag", iterations = 2,
    graph = lrn("classif.debug", save_tasks = TRUE),
    averager = po("classifavg", collect_multiplicity = TRUE)
  )
  p$train(tsk)

  expect_true(anyDuplicated(p$pipeops$classif.debug$state[[1]]$model$task_train$data()) != 0)

  get_orig_id = function(data) {
    tsk$data()[, origline := .I][data, on = colnames(tsk$data()), origline]
  }
  orig_id_1 = get_orig_id(p$pipeops$classif.debug$state[[1]]$model$task_train$data())
  orig_id_2 = get_orig_id(p$pipeops$classif.debug$state[[2]]$model$task_train$data())

  expect_equal(length(orig_id_1), 140)
  expect_equal(length(orig_id_2), 140)
  expect_false(identical(orig_id_1, orig_id_2))

  expect_true(length(unique(orig_id_1)) < 140)
  expect_true(length(unique(orig_id_2)) < 140)
})
