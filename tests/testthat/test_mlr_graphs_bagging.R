context("ppl - pipeline_bagging (deprecated)")

test_that("Bagging warns through all entry points and preserves its defaults", {
  constructors = list(
    pipeline_bagging,
    function(...) ppl("bagging", ...),
    function(...) mlr_graphs$get("bagging", ...)
  )
  for (constructor in constructors) {
    expect_warning({p = constructor(graph = po("nop"))},
      'deprecated.*removed.*ppl\\("bag"\\).*different default.*frac = 1.*replace = TRUE',
      class = "deprecatedWarning")
    expect_equal(p$param_set$values$replicate.reps, 10)
    expect_equal(p$param_set$values$subsample.frac, 0.7)
    expect_false(p$param_set$values$subsample.replace)
    expect_equal(touch(p), touch(pipeline_bag(po("nop"), frac = 0.7, replace = FALSE)))
  }

  task = tsk("iris")
  train_out = p$train(task)[[1L]]
  predict_out = p$predict(task)[[1L]]
  expect_length(train_out, 10L)
  expect_length(predict_out, 10L)
  for (sample in train_out) {
    expect_equal(sample$nrow, 105)
    expect_subset(sample$row_ids, task$row_ids)
    expect_equal(anyDuplicated(sample$row_ids), 0L)
  }
  for (prediction_task in predict_out) {
    expect_equal(prediction_task$data(), task$data())
  }
})

test_that("Bagging forwards explicit arguments to bag", {
  graph = lrn("classif.debug")
  averager = po("classifavg", collect_multiplicity = TRUE)
  expect_warning({p = pipeline_bagging(graph, 3, 0.8, averager, TRUE)}, "deprecated")
  expect_equal(p, pipeline_bag(graph, 3, 0.8, averager, TRUE))
  p$train(tsk("iris"))
  expect_prediction_classif(p$predict(tsk("iris"))[[1L]])
})
