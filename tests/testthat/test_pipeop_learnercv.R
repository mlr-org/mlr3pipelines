context("PipeOpLearnerCV")

test_that("PipeOLearnerCV - basic properties", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearnerCV$new(lrn)
  expect_pipeop(po)
  expect_data_table(po$input, nrow = 1)
  expect_data_table(po$output, nrow = 1)

  task = mlr_tasks$get("iris")
  tsk = train_pipeop(po, list(task = task))[[1]]
  expect_class(tsk, "Task")
  expect_true(tsk$nrow == 150L)
  expect_true(tsk$ncol == 2L)
  expect_true(task$target_names == tsk$target_names)
  expect_true(task$class_names == tsk$class_names)
  vals = factor(unique(tsk$data(col = tsk$feature_names)$response))
  expect_equivalent(vals, task$class_names)

  result = predict_pipeop(po, list(task = task))
  expect_class(result[[1L]], "Prediction")
})
