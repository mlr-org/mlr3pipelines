context("PipeOpLearnerCV")

test_that("PipeOLearnerCV - basic properties", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearnerCV$new(lrn)
  expect_pipeop(po$clone())
  expect_data_table(po$input, nrow = 1)
  expect_data_table(po$output, nrow = 1)

  task = mlr_tasks$get("iris")
  tsk = train_pipeop(po, list(task = task))[[1]]
  expect_class(tsk, "Task")
  expect_true(tsk$nrow == 150L)
  expect_true(tsk$ncol == 2L)
  expect_equal(task$target_names, tsk$target_names)
  expect_equal(task$class_names, tsk$class_names)
  vals = factor(unique(tsk$data(col = tsk$feature_names)$response))
  expect_character(setdiff(vals, task$class_names), len = 0)

  tsk = predict_pipeop(po, list(task = task))[[1]]
  expect_class(tsk, "Task")
  expect_true(tsk$nrow == 150L)
  expect_true(tsk$ncol == 2L)
  expect_equal(task$target_names, tsk$target_names)
  expect_equal(task$class_names, tsk$class_names)
  vals = factor(unique(tsk$data(col = tsk$feature_names)$response))
  expect_character(setdiff(vals, task$class_names), len = 0)

  expect_pipeop_class(PipeOpLearnerCV, list(lrn))
  expect_error(PipeOpLearnerCV$new())
})

test_that("PipeOpLearnerCV - param values", {
  lrn = mlr_learners$get("classif.rpart")
  polrn = PipeOpLearnerCV$new(lrn)
  expect_subset(c("minsplit", "resampling.method", "resampling.folds"), names(polrn$param_set$params))
  expect_equal(polrn$param_set$values, list(resampling.method = "cv", resampling.folds = 3, resampling.keep_response = FALSE, xval = 0))
  polrn$param_set$values$minsplit = 2
  expect_equal(polrn$param_set$values, list(resampling.method = "cv", resampling.folds = 3, resampling.keep_response = FALSE, minsplit = 2, xval = 0))
  polrn$param_set$values$resampling.folds = 4
  expect_equal(polrn$param_set$values, list(resampling.method = "cv", resampling.folds = 4, resampling.keep_response = FALSE, minsplit = 2, xval = 0))
})
