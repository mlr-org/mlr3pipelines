context("PipeOpLearnerCV")

test_that("PipeOpLearnerCV - basic properties", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearnerCV$new(lrn)
  expect_pipeop(po$clone())
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  task = mlr_tasks$get("iris")
  tsk = train_pipeop(po, list(task = task))[[1]]
  expect_class(tsk, "Task")
  expect_true(tsk$nrow == 150L)
  expect_true(tsk$ncol == 2L)
  expect_equal(task$target_names, tsk$target_names)
  expect_equal(task$class_names, tsk$class_names)
  vals = factor(unique(tsk$data(cols = tsk$feature_names)$response))
  expect_character(setdiff(vals, task$class_names), len = 0)

  tsk = predict_pipeop(po, list(task = task))[[1]]
  expect_class(tsk, "Task")
  expect_true(tsk$nrow == 150L)
  expect_true(tsk$ncol == 2L)
  expect_equal(task$target_names, tsk$target_names)
  expect_equal(task$class_names, tsk$class_names)
  vals = factor(unique(tsk$data(cols = tsk$feature_names)$response))
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

#357
test_that("PipeOpLearnerCV - within resampling", {
  lrn = mlr_learners$get("classif.rpart")
  gr = GraphLearner$new(PipeOpLearnerCV$new(lrn) %>>% po(id = "l2", lrn))
  resample(tsk("iris"), gr, rsmp("holdout"))
})

test_that("PipeOpLearnerCV - graph but no id", {
  g = PipeOpNOP$new() %>>% PipeOpLearner$new(LearnerClassifRpart$new())
  po = PipeOpLearnerCV$new(g)
  expect_string(po$id)
})
