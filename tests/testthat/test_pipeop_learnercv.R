context("PipeOpLearnerCV")

test_that("PipeOpLearnerCV - basic properties", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearnerCV$new(lrn)
  expect_pipeop(po$clone(), check_ps_default_values = FALSE)
  expect_data_table(po$input, nrows = 1L)
  expect_data_table(po$output, nrows = 1L)

  task = mlr_tasks$get("iris")
  tsk = train_pipeop(po, list(task = task))[[1L]]
  expect_class(tsk, "Task")
  expect_true(tsk$nrow == 150L)
  expect_true(tsk$ncol == 2L)
  expect_equal(task$target_names, tsk$target_names)
  expect_equal(task$class_names, tsk$class_names)
  vals = factor(unique(tsk$data(cols = tsk$feature_names)$response))
  expect_character(setdiff(vals, task$class_names), len = 0L)

  tsk = predict_pipeop(po, list(task = task))[[1]]
  expect_class(tsk, "Task")
  expect_true(tsk$nrow == 150L)
  expect_true(tsk$ncol == 2L)
  expect_equal(task$target_names, tsk$target_names)
  expect_equal(task$class_names, tsk$class_names)
  vals = factor(unique(tsk$data(cols = tsk$feature_names)$response))
  expect_character(setdiff(vals, task$class_names), len = 0L)

  lrn = mlr_learners$get("classif.featureless")
  iris_with_unambiguous_mode = mlr_tasks$get("iris")$filter(c(1:49, 52:150))  # want featureless learner without randomness
  expect_datapreproc_pipeop_class(PipeOpLearnerCV,
    list(lrn), iris_with_unambiguous_mode, predict_like_train = FALSE, deterministic_train = FALSE, affect_context_independent = FALSE, check_ps_default_values = FALSE)
  # 'insample' PipeOpLearnerCV with deterministic Learner is deterministic in every regard!
  expect_datapreproc_pipeop_class(PipeOpLearnerCV,
    list(lrn, resampling = rsmp("insample")), iris_with_unambiguous_mode, affect_context_independent = FALSE, check_ps_default_values = FALSE)

  expect_error(PipeOpLearnerCV$new())
})

test_that("PipeOpLearnerCV - param values", {
  lrn = mlr_learners$get("classif.rpart")
  polrn = PipeOpLearnerCV$new(lrn)
  expect_subset(c("minsplit", "resampling.folds", "keep_response"), names(polrn$param_set$params))
  expect_equal(polrn$param_set$values, list(resampling.folds = 3L, keep_response = FALSE, xval = 0))
  polrn$param_set$values$minsplit = 2L
  expect_equal(polrn$param_set$values, list(resampling.folds = 3L, keep_response = FALSE, minsplit = 2L, xval = 0))
  polrn$param_set$values$resampling.folds = 4L
  expect_equal(polrn$param_set$values, list(resampling.folds = 4L, keep_response = FALSE, minsplit = 2L, xval = 0))
})

test_that("PipeOpLearnerCV - within resampling", {
  lrn = mlr_learners$get("classif.rpart")
  gr = GraphLearner$new(PipeOpLearnerCV$new(lrn) %>>% po(id = "l2", lrn))
  expect_r6(resample(tsk("iris"), gr, rsmp("holdout")), classes = "ResampleResult")
})

test_that("PipeOpLearnerCV - insample resampling", {
  lrn = mlr_learners$get("classif.featureless")
  iris_with_unambiguous_mode = mlr_tasks$get("iris")$filter(c(1:49, 52:150))  # want featureless learner without randomness

  polrn = PipeOpLearnerCV$new(lrn, rsmp("insample"))
  expect_equal(polrn$train(list(iris_with_unambiguous_mode))[[1L]]$data(),
    cbind(iris_with_unambiguous_mode$data(cols = "Species"),
      classif.featureless.response = factor("virginica", levels = levels(iris[[5]]))))

  lrn = mlr_learners$get("classif.rpart")
  polrn = PipeOpLearnerCV$new(lrn, rsmp("insample"))
  expect_equal(polrn$train(list(iris_with_unambiguous_mode))[[1L]],
    polrn$predict(list(iris_with_unambiguous_mode))[[1L]])
})

test_that("PipeOpLearnerCV - graph but no id", {
  g = PipeOpNOP$new() %>>% PipeOpLearner$new(LearnerClassifRpart$new())
  po = PipeOpLearnerCV$new(g)
  expect_string(po$id)
})

test_that("PipeOpLearnerCV - model active binding to state", {
  lrn = mlr_learners$get("classif.featureless")
  po = PipeOpLearnerCV$new(lrn)
  task = mlr_tasks$get("iris")

  # before training states are NULL
  expect_null(po$state)
  expect_equal(po$learner$state, po$state)
  expect_equal(po$learner_model$state, po$state)

  # after training learner_model's state and state are equal
  train_out = po$train(list(task))
  train_state = po$state
  expect_null(po$learner$state)
  expect_equal(po$learner_model$state, train_state)

  # after predicting states are unchanged
  predict_out = po$predict(list(task))
  expect_equal(po$state, train_state)
  expect_null(po$learner$state)
  expect_equal(po$learner_model$state, po$state)
})

