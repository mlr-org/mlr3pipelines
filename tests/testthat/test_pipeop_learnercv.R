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

  lrn = mlr_learners$get("classif.featureless")
  iris_with_unambiguous_mode = mlr_tasks$get("iris")$filter(c(1:49, 52:150))  # want featureless learner without randomness
  expect_datapreproc_pipeop_class(PipeOpLearnerCV,
    list(lrn), iris_with_unambiguous_mode, predict_like_train = FALSE, deterministic_train = FALSE)
  # 'insample' PipeOpLearnerCV with deterministic Learner is deterministic in every regard!
  expect_datapreproc_pipeop_class(PipeOpLearnerCV,
    list(lrn, resampling = rsmp("insample")), iris_with_unambiguous_mode)

  expect_error(PipeOpLearnerCV$new())

})

test_that("PipeOpLearnerCV - param values", {
  lrn = mlr_learners$get("classif.rpart")
  polrn = PipeOpLearnerCV$new(lrn)
  expect_subset(c("minsplit", "resampling.folds", "keep_response"), names(polrn$param_set$params))
  expect_equal(polrn$param_set$values, list(resampling.folds = 3, keep_response = FALSE, xval = 0))
  polrn$param_set$values$minsplit = 2
  expect_equal(polrn$param_set$values, list(resampling.folds = 3, keep_response = FALSE, minsplit = 2, xval = 0))
  polrn$param_set$values$resampling.folds = 4
  expect_equal(polrn$param_set$values, list(resampling.folds = 4, keep_response = FALSE, minsplit = 2, xval = 0))
})

test_that("PipeOpLearnerCV - within resampling", {
  lrn = mlr_learners$get("classif.rpart")
  gr = GraphLearner$new(PipeOpLearnerCV$new(lrn) %>>% po(id = "l2", lrn))
  resample(tsk("iris"), gr, rsmp("holdout"))
})

test_that("PipeOpLearnerCV - insample resampling", {
  lrn = mlr_learners$get("classif.featureless")
  iris_with_unambiguous_mode = mlr_tasks$get("iris")$filter(c(1:49, 52:150))  # want featureless learner without randomness

  polrn = PipeOpLearnerCV$new(lrn, rsmp("insample"))
  expect_equal(polrn$train(list(iris_with_unambiguous_mode))[[1]]$data(),
    cbind(iris_with_unambiguous_mode$data(cols = "Species"),
      classif.featureless.response = factor("virginica", levels = levels(iris[[5]]))))

  lrn = mlr_learners$get("classif.rpart")
  polrn = PipeOpLearnerCV$new(lrn, rsmp("insample"))
  expect_equal(polrn$train(list(iris_with_unambiguous_mode))[[1]],
    polrn$predict(list(iris_with_unambiguous_mode))[[1]])
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

test_that("PipeOpLearnerCV - different methods", {
  skip_on_cran()  # takes too long

  # Helper
  test_valid_resampled_task = function(polrn, task, predict_type) {
    polrn$learner$predict_type = predict_type

    train_out = polrn$train(list(task))[[1]]
    train_out_data = train_out$data()
    expect_identical(task$row_ids, train_out$row_ids)

    if (task$task_type == "classif") {
      if (polrn$learner$predict_type == "response") {
        feature = train_out$data(cols = grep("*.response", train_out$feature_names, value = TRUE))[[1L]]
        expect_true(is.factor(feature))
        expect_identical(task$class_names, levels(feature))
      } else {  # "prob"
        features = train_out$data(cols = grep("*.prob*", train_out$feature_names, value = TRUE))
        sums = rowSums(is.na(features))
        expect_true(all(sums == 0 | sums == NCOL(features)))  # either all or none missing
        features = features[sums == 0, ]
        expect_true(all(apply(features, MARGIN = 2L, function(x) x >= 0 & x <= 1)))  # between 0 and 1
        expect_equal(rowSums(features), rep_len(1, length.out = NROW(features)))  # sum is 1
      }
    } else {  # "regr"
      if (polrn$learner$predict_type == "response") {
        feature = train_out$data(cols = grep("*.response", train_out$feature_names, value = TRUE))[[1L]]
        expect_true(is.numeric(feature))
      } else {  # "se"
        features = train_out$data(cols = grep("*.response|*.se", train_out$feature_names, value = TRUE))
        expect_true(all(apply(features, MARGIN = 2L, is.numeric)))
      }
    }
  }

  set.seed(1234)
  # faster training
  taskc = tsk("german_credit")$filter(sample(1000, 50))
  taskc$select("age")
  taskr = tsk("boston_housing")$filter(sample(sample(506, 50)))
  taskr$select("rad")

  # cv
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rsmp("cv", folds = 2))
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rsmp("cv", folds = 2))
  test_valid_resampled_task(polrnc, taskc, "response")
  test_valid_resampled_task(polrnc, taskc, "prob")
  test_valid_resampled_task(polrnr, taskr, "se")

  # bootstrap
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rsmp("bootstrap", repeats = 2))
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rsmp("bootstrap", repeats = 2))
  test_valid_resampled_task(polrnc, taskc, "response")
  test_valid_resampled_task(polrnc, taskc, "prob")
  test_valid_resampled_task(polrnr, taskr, "se")

  # holdout
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rsmp("holdout"))
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rsmp("holdout"))
  test_valid_resampled_task(polrnc, taskc, "response")
  test_valid_resampled_task(polrnc, taskc, "prob")
  test_valid_resampled_task(polrnr, taskr, "se")

  # loo
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rsmp("loo"))
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rsmp("loo"))
  test_valid_resampled_task(polrnc, taskc, "response")
  test_valid_resampled_task(polrnc, taskc, "prob")
  test_valid_resampled_task(polrnr, taskr, "se")

  # repeated_cv
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rsmp("repeated_cv", folds = 2, repeats = 2))
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rsmp("repeated_cv", folds = 2, repeats = 2))
  test_valid_resampled_task(polrnc, taskc, "response")
  test_valid_resampled_task(polrnc, taskc, "prob")
  test_valid_resampled_task(polrnr, taskr, "se")

  # subsampling
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rsmp("subsampling", repeats = 2))
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rsmp("subsampling", repeats = 2))
  test_valid_resampled_task(polrnc, taskc, "response")
  test_valid_resampled_task(polrnc, taskc, "prob")
  test_valid_resampled_task(polrnr, taskr, "se")

  # custom
  # classif
  rcm = rsmp("custom")
  rcm$instantiate(taskc, train_sets = list(taskc$row_ids[1:25], taskc$row_ids[26:50]), test_sets = list(taskc$row_ids[1:25], taskc$row_ids[26:50]))  # no multiples no missings
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rcm)
  test_valid_resampled_task(polrnc, taskc, "response")
  test_valid_resampled_task(polrnc, taskc, "prob")

  rcm$instantiate(taskc, train_sets = list(taskc$row_ids[1:25], taskc$row_ids[26:50]), test_sets = list(taskc$row_ids[1:25], taskc$row_ids[1:50]))  # multiples but no missings
  test_valid_resampled_task(polrnc, taskc, "response")
  test_valid_resampled_task(polrnc, taskc, "prob")

  rcm$instantiate(taskc, train_sets = list(taskc$row_ids[1:25], taskc$row_ids[26:50]), test_sets = list(taskc$row_ids[1:25], taskc$row_ids[26:45]))  # no multiples but missings
  test_valid_resampled_task(polrnc, taskc, "response")
  test_valid_resampled_task(polrnc, taskc, "prob")
  polrnc$learner$predict_type = "response"
  feature_out = polrnc$train(list(taskc))[[1L]]$data(cols = "classif.rpart.response")[[1L]]
  expect_true(all(which(is.na(feature_out)) == 46:50))
  polrnc$learner$predict_type = "prob"
  features_out = polrnc$train(list(taskc))[[1L]]$data(cols = c("classif.rpart.prob.good", "classif.rpart.prob.bad"))
  expect_true(all(which(rowSums(is.na(features_out)) == 2L) == 46:50))

  rcm$instantiate(taskc, train_sets = list(taskc$row_ids[1:25], taskc$row_ids[26:50]), test_sets = list(taskc$row_ids[1:25], taskc$row_ids[20:45]))  # multiples and missings
  test_valid_resampled_task(polrnc, taskc, "response")
  test_valid_resampled_task(polrnc, taskc, "prob")
  polrnc$learner$predict_type = "response"
  feature_out = polrnc$train(list(taskc))[[1L]]$data(cols = "classif.rpart.response")[[1L]]
  expect_true(all(which(is.na(feature_out)) == 46:50))
  polrnc$learner$predict_type = "prob"
  features_out = polrnc$train(list(taskc))[[1L]]$data(cols = c("classif.rpart.prob.good", "classif.rpart.prob.bad"))
  expect_true(all(which(rowSums(is.na(features_out)) == 2L) == 46:50))

  # regr
  rcm = rsmp("custom")
  rcm$instantiate(taskr, train_sets = list(taskr$row_ids[1:25], taskr$row_ids[26:50]), test_sets = list(taskr$row_ids[1:25], taskr$row_ids[26:50]))  # no multiples no missings
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rcm)
  test_valid_resampled_task(polrnr, taskr, "se")

  rcm$instantiate(taskr, train_sets = list(taskr$row_ids[1:25], taskr$row_ids[26:50]), test_sets = list(taskr$row_ids[1:25], taskr$row_ids[1:50]))  # multiples but no missings
  test_valid_resampled_task(polrnr, taskr, "se")

  rcm$instantiate(taskr, train_sets = list(taskr$row_ids[1:25], taskr$row_ids[26:50]), test_sets = list(taskr$row_ids[1:25], taskr$row_ids[26:45]))  # no multiples but missings
  test_valid_resampled_task(polrnr, taskr, "se")
  polrnr$learner$predict_type = "se"
  features_out = polrnr$train(list(taskr))[[1L]]$data(cols = c("regr.lm.response", "regr.lm.se"))
  expect_true(all(which(rowSums(is.na(features_out)) == 2L) == 46:50))

  rcm$instantiate(taskr, train_sets = list(taskr$row_ids[1:25], taskr$row_ids[26:50]), test_sets = list(taskr$row_ids[1:25], taskr$row_ids[20:45]))  # multiples and missings
  test_valid_resampled_task(polrnr, taskr, "se")
  polrnr$learner$predict_type = "se"
  features_out = polrnr$train(list(taskr))[[1L]]$data(cols = c("regr.lm.response", "regr.lm.se"))
  expect_true(all(which(rowSums(is.na(features_out)) == 2L) == 46:50))
})
