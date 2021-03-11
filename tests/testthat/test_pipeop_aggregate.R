context("PipeOpAggregate")

test_that("PipeOpAggregate - basic properties", {
  op = PipeOpAggregate$new()
  expect_pipeop(op)

  # generic tests
  task = tsk("iris")
  task$select(cols = "Petal.Length")
  expect_datapreproc_pipeop_class(PipeOpAggregate, task = task)

  op$param_set$values$aggregation = list(NO_DEF = ~ mean(NO_DEF))
  expect_equal(task$data(), op$train(list(task))[[1L]]$data())

  op$param_set$values$aggregation = list()
  op$param_set$values$by = "NO_DEF"
  expect_equal(task$data(), op$train(list(task))[[1L]]$data())

  op$param_set$values$aggregation = list(NO_DEF = ~ mean(NO_DEF))
  expect_error(op$train(list(task)), regexp = "Must be equal to")
  op$param_set$values$aggregation = list(Petal.Length = ~ mean(Petal.Length))
  expect_error(op$train(list(task)), regexp = "Must be element of")

  # toy aggregation works
  calculate_mode = function(x) {
    unique_x = unique(x)
    unique_x[which.max(tabulate(match(x, unique_x)))]
  }
  task$cbind(data.table(row_reference = rep(1:3, each = 50L)))
  task$cbind(data.table(categorical = as.factor(rep(c("a", "b", "c"), 50L))))
  task$set_col_roles("row_reference", roles = "row_reference")
  op$param_set$values$aggregation = list(Petal.Length = ~ mean(Petal.Length), categorical = ~ calculate_mode(categorical))
  op$param_set$values$by = "row_reference"
  train_out = op$train(list(task))[[1L]]
  expect_data_table(train_out$data(), nrows = 3L, ncols = 3L)
  expect_equal(train_out$data(cols = "Petal.Length")[["Petal.Length"]],
    aggregate(Petal.Length ~ row_reference, FUN = mean, data = task$data(cols = c(task$feature_names, task$col_roles$row_reference)))[["Petal.Length"]])
  expect_equal(train_out$data(cols = "categorical")[["categorical"]],
    aggregate(categorical ~ row_reference, FUN = calculate_mode, data = task$data(cols = c(task$feature_names, task$col_roles$row_reference)))[["categorical"]])
})

test_that("PipeOpLearnerCV and PipeOpAggregate- different methods", {
  skip_on_cran()  # takes too long

  calculate_mode = function(x) {
    unique_x = unique(x)
    unique_x[which.max(tabulate(match(x, unique_x)))]
  }

  # helper
  test_valid_resampled_task = function(polrn, poagg, task, predict_type) {
    polrn$learner$predict_type = predict_type

    lrn_out = polrn$train(list(task))[[1L]]
    lrn_out_data = lrn_out$data()
    if (class(polrn)[[1L]] %in% c("ResamplingCV", "ResamplingInsample", "ResamplingLoo")) {
      expect_identical(lrn_out$row_ids, task$row_ids)
    } else {
      expect_subset(lrn_out$data(cols = lrn_out$col_roles$row_reference)[[lrn_out$col_roles$row_reference]], task$row_ids)
    }

    agg_out = poagg$train(list(lrn_out))[[1L]]
    if (class(polrn)[[1L]] %in% c("ResamplingCV", "ResamplingInsample", "ResamplingLoo", "ResamplingRepeatedCV")) {
      expect_identical(agg_out$row_ids, task$row_ids)
    } else {
      expect_subset(agg_out$row_ids, task$row_ids)
    }

    if (task$task_type == "classif") {
      if (polrn$learner$predict_type == "response") {
        feature = agg_out$data(cols = grep("*.response", agg_out$feature_names, value = TRUE))[[1L]]
        expect_true(is.factor(feature))
        expect_identical(task$class_names, levels(feature))
      } else {  # "prob"
        features = agg_out$data(cols = grep("*.prob*", agg_out$feature_names, value = TRUE))
        sums = rowSums(is.na(features))
        expect_true(all(sums == 0 | sums == NCOL(features)))  # either all or none missing
        features = features[sums == 0, ]
        expect_true(all(apply(features, MARGIN = 2L, function(x) x >= 0 & x <= 1)))  # between 0 and 1
        expect_equal(rowSums(features), rep_len(1, length.out = NROW(features)))  # sum is 1
      }
    } else {  # "regr"
      if (polrn$learner$predict_type == "response") {
        feature = agg_out$data(cols = grep("*.response", agg_out$feature_names, value = TRUE))[[1L]]
        expect_true(is.numeric(feature))
      } else {  # "se"
        features = agg_out$data(cols = grep("*.response|*.se", agg_out$feature_names, value = TRUE))
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

  poaggcr = PipeOpAggregate$new(
    param_vals = list(aggregation = list(classif.rpart.response = ~ calculate_mode(classif.rpart.response)),
      by = "pre.classif.rpart"))
  poaggcp = PipeOpAggregate$new(
    param_vals = list(aggregation = list(classif.rpart.prob.bad = ~ mean(classif.rpart.prob.bad), classif.rpart.prob.good = ~ mean(classif.rpart.prob.good)),
      by = "pre.classif.rpart"))
  poaggrs = PipeOpAggregate$new(
    param_vals = list(aggregation = list(regr.lm.response = ~ mean(regr.lm.response), regr.lm.se = ~ mean(regr.lm.se)),
      by = "pre.regr.lm"))

  # cv
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rsmp("cv", folds = 2L))
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rsmp("cv", folds = 2L))
  test_valid_resampled_task(polrnc, poaggcr, taskc, "response")
  test_valid_resampled_task(polrnc, poaggcp, taskc, "prob")
  test_valid_resampled_task(polrnr, poaggrs, taskr, "se")

  # insample
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rsmp("insample"))
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rsmp("insample"))
  test_valid_resampled_task(polrnc, poaggcr, taskc, "response")
  test_valid_resampled_task(polrnc, poaggcp, taskc, "prob")
  test_valid_resampled_task(polrnr, poaggrs, taskr, "se")

  # bootstrap
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rsmp("bootstrap", repeats = 2L))
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rsmp("bootstrap", repeats = 2L))
  test_valid_resampled_task(polrnc, poaggcr, taskc, "response")
  test_valid_resampled_task(polrnc, poaggcp, taskc, "prob")
  test_valid_resampled_task(polrnr, poaggrs, taskr, "se")

  # holdout
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rsmp("holdout"))
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rsmp("holdout"))
  test_valid_resampled_task(polrnc, poaggcr, taskc, "response")
  test_valid_resampled_task(polrnc, poaggcp, taskc, "prob")
  test_valid_resampled_task(polrnr, poaggrs, taskr, "se")

  # loo
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rsmp("loo"))
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rsmp("loo"))
  test_valid_resampled_task(polrnc, poaggcr, taskc, "response")
  test_valid_resampled_task(polrnc, poaggcp, taskc, "prob")
  test_valid_resampled_task(polrnr, poaggrs, taskr, "se")

  # repeated_cv
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rsmp("repeated_cv", folds = 2L, repeats = 2L))
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rsmp("repeated_cv", folds = 2L, repeats = 2L))
  test_valid_resampled_task(polrnc, poaggcr, taskc, "response")
  test_valid_resampled_task(polrnc, poaggcp, taskc, "prob")
  test_valid_resampled_task(polrnr, poaggrs, taskr, "se")

  # subsampling
  polrnc = PipeOpLearnerCV$new(LearnerClassifRpart$new(), rsmp("subsampling", repeats = 2L))
  polrnr = PipeOpLearnerCV$new(mlr3learners::LearnerRegrLM$new(), rsmp("subsampling", repeats = 2L))
  test_valid_resampled_task(polrnc, poaggcr, taskc, "response")
  test_valid_resampled_task(polrnc, poaggcp, taskc, "prob")
  test_valid_resampled_task(polrnr, poaggrs, taskr, "se")
})

