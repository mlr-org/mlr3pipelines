context("WeightedAverage Learner")

test_that("LearnerClassifAvg", {
  skip_if_not_installed("rpart")
  skip_on_cran()  # takes too long
  lrn = LearnerClassifAvg$new()
  expect_learner(lrn)
  df = data.frame(x = matrix(sample(c("a", "b", "c"), 100, replace = TRUE), nrow = 10), y = as.factor(sample(c("a", "b", "c"), 10, replace = TRUE)), stringsAsFactors = TRUE)
  for (col in seq_along(df)) {
    levels(df[[col]]) = c("a", "b", "c")
  }
  tsk = TaskClassif$new(id = "tsk", backend = df, target = "y")

  lrn$train(tsk)
  expect_list(lrn$model, names = "named")
  expect_numeric(lrn$model$weights, len = length(tsk$feature_names))
  prd = lrn$predict(tsk)
  expect_prediction(prd)


  df = data.frame(x = matrix(runif(90), nrow = 30), y = as.factor(sample(c(0, 1), 30, replace = TRUE)))
  tsk = TaskClassif$new(id = "tsk", backend = df, target = "y")

  for (predicttype in c("prob", "response")) {
    intask = (pipeline_greplicate(PipeOpLearnerCV$new(lrn("classif.rpart", predict_type = predicttype)), 3) %>>% PipeOpFeatureUnion$new())$train(tsk)[[1]]

    # Works for accuracy
    lrn = LearnerClassifAvg$new()
    lrn$predict_type = predicttype
    expect_learner(lrn)
    lrn$param_set$values$measure = msr("classif.acc")
    lrn$train(intask)
    expect_list(lrn$model, names = "named")
    expect_numeric(lrn$model$weights, len = 3)
    prd = lrn$predict(intask)
    expect_prediction(prd)

    lrn$predict_type = setdiff(c("prob", "response"), predicttype)
    expect_error(lrn$train(intask), "Trying to predict.*but incoming data has.*factors")

    if (predicttype == "prob") {
      # Works for area under the curve
      lrn = LearnerClassifAvg$new()
      lrn$predict_type = predicttype
      expect_learner(lrn)
      lrn$param_set$values$measure = msr("classif.auc")
      lrn$train(intask)
      expect_list(lrn$model, names = "named")
      expect_numeric(lrn$model$weights, len = 3)
      prd = lrn$predict(intask)
      expect_prediction(prd)
    }
  }
})

test_that("LearnerRegrAvg", {
  skip_on_cran()  # takes too long
  lrn = LearnerRegrAvg$new()
  expect_learner(lrn)
  df = data.frame(x = matrix(rnorm(100), nrow = 10), y = rnorm(100))
  colnames(df)[1:10] = paste0(letters[1:10], ".response")
  tsk = TaskRegr$new(id = "tsk", backend = df, target = "y")

  lrn$train(tsk)
  expect_list(lrn$model, names = "named")
  expect_numeric(lrn$model$weights, len = length(tsk$feature_names))
  prd = lrn$predict(tsk)
  expect_prediction(prd)

  df = data.frame(x = matrix(rnorm(200), nrow = 10), y = rnorm(100))
  colnames(df)[1:10] = paste0(letters[1:10], ".response")
  colnames(df)[11:20] = paste0(letters[1:10], ".se")
  for (i in 11:20) {
    # the shorter df[11:20] = abs(df[11:20]) gives a partial match warning :-/
    df[[i]] = abs(df[[i]])
  }
  tsk = TaskRegr$new(id = "tsk", backend = df, target = "y")

  lrn$predict_type = "response"
  lrn$train(tsk)
  expect_list(lrn$model, names = "named")
  expect_numeric(lrn$model$weights, len = length(tsk$feature_names) / 2)
  prd = lrn$predict(tsk)
  expect_prediction(prd)
  expect_true(all(is.na(prd$se)))

  intask = (pipeline_greplicate(PipeOpLearnerCV$new(lrn("regr.featureless", predict_type = "response")), 3) %>>%
    PipeOpFeatureUnion$new())$train(tsk("boston_housing_classic"))[[1]]

  # Works for accuracy
  lrn = LearnerRegrAvg$new()
  lrn$predict_type = "response"
  expect_learner(lrn)
  lrn$param_set$values$measure = msr("regr.mse")
  lrn$train(intask)
  expect_list(lrn$model, names = "named")
  expect_numeric(lrn$model$weights, len = 3)
  prd = lrn$predict(intask)
  expect_prediction(prd)
  expect_true(all(is.na(prd$se)))
})

test_that("LearnerClassifAvg Pipeline", {
  skip_if_not_installed("rpart")
  skip_on_cran()  # takes too long
  tsk = mlr_tasks$get("iris")
  # Works for response
  # TODO: this is a bit of a deep problem: https://github.com/mlr-org/mlr3pipelines/issues/216
  ## lrn = LearnerClassifAvg$new()
  ## single_pred = PipeOpSubsample$new() %>>%
  ##   PipeOpLearnerCV$new(lrn("classif.rpart"))
  ## pred_set = pipeline_greplicate(single_pred, 3L) %>>%
  ##   PipeOpFeatureUnion$new(innum = 3L, "union") %>>%
  ##   PipeOpLearner$new(lrn)
  ## expect_graph(pred_set)

  ## pred_set$train(tsk)
  ## expect_true(pred_set$is_trained)

  ## prd = pred_set$predict(tsk)[[1]]
  ## expect_prediction(prd)

  lrn = LearnerClassifAvg$new()
  graph = gunion(list(
      PipeOpLearnerCV$new(lrn("classif.rpart")),
      PipeOpLearnerCV$new(lrn("classif.featureless")))) %>>%
    PipeOpFeatureUnion$new() %>>%
    PipeOpLearner$new(lrn)
  expect_graph(graph)
  graph$train(tsk)
  expect_prediction(graph$predict(tsk)[[1]])

  glrn = GraphLearner$new(graph)
  expect_prediction(glrn$train(tsk)$predict(tsk))

  # Works for probabilities
  graph$pipeops$classif.rpart$learner$predict_type = "prob"
  graph$pipeops$classif.featureless$learner$predict_type = "prob"
  graph$pipeops$classif.avg$learner$predict_type = "prob"

  graph$train(tsk)
  prd = graph$predict(tsk)[[1]]
  expect_prediction(prd)

  glrn = GraphLearner$new(graph)
  expect_prediction(glrn$train(tsk)$predict(tsk))

})

test_that("LearnerRegrAvg Pipeline", {
  skip_if_not_installed("rpart")
  skip_on_cran()  # takes too long
  tsk = mlr_tasks$get("boston_housing_classic")
  # Works for response
  # TODO: this is a bit of a deep problem: https://github.com/mlr-org/mlr3pipelines/issues/216
  ## lrn = LearnerRegrAvg$new()
  ## single_pred = PipeOpSubsample$new() %>>%
  ##   PipeOpLearnerCV$new(lrn("regr.rpart"))
  ## pred_set = pipeline_greplicate(single_pred, 3L) %>>%
  ##   PipeOpFeatureUnion$new(innum = 3L, "union") %>>%
  ##   PipeOpLearner$new(lrn)
  ## expect_graph(pred_set)

  ## pred_set$train(tsk)
  ## expect_true(pred_set$is_trained)

  ## prd = pred_set$predict(tsk)[[1]]
  ## expect_prediction(prd)

  lrn = LearnerRegrAvg$new()
  graph = gunion(list(
    PipeOpLearnerCV$new(lrn("regr.rpart")),
    PipeOpLearnerCV$new(lrn("regr.featureless"))
  )) %>>%
    PipeOpFeatureUnion$new() %>>%
    PipeOpLearner$new(lrn)
  expect_graph(graph)
  graph$train(tsk)
  expect_prediction(graph$predict(tsk)[[1]])

  glrn = GraphLearner$new(graph, task_type = "regr")
  expect_prediction(glrn$train(tsk)$predict(tsk))

  # Works if some learners predict "se"
  graph$pipeops$regr.featureless$learner$predict_type = "se"

  graph$train(tsk)
  prd = graph$predict(tsk)[[1]]
  expect_prediction(prd)

  glrn = GraphLearner$new(graph, task_type = "regr")
  expect_prediction(glrn$train(tsk)$predict(tsk))

})
