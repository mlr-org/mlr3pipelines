context("WeighedAverage Learner")

test_that("LearnerClassifAvg", {
  lrn = LearnerClassifAvg$new()
  expect_learner(lrn)
  df = data.frame(x = matrix(sample(c("a", "b", "c"), 100, replace = TRUE), nrow = 10), y = as.factor(sample(c("a", "b", "c"), 10, replace = TRUE)))
  for (col in seq_along(df)) {
    levels(df[[col]]) = c("a", "b", "c")
  }
  tsk = TaskClassif$new(id = "tsk", backend = df, target = "y")

  lrn$train(tsk)
  expect_list(lrn$model, names = "named")
  expect_numeric(lrn$model$weights, len = length(tsk$feature_names))
  prd = lrn$predict(tsk)
  expect_prediction(prd)


  df = data.frame(x = matrix(runif(100), nrow = 10), y = as.factor(sample(c(0,1), 10, replace = TRUE)))
  tsk = TaskClassif$new(id = "tsk", backend = df, target = "y")

  for (predicttype in c("prob", "response")) {
    intask = (greplicate(PipeOpLearnerCV$new(mlr_learners$get("classif.rpart", predict_type = predicttype)), 3) %>>% PipeOpFeatureUnion$new())$train(tsk)[[1]]

    # Works for accuracy
    lrn = LearnerClassifAvg$new()
    lrn$predict_type = predicttype
    expect_learner(lrn)
    lrn$param_set$values = list(measure = "classif.acc", algorithm = "NLOPT_LN_COBYLA")
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
      lrn$param_set$values = list(measure = "classif.auc", algorithm = "NLOPT_LN_COBYLA")
      lrn$train(intask)
      expect_list(lrn$model, names = "named")
      expect_numeric(lrn$model$weights, len = 3)
      prd = lrn$predict(intask)
      expect_prediction(prd)
    }
  }

})

test_that("LearnerRegrAvg", {
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

  lrn$predict_type = "se"
  lrn$param_set$values$est_se = "both"
  expect_error(lrn$train(tsk), "is 'both', but not all incoming Learners")
  lrn$param_set$values$est_se = "within"
  expect_error(lrn$train(tsk), "is 'within', but not all incoming Learners")
  lrn$param_set$values$est_se = "between"
  lrn$train(tsk)
  expect_list(lrn$model, names = "named")
  expect_numeric(lrn$model$weights, len = length(tsk$feature_names))
  prd = lrn$predict(tsk)
  expect_prediction(prd)
  expect_numeric(prd$se, lower = 0, any.missing = FALSE)

  df = data.frame(x = matrix(rnorm(200), nrow = 10), y = rnorm(100))
  colnames(df)[1:10] = paste0(letters[1:10], ".response")
  colnames(df)[11:20] = paste0(letters[1:10], ".se")
  df[11:20] = abs(df[11:20])
  tsk = TaskRegr$new(id = "tsk", backend = df, target = "y")

  lrn$predict_type = "response"
  lrn$train(tsk)
  expect_list(lrn$model, names = "named")
  expect_numeric(lrn$model$weights, len = length(tsk$feature_names) / 2)
  prd = lrn$predict(tsk)
  expect_prediction(prd)
  expect_true(all(is.na(prd$se)))

  lrn$predict_type = "se"
  for (type in c("both", "within", "between")) {
    lrn$param_set$values$est_se = type
    lrn$train(tsk)
    expect_list(lrn$model, names = "named")
    expect_numeric(lrn$model$weights, len = length(tsk$feature_names) / 2)
    prd = lrn$predict(tsk)
    expect_prediction(prd)
    expect_numeric(prd$se, lower = 0, any.missing = FALSE)
  }

  semeas = R6Class("MeasureRegrScaledRMSE",
    inherit = MeasureRegr,
    public = list(
        initialize = function() {
        super$initialize(
          id = "regr.srmse",
          range = c(0, Inf),
          minimize = TRUE,
          predict_type = "se"
        )
      },

      score_internal = function(prediction, ...) {
        sqrt(mean(((prediction$truth - prediction$response) / prediction$se)^2))
      }
    )
  )$new()

  for (predicttype in c("se", "response")) {
    intask = (greplicate(PipeOpLearnerCV$new(mlr_learners$get("regr.featureless", predict_type = predicttype)), 3) %>>% PipeOpFeatureUnion$new())$train("boston_housing")[[1]]

    # Works for accuracy
    lrn = LearnerRegrAvg$new()
    lrn$predict_type = predicttype
    expect_learner(lrn)
    lrn$param_set$values = list(measure = "regr.mse", algorithm = "NLOPT_LN_COBYLA", est_se = "both")
    lrn$train(intask)
    expect_list(lrn$model, names = "named")
    expect_numeric(lrn$model$weights, len = 3)
    prd = lrn$predict(intask)
    expect_prediction(prd)
    if (predicttype == "se") {
      expect_numeric(prd$se, lower = 0, any.missing = FALSE)
    } else {
      expect_true(all(is.na(prd$se)))
    }

    lrn$predict_type = "se"
    lrn$param_set$values$est_se = "between"
    lrn$param_set$values$measure = semeas

    lrn$train(intask)
    expect_list(lrn$model, names = "named")
    expect_numeric(lrn$model$weights, len = 3)
    prd = lrn$predict(intask)
    expect_prediction(prd)
    expect_numeric(prd$se, lower = 0, any.missing = FALSE)
  }

})

test_that("LearnerClassifAvg Pipeline", {
  tsk = mlr_tasks$get("iris")
  # Works for response
  # TODO: this is a bit of a deep problem: https://github.com/mlr-org/mlr3pipelines/issues/216
  ## lrn = LearnerClassifAvg$new()
  ## single_pred = PipeOpSubsample$new() %>>%
  ##   PipeOpLearnerCV$new(mlr_learners$get("classif.rpart"))
  ## pred_set = greplicate(single_pred, 3L) %>>%
  ##   PipeOpFeatureUnion$new(innum = 3L, "union") %>>%
  ##   PipeOpLearner$new(lrn)
  ## expect_graph(pred_set)

  ## pred_set$train(tsk)
  ## expect_true(pred_set$is_trained)

  ## prd = pred_set$predict(tsk)[[1]]
  ## expect_prediction(prd)

  lrn = LearnerClassifAvg$new()
  graph = gunion(list(
      PipeOpLearnerCV$new("classif.rpart"),
      PipeOpLearnerCV$new("classif.featureless"))) %>>%
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
  graph$pipeops$classif.weightedavg$learner$predict_type = "prob"

  graph$train(tsk)
  prd = graph$predict(tsk)[[1]]
  expect_prediction(prd)

  glrn = GraphLearner$new(graph)
  expect_prediction(glrn$train(tsk)$predict(tsk))

})

test_that("LearnerRegrAvg Pipeline", {
  tsk = mlr_tasks$get("boston_housing")
  # Works for response
  # TODO: this is a bit of a deep problem: https://github.com/mlr-org/mlr3pipelines/issues/216
  ## lrn = LearnerRegrAvg$new()
  ## single_pred = PipeOpSubsample$new() %>>%
  ##   PipeOpLearnerCV$new(mlr_learners$get("regr.rpart"))
  ## pred_set = greplicate(single_pred, 3L) %>>%
  ##   PipeOpFeatureUnion$new(innum = 3L, "union") %>>%
  ##   PipeOpLearner$new(lrn)
  ## expect_graph(pred_set)

  ## pred_set$train(tsk)
  ## expect_true(pred_set$is_trained)

  ## prd = pred_set$predict(tsk)[[1]]
  ## expect_prediction(prd)

  lrn = LearnerRegrAvg$new()
  graph = gunion(list(
      PipeOpLearnerCV$new("regr.rpart"),
      PipeOpLearnerCV$new("regr.featureless"))) %>>%
    PipeOpFeatureUnion$new() %>>%
    PipeOpLearner$new(lrn)
  expect_graph(graph)
  graph$train(tsk)
  expect_prediction(graph$predict(tsk)[[1]])

  glrn = GraphLearner$new(graph, task_type = "regr")
  expect_prediction(glrn$train(tsk)$predict(tsk))

  # Works for probabilities
  graph$pipeops$regr.weightedavg$learner$predict_type = "se"

  expect_error(graph$train(tsk), "'both'.*not all incoming.*'se'-prediction")
  graph$param_set$values$regr.weightedavg.est_se = "between"
  graph$train(tsk)
  prd = graph$predict(tsk)[[1]]
  expect_prediction(prd)

  glrn = GraphLearner$new(graph, task_type = "regr")
  expect_prediction(glrn$train(tsk)$predict(tsk))

})
