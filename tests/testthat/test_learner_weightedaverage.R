context("WeighedAverage Learner")

test_that("LearnerClassifWeightedAverage manual", {
  lrn = LearnerClassifWeightedAverage$new()
  expect_learner(lrn)
  df = data.frame(x = matrix(runif(100), nrow = 10), y = as.factor(sample(c(0,1), 10, replace = TRUE)))
  tsk = TaskClassif$new(id = "tsk", backend = df, target = "y")


  # Works in default (equal weights)
  lrn$train(tsk)
  expect_list(lrn$model, names = "named")
  expect_numeric(lrn$model$weights, len = length(tsk$feature_names))
  prd_data = lrn$predict(tsk)
  expect_class(prd_data, "PredictionDataClassif")
  prd = new_prediction(tsk, prd_data)
  expect_prediction(prd)

  # Works for manually setting the weights
  lrn$param_set$values = list(weights.method = "manual", weights = runif(10))
  lrn$train(tsk)
  expect_list(lrn$model, names = "named")
  expect_numeric(lrn$model$weights, len = length(tsk$feature_names))
  prd_data = lrn$predict(tsk)
  expect_class(prd_data, "PredictionDataClassif")
  prd = new_prediction(tsk, prd_data)
  expect_prediction(prd)

  # Fails for wrong inputs
  lrn$param_set$values = list(weights.method = "manual", weights = c(1, 2))
  expect_error(lrn$train(tsk), "length")
  lrn$param_set$values = list(weights.method = "manual", weights = c("a"))
  expect_error(lrn$train(tsk), "numeric")
  lrn$param_set$values = list(weights.method = "manual", weights = c(-1))
  expect_error(lrn$train(tsk), "not >= 0")
  lrn$param_set$values = list(weights.method = "manual", weights = c(0))
  expect_error(lrn$train(tsk), " > 0")

})

test_that("LearnerClassifWeightedAverage NLOPTR", {

  df = data.frame(x = matrix(runif(100), nrow = 10), y = as.factor(sample(c(0,1), 10, replace = TRUE)))
  tsk = TaskClassif$new(id = "tsk", backend = df, target = "y")

  # Works for accuracy
  lrn = LearnerClassifWeightedAverage$new()
  expect_learner(lrn)
  lrn$param_set$values = list(weights.method = "nloptr", measure = "classif.acc", algorithm = "NLOPT_LN_COBYLA")
  lrn$train(tsk)
  expect_list(lrn$model, names = "named")
  expect_numeric(lrn$model$weights, len = length(tsk$feature_names))
  prd_data = lrn$predict(tsk)
  expect_class(prd_data, "PredictionDataClassif")
  prd = new_prediction(tsk, prd_data)
  expect_prediction(prd)


  # Works for area under the curve
  lrn = LearnerClassifWeightedAverage$new()
  expect_learner(lrn)
  lrn$param_set$values = list(weights.method = "nloptr", measure = "classif.auc", algorithm = "NLOPT_LN_COBYLA")
  lrn$train(tsk)
  expect_list(lrn$model, names = "named")
  expect_numeric(lrn$model$weights, len = length(tsk$feature_names))
  prd_data = lrn$predict(tsk)
  expect_class(prd_data, "PredictionDataClassif")
  prd = new_prediction(tsk, prd_data)
  expect_prediction(prd)

  # Works with response / prob mixed:
  df = data.frame(x = matrix(runif(100), nrow = 10), y = as.factor(sample(c(0,1), 10, replace = TRUE)))
  df$x11 = as.factor(sample(0:1, 10, replace = TRUE))
  tsk = TaskClassif$new(id = "tsk", backend = df, target = "y")
  lrn = LearnerClassifWeightedAverage$new()
  expect_learner(lrn)
  lrn$param_set$values = list(weights.method = "nloptr", measure = "classif.acc", algorithm = "NLOPT_LN_COBYLA")
  lrn$train(tsk)
  expect_list(lrn$model, names = "named")
  expect_numeric(lrn$model$weights, len = length(tsk$feature_names))
  prd_data = lrn$predict(tsk)
  expect_class(prd_data, "PredictionDataClassif")
  prd = new_prediction(tsk, prd_data)
  expect_prediction(prd)
})



test_that("LearnerClassifWeightedAverage Pipeline", {

  tsk = mlr_tasks$get("iris")

  # Works for response
  lrn = LearnerClassifWeightedAverage$new()
  single_pred = PipeOpSubsample$new() %>>%
    PipeOpLearnerCV$new(mlr_learners$get("classif.rpart"))
  pred_set = greplicate(single_pred, 3L) %>>%
    PipeOpFeatureUnion$new(innum = 3L, "union") %>>%
    PipeOpLearner$new(lrn)
  expect_graph(pred_set)

  pred_set$train(tsk)
  expect_true(pred_set$is_trained)

  prd_data = pred_set$predict(tsk)
  expect_class(prd_data[[1]], "PredictionDataClassif")
  prd = new_prediction(tsk, prd_data[[1]])
  expect_prediction(prd)


  # Works for probabilities
  lrn = LearnerClassifWeightedAverage$new()
  lrn$param_set$values = list(weights.method = "nloptr", measure = "classif.acc", algorithm = "NLOPT_LN_COBYLA")

  rp = mlr_learners$get("classif.rpart")
  rp$predict_type = "prob"
  single_pred = PipeOpSubsample$new() %>>%
    PipeOpLearnerCV$new(rp)
  pred_set = greplicate(single_pred, 3L) %>>%
    PipeOpFeatureUnion$new(innum = 3L, "union") %>>%
    PipeOpLearner$new(lrn)
  expect_graph(pred_set)

  pred_set$train(tsk)
  expect_true(pred_set$is_trained)

  prd_data = pred_set$predict(tsk)
  expect_class(prd_data[[1]], "PredictionDataClassif")
  prd = new_prediction(tsk, prd_data[[1]])
  expect_prediction(prd)
})


test_that("LearnerClassifWeightedAverage Bagging Usecase", {

  # Bagging requires setting resampling to "nocv"
  subsampled_tree = PipeOpSubsample$new() %>>%
    PipeOpLearnerCV$new(mlr_learners$get("classif.rpart"), param_vals = list(resampling.resampling = "nocv"))
  pred_set = greplicate(subsampled_tree, 3L) %>>%
    PipeOpFeatureUnion$new(innum = 3L, "union") %>>%
    PipeOpLearner$new(LearnerClassifWeightedAverage$new())
  expect_graph(pred_set)

  pred_set$train(tsk)
  expect_true(pred_set$is_trained)

  prd_data = pred_set$predict(tsk)
  expect_class(prd_data[[1]], "PredictionDataClassif")
  prd = new_prediction(tsk, prd_data[[1]])
  expect_prediction(prd)
})

test_that("LearnerClassifWeightedAverage autotest", {
  lrn = LearnerClassifWeightedAverage$new()
  expect_learner(lrn)
  lrn$param_set$values = list(weights.method = "nloptr", measure = "classif.acc", algorithm = "NLOPT_LN_COBYLA")
  result = run_autotest(lrn, exclude = "(sanity|feat_single|feat_all_multiclass)")
  expect_true(result, info = result$error)
})


test_that("LearnerClassifWeightedAverage Pipeline", {

  tsk = mlr_tasks$get("boston_housing")

  # Works for response
  lrn = LearnerRegrWeightedAverage$new()
  single_pred = PipeOpSubsample$new() %>>%
    PipeOpLearnerCV$new(mlr_learners$get("regr.featureless"))
  pred_set = greplicate(single_pred, 3L) %>>%
    PipeOpFeatureUnion$new(innum = 3L, "union") %>>%
    PipeOpLearner$new(lrn)
  expect_graph(pred_set)

  pred_set$train(tsk)
  expect_true(pred_set$is_trained)

  prd_data = pred_set$predict(tsk)
  expect_class(prd_data[[1]], "PredictionDataRegr")
  prd = new_prediction(tsk, prd_data[[1]])
  expect_prediction(prd)


  # Works for "se"
  lrn = LearnerRegrWeightedAverage$new()
  lrn$param_set$values = list(weights.method = "nloptr", measure = "classif.acc", algorithm = "NLOPT_LN_COBYLA", est.se = TRUE)

  ftless = mlr_learners$get("regr.featureless")
  ftless$predict_type = "se"
  single_pred = PipeOpSubsample$new() %>>%
    PipeOpLearnerCV$new(ftless)
  pred_set = greplicate(single_pred, 3L) %>>%
    PipeOpFeatureUnion$new(innum = 3L, "union") %>>%
    PipeOpLearner$new(lrn)
  expect_graph(pred_set)

  pred_set$train(tsk)
  expect_true(pred_set$is_trained)

  prd_data = pred_set$predict(tsk)
  expect_class(prd_data[[1]], "PredictionDataRegr")
  prd = new_prediction(tsk, prd_data[[1]])
  expect_prediction(prd)
})
