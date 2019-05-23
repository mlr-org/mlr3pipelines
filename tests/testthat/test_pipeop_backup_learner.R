context("PipeOpBackupLearner")


test_that("PipeOpBackupLearner - basic properties", {
  po = PipeOpBackupLearner$new()

  expect_pipeop(po)

  expect_data_table(po$input, nrow = 2)
  expect_data_table(po$output, nrow = 1)

  task = mlr_tasks$get("iris")
  regr_task = mlr_tasks$get("boston_housing")

  result = train_pipeop(po, list(learnerin = NULL, taskin = task))
  expect_equal(unname(result), list(NULL))

  demo_classif = mlr_learners$get("classif.featureless", predict_type = "prob")$train(task)$predict(task)
  demo_regr = mlr_learners$get("regr.featureless")$train(regr_task)$predict(regr_task)

  orig_prob = demo_classif$prob
  orig_se = demo_regr$se
  orig_regr_response = demo_regr$response
  before_clone = demo_regr$clone(deep = TRUE)

  demo_classif$response[1:3] = NA
  demo_classif$prob[1:3, ] = NA

  demo_regr$response[1:3] = NA
  demo_regr$se[1:3] = NA

  demo_classif_orig = demo_classif$clone(deep = TRUE)
  demo_regr_orig = demo_regr$clone(deep = TRUE)

  repaired = po$predict(list(demo_classif, task))[[1]]

  expect_equal(repaired$prob, orig_prob)
  expect_true(!any(is.na(repaired$response)))

  po_regr = PipeOpBackupLearner$new(learner = mlr_learners$get("regr.featureless"))
  expect_equal(unname(po_regr$train(list(NULL, regr_task))), list(NULL))
  repaired_regr = po_regr$predict(list(demo_regr, regr_task))[[1]]

  expect_equal(orig_se, repaired_regr$se)
  expect_equal(orig_regr_response, repaired_regr$response)
  expect_equal(before_clone, repaired_regr)

  po_regr = PipeOpBackupLearner$new(learner = mlr_learners$get("regr.rpart"))

  set.seed(1)
  expect_equal(unname(po_regr$train(list(NULL, regr_task))), list(NULL))

  repaired_regr = po_regr$predict(list(demo_regr, regr_task))[[1]]

  set.seed(1)
  normal_prediction = mlr_learners$get("regr.rpart")$train(regr_task)$predict(regr_task)

  expect_equal(unname(repaired_regr$response[1:3]), unname(normal_prediction$response[1:3]))

  expect_true(all(is.na(unname(repaired_regr$se[1:3]))))

  expect_equal(repaired_regr$response[-(1:3)], demo_regr$response[-(1:3)])

  # check that pipeops did not modify input objects
  expect_deep_clone(demo_regr_orig, demo_regr)
  expect_deep_clone(demo_classif_orig, demo_classif)
})

test_that("PipeOpBackupLearner usage", {
  testthat::skip("skip until mlr3 allows this")

  lrn = mlr3learners::LearnerRegrLm$new()

  graph = mlr_pipeops$get("copy", 2) %>>%
    gunion(list(
      mlr_pipeops$get("learner", lrn),
      mlr_pipeops$get("null"))) %>>%
    mlr_pipeops$get("backuplearner", mlr_learners$get("regr.featureless"))

  bh = mlr_tasks$get("boston_housing")
  graph$train(bh)

  bh_missings = bh$data()
  bh_missings[2:4, 2:4] = NA

  ptask = TaskRegr$new("bh_missings", as_data_backend(bh_missings), "medv")

  badprediction = lrn$train(bh)$predict(ptask)

  expect_equal(which(is.na(badprediction$response)), 2:4)

  goodprediction = graph$predict(ptask)[[1]]

  expect_true(all(!is.na(goodprediction$response)))

  expect_equal(goodprediction$response[-(2:4)], badprediction$response[-(2:4)])
})
