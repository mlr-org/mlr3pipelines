context("GraphLearn")

test_that("basic graphlearn tests", {

  task = mlr_tasks$get("iris")

  lrn = mlr_learners$get("classif.rpart")
  gr = PipeOpLearner$new(lrn)

  glrn = GraphLearner$new(gr)
  expect_learner_fits(glrn, task)

  glrn = GraphLearner$new(gr)
  glrn$train(task)
  expect_prediction_classif({graphpred = glrn$predict(task)})
  expect_equal(graphpred,
    lrn$train(task)$predict(task))

  set.seed(1)
  resgraphlrn = resample(task, lrn, mlr_resamplings$get("cv"))
  set.seed(1)
  resjustlrn = resample(task, lrn, mlr_resamplings$get("cv"))
  expect_equal(resgraphlrn$data$prediction, resjustlrn$data$prediction)

  gr2 = PipeOpScale$new() %>>% PipeOpLearner$new(lrn)
  glrn2 = GraphLearner$new(gr2)
  expect_learner_fits(glrn, task)
  glrn2$train(task)
  expect_prediction_classif({graphpred2 = glrn2$predict(task)})

})
