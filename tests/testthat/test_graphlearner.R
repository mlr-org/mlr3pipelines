context("GraphLearn")

test_that("basic graphlearn tests", {
  task = mlr_tasks$get("iris")

  lrn = mlr_learners$get("classif.rpart")
  gr = PipeOpLearner$new(lrn)

  glrn = GraphLearner$new(gr)
  expect_true(run_experiment(task, glrn)$ok)

  glrn = GraphLearner$new(gr)
  glrn$train(task)

  expect_prediction_classif({
    graphpred = glrn$predict(task)
  })
  expect_equal(graphpred,
    lrn$train(task)$predict(task))

  expect_prediction_classif({
    graphpred = glrn$predict(task)
  })
  expect_equal(graphpred, lrn$predict(task))

  set.seed(1)
  lrn = mlr_learners$get("classif.rpart")
  resgraphlrn = resample(task, lrn, mlr_resamplings$get("cv"))
  set.seed(1)
  resjustlrn = resample(task, lrn, mlr_resamplings$get("cv"))
  expect_equal(resgraphlrn$data$prediction, resjustlrn$data$prediction)

  gr2 = PipeOpScale$new() %>>% PipeOpLearner$new(lrn)
  glrn2 = GraphLearner$new(gr2)
  expect_true(run_experiment(task, glrn)$ok)
  glrn2$train(task)
  expect_prediction_classif({
    graphpred2 = glrn2$predict(task)
  })

  scidf = cbind(scale(iris[1:4]), iris[5])
  scalediris = TaskClassif$new("scalediris", as_data_backend(scidf), "Species")

  dblrn = mlr_learners$get("classif.debug")
  dblrn$param_set$values$save_tasks = TRUE

  dbgr = GraphLearner$new(PipeOpScale$new() %>>% PipeOpLearner$new(dblrn))


  expect_equal(dbgr$train(task), dbgr)

  # debuglearner predict() modifies model, but PipeOpLearner does not accept
  # model changes in predict phase, so would ordinarily discard the change.
  # Here we swap the debuglearner model by an environment, which gets updated
  # by-reference, so we can get the $task_predict slot eventually.
  dbmodels = as.environment(dbgr$model$classif.debug$model)
  dbgr$state$model$classif.debug$model = dbmodels

  dbgr$predict(task)

  expect_equal(dbmodels$task_train$data(), scalediris$data())
  expect_equal(dbmodels$task_predict$data(), scalediris$data())
})

test_that("graphlearner parameters behave as they should", {
  dblrn = mlr_learners$get("classif.debug")
  dblrn$param_set$values$save_tasks = TRUE

  dbgr = PipeOpScale$new() %>>% PipeOpLearner$new(dblrn)

  expect_subset(c("scale.center", "scale.scale", "classif.debug.x"), names(dbgr$param_set$params))

  dbgr$param_set$values$classif.debug.x = 1

  expect_equal(dbgr$param_set$values$classif.debug.x, 1)
  expect_equal(dbgr$pipeops$classif.debug$param_set$values$x, 1)
  expect_equal(dbgr$pipeops$classif.debug$learner$param_set$values$x, 1)

  dbgr$pipeops$classif.debug$param_set$values$x = 0

  expect_equal(dbgr$param_set$values$classif.debug.x, 0)
  expect_equal(dbgr$pipeops$classif.debug$param_set$values$x, 0)
  expect_equal(dbgr$pipeops$classif.debug$learner$param_set$values$x, 0)

  dbgr$pipeops$classif.debug$learner$param_set$values$x = 0.5

  expect_equal(dbgr$param_set$values$classif.debug.x, 0.5)
  expect_equal(dbgr$pipeops$classif.debug$param_set$values$x, 0.5)
  expect_equal(dbgr$pipeops$classif.debug$learner$param_set$values$x, 0.5)

  expect_error({
    dbgr$param_set$values$classif.debug.x = "a"
  })
  expect_error({
    dbgr$pipeops$classif.debug$param_set$values$x = "a"
  })
  expect_error({
    dbgr$pipeops$classif.debug$learner$param_set$values$x = "a"
  })

  expect_equal(dbgr$param_set$values$classif.debug.x, 0.5)
  expect_equal(dbgr$pipeops$classif.debug$param_set$values$x, 0.5)
  expect_equal(dbgr$pipeops$classif.debug$learner$param_set$values$x, 0.5)

  dblrn = mlr_learners$get("classif.debug")
  dblrn$param_set$values$message_train = 1
  dblrn$param_set$values$message_predict = 1
  dblrn$param_set$values$warning_train = 1
  dblrn$param_set$values$warning_predict = 1

  pol = PipeOpLearner$new(dblrn, param_vals = list(message_predict = 0, warning_train = 0, warning_predict = 0))

  gl = GraphLearner$new(pol, param_vals = list(classif.debug.warning_train = 1, classif.debug.warning_predict = 1))

  gl$param_set$values$classif.debug.warning_predict = 0

  expect_equal(gl$param_set$values,
    list(classif.debug.message_train = 1, classif.debug.message_predict = 0, classif.debug.warning_train = 1, classif.debug.warning_predict = 0))
})

test_that("graphlearner type inference", {

  # default: classif
  lrn = GraphLearner$new(mlr_pipeops$get("nop"))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  ###########
  # classif #
  ###########

  # inference from pipeoplearner
  lrn = GraphLearner$new(mlr_pipeops$get("learner", "classif.rpart"))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  # inference from output only
  lrn = GraphLearner$new(mlr_pipeops$get("copy", 1) %>>% mlr_pipeops$get("learner", "classif.rpart"))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  # inference from input only
  lrn = GraphLearner$new(mlr_pipeops$get("learner", "classif.rpart") %>>% mlr_pipeops$get("copy", 1))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  # inference when multiple input, but one is a Task
  lrn = GraphLearner$new(gunion(list(mlr_pipeops$get("learner", "classif.rpart"), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch"))
  expect_equal(lrn$task_type, "classif")
  expect_equal(lrn$predict_type, "response")

  ###########
  # regr    #
  ###########

  # inference from pipeoplearner
  lrn = GraphLearner$new(mlr_pipeops$get("learner", "regr.rpart"))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  # inference from output only
  lrn = GraphLearner$new(mlr_pipeops$get("copy", 1) %>>% mlr_pipeops$get("learner", "regr.rpart"))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  # inference from input only
  lrn = GraphLearner$new(mlr_pipeops$get("learner", "regr.rpart") %>>% mlr_pipeops$get("copy", 1))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  # inference when multiple input, but one is a Task
  lrn = GraphLearner$new(gunion(list(mlr_pipeops$get("learner", "regr.rpart"), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch"))
  expect_equal(lrn$task_type, "regr")
  expect_equal(lrn$predict_type, "response")

  ###########
  # Errors  #
  ###########

  # input, output mismatching types
  gr = gunion(list(mlr_pipeops$get("learner", "regr.rpart"), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch") %>>% mlr_pipeops$get("learner", "classif.rpart")
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  gr = gunion(list(mlr_pipeops$get("learner", "classif.rpart"), mlr_pipeops$get("nop"))) %>>% mlr_pipeops$get("unbranch") %>>% mlr_pipeops$get("learner", "regr.rpart")
  expect_error(GraphLearner$new(gr), "multiple possibilities")

  # input two mismatching types
  gr = gunion(list(mlr_pipeops$get("learner", "classif.rpart"), mlr_pipeops$get("learner", "regr.rpart"))) %>>% mlr_pipeops$get("unbranch")
  expect_error(GraphLearner$new(gr), "multiple possibilities")
})
