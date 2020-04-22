context("PipeOpPredictionUnion")

test_that("PipeOpPredictionUnion - basic properties", {
  po = PipeOpPredictionUnion$new(3)
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 3)
  expect_data_table(po$output, nrows = 1)

  expect_pipeop_class(PipeOpPredictionUnion, list(1))
  expect_pipeop_class(PipeOpPredictionUnion, list(3))

  po = PipeOpPredictionUnion$new()
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  po = PipeOpPredictionUnion$new(innum = "test")
  expect_equal(po$input$name, "input1")

  prediction = PredictionRegr$new(row_ids = 1, truth = 1, response = 1)
  prediction$task_type = "test"
  expect_error(po$predict(list(prediction)), regexp = "task types")
})

test_that("PipeOpPredictionUnion - train and predict classif", {
  set.seed(1)
  ids = sample(150, size = 75)
  task1 = mlr_tasks$get("iris")
  task2 = task1$clone(deep = TRUE)
  task1$filter(ids)
  learner1 = mlr_learners$get("classif.rpart")
  learner2 = learner1$clone(deep = TRUE)
  learner1$train(task1)
  learner2$train(task2)

  po = PipeOpPredictionUnion$new(2)
  expect_null(po$train(list(NULL, NULL))[[1L]])

  predict_out1 = po$predict(list(learner1$predict(task1), learner2$predict(task2)))[[1L]]
  expect_equal(predict_out1$row_ids, c(task1$row_ids, task2$row_ids))
  expect_equal(predict_out1$truth,
    unlist(list(learner1$predict(task1)$truth, learner2$predict(task2)$truth), use.names = FALSE))
  expect_equal(predict_out1$response,
    unlist(list(learner1$predict(task1)$response, learner2$predict(task2)$response), use.names = FALSE))

  learner1$predict_type = "prob"
  learner2$predict_type = "prob"

  predict_out2 = po$predict(list(learner1$predict(task1), learner2$predict(task2)))[[1L]]
  expect_equal(predict_out2$prob,
    do.call(rbind, list(learner1$predict(task1)$prob, learner2$predict(task2)$prob)))

  learner1$predict_type = "response"
  expect_error(po$predict(list(learner1$predict(task1), learner2$predict(task2))), regexp = "same task type and predict types")
})

test_that("PipeOpPredictionUnion - train and predict regr", {
  library(mlr3learners)
  set.seed(1)
  ids = sample(32, size = 15)
  task1 = mlr_tasks$get("mtcars")
  task2 = task1$clone(deep = TRUE)
  task1$filter(ids)
  learner1 = mlr_learners$get("regr.lm")
  learner2 = learner1$clone(deep = TRUE)
  learner1$train(task1)
  learner2$train(task2)

  po = PipeOpPredictionUnion$new(2)
  expect_null(po$train(list(NULL, NULL))[[1L]])

  predict_out1 = po$predict(list(learner1$predict(task1), learner2$predict(task2)))[[1L]]
  expect_equal(predict_out1$row_ids, c(task1$row_ids, task2$row_ids))
  expect_equal(predict_out1$truth,
    unlist(list(learner1$predict(task1)$truth, learner2$predict(task2)$truth), use.names = FALSE))
  expect_equal(predict_out1$response,
    unlist(list(learner1$predict(task1)$response, learner2$predict(task2)$response), use.names = FALSE))

  learner1$predict_type = "se"
  learner2$predict_type = "se"

  predict_out2 = po$predict(list(learner1$predict(task1), learner2$predict(task2)))[[1L]]
  expect_equal(predict_out2$se,
    unlist(list(learner1$predict(task1)$se, learner2$predict(task2)$se), use.names = FALSE))

  learner1$predict_type = "response"
  expect_error(po$predict(list(learner1$predict(task1), learner2$predict(task2))), regexp = "same task type and predict types")
})

test_that("PipeOpFilterRows and PipeOpPredictionUnion - use case pima", {
  task = mlr_tasks$get("pima")
  age_ids = which(task$data(cols = "age")[[1L]] < median(task$data(cols = "age")[[1L]]))
  na_ids = which(rowSums(is.na(task$data())) > 0L)
  filter = expression(age < median(age))

  g = PipeOpCopy$new(2) %>>%
    gunion(list(
      PipeOpFilterRows$new("filter1", param_vals = list(filter = filter, na_column = "_all_", skip_during_predict = FALSE)) %>>%
        PipeOpLearner$new(LearnerClassifRpart$new(), "learner1"),
      PipeOpFilterRows$new("filter2", param_vals = list(filter = filter, na_column = "_all_", invert = TRUE, skip_during_predict = FALSE)) %>>%
        PipeOpLearner$new(LearnerClassifRpart$new(), "learner2"))
    ) %>>%
    PipeOpPredictionUnion$new()

  expect_null(g$train(task)[[1L]])
  expect_equal(g$state$filter1$na_ids, na_ids)
  expect_equal(g$state$filter2$na_ids, na_ids)
  expect_equal(g$state$filter1$row_ids, age_ids[age_ids %nin% na_ids])
  expect_equal(g$state$filter2$row_ids, setdiff(1:768, age_ids)[setdiff(1:768, age_ids) %nin% na_ids])

  predict_out = g$predict(task)[[1L]]
  expect_prediction(predict_out)
  expect_setequal(predict_out$row_ids, setdiff(1:768, na_ids))
})
