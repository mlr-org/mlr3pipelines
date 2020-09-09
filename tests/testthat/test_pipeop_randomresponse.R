context("PipeOpRandomResponse")


test_that("basic properties", {
  expect_pipeop_class(PipeOpRandomResponse)
  expect_error(PipeOpCopy$new(param_vals = list(rdistfun = function(n) {})))

  po = PipeOpRandomResponse$new()
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)
})


test_that("train and predict", {
  task1 = mlr_tasks$get("iris")
  task1$row_roles$use = c(1:10, 140:150)
  g1 = LearnerClassifRpart$new() %>>% PipeOpRandomResponse$new()
  g1$pipeops$classif.rpart$learner$predict_type = "prob"
  train_out1 = g1$train(task1)
  expect_list(train_out1)
  expect_null(train_out1[[1L]])
  predict_out1 = g1$predict(task1)
  expect_list(predict_out1)
  expect_prediction(predict_out1[[1L]])
  expect_equal(task1$data(cols = "Species")[[1L]], predict_out1[[1L]]$truth)
  expect_factor(predict_out1[[1L]]$response, levels = levels(predict_out1[[1L]]$truth), ordered = is.ordered(predict_out1[[1L]]$truth))
  expect_null(predict_out1[[1L]]$prob)

  learner1 = LearnerClassifRpart$new()
  learner1$train(task1)
  g1x = LearnerClassifRpart$new() %>>% PipeOpRandomResponse$new()
  g1x$train(task1)
  expect_equal(g1x$predict(task1)[[1L]], learner1$predict(task1))

  requireNamespace("mlr3learners")
  task2 = mlr_tasks$get("mtcars")
  g2 = mlr3learners::LearnerRegrLM$new() %>>% PipeOpRandomResponse$new()
  g2$pipeops$regr.lm$learner$predict_type = "se"
  train_out2 = g2$train(task2)
  expect_list(train_out2)
  expect_null(train_out2[[1L]])
  predict_out2 = g2$predict(task2)
  expect_list(predict_out2)
  expect_prediction(predict_out2[[1L]])
  expect_equal(task2$data(cols = "mpg")[[1L]], predict_out2[[1L]]$truth)
  expect_numeric(predict_out2[[1L]]$response, finite = TRUE, any.missing = FALSE)
  expect_numeric(na.omit(predict_out2[[1L]]$se), len = 0L)

  g2$pipeops$randomresponse$param_set$values$rdistfun = function(n, mean, sd) {
    expect_equal(length(mean), n)
    expect_equal(length(sd), n)
    runif(n, min = 0, max = 1)
  }
  expect_numeric(g2$predict(task2)[[1L]]$response, lower = 0, upper = 1, any.missing = FALSE)

  learner2 = LearnerRegrLM$new()
  learner2$train(task2)
  g2x = LearnerRegrLM$new() %>>% PipeOpRandomResponse$new()
  g2x$train(task2)
  expect_equal(g2x$predict(task2)[[1L]], learner2$predict(task2))
})
