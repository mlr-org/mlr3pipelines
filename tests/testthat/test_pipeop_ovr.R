context("PipeOpOVRSplit")

test_that("PipeOpOVRSplit - basic properties", {
  po = PipeOpOVRSplit$new()
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  expect_pipeop_class(PipeOpOVRSplit)
})

test_that("PipeOpOVRSplit - train and predict", {
  # toy task to split
  dat = data.table(target = as.factor(rep(c("a", "b", "rest"), each = 10)), feature = rnorm(30))
  tsk = TaskClassif$new("test", backend = dat, target = "target")
  po = PipeOpOVRSplit$new()

  tout = train_pipeop(po, list(tsk))
  expect_equal(po$state$levels, tsk$class_names)
  expect_multiplicity(tout[[1]])
  expect_list(tout[[1]], len = 3)
  expect_named(tout[[1]], tsk$class_names)
  expect_true(all(pmap_lgl(list(tout[[1]], names(tout[[1]])), .f = function(task, name) {
    expect_task(task)
      all(task$target_names == tsk$target_names) && task$positive == name && task$negative == "rest." &&
      all.equal(task$truth(), factor(ifelse(tsk$truth() == task$positive, task$positive, "rest."), levels = c(task$positive, "rest.")))
  })))

  pout = predict_pipeop(po, list(tsk))
  expect_multiplicity(pout[[1]])
  expect_list(pout[[1]], len = 3)
  expect_named(pout[[1]], tsk$class_names)
  expect_true(all(pmap_lgl(list(pout[[1]], names(pout[[1]])), .f = function(task, name) {
    expect_task(task)
    task$target_names == tsk$target_names && task$positive == name && task$negative == "rest." &&
      all.equal(task$truth(), factor(ifelse(tsk$truth() == task$positive, task$positive, "rest."), levels = c(task$positive, "rest.")))
  })))
})

context("PipeOpOVRUnite")

test_that("PipeOpOVRUnite - basic properties", {
  po = PipeOpOVRUnite$new()
  expect_pipeop(po)
  expect_data_table(po$input, nrows = 1)
  expect_data_table(po$output, nrows = 1)

  expect_pipeop_class(PipeOpOVRUnite)
})

test_that("PipeOpOVRUnite- train and predict", {
  skip_if_not_installed("rpart")
  # toy tasks that are splitted, trained and predicted manually
  feature = rep(c(1, 0), c(10, 20))
  dat1 = data.table(target = as.factor(rep(c("a", "rest"), c(10, 20))), feature = feature)
  dat2 = data.table(target = as.factor(rep(c("rest", "b", "rest"), c(10, 10, 10))), feature = feature)
  dat3 = data.table(target = as.factor(rep(c("rest", "c"), c(20, 10))), feature = feature)
  tsk1 = TaskClassif$new("t1", backend = dat1, target = "target", positive = "a")
  tsk2 = TaskClassif$new("t2", backend = dat2, target = "target", positive = "b")
  tsk3 = TaskClassif$new("t3", backend = dat3, target = "target", positive = "c")
  po = PipeOpOVRUnite$new()
  lrn = LearnerClassifRpart$new()

  # predict_type "prob"
  lrn$predict_type = "prob"
  tin = map(list(tsk1, tsk2, tsk3), .f = function(task) {
    lrn$train(task)
    lrn$predict(task)
  })
  # Need to first train before predicting
  expect_list(po$train(list(as.Multiplicity(NULL))), len = 1, types = "null")
  pout = po$predict(list(as.Multiplicity(tin)))
  expect_prediction_classif(pout[[1]])

  # predict_type "response"
  lrn$predict_type = "response"
  tin = map(list(tsk1, tsk2, tsk3), .f = function(task) {
    lrn$train(task)
    lrn$predict(task)
  })
  pout = po$predict(list(as.Multiplicity(tin)))
  expect_prediction_classif(pout[[1]])

  # NA handling
  na_response = tin[[1]]$response
  na_response[1] = NA
  tin[[1]] = PredictionClassif$new(row_ids = tin[[1]]$row_ids, truth = tin[[1]]$truth, response = na_response)
  pout = po$predict(list(as.Multiplicity(tin)))
  expect_prediction_classif(pout[[1]])
  expect_equal(pout[[1]]$prob[1, ], c(a = 1/3, b = 1/3, c = 1/3))

  # error handling
  tin[[1]] = PredictionClassif$new(row_ids = tin[[1]]$row_ids, truth = tin[[1]]$truth)
  expect_error(po$predict(list(as.Multiplicity(tin))), regexp = "PipeOpOVRUnite input predictions had missing 'prob' and missing 'response' values")
})

context("PipeOpOVRSplit and PipeOpOVRUnite")

test_that("PipeOpOVRSplit and PipeOpOVRUnite - train and predict", {
  skip_if_not_installed("rpart")
  # same toy task but now we compare the results to the automated Graph's results
  feature = rep(c(1, 0), c(10, 20))
  dat0 = data.table(target = as.factor(rep(c("a", "b", "c"), each = 10)), feature = feature)
  dat1 = data.table(target = as.factor(rep(c("a", "rest"), c(10, 20))), feature = feature)
  dat2 = data.table(target = as.factor(rep(c("rest", "b", "rest"), c(10, 10, 10))), feature = feature)
  dat3 = data.table(target = as.factor(rep(c("rest", "c"), c(20, 10))), feature = feature)
  tsk0 = TaskClassif$new("t0", backend = dat0, target = "target")
  tsk1 = TaskClassif$new("t1", backend = dat1, target = "target", positive = "a")
  tsk2 = TaskClassif$new("t2", backend = dat2, target = "target", positive = "b")
  tsk3 = TaskClassif$new("t3", backend = dat3, target = "target", positive = "c")
  po = PipeOpOVRUnite$new()
  lrn = LearnerClassifRpart$new()
  tin = map(list(tsk1, tsk2, tsk3), .f = function(task) {
    lrn$train(task)
    lrn$predict(task)
  })
  # Need to first train before predicting
  po$train(list(as.Multiplicity(NULL)))
  pout_ref = po$predict(list(as.Multiplicity(tin)))

  gr = PipeOpOVRSplit$new() %>>% LearnerClassifRpart$new() %>>% PipeOpOVRUnite$new()
  expect_graph(gr)
  tout = gr$train(tsk0)
  expect_list(gr$state$ovrunite, len = 0)
  expect_null(tout[[1]])
  pout = gr$predict(tsk0)

  expect_equal(pout_ref[[1]]$prob, pout[[1]]$prob)

  # setting weights to zero results in uniform probs
  gr$param_set$values$ovrunite.weights = rep(0, 3)
  expect_true(all.equal(unique(gr$predict(tsk0)[[1]]$prob), t(c(a = 1/3, b = 1/3, c = 1/3))))
})

test_that("PipeOpOVRSplit and PipeOpOVRUnite - task size", {
  skip_if_not_installed("rpart")
  gr = PipeOpOVRSplit$new() %>>% LearnerClassifRpart$new() %>>% PipeOpOVRUnite$new()
  gr$train(tsk("iris")$filter(c(1:30, 51:80, 101:130)))
  prd = gr$predict(tsk("iris")$filter(c(1:30, 51:80, 101:130)))[[1]]
  expect_prediction_classif(prd)
  expect_true(nrow(prd$data$prob) == 90)
})
