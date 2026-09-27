context("PipeOpBoxCox")

test_that("PipeOpBoxCox - general functionality", {
  skip_if_not_installed("bestNormalize")
  task = mlr_tasks$get("iris")
  op = PipeOpBoxCox$new()
  expect_datapreproc_pipeop_class(PipeOpBoxCox, task = task)
})

test_that("PipeOpBoxCox - receive expected result", {
  skip_if_not_installed("bestNormalize")
  task = mlr_tasks$get("iris")
  op = PipeOpBoxCox$new(param_vals = list(standardize = FALSE))
  result = train_pipeop(op, inputs = list(task))
  result.pred = predict_pipeop(op, inputs = list(task))

  lambda = op$state$bc$Petal.Length$lambda
  lambda.id = lambda != 0

  x = task$data()[[2]]
  x.trans = if (lambda.id) ((x^lambda) - 1)/lambda else log(x)
  expect_equal(x.trans, result[[1]]$data()[[2]])
  expect_equal(x.trans, result.pred[[1]]$data()[[2]])

  # Set lower and upper value for lambda estimation
  op = PipeOpBoxCox$new(param_vals = list(upper = 0.5, lower = 0))
  result = train_pipeop(op, inputs = list(task))
  lambda.new = unlist(lapply(op$state$bc[1:4], function(x) x$lambda))
  expect_true(all(lambda.new <= 0.5 & lambda.new >= 0))

})
