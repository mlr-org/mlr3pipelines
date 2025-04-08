context("PipeOpYeoJohnson")

test_that("PipeOpYeoJohnson - general functionality", {
  skip_if_not_installed("bestNormalize")
  task = mlr_tasks$get("iris")
  op = PipeOpYeoJohnson$new()
  expect_datapreproc_pipeop_class(PipeOpYeoJohnson, task = task)
})

test_that("PipeOpYeoJohnson - receive expected result", {
  skip_if_not_installed("bestNormalize")
  task = mlr_tasks$get("iris")
  op = PipeOpYeoJohnson$new(param_vals = list(standardize = FALSE))
  result = train_pipeop(op, inputs = list(task))
  result.pred = predict_pipeop(op, inputs = list(task))

  # Compare to bestNormalize::yeojohnson
  lapply(2:5, function(j) {
    x.trans = bestNormalize::yeojohnson(task$data()[[j]], standardize = FALSE)$x.t
    expect_equal(x.trans, result[[1]]$data()[[j]])
    expect_equal(x.trans, result.pred[[1]]$data()[[j]])
    return(TRUE)
  })

  # Set lower and upper value for lambda estimation
  op = PipeOpYeoJohnson$new(param_vals = list(upper = 0.5, lower = 0))
  result = train_pipeop(op, inputs = list(task))
  lambda.new = unlist(lapply(op$state$bc[1:4], function(x) x$lambda))
  expect_true(all(lambda.new <= 0.5 & lambda.new >= 0))
})
