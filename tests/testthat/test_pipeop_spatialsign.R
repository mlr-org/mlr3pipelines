context("PipeOpSpatialSign")

test_that("PipeOpSpatialSign - general functionality", {
  task = mlr_tasks$get("iris")
  op = PipeOpSpatialSign$new()
  expect_pipeop(op)
  expect_datapreproc_pipeop_class(PipeOpSpatialSign, task = task)
  result = train_pipeop(op, inputs = list(task))
  expect_task(result[[1]])
  result = predict_pipeop(op, inputs = list(task))
  expect_task(result[[1]])
})

test_that("PipeOpSpatialSign - receive expected result", {
  # Length 1
  task = mlr_tasks$get("iris")
  op = PipeOpSpatialSign$new(param_vals = list(norm = 2, length = 1L))
  result = train_pipeop(op, inputs = list(task))
  t = apply(result[[1]]$data()[, 2:5], MARGIN = 1, function(x) {
    expect_equal(sqrt(sum(x^2)), 1)
  })
  result = op$predict(list(task))
  t = apply(result[[1]]$data()[, 2:5], MARGIN = 1, function(x) {
    expect_equal(sqrt(sum(x^2)), 1)
  })

  # Length 2
  task = mlr_tasks$get("iris")
  op = PipeOpSpatialSign$new(param_vals = list(norm = 2, length = 2))
  result = train_pipeop(op, inputs = list(task))
  t = apply(result[[1]]$data()[, 2:5], MARGIN = 1, function(x) {
    expect_equal(sqrt(sum(x^2)), 2)
  })
  result = op$predict(list(task))
  t = apply(result[[1]]$data()[, 2:5], MARGIN = 1, function(x) {
    expect_equal(sqrt(sum(x^2)), 2)
  })

  # norm 1
  task = mlr_tasks$get("iris")
  op = PipeOpSpatialSign$new(param_vals = list(norm = 1, length = 1L))
  result = train_pipeop(op, inputs = list(task))
  t = apply(result[[1]]$data()[, 2:5], MARGIN = 1, function(x) {
    expect_equal(sum(abs(x)), 1)
  })
  result = op$predict(list(task))
  t = apply(result[[1]]$data()[, 2:5], MARGIN = 1, function(x) {
    expect_equal(sum(abs(x)), 1)
  })

  # norm inf
  task = mlr_tasks$get("iris")
  op = PipeOpSpatialSign$new(param_vals = list(norm = Inf, length = 1L))
  result = train_pipeop(op, inputs = list(task))
  t = apply(result[[1]]$data()[, 2:5], MARGIN = 1, function(x) {
    expect_equal(max(abs(x)), 1)
  })
  result = op$predict(list(task))
  t = apply(result[[1]]$data()[, 2:5], MARGIN = 1, function(x) {
    expect_equal(max(abs(x)), 1)
  })

})
