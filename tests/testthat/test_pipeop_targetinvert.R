context("PipeOpTargetInvert")

test_that("PipeOpTargetInvert - basic properties", {
  expect_pipeop_class(PipeOpTargetInvert, list(id = "po"))

  po = PipeOpTargetInvert$new("po")

  expect_pipeop(po)
  expect_data_table(po$input, nrows = 2L)
  expect_data_table(po$output, nrows = 1L)
})

test_that("PipeOpTargetInvert propagates extra prediction data", {
  po = PipeOpTargetInvert$new()
  po$train(list(NULL, NULL))

  prediction = PredictionRegr$new(
    row_ids = 1:3,
    truth = 1:3,
    response = 4:6
  )
  prediction$data$extra = list(source = "learner", values = 7:9)

  inverter = function(inputs) {
    prediction = inputs[[1L]]
    list(PredictionRegr$new(
      row_ids = prediction$row_ids,
      truth = prediction$truth,
      response = prediction$response * 2
    ))
  }
  output = po$predict(list(inverter, prediction))[[1L]]

  expect_identical(output$data$extra, prediction$data$extra)

  inverter = function(inputs) {
    output = inputs[[1L]]$clone(deep = TRUE)
    output$data$extra = list(source = "inverter")
    list(output)
  }
  output = po$predict(list(inverter, prediction))[[1L]]

  expect_identical(output$data$extra, list(source = "inverter"))
})
