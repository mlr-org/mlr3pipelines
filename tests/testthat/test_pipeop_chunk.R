context("PipeOpChunk")

test_that("PipeOpChunk - basic properties", {
  expect_pipeop_class(PipeOpChunk, list(1))
  expect_pipeop_class(PipeOpChunk, list(3))
  expect_error(PipeOpChunk$new(0))

  po = PipeOpChunk$new(3)
  expect_pipeop(po)

  expect_data_table(po$input, nrow = 1)
  expect_data_table(po$output, nrow = 3)
})


test_that("PipeOpChunk - stratification", {
  po = PipeOpChunk$new(3)
  po$param_set$values = list(stratify = TRUE, shuffle = FALSE)

  tasks = po$train(list(mlr_tasks$get("spam")))
  tabs = lapply(tasks, function(x) table(x$truth()))
  ratio = map_dbl(tabs, function(x) x[["spam"]] / x[["nonspam"]])
  expect_numeric(ratio, len = 3, lower = 0.65, upper = .66)
})
