context("PipeOpSmoteNC")

test_that("PipeOpSmoteNC - basic properties", {
  skip_if_not_installed("themis")

  op = PipeOpSmoteNC$new()
  expect_pipeop(op)

})

test_that("PipeOpSmoteNC - train works as intended", {

  task = mlr_tasks$get("penguins")

  # Only accept: all numeric, numeric + categorical
  # Do not accept: all categorical, (integers, better not check for this)
  task = mlr_tasks$get("breast_cancer") # nur ordered
  task = mlr_tasks$get("iris") # nur numeric
  task = mlr_tasks$get("penguins")
  cols = setdiff(task$feature_names, c("body_mass", "year", "flipper_length")) # remove integers
  task$select(cols) # numeric + factors

})
