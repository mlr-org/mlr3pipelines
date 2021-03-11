context("PipeOpColRoles")

test_that("PipeOpColRoles - basic properties", {
  op = PipeOpColRoles$new()
  task = mlr_tasks$get("iris")
  expect_pipeop(op)
  expect_equal(task, train_pipeop(op, inputs = list(task))$output)
  expect_equal(task, predict_pipeop(op, inputs = list(task))$output)

  expect_datapreproc_pipeop_class(PipeOpColRoles, task = task)
})

test_that("PipeOpColRoles - assertion works", {
  expect_error(PipeOpColRoles$new(param_vals = list(new_role = "wrong")), regexp = "list")
  expect_error(PipeOpColRoles$new(param_vals = list(new_role = list(a = "wrong", b = NA))), regexp = "character")
  expect_error(PipeOpColRoles$new(param_vals = list(new_role = list(a = "wrong", b = "target"))), regexp = "subset")
})

test_that("PipeOpColRoles - name checking works", {
  task = mlr_tasks$get("iris")
  op = PipeOpColRoles$new(param_vals = list(new_role = list(Petal.Length = "name", wrong = "feature")))
  expect_error(train_pipeop(op, inputs = list(task)), regexp = "subset")
})

test_that("PipeOpColRoles - changing the role of a target fails", {
  task = mlr_tasks$get("iris")
  op = PipeOpColRoles$new(param_vals = list(new_role = list(Species = "feature")))
  expect_error(train_pipeop(op, inputs = list(task)), regexp = "role of a target")
})

test_that("PipeOpColRoles - functionality works", {
  task = mlr_tasks$get("iris")
  op = PipeOpColRoles$new(param_vals = list(new_role = list(Petal.Length = c("name", "order"), Petal.Width = character())))
  train_out = train_pipeop(op, inputs = list(task))$output
  expect_equal(train_out$col_roles,
    list(feature = c("Sepal.Length", "Sepal.Width"), target = "Species", name = "Petal.Length",
      order = "Petal.Length", stratum = character(), group = character(), weight = character(), uri = character(0)
    )
  )
  expect_equal(train_out$row_names$row_name, task$data(cols = "Petal.Length")[[1L]])
  expect_true("Petal.Width" %nin% colnames(train_out$data()))
  predict_out = predict_pipeop(op, inputs = list(task))$output
  expect_equal(train_out, predict_out)
})
