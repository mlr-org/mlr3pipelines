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
  task$cbind(data.table(rn = sprintf("%03d", 1:150)))
  op = PipeOpColRoles$new(param_vals = list(new_role = list(rn = "name", wrong = "feature")))
  expect_error(train_pipeop(op, inputs = list(task)), regexp = "subset")
})

test_that("PipeOpColRoles - changing the role of a target fails", {
  task = mlr_tasks$get("iris")
  op = PipeOpColRoles$new(param_vals = list(new_role = list(Species = "feature")))
  expect_error(train_pipeop(op, inputs = list(task)), regexp = "role of a target")
})

test_that("PipeOpColRoles - functionality works", {
  task = mlr_tasks$get("iris")
  task$cbind(data.table(rn = sprintf("%03d", 1:150)))
  op = PipeOpColRoles$new(param_vals = list(new_role = list(rn = "name", Petal.Length = "order", Petal.Width = character(0))))
  train_out = train_pipeop(op, inputs = list(task))$output
  col_roles_actual = train_out$col_roles
  col_roles_expected = list(
    feature = c("Sepal.Length", "Sepal.Width"), target = "Species", name = "rn",
    order = "Petal.Length", stratum = character(0), group = character(0), weight = character(0))
  if ("weights_learner" %in% names(task)) names(col_roles_expected)[names(col_roles_expected) == "weight"] = "weights_learner"
  expect_equal(train_out$col_roles[names(col_roles_expected)], col_roles_expected)
  expect_equal(train_out$row_names$row_name, task$data(cols = "rn")[[1L]])
  expect_true("Petal.Width" %nin% colnames(train_out$data()))
  predict_out = predict_pipeop(op, inputs = list(task))$output
  expect_equal(train_out, predict_out)
})
