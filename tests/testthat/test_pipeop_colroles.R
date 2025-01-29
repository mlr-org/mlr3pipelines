context("PipeOpColRoles")

test_that("PipeOpColRoles - basic properties", {

  op = PipeOpColRoles$new()
  task = mlr_tasks$get("iris")

  expect_pipeop(op)
  expect_equal(task, train_pipeop(op, inputs = list(task))$output)
  expect_equal(task, predict_pipeop(op, inputs = list(task))$output)

  expect_datapreproc_pipeop_class(PipeOpColRoles, task = task)

})

test_that("PipeOpColRoles - assertions on params work", {

  expect_error(PipeOpColRoles$new(param_vals = list(new_role = "wrong")), regexp = "list")
  expect_error(PipeOpColRoles$new(param_vals = list(new_role = list(a = "wrong", b = NA))), regexp = "character,null")
  expect_no_error(PipeOpColRoles$new(param_vals = list(new_role = list(a = NULL))))
  expect_error(PipeOpColRoles$new(param_vals = list(new_role = list(a = "wrong"))), regexp = "subset")
  expect_error(PipeOpColRoles$new(param_vals = list(new_role = list(a = "target"))), regexp = "subset")

  expect_error(PipeOpColRoles$new(param_vals = list(new_role = list(a = "group", b = "group"))), regexp = "up to one column per role")
  expect_error(PipeOpColRoles$new(param_vals = list(new_role = list(a = "weight", b = "weight"))), regexp = "up to one column per role")
  expect_error(PipeOpColRoles$new(param_vals = list(new_role = list(a = "name", b = "name"))), regexp = "up to one column per role")
  expect_error(PipeOpColRoles$new(param_vals = list(new_role = list(a = c("group", "name"), b = c("group", "name")))), regexp = "up to one column per role.*?\"group\", \"name\"")

  expect_error(PipeOpColRoles$new(param_vals = list(new_role_direct = "wrong")), regexp = "list")
  expect_error(PipeOpColRoles$new(param_vals = list(new_role_direct = list(wrong = "x", feature = NA))), regexp = "character,null")
  expect_no_error(PipeOpColRoles$new(param_vals = list(new_role = list(feature = NULL))))
  expect_error(PipeOpColRoles$new(param_vals = list(new_role_direct = list(wrong = "x"))), regexp = "subset")
  expect_error(PipeOpColRoles$new(param_vals = list(new_role_direct = list(target = "y"))), regexp = "subset")

  expect_error(PipeOpColRoles$new(param_vals = list(new_role_direct = list(group = c("x", "y")))), regexp = "up to one column per role")
  expect_error(PipeOpColRoles$new(param_vals = list(new_role_direct = list(weight = c("x", "y")))), regexp = "up to one column per role")
  expect_error(PipeOpColRoles$new(param_vals = list(new_role_direct = list(name = c("x", "y")))), regexp = "up to one column per role")
  expect_error(PipeOpColRoles$new(param_vals = list(new_role_direct = list(group = c("x", "y"), name = c("x", "y")))), regexp = "up to one column per role.*?\"group\", \"name\"")

})

test_that("PipeOpColRoles - only existing columns are accepted", {

  task = mlr_tasks$get("iris")
  task$cbind(data.table(rn = sprintf("%03d", 1:150)))

  op = PipeOpColRoles$new(param_vals = list(new_role = list(rn = "name", wrong = "feature")))
  expect_error(train_pipeop(op, inputs = list(task)), regexp = "subset")

  op = PipeOpColRoles$new(param_vals = list(new_role_direct = list(name = "rn", feature = "wrong")))
  expect_error(train_pipeop(op, inputs = list(task)), regexp = "subset")

})

test_that("PipeOpColRoles - changing the role of a target fails", {

  task = mlr_tasks$get("iris")

  op = PipeOpColRoles$new(param_vals = list(new_role = list(Species = "feature")))
  expect_error(train_pipeop(op, inputs = list(task)), regexp = "role of a target")

  op = PipeOpColRoles$new(param_vals = list(new_role_direct = list(feature = "Species")))
  expect_error(train_pipeop(op, inputs = list(task)), regexp = "role of a target")

})

test_that("PipeOpColRoles - new_role works", {

  task = mlr_tasks$get("iris")
  task$cbind(data.table(rn = sprintf("%03d", 1:150)))

  op = PipeOpColRoles$new(param_vals = list(new_role = list(
    rn = "name", Petal.Length = c("feature", "order"), Petal.Width = character(0), Sepal.Width = NULL))
  )

  train_out = train_pipeop(op, inputs = list(task))[[1L]]

  col_roles_actual = train_out$col_roles
  col_roles_expected = list(
    feature = c("Sepal.Length", "Petal.Length"), target = "Species", name = "rn", order = "Petal.Length",
    stratum = character(0), group = character(0), weight = character(0)
  )

  # Compatibility with upcoming new weights_learner role in mlr3
  if ("weights_learner" %in% names(task)) names(col_roles_expected)[names(col_roles_expected) == "weight"] = "weights_learner"

  expect_equal(train_out$col_roles[names(col_roles_expected)], col_roles_expected)
  expect_equal(train_out$row_names$row_name, task$data(cols = "rn")[[1L]])
  expect_true("Petal.Width" %nin% colnames(train_out$data()))
  expect_true("Sepal.Width" %nin% colnames(train_out$data()))

  predict_out = predict_pipeop(op, inputs = list(task))[[1L]]
  expect_equal(train_out, predict_out)
})

test_that("PipeOpColRoles - new_role_direct works", {

  task = mlr_tasks$get("iris")
  task$cbind(data.table(rn = sprintf("%03d", 1:150)))
  task$col_roles$group = "Species"

  op = PipeOpColRoles$new(param_vals = list(new_role_direct = list(
    name = "rn", order = c("Petal.Length", "Sepal.Length"), feature = character(0), group = NULL))
  )

  train_out = train_pipeop(op, inputs = list(task))[[1L]]

  col_roles_actual = train_out$col_roles
  col_roles_expected = list(
    feature = character(0), target = "Species", name = "rn", order = c("Petal.Length", "Sepal.Length"),
    stratum = character(0), group = character(0), weight = character(0)
  )

  # Compatibility with upcoming new weights_learner role in mlr3
  if ("weights_learner" %in% names(task)) names(col_roles_expected)[names(col_roles_expected) == "weight"] = "weights_learner"

  expect_equal(train_out$col_roles[names(col_roles_expected)], col_roles_expected)
  expect_equal(train_out$row_names$row_name, task$data(cols = "rn")[[1L]])
  expect_equal(train_out$data(), task$data(cols = "Species"))

  predict_out = predict_pipeop(op, inputs = list(task))[[1L]]
  expect_equal(train_out, predict_out)
})

