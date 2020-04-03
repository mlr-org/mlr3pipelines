context("PipeOpFixFactors")

test_that("PipeOpFixFactors", {
  task = mlr_tasks$get("boston_housing")

  chaslevels = task$levels()$chas
  townlevels = task$levels()$town

  expect_datapreproc_pipeop_class(PipeOpFixFactors, task = task)

  expect_datapreproc_pipeop_class(PipeOpFixFactors, task = mlr_tasks$get("iris"))

  op = PipeOpFixFactors$new()
  expect_pipeop(op)

  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  # factor cols are removed
  expect_true(all(c("chas", "town") %in% fn))
  expect_set_equal(nt$levels()$chas, c("0", "1"))

  nt = op$train(list(task$clone()$filter(1)))[[1]]

  expect_equal(nt$levels()$chas, "0")
  expect_equal(nt$levels()$town, "Nahant")
  expect_equal(length(nt$levels()), 2)

  nt = op$predict(list(task))[[1]]

  expect_equal(nt$levels()$chas, "0")
  expect_equal(nt$levels()$town, "Nahant")
  expect_equal(length(nt$levels()), 2)

  expect_equal(levels(nt$data()$chas), "0")
  expect_equal(which(task$data()$chas == "1"), which(is.na(nt$data()$chas)))

  nt = op$train(list(task$clone()$filter(1:2)))[[1]]

  expect_equal(nt$levels()$chas, "0")
  expect_set_equal(nt$levels()$town, c("Nahant", "Swampscott"))
  expect_equal(length(nt$levels()), 2)

  nt = op$predict(list(task))[[1]]
  expect_equal(nt$levels()$chas, "0")
  expect_set_equal(nt$levels()$town, c("Nahant", "Swampscott"))
  expect_equal(length(nt$levels()), 2)

  dattrain = data.table(
    a = factor(c("a", "b", "c", NA), levels = letters),
    b = ordered(c("a", "b", "c", NA)),
    target = 1:4)

  tasktrain = TaskRegr$new("train", dattrain, "target")

  dattest = data.table(
    a = factor(c("a", "b", "c", "d")),
    b = ordered(c("a", "b", "c", "d"), levels = letters[10:1]),
    target = 1:4)

  tasktest = TaskRegr$new("train", dattest, "target")

  op$param_set$values$droplevels = TRUE

  opt = op$train(list(tasktrain))[[1]]

  expect_equal(opt$levels(), list(a = letters[1:3], b = letters[1:3]))
  expect_equal(levels(opt$data()$a), letters[1:3])
  expect_equal(levels(opt$data()$b), letters[1:3])
  expect_true(is.ordered(opt$data()$b))
  expect_false(is.ordered(opt$data()$a))

  opt = op$predict(list(tasktest))[[1]]

  expect_equal(opt$levels(), list(a = letters[1:3], b = letters[1:3]))
  expect_equal(levels(opt$data()$a), letters[1:3])
  expect_equal(levels(opt$data()$b), letters[1:3])
  expect_true(is.ordered(opt$data()$b))
  expect_false(is.ordered(opt$data()$a))
  expect_equal(opt$data()$a, factor(c("a", "b", "c", NA), levels = letters[1:3]))
  expect_equal(opt$data()$b, ordered(c("a", "b", "c", NA), levels = letters[1:3]))

  expect_equal(op$state$levels, list(a = letters[1:3], b = letters[1:3]))

  op$param_set$values$droplevels = FALSE

  opt = op$train(list(tasktrain))[[1]]

  expect_equal(opt$levels(), list(a = letters, b = letters[1:3]))
  expect_equal(levels(opt$data()$a), letters)
  expect_equal(levels(opt$data()$b), letters[1:3])
  expect_true(is.ordered(opt$data()$b))
  expect_false(is.ordered(opt$data()$a))

  opt = op$predict(list(tasktest))[[1]]

  expect_equal(opt$levels(), list(a = letters, b = letters[1:3]))
  expect_equal(levels(opt$data()$a), letters)
  expect_equal(levels(opt$data()$b), letters[1:3])
  expect_true(is.ordered(opt$data()$b))
  expect_false(is.ordered(opt$data()$a))
  expect_equal(opt$data()$a, factor(c("a", "b", "c", "d"), levels = letters))
  expect_equal(opt$data()$b, ordered(c("a", "b", "c", NA), levels = letters[1:3]))

  expect_equal(op$state$levels, list(a = letters, b = letters[1:3]))

})
