context("PipeOpEncodeImpact")

test_that("PipeOpEncodeImpact", {
  task = mlr_tasks$get("boston_housing_classic")

  chaslevels = task$levels()$chas
  townlevels = task$levels()$town

  t2 = po("histbin")$train(list(tsk("iris")))[[1]]

  expect_datapreproc_pipeop_class(PipeOpEncodeImpact, task = task)

  expect_datapreproc_pipeop_class(PipeOpEncodeImpact, task = t2)

  expect_datapreproc_pipeop_class(PipeOpEncodeImpact, task = mlr_tasks$get("iris"))

  op = PipeOpEncodeImpact$new()
  expect_pipeop(op)

  nt = train_pipeop(op, inputs = list(task))[[1L]]
  fn = nt$feature_names

  # factor cols are removed
  expect_true(all(c("chas", "town") %in% fn))
  expect_true("factor" %nin% nt$feature_types$type)

  nt = op$train(list(t2))[[1]]
  fn = nt$feature_names
  # factor cols are removed
  expect_true(all(tsk("iris")$feature_names %nin% fn))
  expect_true("factor" %nin% nt$feature_types$type)

})


test_that("PipeOpImpactEncode on Classification", {

  testdf = data.frame(
      a = factor(c("a", "b", "a", "b", "a", "b")),
      b = factor(c("a", "b", "b", "a", "a", "a")),
      t = factor(c("x", "x", "x", "y", "y", "y")))

  testtask = TaskClassif$new("test", testdf, "t")

  op = PipeOpEncodeImpact$new()

  expect_equal(op$train(list(tsk("iris")))[[1]], tsk("iris"))

  expm = sapply(c("x", "y"), function(x) {
    glm(I(t == x) ~ 0 + a, data = testdf, family = "binomial")$coefficients
  })

  expm = rbind(expm, c(NA, NA))
  rownames(expm) = c("a", "b", ".TEMP.MISSING")

  op$param_set$values$smoothing = 0

  op$train(list(testtask))

  expect_equal(op$state$impact$a, expm)

  op$param_set$values$smoothing = 1e-4
  op$train(list(testtask))
  expect_equal(mean(abs(op$state$impact$a - expm), na.rm = TRUE), 1e-4 / 2 - 1e-4^2 * 3 / 8)

  op$param_set$values$smoothing = 1e-8
  op$train(list(testtask))
  expect_equal(mean(abs(op$state$impact$a - expm), na.rm = TRUE), 0)  # equal to 0 within tolerance

  op$param_set$values$smoothing = 6.362e-9  # similar to what glm uses
  encoded = op$train(list(testtask))[[1]]$data()

  expm2 = sapply(c("x", "y"), function(x) {
    glm(I(t == x) ~ 0 + b, data = testdf, family = "binomial")$coefficients
  })

  expm2 = rbind(expm2, c(NA, NA))
  rownames(expm2) = c("a", "b", ".TEMP.MISSING")

  expect_equal(op$state$impact$b, expm2, tolerance = 1e-5)

  expect_equal(encoded,
    data.table(t = testdf$t, a = op$state$impact$a[testdf$a, ],
      b = op$state$impact$b[testdf$b, ]))

  # test NA handling / imputation

  testdf2 = data.frame(
      a = factor(c("a", "b", "a", "b", NA, "b")),
      b = factor(c("a", "b", "b", "a", "a", NA)),
      t = factor(c("x", "x", "x", "y", "y", "y")))

  testtask2 = TaskClassif$new("test2", testdf2, "t")
  op$param_set$values$impute_zero = FALSE
  encoded = op$train(list(testtask2))[[1]]$data()

  expect_equal(which(is.na(encoded)), c(11, 17, 24, 30))  # there are 6 rows, cols 2 & 3 have row 5 NA, cols 4 & 5 have row 6 NA

  op$param_set$values$impute_zero = TRUE
  encoded = op$train(list(testtask2))[[1]]$data()

  expect_equal(as.numeric(as.matrix(encoded)[c(11, 17, 24, 30)]), c(0, 0, 0, 0))  # imputation by 0

})

test_that("PipeOpImpactEncode on Regression", {

  testdf = data.frame(
      a = factor(c("a", "b", "a", "b", "a", "b")),
      b = factor(c("a", "b", "b", "a", "a", "a")),
      t = c(1, 2, 3, 1, 2, 3))

  testtask = TaskRegr$new("test", testdf, "t")

  expect = data.table(
      a = c(0, 0, 0, 0, 0, 0),
      b = c(-1, 2, 2, -1, -1, -1) / 4,
      t = c(1, 2, 3, 1, 2, 3))

  op = PipeOpEncodeImpact$new()
  op$param_set$values$smoothing = 0

  expect_equal(op$train(list(testtask))[[1]]$data(), expect, ignore.col.order = TRUE)


  expect_equal(op$state$impact$a, t(t(c(a = 0, b = 0, .TEMP.MISSING = NA))))
  expect_equal(op$state$impact$b, t(t(c(a = -1/4, b = 1/2, .TEMP.MISSING = NA))))

  op$param_set$values$smoothing = 1e-4
  expect_false(isTRUE(all.equal(op$train(list(testtask))[[1]]$data(), expect, ignore.col.order = TRUE, tolerance = 1e-5)))

  selector = as_graph(po("select", selector = selector_type("numeric")))

  expect_equal(unname((selector %>>% op)$train(tsk("boston_housing_classic"))), unname(selector$train(tsk("boston_housing_classic"))))


  op$param_set$values$smoothing = 1e-10
  op$param_set$values$impute_zero = TRUE
  op$train(list(testtask))

  testdf2 = data.frame(
      a = factor(c("a", "b", "a", "b", NA, "b")),
      b = factor(c("a", "b", "b", "a", "a", NA)),
      t = c(1, 2, 3, 1, 2, 3))

  testtask2 = TaskRegr$new("test2", testdf2, "t")

  expectdf2 = expect
  expectdf2$b[6] = 0

  expect_equal(op$predict(list(testtask2))[[1]]$data(), expectdf2, ignore.col.order = TRUE)

  encoded = op$train(list(testtask2))[[1]]$data()

  expect_false(any(is.na(encoded)))
  expect_equal(as.numeric(as.matrix(encoded)[c(11, 18)]), c(0, 0))

  expectdf3 = expect
  expectdf3$b = c(-2 / 3, 0.5, 0.5, -2 / 3, -2 / 3, 0)

  expect_equal(encoded, expectdf3, ignore.col.order = TRUE)


  op$param_set$values$impute_zero = FALSE
  encoded = op$train(list(testtask2))[[1]]$data()

  expect_equal(which(is.na(encoded)), c(11, 18))

})

test_that("PipeOpImpactEncode factor level ``", {

  op = PipeOpEncodeImpact$new()

  testdf3 = iris
  levels(testdf3$Species) = c("setosa", "versicolor", "")
  testtask3 = TaskRegr$new(id = "test3", backend = testdf3, target = "Sepal.Length")
  train_out3 = op$train(list(testtask3))[[1L]]

  testtask3ref = TaskRegr$new(id = "test3ref", backend = iris, target = "Sepal.Length")
  train_out3ref = op$train(list(testtask3ref))[[1L]]

  expect_equal(train_out3$data(), train_out3ref$data())

})
