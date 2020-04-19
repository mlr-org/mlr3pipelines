context("PipeOpScale")

test_that("PipeOpScale", {
  expect_datapreproc_pipeop_class(PipeOpScale, task = mlr_tasks$get("iris"))

  expect_datapreproc_pipeop_class(PipeOpScale, task = mlr_tasks$get("boston_housing"))

  expect_datapreproc_pipeop_class(PipeOpScale, task = mlr_tasks$get("pima"))

  data = data.table(
    a = 1:5,
    b = as.numeric(1:5),
    c = letters[1:5],
    d = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    e = rep(1L, 5),
    f = rep(1.0, 5),
    g = rep(0L, 5),
    h = rep(0, 5),
    i = rep(TRUE, 5),
    class = factor(c("+", "+", "+", "-", "-")))

  task = TaskClassif$new(id = "test", target = "class",
    as_data_backend(data))

  po = PipeOpScale$new()

  po$param_set$values[c("center", "scale")] = c(FALSE, FALSE)

  expect_equal(data, po$train(list(task))[[1]]$data(), ignore.col.order = TRUE)
  expect_equal(data, po$predict(list(task))[[1]]$data(), ignore.col.order = TRUE)

  po$param_set$values[c("center", "scale")] = c(TRUE, FALSE)

  data.center = data.table(
    a = (1:5) - 3,
    b = (1:5) - 3,
    c = data$c,
    d = data$d,
    e = rep(0, 5),
    f = rep(0, 5),
    g = rep(0, 5),
    h = rep(0, 5),
    i = rep(TRUE, 5),
    class = data$class)

  expect_equal(data.center, po$train(list(task))[[1]]$data(), ignore.col.order = TRUE)
  expect_equal(data.center, po$predict(list(task))[[1]]$data(), ignore.col.order = TRUE)

  po$param_set$values[c("center", "scale")] = c(FALSE, TRUE)

  scaling = sqrt(sum((1:5)^2) / 4)
  data.center = data.table(
    a = (1:5) / scaling,
    b = (1:5) / scaling,
    c = data$c,
    d = data$d,
    e = rep(2, 5) / sqrt(5),
    f = rep(2, 5) / sqrt(5),
    g = rep(0, 5),
    h = rep(0, 5),
    i = rep(TRUE, 5),
    class = data$class)

  expect_equal(data.center, po$train(list(task))[[1]]$data(), ignore.col.order = TRUE)
  expect_equal(data.center, po$predict(list(task))[[1]]$data(), ignore.col.order = TRUE)

  po$param_set$values[c("center", "scale")] = c(TRUE, TRUE)

  scaling = sqrt(5 / 2)
  data.center = data.table(
    a = ((1:5) - 3) / scaling,
    b = ((1:5) - 3) / scaling,
    c = data$c,
    d = data$d,
    e = rep(0, 5),
    f = rep(0, 5),
    g = rep(0, 5),
    h = rep(0, 5),
    i = rep(TRUE, 5),
    class = data$class)

  expect_equal(data.center, po$train(list(task))[[1]]$data(), ignore.col.order = TRUE)
  expect_equal(data.center, po$predict(list(task))[[1]]$data(), ignore.col.order = TRUE)
})


test_that("PipeOpScale robust works", {
  t = tsk("iris")
  posc = po("scale", robust = TRUE)
  scaled = posc$train(list(t))[[1]]$data()
  prd = posc$predict(list(t))[[1]]$data()
  expect_equal(scaled, prd)
  x = iris[,names(posc$state$center)]
  expect_equal(posc$state$center, apply(x, 2, median))
  x = t(x) - posc$state$center
  expect_equal(posc$state$scale, apply(t(x), 2, mad))
})
