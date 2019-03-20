context("PipeOpImpute")

test_that("PipeOpImpute", {

  task = mlr_tasks$get("pima")

  expect_datapreproc_pipeop_class(PipeOpImpute, task = task)

  expect_datapreproc_pipeop_class(PipeOpImpute, task = mlr_tasks$get("iris"))

  mdata <- data.frame(stringsAsFactors = FALSE,
      a = c(1, 2, 3, 4, 5, NA),
      b = c(1, 2, 3, 4, 5, 6),
      c = c(1L, 2L, 3L, 4L, 5L, NA),
      d = factor(c(letters[1:5], NA), levels = letters[1:6]),
      e = factor(letters[1:6], levels = letters[1:6]),
      f = ordered(c(letters[1:5], NA), levels = letters[1:6]),
      g = ordered(letters[1:6], levels = letters[1:6]),
      h = c(letters[1:5], NA),
      i = letters[1:6],
      j = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
      k = c(TRUE, FALSE, TRUE, FALSE, TRUE, NA),
      l = letters[rep(1:2, 3)])

  task = TaskClassif$new("mdata", as_data_backend(mdata), target = "l")
  mdata$j = NULL
  mdata$k = NULL
  task_no_lgl = TaskClassif$new("mdata", as_data_backend(mdata), target = "l")

  expect_datapreproc_pipeop_class(PipeOpImpute, task = task_no_lgl,
    constargs = list(param_vals = list(
      method_num = "median",
      method_fct = "newlvl",
      add_dummy = "none")))
  expect_datapreproc_pipeop_class(PipeOpImpute, task = task,
    deterministic_train = FALSE, deterministic_predict = FALSE,
    constargs = list(param_vals = list(
      method_num = "median",
      method_fct = "newlvl",
      add_dummy = "none")))

  expect_datapreproc_pipeop_class(PipeOpImpute, task = task_no_lgl,
    constargs = list(param_vals = list(
      method_num = "mean",
      method_fct = "newlvl",
      add_dummy = "missing_train")))
  expect_datapreproc_pipeop_class(PipeOpImpute, task = task,
    deterministic_train = FALSE, deterministic_predict = FALSE,
    constargs = list(param_vals = list(
      method_num = "mean",
      method_fct = "newlvl",
      add_dummy = "missing_train")))

  expect_datapreproc_pipeop_class(PipeOpImpute, task = task,
    deterministic_train = FALSE, deterministic_predict = FALSE,
    constargs = list(param_vals = list(
      method_num = "sample",
      method_fct = "sample",
      add_dummy = "all")))

  expect_datapreproc_pipeop_class(PipeOpImpute, task = task,
    deterministic_train = FALSE, deterministic_predict = FALSE,
    constargs = list(param_vals = list(
      method_num = "hist",
      method_fct = "sample",
      add_dummy = "all")))

  po = PipeOpImpute$new(param_vals = list(
    method_num = "sample", method_fct = "sample", add_dummy = "all"))

  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()

  task_predicted = po$predict(list(task))[[1]]$data()

  expect_equal(task_trained[1, c("a", "c", "d", "f", "h", "k")],
    task_trained[2, c("a", "c", "d", "f", "h", "k")])

  expect_equal(task_predicted[c(5:6), c("a", "c", "d", "f", "h", "k")],
    task_trained[c(1:2), c("a", "c", "d", "f", "h", "k")])

  expect_equal(task_trained$missing_a, c(FALSE, TRUE))
  expect_equal(task_trained$missing_c, c(FALSE, TRUE))
  expect_equal(task_trained$missing_d, c(FALSE, TRUE))
  expect_equal(task_trained$missing_f, c(FALSE, TRUE))
  expect_equal(task_trained$missing_h, c(FALSE, TRUE))
  expect_equal(task_trained$missing_k, c(FALSE, TRUE))

  expect_equal(task_trained$missing_b, c(FALSE, FALSE))
  expect_equal(task_trained$missing_e, c(FALSE, FALSE))
  expect_equal(task_trained$missing_g, c(FALSE, FALSE))
  expect_equal(task_trained$missing_i, c(FALSE, FALSE))
  expect_equal(task_trained$missing_j, c(FALSE, FALSE))

  expect_set_equal(colnames(task_trained), c(letters[1:12], paste0("missing_", letters[1:11])))
  expect_set_equal(colnames(task_predicted), c(letters[1:12], paste0("missing_", letters[1:11])))


  po = PipeOpImpute$new(param_vals = list(
    method_num = "median", method_fct = "newlvl", add_dummy = "all"))

  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()
  task_predicted = po$predict(list(task))[[1]]$data()

  expect_equal(task_trained[1, c("a", "c", "k")],
    task_trained[2, c("a", "c", "k")])

  expect_equal(task_predicted[5:6, ],
    task_trained[1:2])

  expect_set_equal(colnames(task_trained), c(letters[1:12], paste0("missing_", c("a", "b", "c", "j", "k"))))
  expect_set_equal(colnames(task_predicted), c(letters[1:12], paste0("missing_", c("a", "b", "c", "j", "k"))))

  expect_equal(task_trained$d[2], factor(".MISSING", levels = c(letters[1:6], ".MISSING")))
  expect_equal(task_trained$h[2], ".MISSING")

  po = PipeOpImpute$new(param_vals = list(
    method_num = "median", method_fct = "newlvl", add_dummy = "missing_train"))

  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()
  task_predicted = po$predict(list(task$clone(deep = TRUE)$filter(1:3)))[[1]]$data()

  expect_set_equal(colnames(task_trained), c(letters[1:12], paste0("missing_", c("a", "c", "k"))))
  expect_set_equal(colnames(task_predicted), c(letters[1:12], paste0("missing_", c("a", "c", "k"))))

  po = PipeOpImpute$new(param_vals = list(
    method_num = "median", method_fct = "newlvl", add_dummy = "none"))

  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()
  task_predicted = po$predict(list(task$clone(deep = TRUE)$filter(1:3)))[[1]]$data()

  expect_equal(task_predicted, task$clone(deep = TRUE)$filter(1:3)$data())

  po = PipeOpImpute$new(param_vals = list(
    method_num = "hist", method_fct = "newlvl", add_dummy = "missing_train"))

  for (i in range(10)) {
    task_trained = po$train(list(task))[[1]]$data()

    task_predicted = po$predict(list(task))[[1]]$data()

    expect_true(task_trained$a[6] <= 5 && task_trained$a[6] >= 1)
    expect_true(task_trained$c[6] <= 5 && task_trained$c[6] >= 1)
    expect_true(task_predicted$a[6] <= 5 && task_trained$a[6] >= 1)
    expect_true(task_predicted$c[6] <= 5 && task_trained$c[6] >= 1)
  }

})
