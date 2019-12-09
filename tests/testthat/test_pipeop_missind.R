context("PipeOpMissInd")

test_that("PipeOpMissInd", {
  task = mlr_tasks$get("pima")

  expect_datapreproc_pipeop_class(PipeOpMissInd, task = task)

  expect_datapreproc_pipeop_class(PipeOpMissInd, task = mlr_tasks$get("iris"))

  mdata = data.frame(stringsAsFactors = FALSE,
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
    l = letters[rep(1:2, 3)]  # this is 'character' on purpose
  )

  task = TaskClassif$new("mdata", as_data_backend(mdata), target = "l")
  mdata$j = NULL
  mdata$k = NULL
  task_no_lgl = TaskClassif$new("mdata", as_data_backend(mdata), target = "l")

  expect_datapreproc_pipeop_class(PipeOpMissInd, task = task,
    constargs = list(param_vals = list(which = "missing_train", type = "logical")))

  expect_datapreproc_pipeop_class(PipeOpMissInd, task = task,
    constargs = list(param_vals = list(which = "all", type = "logical")))

  expect_datapreproc_pipeop_class(PipeOpMissInd, task = task,
    constargs = list(param_vals = list(which = "all", type = "factor")))

  expect_datapreproc_pipeop_class(PipeOpMissInd, task = task,
    constargs = list(param_vals = list(which = "all", type = "numeric")))

  po = PipeOpMissInd$new(param_vals = list(which = "all", type = "logical"))

  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()

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

  po$param_set$values$type = "factor"
  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()
  expect_equal(task_trained$missing_a, factor(c("present", "missing"), levels = c("missing", "present")))
  expect_equal(task_trained$missing_b, factor(c("present", "present"), levels = c("missing", "present")))

  po$param_set$values$type = "numeric"
  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()
  expect_equal(task_trained$missing_a, c(0, 1))
  expect_equal(task_trained$missing_b, c(0, 0))

  po$param_set$values$which = "missing_train"
  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()
  expect_equal(task_trained$missing_a, c(0, 1))
  expect_equal(task_trained$missing_c, c(0, 1))
  expect_equal(task_trained$missing_d, c(0, 1))
  expect_equal(task_trained$missing_f, c(0, 1))
  expect_equal(task_trained$missing_h, c(0, 1))
  expect_equal(task_trained$missing_k, c(0, 1))
  expect_null(task_trained$missing_b)
  expect_null(task_trained$missing_e)
  expect_null(task_trained$missing_g)
  expect_null(task_trained$missing_i)
  expect_null(task_trained$missing_j)

  task_predicted = po$predict(list(task$clone(deep = TRUE)$filter(1:2)))[[1]]$data()
  expect_equal(task_predicted$missing_a, c(0, 0))
  expect_equal(task_predicted$missing_c, c(0, 0))
  expect_equal(task_predicted$missing_d, c(0, 0))
  expect_equal(task_predicted$missing_f, c(0, 0))
  expect_equal(task_predicted$missing_h, c(0, 0))
  expect_equal(task_predicted$missing_k, c(0, 0))
  expect_null(task_predicted$missing_b)
  expect_null(task_predicted$missing_e)
  expect_null(task_predicted$missing_g)
  expect_null(task_predicted$missing_i)
  expect_null(task_predicted$missing_j)
})
