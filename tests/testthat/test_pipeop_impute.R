context("PipeOpImpute")

test_that("PipeOpImpute", {
  skip_on_cran()  # slow test, so we don't do it on cran
  # create bogus impute pipeop that behaves like the old impute pipeop. This lets us do tests quickly. FIXME needs to be cleaned up. a lot.

  PipeOpTestImpute = R6Class("PipeOpTestImpute", inherit = PipeOpTaskPreprocSimple,
    public = list(
      initialize = function(id = "impute", param_vals = list()) {
        ps = ParamSet$new(list(
          ParamFct$new("method_num", levels = c("median", "mean", "mode", "sample", "hist"), default = "median", tags = c("train", "predict")),
          ParamFct$new("method_fct", levels = c("newlvl", "sample", "mode"), default = "newlvl", tags = c("train", "predict")),
          ParamFct$new("add_dummy", levels = c("none", "missing_train", "all"), default = "missing_train", tags = c("train", "predict"))
        ))
        ps$values = list(method_num = "median", method_fct = "newlvl", add_dummy = "missing_train")
        super$initialize(id, ps, param_vals = param_vals)
      },

      build_graph = function() {
        numimputer = switch(self$param_set$values$method_num,
          median = po("imputemedian"),
          mean = po("imputemean"),
          mode = po("imputemode", id = "num_mode", param_vals = list(ties_method = "first")),
          sample = po("imputesample", id = "num_sample"),
          hist = po("imputehist"))
        fctimputer = switch(self$param_set$values$method_fct,
          newlvl = po("imputenewlvl"),
          sample = po("imputesample", id = "fct_sample"),
          mode = po("imputemode", id = "fct_mode", param_vals = list(ties_method = "first")))

        if (self$param_set$values$add_dummy == "none") {
          dummyselector = selector_none()
        } else if (self$param_set$values$method_fct == "newlvl") {
          dummyselector = selector_invert(selector_type(c("factor", "ordered", "character")))
        } else {
          dummyselector = selector_all()
        }

        graph = list(
          po("select", id = "num_select", selector = selector_type(c("integer", "numeric"))) %>>% numimputer,
          po("select", id = "fct_select", selector = selector_type(c("factor", "ordered", "character"))) %>>% fctimputer,
          po("select", id = "lgl_select", selector = selector_type(c("logical"))) %>>% po("imputesample", id = "lgl_sample"),
          po("select", id = "dummyselector", selector = dummyselector) %>>% po("missind", type = "logical",
            which = switch(self$param_set$values$add_dummy, none = "all", self$param_set$values$add_dummy))
        ) %>>% po("featureunion")
      },

      get_state = function(task) {


        graph = self$build_graph()
        graph$train(task)
        list(gs = graph$state)
      },

      transform = function(task) {
        graph = self$build_graph()
        graph$state = self$state$gs
        graph$predict(task)[[1]]
      }
    )
  )

  task = mlr_tasks$get("pima")

  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task)

  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = mlr_tasks$get("iris"))

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
    l = factor(letters[rep(1:2, 3)])
  )

  task = TaskClassif$new("mdata", as_data_backend(mdata), target = "l")
  mdata$j = NULL
  mdata$k = NULL
  task_no_lgl = TaskClassif$new("mdata", as_data_backend(mdata), target = "l")

  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task_no_lgl,
    constargs = list(param_vals = list(
      method_num = "median",
      method_fct = "newlvl",
      add_dummy = "none")))
  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task,
    deterministic_train = FALSE, deterministic_predict = FALSE,
    constargs = list(param_vals = list(
      method_num = "median",
      method_fct = "newlvl",
      add_dummy = "none")))

  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task_no_lgl,
    constargs = list(param_vals = list(
      method_num = "mean",
      method_fct = "newlvl",
      add_dummy = "missing_train")))
  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task,
    deterministic_train = FALSE, deterministic_predict = FALSE,
    constargs = list(param_vals = list(
      method_num = "mean",
      method_fct = "newlvl",
      add_dummy = "missing_train")))

  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task_no_lgl,
    constargs = list(param_vals = list(
      method_num = "mode",
      method_fct = "mode",
      add_dummy = "missing_train")))
  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task,
    deterministic_train = FALSE, deterministic_predict = FALSE,
    constargs = list(param_vals = list(
      method_num = "mode",
      method_fct = "mode",
      add_dummy = "missing_train")))

  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task,
    deterministic_train = FALSE, deterministic_predict = FALSE,
    constargs = list(param_vals = list(
      method_num = "sample",
      method_fct = "sample",
      add_dummy = "all")))

  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task,
    deterministic_train = FALSE, deterministic_predict = FALSE,
    constargs = list(param_vals = list(
      method_num = "hist",
      method_fct = "sample",
      add_dummy = "all")))

  po = PipeOpTestImpute$new(param_vals = list(
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


  po = PipeOpTestImpute$new(param_vals = list(
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

  po = PipeOpTestImpute$new(param_vals = list(
    method_num = "median", method_fct = "newlvl", add_dummy = "missing_train"))

  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()
  task_predicted = po$predict(list(task$clone(deep = TRUE)$filter(1:3)))[[1]]$data()

  expect_set_equal(colnames(task_trained), c(letters[1:12], paste0("missing_", c("a", "c", "k"))))
  expect_set_equal(colnames(task_predicted), c(letters[1:12], paste0("missing_", c("a", "c", "k"))))

  po = PipeOpTestImpute$new(param_vals = list(
    method_num = "median", method_fct = "newlvl", add_dummy = "none"))

  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()
  task_predicted = po$predict(list(task$clone(deep = TRUE)$filter(1:3)))[[1]]$data()

  expect_equal(task_predicted, task$clone(deep = TRUE)$filter(1:3)$data())

  po = PipeOpTestImpute$new(param_vals = list(
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

test_that("More tests for PipeOpImputeMode", {
 set.seed(1)
 dat <- data.frame(y = rnorm(10L), x1 = as.character(1L:10L), x2 = rnorm(10L), x3 = factor(rep(c(1L, 2L), each = 5L)),
   x4 = ordered(rep(1L:5L, times = 2L)), x5 = 1L:10L, x6 = rep(c(TRUE, FALSE), times = 5L), stringsAsFactors = FALSE)
 dat[c(1L, 10L), ] <- NA
 task = TaskRegr$new("task", backend = dat, target = "y")

 task_NA = task
 task_NA$filter(c(1L, 10L))

 # works for complete NA
 po_NA = PipeOpImputeMode$new()
 task_NA_trained = po_NA$train(list(task_NA))[[1L]]$data()
 expect_equal(levels(task_NA_trained[[4L]]), as.character(1:2))
 expect_equal(levels(task_NA_trained[[5L]]), as.character(1:5))
 expect_false(any(is.na(task_NA_trained[[4L]])))
 expect_false(any(is.na(task_NA_trained[[5L]])))

 expect_equivalent(sapply(po_NA$state$model, FUN = function(x) class(x)[1L]),
   c("numeric", "character", "character", "integer", "logical"))
 task_NA_predicted = po_NA$predict(list(task_NA))[[1L]]$data()

 expect_equal(levels(task_NA_predicted[[4L]]), as.character(1:2))
 expect_equal(levels(task_NA_predicted[[5L]]), as.character(1:5))
 expect_false(any(is.na(task_NA_predicted[[4L]])))
 expect_false(any(is.na(task_NA_predicted[[5L]])))


})
