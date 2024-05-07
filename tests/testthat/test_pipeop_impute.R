context("PipeOpImpute")

test_that("PipeOpImpute", {
  skip_on_cran()  # slow test, so we don't do it on cran
  # create bogus impute pipeop that behaves like the old impute pipeop. This lets us do tests quickly. FIXME needs to be cleaned up. a lot.

  set.seed(1)

  PipeOpTestImpute = R6Class("PipeOpTestImpute", inherit = PipeOpTaskPreprocSimple,
    public = list(
      initialize = function(id = "impute", param_vals = list()) {
        ps = ps(
          method_num = p_fct(c("median", "mean", "mode", "sample", "hist", "oor", "constant"), tags = c("train", "predict")),
          method_fct = p_fct(c("oor", "sample", "mode", "constant"), tags = c("train", "predict")),
          add_dummy = p_fct(c("none", "missing_train", "all"), tags = c("train", "predict")),
          innum = p_uty(tags = c("train", "predict"))
        )
        ps$values = list(method_num = "median", method_fct = "oor", add_dummy = "missing_train")
        super$initialize(id, ps, param_vals = param_vals)
      },

      build_graph = function() {
        numimputer = switch(self$param_set$values$method_num,
          median = po("imputemedian"),
          mean = po("imputemean"),
          mode = po("imputemode", id = "num_mode"),
          sample = po("imputesample", id = "num_sample"),
          hist = po("imputehist"),
          constant = po("imputeconstant", id = "num_constant", param_vals = list(constant = -999)),
          oor = po("imputeoor", id = "num_oor"))
        fctimputer = switch(self$param_set$values$method_fct,
          oor = po("imputeoor", id = "fct_oor"),
          sample = po("imputesample", id = "fct_sample"),
          mode = po("imputemode", id = "fct_mode"),
          constant = po("imputeconstant", id = "fct_constant", param_vals = list(constant = ".MISSING", check_levels = FALSE)))

        if (self$param_set$values$add_dummy == "none") {
          dummyselector = selector_none()
        } else if (self$param_set$values$method_fct == "oor") {
          dummyselector = selector_invert(selector_type(c("factor", "ordered", "character")))
        } else {
          dummyselector = selector_all()
        }

        graph = list(
          po("select", id = "num_select", selector = selector_type(c("integer", "numeric"))) %>>% numimputer,
          po("select", id = "fct_select", selector = selector_type(c("factor", "ordered"))) %>>% fctimputer,
          po("select", id = "lgl_select", selector = selector_type("logical")) %>>% po("imputesample", id = "lgl_sample"),
          po("select", id = "chr_select", selector = selector_type("character")) %>>% po("imputeconstant", id = "chr_const"),
          po("select", id = "dummyselector", selector = dummyselector) %>>% po("missind", type = "logical", affect_columns = NULL,
            which = switch(self$param_set$values$add_dummy, none = "all", self$param_set$values$add_dummy))
        ) %>>% if (is.null(self$param_set$values$innum)) po("featureunion") else po("featureunion", innum = self$param_set$values$innum)
      }
    ),
    private = list(
      .get_state = function(task) {
        graph = self$build_graph()
        internal_valid_task = task$internal_valid_task
        on.exit({task$internal_valid_task = internal_valid_task})
        task$internal_valid_task = NULL
        graph$train(task)
        list(gs = graph)
      },

      .transform = function(task) {
        graph = self$state$gs
        graph$predict(task)[[1]]
      },
      deep_clone = function(name, value) {
        if (name == "state" && "gs" %in% names(value)) {
          value$gs = value$gs$clone(deep = TRUE)
          return(value)
        }
        super$deep_clone(name, value)
      }
    )
  )

  task = mlr_tasks$get("pima")


  expect_datapreproc_pipeop_class(PipeOpTestImpute, constargs = list(param_vals = list(innum = c("a", "b", "c", "d", "e"))), task = task)

  expect_datapreproc_pipeop_class(PipeOpTestImpute, constargs = list(param_vals = list(innum = c("a", "b", "c", "d", "e"))), task = mlr_tasks$get("iris"))



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
    l = factor(letters[rep(1:2, 3)]),
    m = c(-.Machine$integer.max, -10000000L, 0L, 10000000L, .Machine$integer.max, NA)
  )

  task = TaskClassif$new("mdata", as_data_backend(mdata), target = "l")
  mdata$j = NULL
  mdata$k = NULL
  task_no_lgl = TaskClassif$new("mdata", as_data_backend(mdata), target = "l")

  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task_no_lgl,
    constargs = list(param_vals = list(
      method_num = "median",
      method_fct = "oor",
      add_dummy = "none")))

  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task,
    deterministic_train = FALSE, deterministic_predict = FALSE,
    constargs = list(param_vals = list(
      method_num = "median",
      method_fct = "oor",
      add_dummy = "none")))

  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task_no_lgl,
    constargs = list(param_vals = list(
      method_num = "mean",
      method_fct = "oor",
      add_dummy = "missing_train")))

  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task,
    deterministic_train = FALSE, deterministic_predict = FALSE,
    constargs = list(param_vals = list(
      method_num = "mean",
      method_fct = "oor",
      add_dummy = "missing_train")))

  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task_no_lgl,
    deterministic_train = FALSE, deterministic_predict = FALSE,
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


  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task,
  deterministic_train = FALSE, deterministic_predict = FALSE,
  constargs = list(param_vals = list(
    method_num = "constant",
    method_fct = "constant",
    add_dummy = "all")))

  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task_no_lgl,
    deterministic_train = FALSE, deterministic_predict = FALSE,
    constargs = list(param_vals = list(
      method_num = "oor",
      method_fct = "oor",
      add_dummy = "missing_train")))
  expect_datapreproc_pipeop_class(PipeOpTestImpute, task = task,
    deterministic_train = FALSE, deterministic_predict = FALSE,
    constargs = list(param_vals = list(
      method_num = "oor",
      method_fct = "oor",
      add_dummy = "missing_train")))

  po = PipeOpTestImpute$new(param_vals = list(
    method_num = "sample", method_fct = "sample", add_dummy = "all"))

  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()

  task_predicted = po$predict(list(task))[[1]]$data()

  expect_equal(task_trained[1, c("a", "c", "d", "f", "k", "m")],
    task_trained[2, c("a", "c", "d", "f", "k", "m")])

  expect_equal(task_predicted[c(5:6), c("a", "c", "d", "f", "k", "m")],
    task_trained[c(1:2), c("a", "c", "d", "f", "k", "m")])

  expect_equal(task_trained$missing_a, c(FALSE, TRUE))
  expect_equal(task_trained$missing_c, c(FALSE, TRUE))
  expect_equal(task_trained$missing_d, c(FALSE, TRUE))
  expect_equal(task_trained$missing_f, c(FALSE, TRUE))
  expect_equal(task_trained$missing_h, c(FALSE, TRUE))
  expect_equal(task_trained$missing_k, c(FALSE, TRUE))
  expect_equal(task_trained$missing_m, c(FALSE, TRUE))

  expect_equal(task_trained$missing_b, c(FALSE, FALSE))
  expect_equal(task_trained$missing_e, c(FALSE, FALSE))
  expect_equal(task_trained$missing_g, c(FALSE, FALSE))
  expect_equal(task_trained$missing_i, c(FALSE, FALSE))
  expect_equal(task_trained$missing_j, c(FALSE, FALSE))

  expect_set_equal(colnames(task_trained), c(letters[1:13], paste0("missing_", letters[c(1:11, 13)])))
  expect_set_equal(colnames(task_predicted), c(letters[1:13], paste0("missing_", letters[c(1:11, 13)])))

  po = PipeOpTestImpute$new(param_vals = list(
    method_num = "median", method_fct = "oor", add_dummy = "all"))

  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()
  task_predicted = po$predict(list(task))[[1]]$data()

  expect_equal(task_trained[1, c("a", "c", "k", "m")],
    task_trained[2, c("a", "c", "k", "m")])

  expect_equal(task_predicted[5:6, ],
    task_trained[1:2])

  expect_set_equal(colnames(task_trained), c(letters[1:13], paste0("missing_", c("a", "b", "c", "j", "k", "m"))))
  expect_set_equal(colnames(task_predicted), c(letters[1:13], paste0("missing_", c("a", "b", "c", "j", "k", "m"))))

  expect_equal(task_trained$d[2], factor(".MISSING", levels = c(letters[1:6], ".MISSING")))
  expect_equal(task_trained$h[2], ".MISSING")

  po = PipeOpTestImpute$new(param_vals = list(
    method_num = "median", method_fct = "oor", add_dummy = "missing_train"))

  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()
  task_predicted = po$predict(list(task$clone(deep = TRUE)$filter(1:3)))[[1]]$data()

  expect_set_equal(colnames(task_trained), c(letters[1:13], paste0("missing_", c("a", "c", "k", "m"))))
  expect_set_equal(colnames(task_predicted), c(letters[1:13], paste0("missing_", c("a", "c", "k", "m"))))

  po = PipeOpTestImpute$new(param_vals = list(
    method_num = "median", method_fct = "oor", add_dummy = "none"))

  task_trained = po$train(list(task$clone(deep = TRUE)$filter(5:6)))[[1]]$data()
  task_predicted = po$predict(list(task$clone(deep = TRUE)$filter(1:3)))[[1]]$data()

  expect_equal(task_predicted, task$clone(deep = TRUE)$filter(1:3)$data(), ignore.col.order = TRUE)

  po = PipeOpTestImpute$new(param_vals = list(
    method_num = "hist", method_fct = "oor", add_dummy = "missing_train"))

  for (i in range(10)) {
    task_trained = po$train(list(task))[[1]]$data()

    task_predicted = po$predict(list(task))[[1]]$data()

    expect_true(task_trained$a[6] <= 5 && task_trained$a[6] >= 1)
    expect_true(task_trained$c[6] <= 5 && task_trained$c[6] >= 1)
    expect_true(task_predicted$a[6] <= 5 && task_trained$a[6] >= 1)
    expect_true(task_predicted$c[6] <= 5 && task_trained$c[6] >= 1)
  }

  # impute full na columns:
  po = PipeOpTestImpute$new(param_vals = list(method_num = "median", method_fct = "oor"))

  mdata = data.table(
    stringsAsFactors = FALSE,
    a = as.numeric(rep(NA, 3)),
    b = as.integer(rep(NA, 3)),
    c = factor(rep(NA, 3), levels = "a"),
    d = factor(rep(NA, 3), ordered = TRUE, levels = "a"),
    e = as.logical(rep(NA, 3)),
    f = as.character(rep(NA, 3)),
    t = as.factor(letters[rep(1:2, 3)])
  )
  task = TaskClassif$new("mdata", as_data_backend(mdata), target = "t")

  pmap(list(map_chr(map(mdata[, -"t"], class), 1L), colnames(mdata[, -"t"])), function(type, name) {
    po$param_set$values$affect_columns = selector_type(type)
    cst = switch(type,
        factor = factor("a"),
        integer = 0L,
        logical = c(TRUE, FALSE),
        numeric = 0,
        ordered = ordered("a"),
        character = ".MISSING"
      )
    out1 = po$train(list(task))[[1]]$data()
    out2 = po$predict(list(task))[[1]]$data()
    expect_true(all(out1[[name]] %in% cst))
    expect_true(all(out2[[name]] %in% cst))
    if (type != "logical") {
      expect_equal(out1, out2)
    }
  })
})

test_that("More tests for PipeOpImputeMode", {
  set.seed(1)
  dat = data.frame(y = rnorm(10L), x1 = as.character(1L:10L), x2 = rnorm(10L), x3 = factor(rep(c(1L, 2L), each = 5L)),
  x4 = ordered(rep(1L:5L, times = 2L)), x5 = 1L:10L, x6 = rep(c(TRUE, FALSE), times = 5L), stringsAsFactors = FALSE)
  dat[c(1L, 10L), ] = NA
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

test_that("More tests for PipeOpImputeConstant", {
 set.seed(1)
 dat = data.frame(y = rnorm(10L), x1 = as.character(1L:10L), x2 = rnorm(10L), x3 = factor(rep(c(1L, 2L), each = 5L)),
   x4 = ordered(rep(1L:5L, times = 2L)), x5 = 1L:10L, x6 = rep(c(TRUE, FALSE), times = 5L),
   x7 = as.POSIXct(1L:10L, origin = "1960-01-01", tz = "GMT"), stringsAsFactors = FALSE)
 dat[1L, ] = NA
 task = TaskRegr$new("task", backend = dat, target = "y")

 po = PipeOpImputeConstant$new(param_vals = list(constant = "test"))

 expect_error(po$train(list(task)))

 po$param_set$values$affect_columns = selector_type("character")
 train_out = po$train(list(task))[[1L]]
 expect_equal(train_out$feature_types, task$feature_types)
 expect_equal(sum(train_out$missings()), 7L)
 expect_equal(train_out$data(cols = "x1")[[1L]][1L], "test")

 po$param_set$values = list(constant = -999, check_levels = TRUE, affect_columns = selector_type("numeric"))
 train_out = po$train(list(task))[[1L]]
 expect_equal(train_out$feature_types, task$feature_types)
 expect_equal(sum(train_out$missings()), 7L)
 expect_equal(train_out$data(cols = "x2")[[1L]][1L], -999)

 po$param_set$values = list(constant = "test", check_levels = TRUE, affect_columns = selector_type("factor"))
 expect_error(po$train(list(task))[[1L]])
 po$param_set$values$check_levels = FALSE
 train_out = po$train(list(task))[[1L]]
 expect_equal(train_out$feature_types, task$feature_types)
 expect_equal(sum(train_out$missings()), 7L)
 expect_equal(po$train(list(task))[[1L]]$data(cols = "x3")[[1L]][1L], factor("test", levels = c("1", "2", "test")))
 po$param_set$values$constant = factor("test", levels = c("test", "another"))
 expect_equal(po$train(list(task))[[1L]]$data(cols = "x3")[[1L]][1L], factor("test", levels = c("1", "2", "test")))

 po$param_set$values = list(constant = "test", check_levels = TRUE, affect_columns = selector_type("ordered"))
 expect_error(po$train(list(task))[[1L]])
 po$param_set$values$check_levels = FALSE
 train_out = po$train(list(task))[[1L]]
 expect_equal(train_out$feature_types, task$feature_types)
 expect_equal(sum(train_out$missings()), 7L)
 expect_equal(po$train(list(task))[[1L]]$data(cols = "x4")[[1L]][1L], ordered("test", levels = c("1", "2", "3", "4", "5", "test")))
 po$param_set$values$constant = factor("test", levels = c("test", "another"))
 expect_equal(po$train(list(task))[[1L]]$data(cols = "x4")[[1L]][1L], ordered("test", levels = c("1", "2", "3", "4", "5", "test")))

 po$param_set$values = list(constant = -999, check_levels = TRUE, affect_columns = selector_type("integer"))
 train_out = po$train(list(task))[[1L]]
 expect_equal(train_out$feature_types, task$feature_types)
 expect_equal(sum(train_out$missings()), 7L)
 expect_equal(train_out$data(cols = "x5")[[1L]][1L], -999)

 po$param_set$values = list(constant = TRUE, check_levels = TRUE, affect_columns = selector_type("logical"))
 train_out = po$train(list(task))[[1L]]
 expect_equal(train_out$feature_types, task$feature_types)
 expect_equal(sum(train_out$missings()), 7L)
 expect_equal(train_out$data(cols = "x6")[[1L]][1L], TRUE)

 pos_impute = as.POSIXct(1000000, origin = "1960-01-01", tz = "GMT")
 po$param_set$values = list(constant = pos_impute, check_levels = TRUE, affect_columns = selector_type("POSIXct"))
 train_out = po$train(list(task))[[1L]]
 expect_equal(sum(train_out$missings()), 7L)
 expect_equal(train_out$data(cols = "x7")[[1L]][1L], pos_impute)
})


test_that("More tests for Integers", {
  data <- data.table(x = c(-.Machine$integer.max, -10000000L, 0L, 10000000L, .Machine$integer.max, rep(NA, 1001)), t = 1:1006)

  task = TaskRegr$new("task", backend = data, target = "t")
  pos = list(PipeOpImputeHist$new(), PipeOpImputeMean$new(), PipeOpImputeSample$new(), PipeOpImputeMedian$new(), PipeOpImputeMode$new(), PipeOpImputeOOR$new())


  for (po in pos) {
    result <- po$train(list(task))[[1]]

    expect_integer(result$data()$x, info = po$id)
    expect_false(any(is.na(result$data()$x)), info = po$id)
    expect_equal(result$missings(), c(t = 0, x = 0), info = po$id)
  }

})

test_that("impute, test rows and affect_columns", {
  po_impute = po("imputeconstant", affect_columns = selector_name("insulin"), constant = 2)
  task = tsk("pima")
  task$divide(1:30)
  outtrain = po_impute$train(list(task))[[1L]]
  outpredict = po_impute$predict(list(task$internal_valid_task))[[1L]]
  expect_true(isTRUE(all.equal(outtrain$internal_valid_task$data(), outpredict$data())))
})
