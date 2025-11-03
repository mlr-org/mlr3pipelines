context("PipeOpDropCollinear")

lib_path = file.path(testthat::test_path(".."), "lib")
if (dir.exists(lib_path)) {
  .libPaths(unique(c(normalizePath(lib_path), .libPaths())))
}

test_that("PipeOpDropCollinear - basic properties", {
  task = TaskRegr$new("toy",
    backend = data.table::data.table(
      y = 1:6,
      x1 = 1:6,
      x2 = 6:1,
      x3 = c(1, 2, 3, 1, 2, 3)
    ),
    target = "y"
  )
  expect_datapreproc_pipeop_class(PipeOpDropCollinear, task = task)
})

test_that("PipeOpDropCollinear removes highly correlated numeric features", {
  data = data.table::data.table(
    y = seq_len(10),
    x1 = seq_len(10),
    x2 = 2 * seq_len(10) + 1,
    x3 = seq_len(10) + rep(c(0, 1), 5),
    x4 = -4 * seq_len(10),
    f = factor(rep(c("a", "b"), 5))
  )
  task = TaskRegr$new("collinear", backend = data, target = "y")

  po = PipeOpDropCollinear$new()
  res = po$train(list(task))[[1]]

  expect_equal(res$feature_names, c("f", "x1", "x3"))
  expect_equal(po$state$keep, c("f", "x1", "x3"))
  expect_equal(po$state$dropped, c("x4", "x2"))
  expect_true(all(is.na(diag(po$state$correlation))))

  pred = po$predict(list(task$clone()))[[1]]
  expect_equal(pred$feature_names, res$feature_names)
  expect_equal(pred$data(rows = pred$row_ids, cols = pred$feature_names),
    res$data(rows = res$row_ids, cols = res$feature_names))

  po_keep = PipeOpDropCollinear$new(param_vals = list(threshold = 1))
  kept = po_keep$train(list(task))[[1]]
  expect_equal(kept$feature_names, task$feature_names)
  expect_length(po_keep$state$dropped, 0)
})

test_that("PipeOpDropCollinear respects missing values handling and affect_columns", {
  data = data.table::data.table(
    y = seq_len(8),
    x1 = c(NA, 2:8),
    x2 = c(NA, 4, 6, 8, 10, 12, 14, 16),
    x3 = c(1, 1, 2, 2, 3, 3, 4, 4),
    g = factor(rep(letters[1:2], each = 4))
  )
  task = TaskRegr$new("collinear_na", backend = data, target = "y")

  po_default = PipeOpDropCollinear$new()
  res_default = po_default$train(list(task))[[1]]
  expect_equal(po_default$state$dropped, "x2")
  expect_equal(res_default$feature_names, c("g", "x1", "x3"))

  po_use = PipeOpDropCollinear$new(param_vals = list(use = "complete.obs"))
  res_use = po_use$train(list(task))[[1]]
  expect_equal(po_use$state$dropped, "x2")
  expect_equal(res_use$feature_names, c("g", "x1", "x3"))

  po_affect = PipeOpDropCollinear$new(param_vals = list(threshold = 0.5))
  po_affect$param_set$values$affect_columns = selector_grep("^x[12]$")
  res_affect = po_affect$train(list(task))[[1]]
  expect_equal(res_affect$feature_names, c("x1", "g", "x3"))
  expect_equal(po_affect$state$dropped, "x2")
})
