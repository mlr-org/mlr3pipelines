context("PipeOp")

# PO defined in helper_pipeops.R
test_that("PipeOp - General functions", {
  # Test a lot of the standard slots of a PipeOp
  po_1 = PipeOpDebugBasic$new()
  expect_class(po_1, "PipeOpDebugBasic")
  expect_true(po_1$id == "debug.basic")
  expect_false(po_1$is_trained)
  expect_class(po_1$param_set, "ParamSet")
  expect_list(po_1$param_vals, names = "unique")
  expect_output(print(po_1), "PipeOp:")
  expect_equal(po_1$packages, character(0))
  expect_null(po_1$state)

  expect_output(expect_equal(po_1$train(list(1)), list(1)), "training debug.basic")
  expect_equal(po_1$state, list(1))
  expect_true(po_1$is_trained)
  expect_output(expect_equal(po_1$predict(list(2)), list(1, 2)), "predicting debug.basic")
})


test_that("PipeOp - simple tests with PipeOpScale", {
  p = PipeOpScale$new()
  expect_class(p, "PipeOpScale")
  expect_false(p$is_trained)
  expect_class(p$param_set, "ParamSet")
})
