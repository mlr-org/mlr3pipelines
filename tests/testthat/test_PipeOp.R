context("PipeOp")

# PO defined in helper_pipeops.R
test_that("PipeOp - General functions", {
  # Test a lot of the standard slots of a PipeOp
  po_1 = PipeOpTest1$new()
  expect_class(po_1, "PipeOpTest1")
  expect_true(po_1$id == "th_po")
  expect_false(po_1$is_trained)
  expect_class(po_1$param_set, "ParamSet")
  expect_list(po_1$param_vals, names = "unique")
  expect_output(print(po_1), "PipeOp:")
  expect_true(po_1$packages == "package1")
  expect_null(po_1$state)
  expect_null(po_1$result)

  expect_true(po_1$train() == 1)
  expect_true(po_1$state == 1)
  expect_true(po_1$is_trained)
  expect_true(po_1$predict() == 2)
})



# PO defined in helper_pipeops.R
test_that("Test auxiliary PipeOps", {
  expect_pipeop_class(po_1)
  expect_pipeop_class(PipeOpDebugBasic)
  expect_pipeop_class(PipeOpDebugMulti, list(inputs = 1, outputs = 1))
  expect_pipeop_class(PipeOpDebugMulti, list(inputs = 2, outputs = 3))
})

test_that("PipeOp - simple tests with PipeOpScale", {
  p = PipeOpScale$new()
  expect_class(p, "PipeOpScale")
  expect_false(p$is_trained)
  expect_class(p$param_set, "ParamSet")
})
