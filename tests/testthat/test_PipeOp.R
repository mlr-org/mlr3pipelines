context("PipeOp")

# PO defined in helper_pipeops.R
test_that("PipeOp - General functions", {
  # Test a lot of the standard slots of a PipeOp
  po_1 = PipeOpDebugBasic$new()
  expect_class(po_1, "PipeOpDebugBasic")
  expect_true(po_1$id == "debug.basic")
  expect_false(po_1$is_trained)
  expect_class(po_1$param_set, "ParamSet")
  expect_list(po_1$param_set$values, names = "unique")
  expect_output(print(po_1), "PipeOp:")
  expect_equal(po_1$packages, character(0))
  expect_null(po_1$state)

  expect_output(expect_equal(po_1$train(list(1)), list(1)), "Training debug.basic")
  expect_equal(po_1$state, list(1))
  expect_true(po_1$is_trained)
  expect_output(expect_equal(po_1$predict(list(2)), list(1, 2)), "Predicting debug.basic")
})


test_that("PipeOp - simple tests with PipeOpScale", {
  p = PipeOpScale$new()
  expect_class(p, "PipeOpScale")
  expect_false(p$is_trained)
  expect_class(p$param_set, "ParamSet")
})

test_that("PipeOp printer", {

  expect_output(print(PipeOpNULL$new()),
    "PipeOp.*<NULL>.*not trained.*values.*list().*Input channels.*input \\[\\*,\\*\\]\n.*Output channels.*output \\[\\*,\\*\\]$")


  expect_output(print(PipeOpDebugMulti$new(3, 4)),
    "PipeOp.*<debug.multi>.*not trained.*values.*list().*Input channels.*input_1 \\[\\*,\\*\\], input_2 \\[\\*,\\*\\], input_3 \\[\\*,\\*\\]\n.*Output channels.*output_1 \\[\\*,\\*\\], output_2 \\[\\*,\\*\\], output_3 \\[\\*,\\*\\], output_4 \\[\\*,\\*\\]$")


  expect_output(print(PipeOpDebugMulti$new(100, 0)),
    "\\[\\.\\.\\. \\([0-9]+ lines omitted\\)\\]")

  expect_output(print(PipeOpBranch$new(c("odin", "dva", "tri"))),
    "Output channels.*odin \\[\\*,\\*\\], dva \\[\\*,\\*\\], tri \\[\\*,\\*\\]$")

  expect_output(print(PipeOpLearner$new(mlr_learners$get("classif.debug"))),
    "PipeOp.*<debug>.*Input channels.*input \\[Task,Task\\]\nOutput channels.*output \\[NULL,Prediction\\]$")

})

test_that("Prevent creation of PipeOps with no channels", {

  expect_class(PipeOp$new("id", input = data.table(name = "input", train = "*", predict = "*"),
    output = data.table(name = "output", train = "*", predict = "*")), "PipeOp")

  expect_error(PipeOp$new("id", input = data.table(name = "input", train = "*", predict = "*")[FALSE],
    output = data.table(name = "output", train = "*", predict = "*")), "input.*at least 1 row")

  expect_error(PipeOp$new("id", input = data.table(name = "input", train = "*", predict = "*"),
    output = data.table(name = "output", train = "*", predict = "*")[FALSE]), "output.*at least 1 row")

})
