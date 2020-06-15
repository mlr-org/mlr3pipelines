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
  assert_subset(po_1$tags, mlr_reflections$pipeops$valid_tags)

  expect_output(expect_equal(po_1$train(list(1)), list(output = 1)), "Training debug.basic")
  expect_equal(po_1$state, list(input = 1))
  expect_true(po_1$is_trained)
})


test_that("PipeOp - simple tests with PipeOpScale", {
  p = PipeOpScale$new()
  expect_class(p, "PipeOpScale")
  expect_false(p$is_trained)
  expect_class(p$param_set, "ParamSet")
})

test_that("PipeOp printer", {
  expect_output(print(PipeOpNOP$new()),
    "PipeOp.*<nop>.*not trained.*values.*list().*Input channels.*input \\[\\*,\\*\\]\n.*Output channels.*output \\[\\*,\\*\\]$")


  expect_output(print(PipeOpDebugMulti$new(3, 4)),
    "PipeOp.*<debug.multi>.*not trained.*values.*list().*Input channels.*input_1 \\[\\*,\\*\\], input_2 \\[\\*,\\*\\], input_3 \\[\\*,\\*\\]\n.*Output channels.*output_1 \\[\\*,\\*\\], output_2 \\[\\*,\\*\\], output_3 \\[\\*,\\*\\], output_4 \\[\\*,\\*\\]$")


  expect_output(print(PipeOpDebugMulti$new(100, 0)),
    "\\[\\.\\.\\. \\([0-9]+ lines omitted\\)\\]")

  expect_output(print(PipeOpBranch$new(c("odin", "dva", "tri"))),
    "Output channels.*odin \\[\\*,\\*\\], dva \\[\\*,\\*\\], tri \\[\\*,\\*\\]$")

  expect_output(print(PipeOpLearner$new(mlr_learners$get("classif.debug"))),
    "PipeOp.*<classif.debug>.*Input channels.*input \\[TaskClassif,TaskClassif\\]\nOutput channels.*output \\[NULL,PredictionClassif\\]$")
})

test_that("Prevent creation of PipeOps with no channels", {
  expect_class(PipeOp$new("id", input = data.table(name = "input", train = "*", predict = "*"),
    output = data.table(name = "output", train = "*", predict = "*")), "PipeOp")

  expect_error(PipeOp$new("id", input = data.table(name = "input", train = "*", predict = "*")[FALSE],
    output = data.table(name = "output", train = "*", predict = "*")), "input.*at least 1 row")

  expect_error(PipeOp$new("id", input = data.table(name = "input", train = "*", predict = "*"),
    output = data.table(name = "output", train = "*", predict = "*")[FALSE]), "output.*at least 1 row")
})

test_that("Errors occur for inputs", {
  po = PipeOp$new("id", input = data.table(name = "input", train = "*", predict = "*"),
    output = data.table(name = "output", train = "*", predict = "*"))
  expect_error(train_pipeop(po, list(mlr_tasks$get("iris"))), "abstract")
  po$state = list(NULL)
  expect_error(predict_pipeop(po, list(mlr_tasks$get("iris"))), "abstract")
  expect_error({
    po$param_set = ParamSet$new()
  }, "read-only")
})

test_that("Caching ABs", {
  po = po("scale")
  expect_true(po$cache)
  expect_true(po$cache_state)
  expect_true(length(po$stochastic) == 0L)
  po$cache = FALSE
  expect_error({po$cache_state = TRUE})
  po$stochastic = "train"
  expect_true(!po$cache)
  expect_true(po$stochastic == "train")
})
