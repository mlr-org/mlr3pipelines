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
  expect_equal(po_1$packages, "mlr3pipelines")
  expect_null(po_1$state)
  assert_subset(po_1$tags, mlr_reflections$pipeops$valid_tags)
  expect_error(po_1$predict(list(tsk("iris"))), "has not been trained yet")

  expect_output(expect_equal(po_1$train(list(1)), list(output = 1)), "Training debug.basic")
  expect_equal(po_1$state, list(input = 1))
  expect_true(po_1$is_trained)
  expect_error(po_1$train(tsk("iris")), regexp = "type 'list'")
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

test_that("Errors during training set $state to NULL", {
  po = PipeOp$new("id", input = data.table(name = "input", train = "*", predict = "*"),
    output = data.table(name = "output", train = "*", predict = "*"))
  expect_null(po$state)
  po$state = list("not_null")
  expect_error(po$train(list(mlr_tasks$get("iris"))), regexp = "abstract")
  expect_null(po$state)  # state is completely reset to NULL
})

test_that("Informative error and warning messages", {

  gr = as_graph(lrn("classif.debug"))

  gr$param_set$values$classif.debug.warning_train = 1
  gr$param_set$values$classif.debug.warning_predict = 1

  # two 'expect_warning', because we want to 'expect' that there is exactly one warning.
  # a function argument for expect_warning that tests exactly this would be a good idea, and has therefore been removed -.-
  expect_no_warning(expect_warning(gr$train(tsk("iris")), "This happened PipeOp classif.debug's \\$train\\(\\)$"))

  expect_no_warning(suppressWarnings(gr$train(tsk("iris"))))

  expect_no_warning(expect_warning(gr$predict(tsk("iris")), "This happened PipeOp classif.debug's \\$predict\\(\\)$"))

  expect_no_warning(suppressWarnings(gr$predict(tsk("iris"))))

  gr$param_set$values$classif.debug.warning_train = 0
  gr$param_set$values$classif.debug.warning_predict = 0
  
  gr$param_set$values$classif.debug.error_train = 1
  expect_error(gr$train(tsk("iris")), "This happened PipeOp classif.debug's \\$train\\(\\)$")

  gr$param_set$values$classif.debug.error_train = 0
  gr$param_set$values$classif.debug.error_predict = 1
  # Need to first train the Graph for predict to work
  gr$train(tsk("iris"))
  expect_error(gr$predict(tsk("iris")), "This happened PipeOp classif.debug's \\$predict\\(\\)$")

  potest = R6::R6Class("potest", inherit = PipeOp,
    private = list(
      .train = function(input) {
        self$state = list()
        suppressWarnings(warning("test"))
        list(1)
      },
      .predict = function(input) {
        suppressWarnings(warning("test"))
        list(1)
      }
    )
  )$new(id = "potest", input = data.table(name = "input", train = "*", predict = "*"), output = data.table(name = "input", train = "*", predict = "*"))

  expect_no_warning(potest$train(list(1)))
  expect_no_warning(potest$predict(list(1)))

})

test_that("properties", {
  f = function(properties) {
    PipeOp$new(
      id = "potest",
      input = data.table(name = "input", train = "*", predict = "*"),
      output = data.table(name = "input", train = "*", predict = "*"),
      properties = properties
    )
  }

  expect_error(f("abc"))
  po1 = f("validation")
  expect_equal(po1$properties, "validation")
})
