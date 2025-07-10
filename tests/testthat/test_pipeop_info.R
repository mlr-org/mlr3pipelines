library("testthat")

# here we check whether the PipeOpInfo po inherits from PipeOp
# this actully does not work rn --> check
test_that("basic properties", {
  po = PipeOpInfo$new("info")
  expect_pipeop(po) # redundant?
  browser()
  expect_pipeop_class(PipeOpInfo, list(id = "info"))
})


# loop form with lapply
test_that("output behavior is appropriate", {
  # Creation of Prediction Object
  browser()
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  mtcars_new = subset(mtcars[sample(nrow(mtcars), size = 10), ], select = -mpg)
  prediction = lrn_rpart$predict_newdata(mtcars)
  prediction_new = lrn_rpart$predict_newdata(mtcars_new)
  # Actual Test
  inputs = c(tsk("iris"), prediction, prediction_new, NULL, "default_string")
  output = c("lgr::mlr3/mlr3pipelines::info", "cat", "warning", "message", "none")
  expect_func = list(expect_output, expect_output, expect_warning, expect_message, expect_silent)
  for (j in inputs) {
    for (i in seq_len(length(output))) {
      poinfo = PipeOpInfo$new(id = "info", log_target = output[i])
      expect_func[[i]](invisible(poinfo$train(list(j))))
      expect_func[[i]](invisible(poinfo$predict(list(j))))
    }
  }
})

# Notiz @myself: mapply/lapply geben output zurück; für Transformation verwenden; in solchen Fällen lieber for
# mlr3misc: map Funktionen existieren (walk-Funktion); lapply mit Ergebnis weggeschmissen; walk kommuniziert an andere Codeleser was ich genau will

test_that("check whether input and output are equal", {
  # Creation of Prediction Object
  browser()
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  mtcars_new = subset(mtcars[sample(nrow(mtcars), size = 10), ], select = -mpg)
  prediction = lrn_rpart$predict_newdata(mtcars)
  prediction_new = lrn_rpart$predict_newdata(mtcars_new)
  # Actual Test
  inputs = c(tsk("iris"), prediction, prediction_new, NULL, "default_string")
  output = c("lgr::mlr3/mlr3pipelines::info", "cat", "warning", "message", "none")
  for (j in inputs) {
    for (i in seq_len(length(output))) {
      poinfo = PipeOpInfo$new(id = "info", log_target = output[i])
      suppressMessages(suppressWarnings(invisible(capture.output(expect_identical(poinfo$train(list(j))[[1]], j)))))
      suppressMessages(suppressWarnings(invisible(capture.output(expect_identical(poinfo$predict(list(j))[[1]], j)))))
    }
  }
})

# Test necessary?
test_that("check, if collect_multiplicity works", {
  # Prepare Multiplicity Object
  po_prepare = po("ovrsplit")
  OVR = po_prepare$train(list(tsk("iris")))
  mapply(function(output, expect_func) {
    browser()
    po = PipeOpInfo$new(id = "info", collect_multiplicity = FALSE, printer = list(Multiplicity = function(x) lapply(x, FUN = function(y) {print(list(task = y, data = y$data()[, 1:min(10, ncol(y$data()))]), topn = 5)})), log_target = output)
    expect_silent(expect_func(po$train(OVR)))   # check silent with Martin
    expect_silent(expect_func(po$predict(OVR)))},
    output = c("lgr::mlr3/mlr3pipelines::info", "cat", "warning", "message", "none"),
    expect_func = list(expect_output, expect_output, expect_warning, expect_message, expect_silent)
  )
})


test_that("malformed log_target handled accordingly", {
  malformed_log_target = c("malformed", "::", "::::::", "log::", "log::log_level", "log::log_level::", "log::log_level::message::", "::log")
  lapply(malformed_log_target, function(x) expect_error(PipeOpInfo$new("info", log_target = x)))
})







# Structure of the test
test_that("output behavior is appropriate", {
  po_cat = PipeOpInfo$new(id = "info", log_target = "cat")
  expect_output(invisible(poinfo_cat$train(list(tsk("iris"))))) # expect output quite generic - is there a way to check cat() more specifically,
  po_warning = PipeOpInfo$new(id = "info", log_target = "warning")
  expect_warning(invisible(po_warning$train(list(tsk("iris")))))
  po_message = PipeOpInfo$new(id = "info", log_target = "message")
  expect_message(invisible(po_message$train(list(tsk("iris")))))
  po_none = PipeOpInfo$new(id = "info", log_target = "none")
  expect_silent(invisible(po_none$train(list(tsk("iris"))))) # check it again

  expect_output(poinfo_cat$predict(list(tsk("iris"))))
  expect_warning(po_warning$predict(list(tsk("iris"))))
  expect_message(po_message$predict(list(tsk("iris"))))
  expect_silent(po_none$predict(list(tsk("iris"))))
}
)

# Looped Versions
# loop form with for
test_that("output behavior is appropriate", {
  for (input in c(tsk("iris"), prediction)) {
    mapply(function(output, func_list) {
      po = PipeOpInfo$new(id = "info", log_target = output)
      func_list(po$train(list(inputs)))
      func_list(po$predict(list(inputs)))},
      output = c("cat", "warning", "message", "none"),
      func_list = list(expect_output, expect_warning, expect_message, expect_silent)
    )
  }})



