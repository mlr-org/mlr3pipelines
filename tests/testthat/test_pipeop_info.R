library("testthat")

context("PipeOpInfo") # deprecated in 3rd edition

# here we check whether the PipeOpInfo po inherits from PipeOp
test_that("basic properties", {
  po = PipeOpInfo$new("info")
  expect_pipeop(po)
  browser()
  expect_pipeop_class(PipeOpInfo, list(id = "info"))
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


# loop form with lapply
test_that("output behavior is appropriate", {
  # Creation of Prediction Object
  lrn_rpart = lrn("regr.rpart")$train(tsk("mtcars"))
  mtcars_new = subset(mtcars[sample(nrow(mtcars), size = 10), ], select = -mpg)
  prediction = lrn_rpart$predict_newdata(mtcars)
  prediction_new = lrn_rpart$predict_newdata(mtcars_new)
  # Actual Test
  inputs = list(tsk("iris"), prediction, prediction_new, NULL, "default_string")
  lapply(inputs, function(inputs) {
    mapply(function(output, func_list) {
      po = PipeOpInfo$new(id = "info", log_target = output)
      func_list(po$train(list(inputs)))
      func_list(po$predict(list(inputs)))},
      output = c("cat", "warning", "message", "none"),
      func_list = list(expect_output, expect_warning, expect_message, expect_silent)
    )
  })
})

# Test not necessary?
test_that("collect_multiplicity works", {
  # Prepare Multiplicity Object
  po_prepare = po("ovrsplit")
  OVR = po_prepare$train(list(tsk("iris")))
  mapply(function(output, func_list) {
    po = PipeOpInfo$new(id = "info", collect_multiplicity = TRUE, printer = list(Multiplicity = function(x) lapply(x, FUN = function(y) {print(list(task = y, data = y$data()[, 1:min(10, ncol(y$data()))]), topn = 5)})), log_target = output)
    func_list(po$train(OVR))
    func_list(po$predict(OVR))},
    output = c("cat", "warning", "message", "none"),
    func_list = list(expect_output, expect_warning, expect_message, expect_silent)
  )
})


test_that("malformed log_target handled accordingly", {
  malformed_log_target = c("malformed", "::", "::::::", "log::", "log::log_level", "log::log_level::", "log::log_level::message::")
  lapply(malformed_log_target,
    function(x) expect_error(PipeOpInfo$new("info", log_target = x)))
})

