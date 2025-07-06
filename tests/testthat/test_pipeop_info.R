library("testthat")

context("PipeOpInfo") # deprecated in 3rd edition

# here we check whether the PipeOpInfo po inherits from PipeOp
test_that("basic properties", {
  po = PipeOpInfo$new("info")
  expect_pipeop(po)
  browser()
  expect_pipeop_class(PipeOpInfo, list(id = "info"))
})


# Works with Task as input
# expect_output
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
# could be condensed into a loop
# formulate the same for $predict?


for (i in c("cat", "warning", "message", "none")) {

}
test_that("output behavior is appropriate", {
for (input in c(tsk("iris"), prediction)) {
mapply(function(output, func_list) {
  po = PipeOpInfo$new(id = "info", log_target = output)
  func_list(po$train(list(input)))},
  output = c("cat", "warning", "message", "none"),
  func_list = list(expect_output, expect_warning, expect_message, expect_silent)
)
}})

test_that("output behavior is appropriate", {
  inputs = c(tsk("iris"), prediction)
  lapply(inputs, function(inputs) {
    mapply(function(output, func_list) {
      po = PipeOpInfo$new(id = "info", log_target = output)
      func_list(po$train(list(inputs)))},
      output = c("cat", "warning", "message", "none"),
      func_list = list(expect_output, expect_warning, expect_message, expect_silent)
    )
  })
})
