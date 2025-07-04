library("testthat")

context("PipeOpInfo") # deprecated in 3rd edition

# here we check whether the PipeOpInfo po inherits from PipeOp
test_that("basic properties", {
  po = PipeOpInfo$new("info")
  expect_pipeop(po)
  browser()
  expect_pipeop_class(PipeOpInfo, list(id = "info"))
})

# expect_output
test_that("output behavior is appropriate", {
  po_cat = PipeOpInfo$new(id = "info", log_target = "cat")
  expect_output(poinfo_cat$train(list(tsk("iris"))))
  po_warning = PipeOpInfo$new(id = "info", log_target = "warning")
  expect_warning(po_warning$train(list(tsk("iris"))))
  po_message = PipeOpInfo$new(id = "info", log_target = "message")
  expect_message(po_message$train(list(tsk("iris"))))
  po_none = PipeOpInfo$new(id = "info", log_target = "none")
  expect_silent(po_none$train(list(tsk("iris")))) #passt das? -- nochmal nachgucken
}
)
# kann man eventuell mit ner schleife zu kürzerem Code machen


