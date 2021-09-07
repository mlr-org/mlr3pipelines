context("meta")
# test some helper functions


test_that("expect_deep_clone catches non-deep clones", {
  po = PipeOpDebugBasic$new()

  expect_error(expect_deep_clone(po, po))  # can't use expect_failure for some reason
  po1 = po$clone(deep = TRUE)
  expect_deep_clone(po, po1)
  po1$state = 1
  expect_failure(expect_deep_clone(po, po1))

  po$state = 1
  expect_deep_clone(po, po1)

  po$state = new.env()
  po1$state = new.env()
  expect_deep_clone(po, po1)

  po$state = po1$state
  expect_error(expect_deep_clone(po, po1))

  po1$state = 1
  po$state = po1$state
  expect_deep_clone(po, po1)
})

test_that("expect_shallow_clone catches non-clones", {
  gr = Graph$new()$add_pipeop(PipeOpScale$new())
  expect_error(expect_deep_clone(gr, gr$clone()))
  expect_shallow_clone(gr, gr$clone())
  expect_error(expect_shallow_clone(gr, gr))
  expect_error(expect_shallow_clone(gr, NULL))
})

# PO defined in helper_pipeops.R
test_that("Test auxiliary PipeOps", {
  expect_pipeop_class(PipeOpDebugBasic)
  expect_pipeop_class(PipeOpDebugMulti, list(inputs = 1, outputs = 1))
  expect_pipeop_class(PipeOpDebugMulti, list(inputs = 2, outputs = 3))
})
