context("meta")
# test some helper functions


test_that("expect_deep_clone catches non-deep clones", {

  po = PipeOpTest1$new()

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
