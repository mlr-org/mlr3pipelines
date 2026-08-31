context("meta")
# test some helper functions

test_that("expect_equal_data_table uses data.table comparison semantics", {
  actual = data.table(
    x = factor(c("a", "b"), levels = c("a", "b", "unused")),
    y = 1:2
  )
  expected = data.table(x = factor(c("a", "b")), y = 1:2)

  expect_success(expect_equal_data_table(actual, expected))
  expect_failure(expect_equal_data_table(actual, expected, trim_levels = FALSE))

  columns_reordered = actual[, .(y, x)]
  expect_failure(expect_equal_data_table(actual, columns_reordered))
  expect_success(expect_equal_data_table(actual, columns_reordered, ignore_col_order = TRUE))

  rows_reordered = actual[2:1]
  expect_failure(expect_equal_data_table(actual, rows_reordered))
  expect_success(expect_equal_data_table(actual, rows_reordered, ignore_row_order = TRUE))

  different_row_names = copy(actual)
  attr(different_row_names, "row.names") = c(10L, 20L)
  expect_success(expect_equal_data_table(actual, different_row_names))

  keyed = copy(actual)
  setkey(keyed, y)
  expect_failure(expect_equal_data_table(actual, keyed))
  expect_success(expect_equal_data_table(actual, keyed, check_attributes = FALSE))

  different = copy(actual)
  different$y[[1L]] = 10L
  expect_failure(
    expect_equal_data_table(actual, different),
    "Expected `actual` to equal `different` as data.tables"
  )
})

test_that("expect_equal_r6 does not evaluate active bindings", {
  ActiveBindingClass = R6::R6Class(
    "ActiveBindingClass",
    public = list(value = 1),
    active = list(problematic = function(value) stop("active binding was evaluated"))
  )
  object = ActiveBindingClass$new()
  expected = ActiveBindingClass$new()

  expect_success(expect_equal_r6(object, expected))

  expected$value = 2
  expect_failure(expect_equal_r6(object, expected))
})


test_that("expect_deep_clone catches non-deep clones", {
  po = PipeOpDebugBasic$new()

  expect_condition(expect_deep_clone(po, po), class = "expectation_failure")
  po1 = po$clone(deep = TRUE)
  expect_deep_clone(po, po1)
  po1$state = 1
  expect_condition(expect_deep_clone(po, po1), class = "expectation_failure")

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
