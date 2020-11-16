
# compatibility to with broken testthat v3 behaviour
expect_equal = function(object, expected, ..., info = NULL, label = NULL) {
  expect_true(all.equal(object, expected, ...), info = info, label = label)
}

# suppress warnings as long as half the world still uses v2
context = function(...) suppressWarnings(testthat::context(...))
expect_is = function(...) suppressWarnings(testthat::expect_is(...))
expect_equivalent = function(...) suppressWarnings(testthat::expect_equivalent(...))

library("checkmate")
