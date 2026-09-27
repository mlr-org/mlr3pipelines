# suppress warnings as long as half the world still uses v2
context = function(...) suppressWarnings(testthat::context(...))
expect_is = function(...) suppressWarnings(testthat::expect_is(...))
expect_equivalent = function(...) suppressWarnings(testthat::expect_equivalent(...))

library("checkmate")
