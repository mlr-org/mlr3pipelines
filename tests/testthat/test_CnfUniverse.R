# Test the creation of a CnfUniverse
test_that("CnfUniverse is created correctly", {
  u = CnfUniverse()

  expect_s3_class(u, "CnfUniverse")
  expect_environment(u)
})

# Test symbol creation in CnfUniverse and retrieval by name
test_that("CnfSymbol can be added and retrieved", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  # Test retrieval using $
  expect_s3_class(u$X, "CnfSymbol")
  expect_equal(u$X, X)

  # Test retrieval using [[
  expect_equal(u[["X"]], c("a", "b", "c"))
})

# Test that retrieving non-existent symbols throws an error
test_that("Accessing non-existent symbol throws an error", {
  u = CnfUniverse()

  expect_error(u$Z, "Variable 'Z' does not exist in the universe.")
})

# Test that duplicate symbols cannot be created
test_that("Cannot add duplicate symbols to CnfUniverse", {
  u = CnfUniverse()
  CnfSymbol(u, "X", c("a", "b", "c"))

  expect_error(CnfSymbol(u, "X", c("d", "e")), "Variable 'X' already exists in the universe.")
})

# Test print and format methods for CnfUniverse
test_that("print and format methods for CnfUniverse work correctly", {
  u = CnfUniverse()
  CnfSymbol(u, "X", c("a", "b", "c"))

  # Test print method
  expect_output(print(u), "CnfUniverse with variables:\n  X: \\{a, b, c\\}")

  # Test format method
  expect_equal(format(u), "CnfUniverse(1)")

  CnfSymbol(u, "Y", c("d", "e", "f"))

  expect_output(print(u), "CnfUniverse with variables:\n  Y: \\{d, e, f\\}\n  X: \\{a, b, c\\}|CnfUniverse with variables:\n  X: \\{a, b, c\\}\n  Y: \\{d, e, f\\}")
  expect_equal(format(u), "CnfUniverse(2)")
})

# Test print and format methods for an empty CnfUniverse
test_that("print and format methods for empty CnfUniverse work correctly", {
  u = CnfUniverse()

  # Test print method
  expect_output(print(u), "CnfUniverse \\(empty\\)\\.")

  # Test format method
  expect_equal(format(u), "CnfUniverse(0)")
})


test_that("all.equal recognizes (in)equality", {

  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  expect_true(all.equal(u, u))

  u2 = CnfUniverse()
  X2 = CnfSymbol(u2, "X", c("a", "b", "c"))

  expect_true(all.equal(u, u2))

  u_empty = CnfUniverse()
  u_empty_2 = CnfUniverse()

  expect_true(all.equal(u_empty, u_empty_2))

  u3 = CnfUniverse()
  X3 = CnfSymbol(u3, "X", c("c", "b", "a"))

  expect_string(all.equal(u, u3), pattern = "string mismatch")

  expect_string(paste(all.equal(u, u_empty), collapse = "\n"), pattern = "Length mismatch")

})
