
# Test the creation of a CnfSymbol in a CnfUniverse
test_that("CnfSymbol is created correctly in CnfUniverse", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  expect_s3_class(X, "CnfSymbol")
  expect_equal(c(X), "X")

  expect_equal(u[["X"]], c("a", "b", "c"))
})

# Test that CnfSymbol cannot be created with an empty domain
test_that("CnfSymbol cannot be created with an empty domain", {
  u = CnfUniverse()

  expect_error(CnfSymbol(u, "X", character(0)),
               "Must have length >= 1")
})

# Test print and format methods for CnfSymbol
test_that("print and format methods for CnfSymbol work correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  # Test print method
  expect_output(print(X), "CnfSymbol 'X' with domain \\{a, b, c\\}.")

  # Test format method
  expect_equal(format(X), "CnfSymbol(X)")
})

# Test the %among% operator with CnfSymbol
test_that("%among% operator works correctly for CnfSymbol", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  # Test valid uses of %among%
  atom1 = X %among% c("a", "b")
  expect_s3_class(atom1, "CnfAtom")

  atom2 = X %among% "a"
  expect_s3_class(atom2, "CnfAtom")

  atom3 = X %among% character(0)  # Contradiction case
  expect_s3_class(atom3, "CnfAtom")
  expect_true(isFALSE(as.logical(atom3)))

  atom4 = X %among% c("a", "b", "c")  # Tautology case
  expect_s3_class(atom4, "CnfAtom")
  expect_true(isTRUE(as.logical(atom4)))
})

# Test invalid use of %among% operator with values not in the domain
test_that("Invalid values in %among% operator throw an error", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  expect_error(X %among% c("d", "e"),
               "values.*Must be a subset of")
})
