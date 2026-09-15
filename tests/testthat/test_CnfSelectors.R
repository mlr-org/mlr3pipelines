test_that("CnfClause rejects dimensional selectors", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  clause = as.CnfClause(X %among% c("b", "c"))

  for (indices in list(c(1L, 1L), c("X", "X"), c(TRUE, TRUE))) {
    expect_error(clause[matrix(indices, nrow = 1L)], "atomic vector")
    expect_error(clause[array(indices, c(1L, 2L, 1L))], "atomic vector")
  }
  expect_error(clause[matrix(integer(0), 0L, 0L)], "atomic vector")
  expect_error(as.CnfClause(TRUE)[matrix(TRUE)], "atomic vector")
  expect_error(as.CnfClause(FALSE)[matrix(FALSE)], "atomic vector")
})

test_that("CnfClause rejects missing selectors of every accepted type", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("a", "b", "c"))
  unit = as.CnfClause(X %among% "a")
  clause = X %among% "a" | Y %among% "b"

  for (index in list(NA, NA_integer_, NA_real_, NA_character_)) {
    expect_error(unit[index], "missing")
  }
  for (indices in list(c(FALSE, NA), c(TRUE, NA), c(1, NA), c("X", NA))) {
    expect_error(clause[indices], "missing")
  }
})

test_that("CnfClause retains ordinary vector selection semantics", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("a", "b", "c"))
  x = X %among% c("b", "c")
  y = Y %among% "a"
  clause = x | y

  expect_identical(clause[c("Y", "X", "Y")], CnfClause(list(y, x)))
  expect_identical(clause[c(2L, 1L, 2L)], CnfClause(list(y, x)))
  expect_identical(clause[c(FALSE, TRUE)], as.CnfClause(y))
  expect_identical(clause[c(1L, 0L, 1L)], as.CnfClause(x))
  for (index in list(NULL, integer(0), character(0), logical(0), 0, FALSE)) {
    selected = clause[index]
    expect_false(as.logical(selected))
    expect_identical(attr(selected, "universe"), u)
  }

  # Repeating a selected disjunct must not change this contradiction.
  unit = as.CnfClause(x)
  clauses = list(
    unit[c(1L, 1L)],
    X %among% "b" | Y %among% "c",
    X %among% "c" | Y %among% "b",
    X %among% "a" | Y %among% "a"
  )
  expect_false(as.logical(CnfFormula(clauses)))
})
