# Test the creation of a CnfFormula from CnfClauses
test_that("CnfFormula is created correctly from CnfClauses", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  clause1 = CnfClause(list(CnfAtom(X, c("a", "b"))))
  clause2 = CnfClause(list(CnfAtom(Y, c("d", "e"))))

  formula = CnfFormula(list(clause1, clause2))

  expect_s3_class(formula, "CnfFormula")
  expect_length(as.list(formula), 2)

  clauses = as.list(formula)
  expect_true(identical(clauses[[1]], clause1) || identical(clauses[[1]], clause2))
  expect_true(identical(clauses[[2]], clause1) || identical(clauses[[2]], clause2))
})

# Test the handling of tautologies and contradictions in CnfFormula
test_that("CnfFormula handles tautologies and contradictions correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  tautology = CnfFormula(list(CnfClause(list(CnfAtom(X, c("a", "b", "c"))))))
  contradiction = CnfFormula(list(CnfClause(list(CnfAtom(X, character(0))))))

  # Tautology should evaluate to TRUE
  expect_true(as.logical(tautology))

  # Contradiction should evaluate to FALSE
  expect_false(as.logical(contradiction))

  # Conjunction of a tautology and a clause should result in the clause
  atom = CnfAtom(X, c("a"))
  clause = CnfClause(list(atom))
  formula = tautology & clause
  expect_identical(as.list(formula), list(clause))

  # Conjunction of a contradiction with a clause should result in contradiction
  formula_ctn = contradiction & clause
  expect_s3_class(formula_ctn, "CnfFormula")
  expect_false(as.logical(formula_ctn))
})

# Test conjunction (&) of CnfFormulas
test_that("Conjunction of CnfFormulas works correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  clause1 = CnfClause(list(CnfAtom(X, c("a", "b"))))
  clause2 = CnfClause(list(CnfAtom(Y, c("d", "e"))))

  formula1 = CnfFormula(list(clause1))
  formula2 = CnfFormula(list(clause2))

  conjunction = formula1 & formula2

  expect_s3_class(conjunction, "CnfFormula")
  expect_length(as.list(conjunction), 2)

  clauses = as.list(conjunction)
  expect_true(identical(clauses[[1]], clause1) || identical(clauses[[1]], clause2))
  expect_true(identical(clauses[[2]], clause1) || identical(clauses[[2]], clause2))

  # Check that conjunction of a tautology with a formula is the formula
  tautology = CnfFormula(list(CnfClause(list(CnfAtom(X, c("a", "b", "c"))))))
  conjunction_with_tautology = tautology & formula1
  expect_identical(as.list(conjunction_with_tautology), list(clause1))
})

# Test disjunction (|) of CnfFormulas
test_that("Disjunction of CnfFormulas works correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  clause1 = CnfClause(list(CnfAtom(X, c("a"))))
  clause2 = CnfClause(list(CnfAtom(Y, c("d"))))
  clause3 = CnfClause(list(CnfAtom(X, c("b"))))
  clause4 = CnfClause(list(CnfAtom(Y, c("e"))))

  formula1 = CnfFormula(list(clause1, clause2))
  formula2 = CnfFormula(list(clause3, clause4))

  disjunction = formula1 | formula2

  expect_s3_class(disjunction, "CnfFormula")
  expect_length(as.list(disjunction), 2)

  formula1
  formula2

  # (X = a & Y = d) | (X = b & Y = e) <=>
  # (X = a | X = b) & (Y = d | Y = e) & (X = a | Y = e) & (X = b | Y = d)
  ## can drop the first two clauses as they are always true whenever the latter two are true
  expected_clause_1 = CnfClause(list(CnfAtom(X, "a"), CnfAtom(Y, "e")))
  expected_clause_2 = CnfClause(list(CnfAtom(X, "b"), CnfAtom(Y, "d")))

  clauses = as.list(disjunction)
  expect_true(isTRUE(all.equal(clauses[[1]], expected_clause_1)) || isTRUE(all.equal(clauses[[1]], expected_clause_2)))
  expect_true(isTRUE(all.equal(clauses[[1]], expected_clause_2)) || isTRUE(all.equal(clauses[[1]], expected_clause_1)))

  # Check that disjunction of a tautology with a formula is the same formula
  tautology = CnfFormula(list(CnfClause(list(CnfAtom(X, c("a", "b", "c"))))))
  disjunction_with_tautology = tautology | formula1
  expect_s3_class(disjunction_with_tautology, "CnfFormula")
  expect_true(
    identical(as.list(disjunction_with_tautology), list(clause1, clause2)) ||
    identical(as.list(disjunction_with_tautology), list(clause2, clause1))
  )
})

# Test negation of CnfFormula
test_that("Negation of CnfFormula works correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  clause = CnfClause(list(CnfAtom(X, c("a", "b")), CnfAtom(Y, c("d"))))
  formula = CnfFormula(list(clause))

  negated_formula = !formula

  expect_s3_class(negated_formula, "CnfFormula")
  expect_length(as.list(negated_formula), 2)

  expect_true(
    identical(as.list(negated_formula), list(CnfClause(list(CnfAtom(X, "c"))), CnfClause(list(CnfAtom(Y, c("e", "f")))))) ||
    identical(as.list(negated_formula), list(CnfClause(list(CnfAtom(Y, c("e", "f")))), CnfClause(list(CnfAtom(X, "c")))))
  )

  # Check that negation of a tautology is a contradiction
  tautology = CnfFormula(list(CnfClause(list(CnfAtom(X, c("a", "b", "c"))))))
  expect_false(as.logical(!tautology))

  # Check that negation of a contradiction is a tautology
  contradiction = CnfFormula(list(CnfClause(list(CnfAtom(X, character(0))))))
  expect_true(as.logical(!contradiction))
})

# Test complex conjunction and disjunction operations on CnfFormula
test_that("Complex operations on CnfFormula work correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))
  Z = CnfSymbol(u, "Z", c("g", "h", "i"))

  formula1 = (CnfAtom(X, c("a", "b")) | CnfAtom(Y, c("d", "e"))) & CnfAtom(Z, c("g", "h"))
  formula2 = !(CnfAtom(X, c("a", "b")) | CnfAtom(Y, c("d", "e")))

  expect_s3_class(formula1, "CnfFormula")
  expect_s3_class(formula2, "CnfFormula")

  # Check that complex formula works correctly with negation and conjunction
  negated_formula1 = !formula1
  expect_s3_class(negated_formula1, "CnfFormula")

  conjunction_formula = formula1 & formula2
  expect_s3_class(conjunction_formula, "CnfFormula")

  # conjunction_formula is a contradiction, since formula2 is the negation of the
  # first part of formula1.
  # We don't yet test whether this is obvious enough for the simplification algorithm
  # to catch, but negating it should give a tautology that can directly be recognized;
  expect_true(!conjunction_formula)
})

# Test print and format methods for CnfFormula
test_that("print and format methods for CnfFormula work correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  formula = CnfFormula(list(CnfClause(list(CnfAtom(X, c("a", "b")), CnfAtom(Y, c("d"))))))

  # Test print method
  expect_output(print(formula), "CnfFormula:\n     \\(X . \\{a, b\\} \\| Y . \\{d\\}\\)|CnfFormula:\n     \\(Y . \\{d\\} \\| X . \\{a, b\\}\\)")

  # Test format method
  expect_equal(format(formula), "CnfFormula(1)")
})

# Test conversion between logical and CnfFormula
test_that("as.logical and as.CnfFormula conversions work correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  formula_true = CnfFormula(list(CnfClause(list(CnfAtom(X, c("a", "b", "c"))))))
  formula_false = CnfFormula(list(CnfClause(list(CnfAtom(X, character(0))))))

  # Convert CnfFormula to logical
  expect_true(as.logical(formula_true))
  expect_false(as.logical(formula_false))

  # Convert logical to CnfFormula
  expect_s3_class(as.CnfFormula(TRUE), "CnfFormula")
  expect_true(as.logical(as.CnfFormula(TRUE)))

  expect_s3_class(as.CnfFormula(FALSE), "CnfFormula")
  expect_false(as.logical(as.CnfFormula(FALSE)))

  expect_identical(as.logical(CnfFormula(list(CnfClause(list(CnfAtom(X, c("a", "b")))))) & CnfFormula(list(CnfClause(list(CnfAtom(Y, c("d"))))))),
    NA
  )
})

# Test invalid creation of CnfFormula with clauses from different universes
test_that("CnfFormula throws error when clauses are from different universes", {
  u1 = CnfUniverse()
  u2 = CnfUniverse()
  X1 = CnfSymbol(u1, "X", c("a", "b", "c"))
  X2 = CnfSymbol(u2, "X", c("a", "b", "c"))

  clause1 = CnfClause(list(CnfAtom(X1, c("a", "b"))))
  clause2 = CnfClause(list(CnfAtom(X2, c("a", "b"))))

  expect_error(CnfFormula(list(clause1, clause2)), "All clauses must be in the same universe")
})
