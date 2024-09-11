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


test_that("all.equal recognizes (in)equality for CnfFormula", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))
  Z = CnfSymbol(u, "Z", c("g", "h", "i"))

  # Test equality between identical CnfFormulas
  atom1 = CnfAtom(X, c("a", "b"))
  atom2 = CnfAtom(Y, c("d", "e"))
  clause1 = CnfClause(list(atom1, atom2))

  atom3 = CnfAtom(Z, c("g"))
  clause2 = CnfClause(list(atom3))

  formula1 = CnfFormula(list(clause1, clause2))
  formula2 = CnfFormula(list(clause1, clause2))

  expect_true(all.equal(formula1, formula2))

  # Test equality with the same clauses but in different order
  formula2 = CnfFormula(list(clause2, clause1))

  expect_true(all.equal(formula1, formula2))

  # Test inequality for different CnfFormulas with different atoms
  atom4 = CnfAtom(X, c("b", "c"))
  clause3 = CnfClause(list(atom4, atom2))
  formula2 = CnfFormula(list(clause3, clause2))

  expect_string(all.equal(formula1, formula2), pattern = "string mismatch")

  # Test equality for CnfFormulas where the values of one symbol in clauses are in a different order
  atom5 = CnfAtom(X, c("b", "a"))  # same values, different order
  clause4 = CnfClause(list(atom5, atom2))
  formula2 = CnfFormula(list(clause4, clause2))

  expect_true(all.equal(formula1, formula2))

  # Test inequality when formulas have different symbols
  atom6 = CnfAtom(Z, c("h"))
  clause5 = CnfClause(list(atom6))
  formula2 = CnfFormula(list(clause1, clause5))

  expect_string(all.equal(formula1, formula2), pattern = "string mismatch")

  # Test equality for logical CnfFormula (TRUE and FALSE) vs a formula
  formula_tautology = CnfFormula(list(CnfClause(list(CnfAtom(X, c("a", "b", "c"))))))  # tautology (TRUE)
  formula_contradiction = CnfFormula(list(CnfClause(list(CnfAtom(X, character(0))))))  # contradiction (FALSE)

  expect_string(all.equal(formula1, formula_tautology), pattern = "not both logicals")
  expect_string(all.equal(formula1, formula_contradiction), pattern = "not both logicals")

  # Test equality for a logical TRUE formula and another logical TRUE formula
  clause_ttl1 = CnfClause(list(CnfAtom(X, c("a", "b", "c"))))
  clause_ttl2 = CnfClause(list(CnfAtom(Y, c("d", "e", "f"))))
  formula_ttl1 = CnfFormula(list(clause_ttl1))
  formula_ttl2 = CnfFormula(list(clause_ttl2))

  expect_true(all.equal(formula_ttl1, formula_ttl2))
  expect_true(all.equal(formula_ttl1, as.CnfFormula(TRUE)))

  # Test equality for a logical FALSE formula and another logical FALSE formula
  formula_false1 = CnfFormula(list(CnfClause(list(CnfAtom(X, character(0))))))
  formula_false2 = CnfFormula(list(CnfClause(list(CnfAtom(Y, character(0))))))

  expect_true(all.equal(formula_false1, formula_false2))
  expect_string(all.equal(formula1, formula_false1), pattern = "not both logicals")
  expect_true(all.equal(formula_false1, as.CnfFormula(FALSE)))

  # Test inequality between a CnfFormula and a non-CnfFormula object
  expect_string(all.equal(formula1, "not a CnfFormula"), pattern = "current is not a CnfFormula")

  # Test equality between CnfFormulas in different universes
  u1 = CnfUniverse()
  u2 = CnfUniverse()
  X1 = CnfSymbol(u1, "X", c("a", "b", "c"))
  X2 = CnfSymbol(u2, "X", c("a", "b", "c"))

  atom_u1 = CnfAtom(X1, c("a", "b"))
  atom_u2 = CnfAtom(X2, c("a", "b"))
  clause_u1 = CnfClause(list(atom_u1))
  clause_u2 = CnfClause(list(atom_u2))
  formula_u1 = CnfFormula(list(clause_u1))
  formula_u2 = CnfFormula(list(clause_u2))

  expect_true(all.equal(formula_u1, formula_u2))

  # Test inequality when universes differ in length
  u3 = CnfUniverse()
  X3 = CnfSymbol(u3, "X", c("a", "b", "c", "d"))
  atom_u3 = CnfAtom(X3, c("a", "b"))
  clause_u3 = CnfClause(list(atom_u3))
  formula_u3 = CnfFormula(list(clause_u3))

  expect_string(all.equal(formula_u1, formula_u3), pattern = "Lengths \\(3, 4\\) differ")

  # Test disjunction and conjunction in CnfFormula
  atom7 = CnfAtom(X, c("a"))
  atom8 = CnfAtom(Y, c("d"))
  clause6 = CnfClause(list(atom7, atom8))
  clause7 = CnfClause(list(atom1, atom2))

  formula1 = CnfFormula(list(clause6))
  formula2 = CnfFormula(list(clause7))

  formula_disjunction = formula1 | formula2
  formula_conjunction = formula1 & formula2

  # Test all.equal on conjunction and disjunction cases
  expect_string(paste(all.equal(formula_disjunction, formula1), collapse = "\n"), pattern = "Lengths \\(2, 1\\) differ")
  expect_true(all.equal(formula_conjunction, formula1))


  # Explicitly constructed object all.equals test
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))
  Z = CnfSymbol(u, "Z", c("g", "h", "i"))

  f1 = structure(list(
    list(X = c("a", "b"), Y = c("d")),
    list(Z = c("g"))
  ), universe = u, class = "CnfFormula")
  f2 = structure(list(
    list(Y = c("d"), X = c("a", "b")),
    list(Z = c("g"))
  ), universe = u, class = "CnfFormula")
  f3 = structure(list(
    list(X = c("b", "a"), Y = c("d")),
    list(Z = c("g"))
  ), universe = u, class = "CnfFormula")
  f4 = structure(list(
    list(Y = c("d"), X = c("b", "a")),
    list(Z = c("g"))
  ), universe = u, class = "CnfFormula")
  f5 = structure(list(
    list(Z = c("g")),
    list(X = c("a", "b"), Y = c("d"))
  ), universe = u, class = "CnfFormula")

  f1_unequal = structure(list(
    list(X = c("a", "b")),
    list(Z = c("g"))
  ), universe = u, class = "CnfFormula")

  for (fx in list(f1, f2, f3, f4, f5)) {
    for (fy in list(f1, f2, f3, f4, f5)) {
      expect_true(all.equal(fx, fy))
    }
  }

  for (fx in list(f1, f2, f3, f4, f5)) {
    expect_string(all.equal(fx, f1_unequal), pattern = "string mismatch|[Ll]ength mismatch")
  }

})
