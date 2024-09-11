# Test the creation of a CnfClause from CnfAtoms
test_that("CnfClause is created correctly from CnfAtoms", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  atom1 = CnfAtom(X, c("a", "b"))
  atom2 = CnfAtom(Y, c("d", "e"))
  clause = CnfClause(list(atom1, atom2))

  expect_s3_class(clause, "CnfClause")
  expect_length(as.list(clause), 2)

  atoms = as.list(clause)
  if (identical(atoms[[1]]$symbol, "X")) {
    expect_identical(atoms[[1]], atom1)
    expect_identical(atoms[[2]], atom2)
  } else {
    expect_identical(atoms[[1]], atom2)
    expect_identical(atoms[[2]], atom1)
  }

})

test_that("CnfClause with overlapping symbols works correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  atom1 = CnfAtom(X, c("a", "b"))
  atom2 = CnfAtom(Y, c("d", "e"))
  atom1_sub = CnfAtom(X, c("a"))
  atom2_sub = CnfAtom(Y, c("d"))

  expect_identical(
    CnfClause(list(atom1, atom1_sub)),
    CnfClause(list(atom1))
  )

  expect_identical(CnfClause(list(atom1, atom1)), CnfClause(list(atom1)))

  combined1 = CnfClause(list(atom1, atom2, atom1_sub, atom2_sub))
  combined2 = CnfClause(list(atom1, atom2))
  combinedswitched = CnfClause(list(atom2, atom1))

  expect_true(identical(combined1, combined2) || identical(combined1, combinedswitched))

  # only one of them is identical!
  # this must be taken out if at any point we decide to sort the atoms
  expect_true(!identical(combined1, combined2) || !identical(combined1, combinedswitched))

  combined1 = CnfClause(list(atom1, atom2, atom1_sub))
  combined2 = CnfClause(list(atom1, atom2))
  combinedswitched = CnfClause(list(atom2, atom1))

  expect_true(identical(combined1, combined2) || identical(combined1, combinedswitched))

  # only one of them is identical!
  # this must be taken out if at any point we decide to sort the atoms
  expect_true(!identical(combined1, combined2) || !identical(combined1, combinedswitched))

})

# Test the handling of tautologies and contradictions in CnfClause
test_that("CnfClause handles tautologies and contradictions correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  tautology = CnfClause(list(CnfAtom(X, c("a", "b", "c"))))
  contradiction = CnfClause(list(CnfAtom(X, character(0))))

  # Tautology should evaluate to TRUE
  expect_true(as.logical(tautology))

  # Contradiction should evaluate to FALSE
  expect_false(as.logical(contradiction))
})

# Test disjunction (|) of CnfClauses
test_that("Disjunction of CnfClauses works correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  atom1 = CnfAtom(X, c("a"))
  atom2 = CnfAtom(Y, c("d"))
  atom3 = CnfAtom(X, c("b"))
  atom4 = CnfAtom(Y, c("e"))

  clause1 = CnfClause(list(atom1, atom2))
  clause2 = CnfClause(list(atom3, atom4))

  disjunct = clause1 | clause2

  expect_s3_class(disjunct, "CnfClause")
  expect_length(as.list(disjunct), 2)

  # Check that the disjunction has unified the atoms correctly
  atoms = as.list(disjunct)
  if (identical(atoms[[1]]$symbol, "X")) {
    expect_equal(atoms[[1]]$values, c("a", "b"))
    expect_equal(atoms[[2]]$values, c("d", "e"))
  } else {
    expect_equal(atoms[[1]]$values, c("d", "e"))
    expect_equal(atoms[[2]]$values, c("a", "b"))
  }

  tautology = CnfClause(list(CnfAtom(X, c("a", "b", "c"))))
  contradiction = CnfClause(list(CnfAtom(X, character(0))))

  # Check that disjunction of a tautology with another clause is a tautology
  ttl = tautology | clause1
  expect_true(as.logical(ttl))

  # Check that disjunction of a contradiction with another clause is the other clause
  ctn = contradiction | clause1
  expect_identical(as.list(ctn), as.list(clause1))


  # tautologies that come up in the middle of a disjunction also work

  atom1_complement = CnfAtom(X, c("b", "c"))
  expect_identical(
    CnfClause(list(atom1, atom1_complement)),
    tautology
  )

})

# Test conjunction (&) of CnfClauses
test_that("Conjunction of CnfClauses works correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))
  tautology = CnfClause(list(CnfAtom(X, c("a", "b", "c"))))
  contradiction = CnfClause(list(CnfAtom(X, character(0))))


  atom1 = CnfAtom(X, c("a", "b"))
  atom2 = CnfAtom(Y, c("d", "e"))

  clause1 = CnfClause(list(atom1))
  clause2 = CnfClause(list(atom2))

  formula = clause1 & clause2

  expect_s3_class(formula, "CnfFormula")
  expect_length(as.list(formula), 2)
  if (identical(as.list(as.list(formula)[[1]])[[1]]$symbol, "X")) {
    expect_identical(lapply(as.list(formula), as.list), list(list(atom1), list(atom2)))
  } else {
    expect_identical(lapply(as.list(formula), as.list), list(list(atom2), list(atom1)))
  }

  # Check that conjunction of a tautology with another clause is the other clause
  ttl = tautology & clause2
  expect_s3_class(ttl, "CnfFormula")
  expect_identical(lapply(as.list(ttl), as.list), list(list(atom2)))

  # Check that conjunction of a contradiction with another clause is a contradiction
  ctn = contradiction & clause2
  expect_s3_class(ctn, "CnfFormula")
  expect_false(as.logical(ctn))
})

# Test negation of CnfClause
test_that("Negation of CnfClause works correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))
  tautology = CnfClause(list(CnfAtom(X, c("a", "b", "c"))))
  contradiction = CnfClause(list(CnfAtom(X, character(0))))

  clause = CnfClause(list(CnfAtom(X, c("a", "b")), CnfAtom(Y, c("d"))))
  negated_clause = !clause

  expect_s3_class(negated_clause, "CnfFormula")
  expect_length(as.list(negated_clause), 2)


  # Check that negation of a tautology is a contradiction
  expect_false(as.logical(!tautology))
  expect_s3_class(!tautology, "CnfFormula")

  # Check that negation of a contradiction is a tautology
  expect_true(as.logical(!contradiction))
})

# Test subset operations on CnfClause
test_that("Subsetting of CnfClause works correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  clause = CnfClause(list(CnfAtom(X, c("a", "b")), CnfAtom(Y, c("d", "e"))))

  subset_clause = clause["X"]

  expect_s3_class(subset_clause, "CnfClause")
  expect_length(as.list(subset_clause), 1)
  expect_identical(as.list(subset_clause)[[1]]$symbol, "X")

  expect_error(clause[0], "Cannot subset a FALSE clause")
  expect_error(clause["Z"], "Invalid index type.")
})

# Test print and format methods for CnfClause
test_that("print and format methods for CnfClause work correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  clause = CnfClause(list(CnfAtom(X, c("a", "b")), CnfAtom(Y, c("d"))))

  # Test print method
  expect_output(print(clause), "CnfClause")

  # Test format method
  expect_equal(format(clause), "CnfClause(2)")
})

# Test conversion between logical and CnfClause
test_that("as.logical and as.CnfClause conversions work correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  atom_true = CnfClause(list(CnfAtom(X, c("a", "b", "c"))))
  atom_false = CnfClause(list(CnfAtom(X, character(0))))

  # Convert CnfClause to logical
  expect_true(as.logical(atom_true))
  expect_false(as.logical(atom_false))

  # Convert logical to CnfClause
  expect_s3_class(as.CnfClause(TRUE), "CnfClause")
  expect_true(as.logical(as.CnfClause(TRUE)))

  expect_s3_class(as.CnfClause(FALSE), "CnfClause")
  expect_false(as.logical(as.CnfClause(FALSE)))
})
