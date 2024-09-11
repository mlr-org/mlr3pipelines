

# Test the creation of a CnfAtom
test_that("CnfAtom is created correctly from CnfSymbol", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  atom = CnfAtom(X, c("a", "b"))

  expect_s3_class(atom, "CnfAtom")
  expect_equal(atom$symbol, "X")
  expect_equal(atom$values, c("a", "b"))
})

# Test the creation of a tautology and contradiction with CnfAtom
test_that("CnfAtom handles tautologies and contradictions correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  tautology = CnfAtom(X, c("a", "b", "c"))
  contradiction = CnfAtom(X, character(0))

  # Tautology should evaluate to TRUE
  expect_true(as.logical(tautology))

  # Contradiction should evaluate to FALSE
  expect_false(as.logical(contradiction))
})

# Test negation of CnfAtom
test_that("Negation of CnfAtom works correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  atom = CnfAtom(X, c("a", "b"))
  negated_atom = !atom

  # Check that negation works correctly
  expect_s3_class(negated_atom, "CnfAtom")
  expect_equal(negated_atom$symbol, "X")
  expect_equal(negated_atom$values, "c")  # Negation should result in the complement of the original values

  # Check that negation of a tautology is a contradiction
  expect_false(as.logical(!CnfAtom(X, c("a", "b", "c"))))

  # Check that negation of a contradiction is a tautology
  expect_true(as.logical(!CnfAtom(X, character(0))))
})

# Test conjunction (&) of CnfAtoms
test_that("Conjunction of CnfAtoms works correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  atom1 = CnfAtom(X, c("a", "b"))
  atom2 = CnfAtom(Y, c("d", "e"))

  formula = atom1 & atom2

  expect_s3_class(formula, "CnfFormula")
  expect_length(as.list(formula), 2)

  atoms = unlist(lapply(as.list(formula), as.list), recursive = FALSE)
  # we don't know if atoms[[1]] is atom1 or atom2
  if (identical(atoms[[1]]$symbol, "X")) {
    expect_identical(atoms[[1]], atom1)
    expect_identical(atoms[[2]], atom2)
  } else {
    expect_identical(atoms[[1]], atom2)
    expect_identical(atoms[[2]], atom1)
  }

  # Check that conjunction of a tautology with another atom is the other atom
  ttl = CnfAtom(X, c("a", "b", "c")) & atom2
  expect_s3_class(ttl, "CnfFormula")
  expect_identical(lapply(as.list(ttl), as.list), list(list(atom2)))

  # Check that conjunction of a contradiction with another atom is a contradiction
  ctn = CnfAtom(X, character(0)) & atom2
  expect_s3_class(ctn, "CnfFormula")
  expect_false(as.logical(ctn))
})

# Test disjunction (|) of CnfAtoms
test_that("Disjunction of CnfAtoms works correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  atom1 = CnfAtom(X, c("a", "b"))
  atom2 = CnfAtom(Y, c("d", "e"))

  clause = atom1 | atom2

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

  # Check that disjunction of a tautology with another atom is a tautology
  ttl = CnfAtom(X, c("a", "b", "c")) | atom2
  expect_true(as.logical(ttl))
  expect_s3_class(ttl, "CnfClause")

  # Check that disjunction of a contradiction with another atom is the other atom
  ctn = CnfAtom(X, character(0)) | atom2
  expect_identical(as.list(ctn), list(atom2))
})

# Test print and format methods for CnfAtom
test_that("print and format methods for CnfAtom work correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  atom = CnfAtom(X, c("a", "b"))

  # Test print method
  expect_output(print(atom), "CnfAtom: X ∈ \\{a, b\\}.")

  # Test format method
  expect_equal(format(atom), "CnfAtom(X)")
})

# Test conversion between logical and CnfAtom
test_that("as.logical and as.CnfAtom conversions work correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  atom_true = CnfAtom(X, c("a", "b", "c"))
  atom_false = CnfAtom(X, character(0))
  atom_neither = CnfAtom(X, c("a", "b"))

  # Convert CnfAtom to logical
  expect_true(as.logical(atom_true))
  expect_false(as.logical(atom_false))
  expect_identical(as.logical(atom_neither), NA)

  # Convert logical to CnfAtom
  expect_s3_class(as.CnfAtom(TRUE), "CnfAtom")
  expect_true(as.logical(as.CnfAtom(TRUE)))

  expect_s3_class(as.CnfAtom(FALSE), "CnfAtom")
  expect_false(as.logical(as.CnfAtom(FALSE)))

  expect_error(as.CnfAtom(NA), "May not be NA")
  expect_error(as.CnfAtom(c(TRUE, TRUE)), "Must have length 1.")
})

# Test invalid creation of CnfAtom with values not in the domain
test_that("CnfAtom throws error when values are not in the symbol's domain", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))

  expect_error(CnfAtom(X, c("d", "e")), "Must be a subset of \\{'a','b','c'\\}")
})

