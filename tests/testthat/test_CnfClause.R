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
  expect_s3_class(!contradiction, "CnfFormula")
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
  expect_identical(subset_clause, CnfClause(list(CnfAtom(X, c("a", "b")))))

  expect_identical(clause[0], CnfClause(list(CnfAtom(X, character(0)))))
  expect_identical(clause[integer(0)], CnfClause(list(CnfAtom(X, character(0)))))
  expect_identical(clause[c(FALSE, FALSE)], CnfClause(list(CnfAtom(X, character(0)))))

  expect_error(clause["Z"], "Must be a subset of \\{'X','Y'\\}")

  expect_identical(clause[c(1, 2)], clause)
  expect_identical(clause[c(TRUE, TRUE)], clause)

  expect_identical(clause[c(1, 2, 1, 2)], clause)
  expect_identical(clause[c(1, 0, 2, 0)], clause)

})

# Test print and format methods for CnfClause
test_that("print and format methods for CnfClause work correctly", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  clause = CnfClause(list(CnfAtom(X, c("a", "b")), CnfAtom(Y, c("d"))))

  # Test print method
  expect_output(print(clause), "CnfClause:\n  X . \\{a, b\\} \\| Y . \\{d\\}|CnfClause:\n  Y . \\{d\\} \\| X . \\{a, b\\}")

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

  atom_neither = CnfClause(list(CnfAtom(X, c("a", "b"))))
  expect_identical(as.logical(atom_neither), NA)
})

# Test invalid creation of CnfClause with clauses from different universes
test_that("CnfClause throws error when clauses are from different universes", {
  u1 = CnfUniverse()
  u2 = CnfUniverse()
  X1 = CnfSymbol(u1, "X", c("a", "b", "c"))
  X2 = CnfSymbol(u2, "X", c("a", "b", "c"))

  expect_error(CnfClause(list(CnfAtom(X1, c("a", "b")), CnfAtom(X2, c("a", "b")))), "All symbols must be in the same universe")
})


test_that("all.equal recognizes (in)equality for CnfClause", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))

  # Test equality between identical CnfClauses
  atom1 = CnfAtom(X, c("a", "b"))
  atom2 = CnfAtom(Y, c("d", "e"))
  clause1 = CnfClause(list(atom1, atom2))
  clause2 = CnfClause(list(atom1, atom2))

  expect_true(all.equal(clause1, clause2))

  # Test equality with the same atoms but in different order
  clause2 = CnfClause(list(atom2, atom1))

  expect_true(all.equal(clause1, clause2))

  # Test inequality for different CnfClauses with the same symbols but different values
  atom3 = CnfAtom(X, c("b", "c"))
  clause2 = CnfClause(list(atom3, atom2))

  expect_string(all.equal(clause1, clause2), pattern = "string mismatches")

  # Test equality for CnfClauses where the values of one symbol are in a different order
  atom4 = CnfAtom(X, c("b", "a"))  # same values, different order
  clause2 = CnfClause(list(atom4, atom2))

  expect_true(all.equal(clause1, clause2))

  # Test inequality when symbols in CnfClauses differ
  Z = CnfSymbol(u, "Z", c("g", "h", "i"))
  atom5 = CnfAtom(Z, c("g"))
  clause2 = CnfClause(list(atom1, atom5))

  expect_string(paste(all.equal(clause1, clause2), collapse = "\n"), pattern = "string mismatch")

  # Test equality for logical CnfClause (TRUE and FALSE) vs a clause
  clause_tautology = CnfClause(list(CnfAtom(X, c("a", "b", "c"))))  # tautology (TRUE)
  clause_contradiction = CnfClause(list(CnfAtom(X, character(0))))  # contradiction (FALSE)

  expect_string(all.equal(clause1, clause_tautology), pattern = "not both logicals")
  expect_string(all.equal(clause1, clause_contradiction), pattern = "not both logicals")
  expect_string(all.equal(clause_tautology, clause_contradiction), pattern = "logicals but not equal")

  # Test equality for a logical TRUE clause and another logical TRUE clause
  atom_ttl1 = CnfAtom(X, c("a", "b", "c"))
  atom_ttl2 = CnfAtom(Y, c("d", "e", "f"))
  clause_ttl1 = CnfClause(list(atom_ttl1))
  clause_ttl2 = CnfClause(list(atom_ttl2))

  expect_true(all.equal(clause_ttl1, clause_ttl2))
  expect_true(all.equal(clause_ttl1, as.CnfClause(TRUE)))

  # Test equality for a logical FALSE clause and another logical FALSE clause
  clause_false1 = CnfClause(list(CnfAtom(X, character(0))))
  clause_false2 = CnfClause(list(CnfAtom(Y, character(0))))

  expect_true(all.equal(clause_false1, clause_false2))
  expect_true(all.equal(clause_false1, as.CnfClause(FALSE)))

  # Test inequality between a CnfClause and a non-CnfClause object
  expect_string(all.equal(clause1, "not a CnfClause"), pattern = "current is not a CnfClause")

  # Test equality between CnfClauses in different universes
  u1 = CnfUniverse()
  u2 = CnfUniverse()
  X1 = CnfSymbol(u1, "X", c("a", "b", "c"))
  X2 = CnfSymbol(u2, "X", c("a", "b", "c"))

  atom_u1 = CnfAtom(X1, c("a", "b"))
  atom_u2 = CnfAtom(X2, c("a", "b"))
  clause_u1 = CnfClause(list(atom_u1))
  clause_u2 = CnfClause(list(atom_u2))

  expect_true(all.equal(clause_u1, clause_u2))

  # Test inequality when universes differ in length
  u3 = CnfUniverse()
  X3 = CnfSymbol(u3, "X", c("a", "b", "c", "d"))
  atom_u3 = CnfAtom(X3, c("a", "b"))
  clause_u3 = CnfClause(list(atom_u3))

  expect_string(all.equal(clause_u1, clause_u3), pattern = "Lengths \\(3, 4\\) differ")

  # Explicitly constructed object all.equals test
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("d", "e", "f"))
  Z = CnfSymbol(u, "Z", c("g", "h", "i"))

  c1 = structure(list(
    X = c("a", "b"), Y = c("d")
  ), universe = u, class = "CnfClause")
  c2 = structure(list(
    X = c("b", "a"), Y = c("d")
  ), universe = u, class = "CnfClause")
  c3 = structure(list(
    Y = c("d"), X = c("a", "b")
  ), universe = u, class = "CnfClause")
  c4 = structure(list(
    Y = c("d"), X = c("b", "a")
  ), universe = u, class = "CnfClause")
  c_unequal = structure(list(
    X = c("a", "b"), Z = c("g")
  ), universe = u, class = "CnfClause")


  for (cx in list(c1, c2, c3, c4)) {
    for (cy in list(c1, c2, c3, c4)) {
      expect_true(all.equal(cx, cy))
    }
  }

  for (cx in list(c1, c2, c3, c4)) {
    expect_string(paste(all.equal(cx, c_unequal), collapse = "\n"), pattern = "string mismatch|[Ll]ength mismatch")
  }


})
