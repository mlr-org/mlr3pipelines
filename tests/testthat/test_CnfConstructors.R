test_that("CnfClause accepts universe-free constants in every position", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  atom = CnfAtom(X, "a")
  clause = as.CnfClause(atom)
  tautology = as.CnfClause(CnfAtom(X, c("a", "b", "c")))

  for (convert in list(as.CnfAtom, as.CnfClause)) {
    for (value in c(FALSE, TRUE)) {
      constant = convert(value)
      expected = if (value) tautology else clause
      for (entry in list(atom, clause)) {
        for (inputs in list(list(constant, entry), list(entry, constant), list(constant, entry, constant))) {
          expect_identical(CnfClause(inputs), expected)
        }
      }
    }
  }
})

test_that("CnfFormula accepts universe-free constants in every position", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  clause = as.CnfClause(CnfAtom(X, "a"))
  formula = as.CnfFormula(clause)
  contradiction = as.CnfFormula(CnfAtom(X, character(0)))

  for (convert in list(as.CnfClause, as.CnfFormula)) {
    for (value in c(TRUE, FALSE)) {
      constant = convert(value)
      expected = if (value) formula else contradiction
      for (entry in list(clause, formula)) {
        for (inputs in list(list(constant, entry), list(entry, constant), list(constant, entry, constant))) {
          expect_identical(CnfFormula(inputs), expected)
        }
      }
    }
  }
})

test_that("constant-only constructors retain the first available universe", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b"))

  expect_identical(CnfClause(list()), as.CnfClause(FALSE))
  expect_identical(CnfFormula(list()), as.CnfFormula(TRUE))
  for (value in c(FALSE, TRUE)) {
    atom = CnfAtom(X, if (value) c("a", "b") else character(0))
    for (convert in list(as.CnfAtom, as.CnfClause)) {
      constant = convert(value)
      owned = convert(atom)
      expect_identical(CnfClause(list(constant, constant)), as.CnfClause(value))
      expect_identical(CnfClause(list(constant, owned)), as.CnfClause(atom))
      expect_identical(CnfClause(list(owned, constant)), as.CnfClause(atom))
    }
    for (convert in list(as.CnfClause, as.CnfFormula)) {
      constant = convert(value)
      owned = convert(atom)
      expect_identical(CnfFormula(list(constant, constant)), as.CnfFormula(value))
      expect_identical(CnfFormula(list(constant, owned)), as.CnfFormula(atom))
      expect_identical(CnfFormula(list(owned, constant)), as.CnfFormula(atom))
    }
  }

  v = CnfUniverse()
  Y = CnfSymbol(v, "Y", c("a", "b"))
  true_atom = CnfAtom(X, c("a", "b"))
  false_atom = CnfAtom(Y, character(0))
  expect_identical(CnfClause(list(as.CnfAtom(FALSE), true_atom, false_atom)), as.CnfClause(true_atom))
  expect_identical(CnfFormula(list(as.CnfClause(TRUE), as.CnfClause(false_atom), as.CnfClause(true_atom))),
    as.CnfFormula(false_atom))
})

test_that("constant owners do not override a nonconstant universe", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b"))
  v = CnfUniverse()
  Y = CnfSymbol(v, "Y", c("a", "b"))
  atom = CnfAtom(X, "a")
  clause = as.CnfClause(atom)
  formula = as.CnfFormula(atom)

  for (value in c(FALSE, TRUE)) {
    constant = CnfAtom(Y, if (value) c("a", "b") else character(0))
    expected_atom = if (value) CnfAtom(X, c("a", "b")) else atom
    for (convert in list(as.CnfAtom, as.CnfClause)) {
      expect_identical(CnfClause(list(convert(constant), clause)), as.CnfClause(expected_atom))
      expect_identical(CnfClause(list(clause, convert(constant))), as.CnfClause(expected_atom))
    }
    expected_formula = if (value) formula else as.CnfFormula(CnfAtom(X, character(0)))
    for (convert in list(as.CnfClause, as.CnfFormula)) {
      expect_identical(CnfFormula(list(convert(constant), formula)), expected_formula)
      expect_identical(CnfFormula(list(formula, convert(constant))), expected_formula)
    }
  }
})

test_that("constructors reject incompatible nonconstant universes before simplification", {
  u = CnfUniverse()
  v = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b"))
  Y = CnfSymbol(v, "X", c("a", "b"))
  atom_x = CnfAtom(X, "a")
  atom_y = CnfAtom(Y, "a")

  for (convert_x in list(as.CnfAtom, as.CnfClause)) {
    for (convert_y in list(as.CnfAtom, as.CnfClause)) {
      inputs = list(convert_x(atom_x), convert_y(atom_y))
      expect_error(CnfClause(inputs), "All symbols must be in the same universe", fixed = TRUE)
      expect_error(CnfClause(rev(inputs)), "All symbols must be in the same universe", fixed = TRUE)
      expect_error(CnfClause(c(list(as.CnfAtom(TRUE)), inputs)), "All symbols must be in the same universe", fixed = TRUE)
      expect_error(CnfClause(c(inputs, list(as.CnfClause(FALSE)))), "All symbols must be in the same universe", fixed = TRUE)
    }
  }
  for (convert_x in list(as.CnfClause, as.CnfFormula)) {
    for (convert_y in list(as.CnfClause, as.CnfFormula)) {
      inputs = list(convert_x(atom_x), convert_y(atom_y))
      expect_error(CnfFormula(inputs), "All clauses must be in the same universe", fixed = TRUE)
      expect_error(CnfFormula(rev(inputs)), "All clauses must be in the same universe", fixed = TRUE)
      expect_error(CnfFormula(c(list(as.CnfClause(FALSE)), inputs)), "All clauses must be in the same universe", fixed = TRUE)
      expect_error(CnfFormula(c(inputs, list(as.CnfFormula(TRUE)))), "All clauses must be in the same universe", fixed = TRUE)
    }
  }
  expect_error(CnfClause(list(as.CnfAtom(TRUE), TRUE)), "May only contain", fixed = TRUE)
  expect_error(CnfFormula(list(as.CnfClause(FALSE), FALSE)), "May only contain", fixed = TRUE)
})

test_that("FALSE absorbs previously collected nested formulas", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b"))
  Y = CnfSymbol(u, "Y", c("a", "b"))
  clause = as.CnfClause(CnfAtom(X, "a"))
  formula = CnfFormula(list(clause, as.CnfClause(CnfAtom(Y, "b"))))
  other_formula = as.CnfFormula(CnfAtom(Y, "a"))
  contradiction = CnfAtom(X, character(0))

  for (convert in list(as.CnfClause, as.CnfFormula)) {
    for (constant in list(convert(FALSE), convert(contradiction))) {
      for (inputs in list(list(formula, constant), list(formula, other_formula, constant),
        list(clause, formula, constant), list(formula, clause, constant), list(constant, formula, other_formula))) {
        expect_identical(CnfFormula(inputs), as.CnfFormula(contradiction))
      }
    }
  }
})

test_that("logical disjunctions with clauses preserve their result class", {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b"))
  proper = as.CnfClause(CnfAtom(X, "a"))
  clauses = list(proper, as.CnfClause(FALSE), as.CnfClause(CnfAtom(X, character(0))),
    as.CnfClause(TRUE), as.CnfClause(CnfAtom(X, c("a", "b"))))

  for (clause in clauses) {
    for (value in c(TRUE, FALSE)) {
      for (result in list(value | clause, clause | value)) {
        expect_s3_class(result, "CnfClause")
        if (value) {
          expect_true(as.logical(result))
        } else if (is.logical(clause)) {
          expect_identical(as.vector(result), as.vector(clause))
        } else {
          expect_identical(result, clause)
        }
        expect_s3_class(CnfFormula(list(result)), "CnfFormula")
      }
    }
  }
})
