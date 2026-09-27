with_cnf_comparison_locale = function(code) {
  old_ctype = Sys.getlocale("LC_CTYPE")
  old_collate = Sys.getlocale("LC_COLLATE")
  on.exit({
    Sys.setlocale("LC_COLLATE", old_collate)
    Sys.setlocale("LC_CTYPE", old_ctype)
  })
  for (locale in c("C.UTF-8", "en_US.UTF-8", "English_United States.utf8")) {
    ctype = suppressWarnings(Sys.setlocale("LC_CTYPE", locale))
    collate = suppressWarnings(Sys.setlocale("LC_COLLATE", locale))
    if (nzchar(ctype) && nzchar(collate) && l10n_info()[["UTF-8"]]) return(force(code))
  }
  skip("A UTF-8 locale is required for this comparison")
}

test_that("CnfFormula comparison ignores equivalent text encodings", {
  with_cnf_comparison_locale({
    u = CnfUniverse()
    values = c("\u00e9", "\u00f6")
    X = CnfSymbol(u, "X", c(values, "other"))
    Y = CnfSymbol(u, "Y", c("a", "b", "c"))
    make_formula = function(values) CnfFormula(list(
      CnfClause(list(X %among% values[[1L]], Y %among% "a")),
      CnfClause(list(X %among% values[[2L]], Y %among% "b"))
    ))
    f = make_formula(values)
    g = make_formula(iconv(values, from = "UTF-8", to = "latin1"))
    # Older stored objects may still use Latin-1 even after constructors normalize text.
    legacy = f
    legacy[] = lapply(unclass(f), function(clause) {
      lapply(clause, function(values) iconv(values, from = "UTF-8", to = "latin1"))
    })
    different = make_formula(rev(values))
    for (collate in c(Sys.getlocale("LC_COLLATE"), "C")) {
      Sys.setlocale("LC_COLLATE", collate)
      expect_identical(f, g)
      expect_true(all.equal(f, g))
      expect_identical(f, legacy)
      expect_true(all.equal(f, legacy))
      expect_false(isTRUE(all.equal(f, different)))
    }
  })
})

test_that("CNF comparisons distinguish strings even when their collation ties", {
  with_cnf_comparison_locale({
    u = CnfUniverse()
    values = c("\u00e9", "e\u0301")
    X = CnfSymbol(u, "X", c(values, "other"))
    a = X %among% values
    b = X %among% rev(values)
    different = X %among% values[[1L]]
    for (coerce in list(as.CnfAtom, as.CnfClause, as.CnfFormula)) {
      expect_true(all.equal(coerce(a), coerce(b)))
      expect_false(isTRUE(all.equal(coerce(a), coerce(different))))
      expect_false(isTRUE(all.equal(coerce(different), coerce(X %among% values[[2L]]))))
    }

    A = CnfSymbol(u, values[[1L]], c("0", "1", "2"))
    B = CnfSymbol(u, values[[2L]], c("0", "1", "2"))
    left = CnfClause(list(A %among% "0", B %among% "1"))
    right = CnfClause(list(B %among% "1", A %among% "0"))
    swapped = CnfClause(list(A %among% "1", B %among% "0"))
    for (coerce in list(as.CnfClause, as.CnfFormula)) {
      expect_true(all.equal(coerce(left), coerce(right)))
      expect_false(isTRUE(all.equal(coerce(left), coerce(swapped))))
    }
  })
})

test_that("CnfFormula comparison normalizes stored symbol-name encodings", {
  with_cnf_comparison_locale({
    u = CnfUniverse()
    X = CnfSymbol(u, "\u00e9", c("a", "b", "c"))
    Y = CnfSymbol(u, "\u00f6", c("d", "e", "f"))
    formula = CnfFormula(list(
      CnfClause(list(X %among% "a", Y %among% "d")),
      CnfClause(list(X %among% "b", Y %among% "e"))
    ))
    legacy = formula
    legacy[] = lapply(unclass(formula), function(clause) {
      names(clause) = iconv(names(clause), from = "UTF-8", to = "latin1")
      clause
    })
    expect_identical(formula, legacy)
    expect_true(all.equal(formula, legacy))
  })
})

test_that("CNF universe comparison preserves symbol and domain associations", {
  with_cnf_comparison_locale({
    symbol_names = c("38\u00e9", "38e\u0301")
    domains = list(c("0", "1"), c("0", "1", "2"))
    make_universe = function(order, domains) {
      u = CnfUniverse()
      for (i in order) CnfSymbol(u, symbol_names[[i]], domains[[i]])
      u
    }
    u = make_universe(1:2, domains)
    v = make_universe(2:1, domains)
    swapped = make_universe(1:2, rev(domains))
    reversed = make_universe(1:2, lapply(domains, rev))

    expect_true(all.equal(u, v))
    expect_false(isTRUE(all.equal(u, swapped)))
    expect_false(isTRUE(all.equal(u, reversed)))
    a = CnfAtom(`$.CnfUniverse`(u, symbol_names[[1L]]), "0")
    b = CnfAtom(`$.CnfUniverse`(v, symbol_names[[1L]]), "0")
    other = CnfAtom(`$.CnfUniverse`(swapped, symbol_names[[1L]]), "0")
    for (coerce in list(as.CnfAtom, as.CnfClause, as.CnfFormula)) {
      expect_true(all.equal(coerce(a), coerce(b)))
      expect_false(isTRUE(all.equal(coerce(a), coerce(other))))
      expect_true(all.equal(coerce(a), coerce(other), check.attributes = FALSE))
    }
  })
})

test_that("CnfUniverse comparison retains environment comparison options", {
  u = CnfUniverse()
  v = CnfUniverse()
  expect_true(all.equal(u, v))
  expect_true(all.equal(u, u, evaluate = FALSE))
  CnfSymbol(u, "X", c("a", "b"))
  CnfSymbol(v, "X", c("a", "b"))
  expect_identical(all.equal(u, v, evaluate = FALSE),
    base::all.equal.environment(u, v, evaluate = FALSE))

  # Hidden bindings are ordinary symbols; all.names controls their inclusion.
  CnfSymbol(u, ".hidden", c("a", "b"))
  CnfSymbol(v, ".hidden", c("b", "a"))
  expect_false(isTRUE(all.equal(u, v)))
  expect_true(all.equal(u, v, all.names = FALSE))
  expect_identical(all.equal(u, list()), base::all.equal.environment(u, list()))

  plain = list2env(list(X = c("a", "b")), parent = emptyenv())
  expect_true(all.equal(u, plain, all.names = FALSE))
})
