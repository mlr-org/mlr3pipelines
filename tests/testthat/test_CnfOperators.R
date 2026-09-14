cnf_operator_fixture = function() {
  u = CnfUniverse()
  X = CnfSymbol(u, "X", c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("a", "b", "c"))
  Z = CnfSymbol(u, "Z", c("a", "b", "c"))
  assignments = expand.grid(X = c("a", "b", "c"), Y = c("a", "b", "c"),
    Z = c("a", "b", "c"), stringsAsFactors = FALSE)
  list(
    objects = list(
      CnfAtom(X, "a"),
      CnfClause(list(CnfAtom(X, "b"), CnfAtom(Y, "a"))),
      CnfFormula(list(
        CnfClause(list(CnfAtom(X, "a"), CnfAtom(Y, "b"))),
        CnfClause(list(CnfAtom(X, "b"), CnfAtom(Z, "a")))
      ))
    ),
    truth = list(
      assignments$X == "a",
      assignments$X == "b" | assignments$Y == "a",
      (assignments$X == "a" | assignments$Y == "b") &
        (assignments$X == "b" | assignments$Z == "a")
    ),
    assignments = assignments
  )
}

# Evaluate stored occurrences by position, without CNF comparison/coercion helpers.
cnf_operator_truth = function(object, assignments) {
  n = nrow(assignments)
  if (is.logical(object)) return(rep(object[[1L]], n))
  if (inherits(object, "CnfAtom")) return(assignments[[object$symbol]] %in% object$values)
  clause_truth = function(clause) {
    entries = unclass(clause)
    if (is.logical(entries)) return(rep(entries[[1L]], n))
    Reduce(`|`, lapply(seq_along(entries), function(i) {
      assignments[[names(entries)[[i]]]] %in% entries[[i]]
    }), init = rep(FALSE, n))
  }
  if (inherits(object, "CnfClause")) return(clause_truth(object))
  stopifnot(inherits(object, "CnfFormula"))
  entries = unclass(object)
  Reduce(`&`, lapply(seq_along(entries), function(i) clause_truth(entries[[i]])), init = rep(TRUE, n))
}

expect_cnf_operator = function(object, truth, class, assignments) {
  expect_s3_class(object, class)
  expect_identical(cnf_operator_truth(object, assignments), truth)
}

test_that("CNF classes register one shared handler for each binary operator", {
  # Old R cannot resolve distinct Ops methods on the two operand classes.
  for (operator in c("&", "|")) {
    method = getS3method(operator, "CnfAtom")
    for (class in c("CnfClause", "CnfFormula")) {
      expect_true(identical(getS3method(operator, class), method), info = paste(operator, class))
    }
  }
})

test_that("public mixed CNF operators preserve truth and result classes in both orders", {
  fixture = cnf_operator_fixture()
  for (i in seq_along(fixture$objects)) {
    expect_identical(cnf_operator_truth(fixture$objects[[i]], fixture$assignments), fixture$truth[[i]])
  }
  # The Formula must retain two non-unit clauses, so OR exercises distribution.
  expect_identical(lengths(unclass(fixture$objects[[3L]])), c(2L, 2L))
  for (pair in list(c(1L, 2L), c(1L, 3L), c(2L, 3L))) {
    for (order in list(pair, rev(pair))) {
      left = fixture$objects[[order[[1L]]]]
      right = fixture$objects[[order[[2L]]]]
      p = fixture$truth[[order[[1L]]]]
      q = fixture$truth[[order[[2L]]]]
      or_class = if (3L %in% pair) "CnfFormula" else "CnfClause"
      expect_cnf_operator(left & right, p & q, "CnfFormula", fixture$assignments)
      expect_cnf_operator(left | right, p | q, or_class, fixture$assignments)
    }
  }
})

test_that("public CNF operators handle bare and differently classed constants", {
  fixture = cnf_operator_fixture()
  constants = list(list(TRUE, as.CnfClause(FALSE)), list(FALSE, as.CnfFormula(TRUE)),
    list(TRUE, as.CnfAtom(FALSE)))
  for (i in seq_along(fixture$objects)) {
    proper = fixture$objects[[i]]
    p = fixture$truth[[i]]
    for (constant in constants[[i]]) {
      q = rep(constant[[1L]], nrow(fixture$assignments))
      or_class = if (inherits(proper, "CnfFormula") || inherits(constant, "CnfFormula")) "CnfFormula" else "CnfClause"
      for (operands in list(list(proper, constant), list(constant, proper))) {
        expect_cnf_operator(operands[[1L]] & operands[[2L]], p & q, "CnfFormula", fixture$assignments)
        expect_cnf_operator(operands[[1L]] | operands[[2L]], p | q, or_class, fixture$assignments)
      }
    }
  }
  for (pair in list(list(as.CnfAtom(TRUE), as.CnfClause(FALSE)),
    list(as.CnfAtom(FALSE), as.CnfFormula(TRUE)), list(as.CnfClause(TRUE), as.CnfFormula(FALSE)))) {
    or_class = if (any(vapply(pair, inherits, logical(1), "CnfFormula"))) "CnfFormula" else "CnfClause"
    for (operands in list(pair, rev(pair))) {
      expect_cnf_operator(operands[[1L]] & operands[[2L]], rep(FALSE, nrow(fixture$assignments)),
        "CnfFormula", fixture$assignments)
      expect_cnf_operator(operands[[1L]] | operands[[2L]], rep(TRUE, nrow(fixture$assignments)),
        or_class, fixture$assignments)
    }
  }
})

test_that("CNF negation preserves classes and supports nested mixed composition", {
  fixture = cnf_operator_fixture()
  coercions = list(as.CnfAtom, as.CnfClause, as.CnfFormula)
  for (i in seq_along(fixture$objects)) {
    class = if (i == 1L) "CnfAtom" else "CnfFormula"
    expect_cnf_operator(!fixture$objects[[i]], !fixture$truth[[i]], class, fixture$assignments)
    for (value in c(FALSE, TRUE)) {
      expect_cnf_operator(!coercions[[i]](value), rep(!value, nrow(fixture$assignments)), class, fixture$assignments)
    }
  }
  a = fixture$objects[[1L]]
  clause = fixture$objects[[2L]]
  formula = fixture$objects[[3L]]
  expected = !((fixture$truth[[1L]] | fixture$truth[[2L]]) & !fixture$truth[[3L]])
  expect_cnf_operator(!((a | clause) & !formula), expected, "CnfFormula", fixture$assignments)
})

test_that("mixed nonconstant CNF operators reject different universes", {
  first = cnf_operator_fixture()$objects
  second = cnf_operator_fixture()$objects
  for (pair in list(c(1L, 2L), c(1L, 3L), c(2L, 3L))) {
    for (operands in list(list(first[[pair[[1L]]]], second[[pair[[2L]]]]),
      list(second[[pair[[2L]]]], first[[pair[[1L]]]]))) {
      expect_error(operands[[1L]] & operands[[2L]], "same universe")
      expect_error(operands[[1L]] | operands[[2L]], "same universe")
    }
  }
})
