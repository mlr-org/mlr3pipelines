test_that("CnfFormula Regression Tests", {
  skip_on_cran()
  testfile = xzfile(test_path("testdata", "cnf.xz"))
  testcases = readLines(testfile)
  close(testfile)

  u = CnfUniverse()
  W = CnfSymbol(u, "W", c("p", "q", "r"))
  X = CnfSymbol(u, "X", c("s", "t", "u"))
  Y = CnfSymbol(u, "Y", c("v", "w", "x"))
  Z = CnfSymbol(u, "Z", c("y", "z", "a"))

  formula_to_expression = function(formula) {
    if (is.logical(formula)) return(c(formula))
    clause_exprs = lapply(as.CnfFormula(formula), function(clause) {
      atom_exprs = lapply(clause, function(atom) {
        substitute(symbol %among% values, list(symbol = as.name(atom$symbol), values = atom$values))
      })
      Reduce(function(x, y) substitute(x | y, list(x = x, y = y)), atom_exprs)
    })
    Reduce(function(x, y) substitute(x & y, list(x = x, y = y)), clause_exprs)
  }

  evaluate_expression = function(expression, assignment) {
    substituted = do.call(substitute, list(expression, c(list("%among%" = quote(`%in%`)), assignment)))
    eval(substituted, envir = baseenv())
  }

  expression_weight = function(expression) {
    if (is.logical(expression)) return(0)
    sum(all.names(expression) %in% c("|", "&")) + 1
  }

  varnames = names(u)
  assignments = expand.grid(lapply(varnames, function(var) u[[var]]), stringsAsFactors = FALSE)
  colnames(assignments) = varnames

  stats = list(expweight = numeric(0), simpweight = numeric(0), was_tautology = logical(0), was_contradiction = logical(0))

  for (line in testcases) {
    expression = parse(text = line)[[1]]
    simplified = formula_to_expression(eval(expression))
    stats$expweight[[length(stats$expweight) + 1]] = expression_weight(expression)
    stats$simpweight[[length(stats$simpweight) + 1]] = expression_weight(simplified)

    truevals = evaluate_expression(expression, assignments)
    simpvals = evaluate_expression(simplified, assignments)

    stats$was_tautology[[length(stats$was_tautology) + 1]] = all(truevals)
    stats$was_contradiction[[length(stats$was_contradiction) + 1]] = all(!truevals)

    if (!all(truevals == simpvals)) {
      trueval = truevals[[which(!truevals == simpvals)[1]]]
      simpval = simpvals[[which(!truevals == simpvals)[1]]]
      assignment = assignments[which(!truevals == simpvals)[1], , drop = FALSE]
    } else {
      # alibi
      trueval = simpval = TRUE
      assignment = NULL
    }
    expect_equal(trueval, simpval,
      info = sprintf("Expression: %s\nAssignment:\n%s\nSimplified to: %s",
        deparse1(expression),
        paste(capture.output(print(assignment)), collapse = "\n"),
        deparse1(simplified)
    ))
  }

  dti <- as.data.table(stats)


  dti[, .(
      ew = mean(expweight), sw = mean(simpweight),
      etriv = mean(expweight == 0),
      striv = mean(simpweight == 0),
      could_simplify = mean(expweight > simpweight),
      was_tautology = mean(was_tautology),
      was_contradiction = mean(was_contradiction),
      tautologies_not_recognized = mean(was_tautology & simpweight > 0),
      contradictions_not_recognized = mean(was_contradiction & simpweight > 0)
    )]

})

# Truth-table helpers for the directed regression cases and the property test
# below. `clauses` is a list of named lists (symbol -> allowed values); the
# formula is their conjunction, each clause the disjunction of its ranges.
cnf_test_eval_clauses = function(clauses, assignments) {
  Reduce(`&`, lapply(clauses, function(cl) {
    Reduce(`|`, lapply(names(cl), function(s) assignments[[s]] %in% cl[[s]]))
  }))
}

cnf_test_eval_formula = function(f, assignments) {
  if (!is.na(as.logical(f))) return(rep(as.logical(f), nrow(assignments)))
  Reduce(`&`, lapply(as.list(f), function(clause) {
    Reduce(`|`, lapply(as.list(clause), function(atom) {
      assignments[[atom$symbol]] %in% atom$values
    }))
  }))
}

# build a CnfFormula from bare clauses through the public API and compare its
# truth table against the direct evaluation of the input clauses.
cnf_test_check_case = function(domains, clauses, label) {
  u = CnfUniverse()
  syms = list()
  for (nm in names(domains)) syms[[nm]] = CnfSymbol(u, nm, domains[[nm]])
  clause_objs = lapply(clauses, function(cl) {
    CnfClause(lapply(names(cl), function(s) syms[[s]] %among% cl[[s]]))
  })
  f = CnfFormula(clause_objs)
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  expect_identical(cnf_test_eval_formula(f, assignments), cnf_test_eval_clauses(clauses, assignments),
    info = paste0(label, ": simplification must preserve the truth table"))
  if (is.na(as.logical(f))) {
    for (clause in as.list(f)) {
      for (atom in as.list(clause)) {
        expect_true(length(atom$values) > 0 && !all(domains[[atom$symbol]] %in% atom$values),
          info = paste0(label, ": output ranges must be proper nonempty subsets"))
      }
    }
  }
  invisible(f)
}

test_that("simplify_cnf: directed regression cases for rare simplification paths", {
  # Cases distilled from the verification campaign in attic/cnf_verify. They
  # exercise unit merging, the use_inso skip in register_unit, propagation
  # cascades that create units mid-simplification, both HLA phases (which rely
  # on completed unit propagation for soundness), and 2nd-order SSE.

  # documented resolution subsumption example (?CnfFormula)
  cnf_test_check_case(
    list(X = c("a", "b", "c"), Y = c("d", "e", "f")),
    list(list(X = "a", Y = "d"), list(X = "b", Y = "e"), list(Y = c("d", "e"))),
    "resolution subsumption")

  # documented 'hidden tautology' example; eliminated through the hidden
  # subsumption path in the HLA phase
  cnf_test_check_case(
    list(X = c("a", "b", "c"), Y = c("d", "e", "f"), Z = c("g", "h", "i")),
    list(
      list(X = c("a", "b"), Y = c("d", "e")),
      list(X = "a", Z = c("g", "h")),
      list(X = "b", Z = c("h", "i")),
      list(Y = c("d", "e"), Z = c("g", "i"))
    ),
    "hidden subsumption via HLA")

  # unit-HLA: donors force the unit to be implied by the rest (X = "c" is
  # impossible given the two donors, so the unit clause is redundant)
  cnf_test_check_case(
    list(X = c("a", "b", "c"), Y = c("d", "e", "f")),
    list(list(X = c("a", "b")), list(X = "a", Y = "d"), list(X = "b", Y = "e")),
    "unit eliminated by unit-HLA")

  # unit merge chain: propagation of the Y unit turns clause 3 into a second
  # X unit mid-simplification, which must be intersected with clause 1
  cnf_test_check_case(
    list(X = c("a", "b", "c"), Y = c("d", "e", "f")),
    list(
      list(X = c("a", "b")),
      list(Y = "d"),
      list(X = c("a", "c"), Y = "e"),
      list(X = c("b", "c"), Y = c("d", "f"))
    ),
    "unit created and merged mid-simplification")

  # use_inso equality case: clause range on the unit symbol equals the unit
  # range (subsumption elimination must fire, not be skipped)
  cnf_test_check_case(
    list(X = c("a", "b", "c"), Y = c("d", "e", "f")),
    list(list(X = c("a", "b")), list(X = c("a", "b"), Y = c("d", "e")), list(Y = "d")),
    "clause range equals unit range")

  # regression: the exact 7-clause formula on which breaking unit propagation
  # in register_unit (attic/cnf_verify exp03, mutant M14) makes the unit-HLA
  # phase produce a semantically wrong result
  cnf_test_check_case(
    list(V1 = c("x1", "x2"), V2 = c("x1", "x2", "x3")),
    list(
      list(V2 = c("x1", "x2"), V1 = "x1"),
      list(V2 = "x1", V1 = "x1"),
      list(V1 = "x2", V2 = "x3"),
      list(V1 = "x2", V2 = c("x3", "x2")),
      list(V1 = "x1", V2 = c("x2", "x1")),
      list(V1 = "x1", V2 = c("x2", "x1")),
      list(V1 = "x1", V2 = "x2")
    ),
    "M14 killer (unit-HLA after propagation)")

  # regression: 10-clause formula distinguishing the exact use_inso skip
  # condition from a slightly-too-eager one (exp03, mutant M19)
  cnf_test_check_case(
    list(
      V1 = c("x1", "x2", "x3", "x4"),
      V2 = c("x1", "x2", "x3", "x4", "x5"),
      V3 = c("x1", "x2", "x3"),
      V4 = c("x1", "x2", "x3", "x4", "x5")
    ),
    list(
      list(V1 = c("x1", "x4", "x2"), V2 = c("x2", "x4"), V4 = c("x2", "x4", "x1")),
      list(V3 = "x3", V4 = "x2", V1 = "x3"),
      list(V1 = c("x2", "x1")),
      list(V4 = "x2", V2 = c("x2", "x3", "x4", "x1")),
      list(V2 = "x1", V3 = c("x1", "x2"), V4 = c("x5", "x4", "x2", "x3")),
      list(V1 = c("x4", "x1"), V2 = c("x1", "x3", "x2"), V3 = "x2"),
      list(V1 = c("x1", "x4"), V2 = c("x5", "x4")),
      list(V1 = c("x1", "x2")),
      list(V1 = c("x3", "x4"), V4 = c("x3", "x1", "x5"), V2 = c("x2", "x3", "x1")),
      list(V1 = c("x2", "x4", "x3"), V2 = c("x2", "x3", "x5", "x1"))
    ),
    "M19 killer (use_inso skip condition)")

  # 2nd-order SSE with disjoint ranges outside the target (oneend/twoend)
  cnf_test_check_case(
    list(X = c("a", "b", "c"), Y = c("d", "e", "f"), Z = c("g", "h", "i")),
    list(
      list(X = "a", Y = "d"),
      list(X = "b", Y = "e", Z = "g"),
      list(Y = c("d", "e"), Z = c("g", "h"))
    ),
    "2nd-order self-subsumption")

  # contradiction reached through cascading unit propagation
  cnf_test_check_case(
    list(X = c("a", "b"), Y = c("d", "e"), Z = c("g", "h")),
    list(
      list(X = "a"),
      list(X = "b", Y = "d"),
      list(Y = "e", Z = "g"),
      list(Z = "h")
    ),
    "cascading contradiction")
})

test_that("unit merge during the pairwise phase still subsumption-eliminates equal-range clauses", {
  # Regression (found by attic/cnf_verify/exp13_boundary_search.R): when a
  # clause becomes a unit during the pairwise phase and is merged with an
  # existing unit on the same symbol such that the intersection is *smaller*
  # than the merging unit's own range, the is_not_subset_of-based skip in
  # register_unit() used to be decided against the merging unit's own range.
  # A clause whose range equals the intersection then hid inside "strict
  # subset of the merging unit" and was never subsumption-eliminated, leaving
  # a redundant clause in the output (semantically still correct).
  # minimized reproducer (attic/cnf_verify/results/i3_minimized.rds): both V1
  # units emerge mid-simplification from 2nd-order SSE; their merge intersects
  # to {v1_1, v1_5}, and the first clause's V1 range -- equal to that
  # intersection -- must be recognized as subsumed.
  u = CnfUniverse()
  V1 = CnfSymbol(u, "V1", paste0("v1_", 1:5))
  V2 = CnfSymbol(u, "V2", c("v2_2", "v2_3", "v2_5"))
  f = CnfFormula(list(
    CnfClause(list(V2 %among% "v2_5", V1 %among% c("v1_2", "v1_1", "v1_5"))),
    CnfClause(list(V1 %among% c("v1_1", "v1_3", "v1_5"), V2 %among% "v2_3")),
    CnfClause(list(V1 %among% c("v1_4", "v1_1", "v1_5"), V2 %among% "v2_2"))
  ))
  expected = CnfFormula(list(CnfClause(list(V1 %among% c("v1_1", "v1_5")))))
  expect_true(isTRUE(all.equal(f, expected)))

  # the original found case: output must not contain a clause subsumed by
  # another output clause, and must stay equivalent
  domains = list(
    V1 = c("v1_1", "v1_2", "v1_3", "v1_4", "v1_5"),
    V2 = c("v2_1", "v2_2", "v2_3", "v2_4", "v2_5")
  )
  clauses = list(
    list(V2 = c("v2_5", "v2_1", "v2_3"), V1 = c("v1_2", "v1_1")),
    list(V2 = "v2_5", V1 = c("v1_2", "v1_1", "v1_5")),
    list(V2 = c("v2_5", "v2_1", "v2_3"), V1 = c("v1_1", "v1_2", "v1_3")),
    list(V1 = c("v1_1", "v1_3", "v1_5"), V2 = "v2_3"),
    list(V1 = c("v1_4", "v1_1", "v1_5"), V2 = "v2_2"),
    list(V2 = c("v2_5", "v2_1", "v2_3", "v2_2"))
  )
  f2 = cnf_test_check_case(domains, clauses, "exp13 boundary case")
  bare = lapply(as.list(f2), function(clause) {
    atoms = as.list(clause)
    structure(lapply(atoms, `[[`, "values"), names = map_chr(atoms, `[[`, "symbol"))
  })
  subsumes = function(a, b) {
    all(names(a) %in% names(b)) && all(vapply(names(a), function(s) all(a[[s]] %in% b[[s]]), NA))
  }
  for (i in seq_along(bare)) {
    for (j in seq_along(bare)) {
      expect_true(i == j || !subsumes(bare[[i]], bare[[j]]),
        info = sprintf("output clause %d is subsumed by clause %d", j, i))
    }
  }
})

test_that("simplify_cnf: seeded random property test against truth tables", {
  skip_on_cran()
  # compact version of the fuzzers in attic/cnf_verify: random universes and
  # clause sets, exact truth-table comparison. Deterministic via set.seed.
  set.seed(20260713)
  for (trial in seq_len(150)) {
    n_sym = sample(2:4, 1)
    domains = list()
    for (i in seq_len(n_sym)) {
      domains[[paste0("V", i)]] = paste0("x", seq_len(sample(2:4, 1)))
    }
    n_clauses = sample(1:8, 1)
    clauses = lapply(seq_len(n_clauses), function(j) {
      cl_syms = sample(names(domains), sample.int(n_sym, 1))
      cl = list()
      for (s in cl_syms) {
        d = domains[[s]]
        cl[[s]] = sample(d, sample.int(length(d) - 1L, 1))
      }
      cl
    })
    cnf_test_check_case(domains, clauses, sprintf("property trial %d", trial))
  }
})

test_that("nested unit propagation preserves deferred restrictions", {
  # During the original order, X1 -> X0 -> X1 registration temporarily skips
  # a stored X1 = 3 range under the new X1 = 6 unit. The older propagation
  # frame must still finish its restriction before unit-HLA runs.
  domains = list(X0 = c("1", "2", "5", "6"), X1 = c("0", "3", "5", "6"))
  clauses = list(
    list(X0 = c("2", "6"), X1 = "6"),
    list(X0 = "2", X1 = c("6", "3")),
    list(X0 = "5", X1 = c("3", "0")),
    list(X1 = "3", X0 = c("5", "2")),
    list(X0 = "1", X1 = c("6", "5"))
  )
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  expected = assignments$X0 == "5" & assignments$X1 == "6"
  expect_identical(cnf_test_eval_clauses(clauses, assignments), expected)
  orders = list(1:5, 5:1, c(3:5, 1:2), c(2, 4, 1, 5, 3))
  for (order in orders) {
    for (reverse_symbols in c(FALSE, TRUE)) {
      input = clauses[order]
      if (reverse_symbols) input = lapply(input, rev)
      formula = cnf_test_check_case(domains, input, "deferred unit restriction")
      expect_identical(cnf_test_eval_formula(formula, assignments), expected)
    }
  }
})

test_that("resolution preserves models when an already-contained donor literal disappears", {
  # The last two clauses allow removal of T = 1 from the second clause.
  # That also changes a different resolution union. The current scheduler
  # may leave that second reduction to another pass, but both passes must
  # preserve every model. Completeness examples are recorded in attic/cnf_verify3.
  domains = list(S = c("0", "1"), T = c("1", "2", "3"), U = c("0", "1"), R = c("0", "1"))
  clauses = list(
    list(T = c("1", "2"), R = "0"),
    list(S = "0", T = "1", R = "0"),
    list(S = "1", T = c("2", "3")),
    list(U = "1", T = "2"),
    list(S = "0", U = "0", R = "0")
  )
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  expected = cnf_test_eval_clauses(clauses, assignments)
  expect_equal(sum(expected), 9L)
  formula = cnf_test_check_case(domains, clauses, "disappearing donor literal")
  expect_identical(cnf_test_eval_formula(CnfFormula(as.list(formula)), assignments), expected)
})

test_that("splitting domain values into unequal numbers of labels preserves CNF semantics", {
  domains = list(X = c("a", "b", "c", "d"), Y = c("a", "b", "c"))
  clauses = list(
    list(X = c("a", "b"), Y = "a"),
    list(Y = "b", X = c("a", "c")),
    list(Y = "c", X = c("a", "d"))
  )
  # Every old value gets a different, nonzero multiplicity. Length-based
  # comparisons must still mean equality of contained sets, not cardinality
  # comparisons between unrelated ranges.
  refinements = lapply(domains, function(domain) {
    set_names(lapply(seq_along(domain), function(i) paste0(domain[[i]], seq_len(i))), domain)
  })
  refined_domains = lapply(refinements, unlist, use.names = FALSE)
  refined_clauses = lapply(clauses, function(clause) {
    set_names(lapply(names(clause), function(symbol) {
      unlist(refinements[[symbol]][clause[[symbol]]], use.names = FALSE)
    }), names(clause))
  })
  formula = cnf_test_check_case(domains, clauses, "original value classes")
  refined_formula = cnf_test_check_case(refined_domains, refined_clauses, "refined value classes")
  assignments = expand.grid(refined_domains, stringsAsFactors = FALSE)
  projected = assignments
  for (symbol in names(domains)) {
    labels = unlist(refinements[[symbol]], use.names = FALSE)
    old_values = rep(names(refinements[[symbol]]), lengths(refinements[[symbol]]))
    projected[[symbol]] = old_values[match(assignments[[symbol]], labels)]
  }
  expect_identical(cnf_test_eval_formula(refined_formula, assignments), cnf_test_eval_formula(formula, projected))
})

test_that("unit implication chains simplify in either clause order", {
  universe = CnfUniverse()
  symbols = lapply(seq_len(24L), function(i) CnfSymbol(universe, paste0("X", i), c("0", "1")))
  clauses = lapply(seq_len(length(symbols) - 1L), function(i) {
    CnfClause(list(CnfAtom(symbols[[i]], "0"), CnfAtom(symbols[[i + 1L]], "1")))
  })
  initial_unit = as.CnfClause(CnfAtom(symbols[[1L]], "1"))
  expected = CnfFormula(lapply(symbols, function(symbol) as.CnfClause(CnfAtom(symbol, "1"))))
  for (input in list(clauses, rev(clauses))) {
    # Clause order may differ; use the order-independent CNF comparison.
    expect_true(all.equal(CnfFormula(c(list(initial_unit), input)), expected))
  }
})

test_that("simplification tolerates comparisons completed by nested callbacks", {
  # A recursive restriction clears a comparison that an older update loop
  # still has queued. Counting that transition twice breaks this example.
  # The independent source-copy control and reduction are in attic/cnf_verify3/root.
  domains = list(
    X3 = c("v3", "v4", "v6"),
    X1 = c("v1", "v3", "v4", "v5", "v6"),
    X4 = paste0("v", 1:6),
    X2 = paste0("v", 1:4)
  )
  clauses = list(
    list(X3 = "v6", X1 = c("v3", "v5")),
    list(X4 = "v2", X1 = c("v6", "v4")),
    list(X2 = "v4", X4 = "v6"),
    list(X2 = c("v2", "v4", "v1"), X1 = "v1", X3 = "v3"),
    list(X2 = "v3", X4 = c("v2", "v4")),
    list(X1 = c("v3", "v5"), X2 = c("v4", "v3", "v2"), X4 = "v3"),
    list(X3 = c("v3", "v6"), X4 = "v5"),
    list(X2 = "v2", X1 = "v5", X4 = c("v4", "v1")),
    list(X4 = "v2", X3 = c("v6", "v4"), X2 = c("v2", "v3")),
    list(X1 = "v6", X3 = c("v3", "v4")),
    list(X1 = "v3", X3 = c("v6", "v3"))
  )
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  expected = assignments$X2 == "v4" & (
    (assignments$X1 == "v5" & assignments$X3 == "v3" & assignments$X4 == "v2") |
      (assignments$X1 == "v6" & assignments$X3 == "v6" & assignments$X4 == "v4")
  )
  expect_equal(sum(expected), 2L)
  expect_identical(cnf_test_eval_clauses(clauses, assignments), expected)
  formula = cnf_test_check_case(domains, clauses, "comparison completed by a nested callback")
  expect_identical(cnf_test_eval_formula(formula, assignments), expected)
})

test_that("unit propagation tolerates a queued clause becoming a different unit", {
  # The last two clauses derive X in {0,1}. Its propagation through Y derives
  # X = 0, and nested work turns the third clause into Z = 0 before the older
  # X propagation frame reaches that same queued clause.
  domains = list(X = c("0", "1", "2"), Y = c("0", "1"), Z = c("0", "1"), W = c("0", "1"))
  clauses = list(
    list(X = "2", Y = "0"),
    list(Y = "1", X = "0"),
    list(X = "2", Z = "0"),
    list(X = c("0", "1"), W = "0"),
    list(X = c("0", "1"), W = "1")
  )
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  expected = assignments$X == "0" & assignments$Y == "0" & assignments$Z == "0"
  expect_equal(sum(expected), 2L)
  expect_identical(cnf_test_eval_clauses(clauses, assignments), expected)
  for (order in list(1:5, 5:1, c(4, 5, 1, 2, 3))) {
    formula = cnf_test_check_case(domains, clauses[order], "queued clause becomes a different unit")
    expect_identical(cnf_test_eval_formula(formula, assignments), expected)
  }
})
