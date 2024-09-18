



test_that("CnfFormula Regression Tests", {

  testcases = readLines(test_path("testdata", "cnfcases.txt"))

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

  stats = list(depth = integer(0), expweight = numeric(0), simpweight = numeric(0), was_tautology = logical(0), was_contradiction = logical(0))

  for (line in testcases) {
    expression = parse(text = line)[[1]]
    simplified = formula_to_expression(eval(expression))
    stats$depth[[length(stats$depth) + 1]] = depth_to_check
    stats$expweight[[length(stats$expweight) + 1]] = expression_weight(expression)
    stats$simpweight[[length(stats$simpweight) + 1]] = expression_weight(simplified)
    vars = intersect(names(u), all.names(expression))
    assignments = expand.grid(lapply(vars, function(var) u[[var]]), stringsAsFactors = FALSE)
    colnames(assignments) = vars

    truevals = rep(NA, nrow(assignments))
    for (i in seq_len(nrow(assignments))) {
      assignment = assignments[i, , drop = FALSE]
      trueval = evaluate_expression(expression, assignment)
      simpval = evaluate_expression(simplified, assignment)
      if (trueval != simpval) break
      truevals[[i]] = trueval
    }
    stats$was_tautology[[length(stats$was_tautology) + 1]] = all(truevals)
    stats$was_contradiction[[length(stats$was_contradiction) + 1]] = all(!truevals)
    expect_equal(trueval, simpval,
        info = sprintf("Expression: %s\nAssignment:\n%s\nSimplified to: %s",
          deparse1(expression),
          paste(capture.output(print(assignment)), collapse = "\n"),
          deparse1(simplified)
    ))
    cat(".")
  }

  dti <- as.data.table(stats)
  # })

  # dti <- rbindlist(dti_col, idcol = "seedoffset")

  dti[, .(
      ew = mean(expweight), sw = mean(simpweight),
      etriv = mean(expweight == 0),
      striv = mean(simpweight == 0),
      could_simplify = mean(expweight > simpweight),
      was_tautology = mean(was_tautology),
      was_contradiction = mean(was_contradiction),
      tautologies_not_recognized = mean(was_tautology & simpweight > 0),
      contradictions_not_recognized = mean(was_contradiction & simpweight > 0)
    ), by = "depth"]





})


