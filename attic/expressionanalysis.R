


u = CnfUniverse()
W = CnfSymbol(u, "W", c("p", "q", "r"))
X = CnfSymbol(u, "X", c("s", "t", "u"))
Y = CnfSymbol(u, "Y", c("v", "w", "x"))
Z = CnfSymbol(u, "Z", c("y", "z", "a"))

random_atom_expression = function() {
  symbol = sample(alist(W, X, Y, Z), 1)[[1]]
  values = sample(u[[deparse(symbol)]], sample(2, 1))
  substitute(symbol %among% values, list(symbol = symbol, values = values))
}

random_cnf_expression = function(depth, do_cnf = FALSE, ddepth = 0) {
  if (depth <= 1) {
    # base case: return a random symbol expression call
    return(random_atom_expression())
  }
  # recursively build a random tree of expressions
  if (do_cnf) {
    op = if (depth <= ddepth) quote(`|`) else quote(`&`)
  } else {
    op = sample(alist(`&`, `|`), 1)[[1]]  # randomly choose between AND and OR
  }
  left_expr = random_cnf_expression(depth - 1, do_cnf, ddepth)
  right_expr = random_cnf_expression(depth - 1, do_cnf, ddepth)

  # create a call object for the expression: (left_expr op right_expr)
  substitute(op(left_expr, right_expr), list(op = op, left_expr = left_expr, right_expr = right_expr))
}

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

dti_col <- parallel::mclapply(mc.cores = 16, 1:16, function(ss) {
  stats = list(depth = integer(0), expweight = numeric(0), simpweight = numeric(0), was_tautology = logical(0), was_contradiction = logical(0))
  set.seed(3 + ss)
  for (depth_to_check in 2:11) {
    for (repetition in seq_len(2000)) {
      if (depth_to_check == 10) {
        expression = random_cnf_expression(10, TRUE, 4)
      } else if (depth_to_check == 11) {
        expression = random_cnf_expression(5, TRUE, 2)
      }  else {
        expression = random_cnf_expression(depth_to_check)
      }
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
    }
  }
  as.data.table(stats)
})

dti <- rbindlist(dti_col, idcol = "seedoffset")

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
