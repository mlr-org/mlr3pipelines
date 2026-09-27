
# arguments: 1 - seed,
# 2: varcount: 4 or 5
# 3 - value per var count: 3 or 4
# 4: maxdepth: 12 or 13; last 2 are special
# 5: with_subexps: TRUE or FALSE

# Get 'ss' from command line arguments
args <- commandArgs(trailingOnly = TRUE)
checkmate::assert_integer(ss <- as.integer(args[1]), lower = 1, len = 1, any.missing = FALSE)
checkmate::assert_integer(varcount <- as.integer(args[2]), lower = 3, upper = 7, len = 1, any.missing = FALSE)
checkmate::assert_integer(vals_per_count <- as.integer(args[3]), lower = 2, upper = 6, len = 1, any.missing = FALSE)
checkmate::assert_integer(maxdepth <- as.integer(args[4]), lower = 3, upper = 20, len = 1, any.missing = FALSE)
checkmate::assert_logical(with_subexps <- as.logical(args[5]), len = 1)

library("mlr3pipelines")
library("testthat")
library("data.table")
options(width = 10000)

u = CnfUniverse()

for (varidx in seq_len(varcount)) {
  assign(sprintf("V%i", varidx), CnfSymbol(u, sprintf("V%i", varidx), paste0("v", varidx, "_", seq_len(vals_per_count))))
}

all_vars = lapply(sprintf("V%i", seq_len(varcount)), as.name)

random_atom_expression = function() {
  symbol = sample(all_vars, 1)[[1]]
  values = sample(u[[deparse(symbol)]], sample(vals_per_count - 1, 1))
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

varnames = names(u)
assignments = expand.grid(lapply(varnames, function(var) u[[var]]), stringsAsFactors = FALSE)
colnames(assignments) = varnames

stats = list(depth = integer(0), expweight = numeric(0), simpweight = numeric(0), was_tautology = logical(0), was_contradiction = logical(0))
set.seed(ss)
for (depth_to_check in 2:maxdepth) {
  for (repetition in seq_len(2000)) {
    if (!with_subexps) {
      if (depth_to_check == (maxdepth - 1)) {
        expression = random_cnf_expression(10, TRUE, 4)
      } else if (depth_to_check == maxdepth) {
        expression = random_cnf_expression(5, TRUE, 2)
      }  else {
        expression = random_cnf_expression(depth_to_check)
      }
      cat(sprintf("%s\n", deparse1(expression)))
      simplified = formula_to_expression(eval(expression))
    } else {
      subexps = replicate(if (depth_to_check == maxdepth) 10 else 4, {
        repeat {
          if (depth_to_check == maxdepth) {
            expression = random_cnf_expression(10, TRUE, 4)
          } else {
            expression = random_cnf_expression(depth_to_check)
          }
          cat(sprintf("%s\n", deparse1(expression)))
          result = eval(expression)
          if (!is.logical(result)) break
        }
        result
      }, simplify = FALSE)
      expression = Reduce(function(x, y) substitute(x & y, list(x = x, y = y)), lapply(subexps, formula_to_expression))
      cat(sprintf("%s\n\n", deparse1(expression)))
      simplified = formula_to_expression(CnfFormula(subexps))
    }

    stats$depth[[length(stats$depth) + 1]] = depth_to_check
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
}
dti <- as.data.table(stats)

saveRDS(dti, sprintf("expression_experiments_3_%05i.rds", ss))

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
