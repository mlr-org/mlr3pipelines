source("experiments/test_harness.R")

# Expression tree evaluation fuzzer, similar to the attic experiments
# but sourcing files directly instead of loading the package.

n_failures = 0
n_tests = 0

u = CnfUniverse()
V = CnfSymbol(u, "V", c("m", "n", "o"))
W = CnfSymbol(u, "W", c("p", "q", "r"))
X = CnfSymbol(u, "X", c("s", "t", "u"))
Y = CnfSymbol(u, "Y", c("v", "w", "x"))
Z = CnfSymbol(u, "Z", c("y", "z", "a"))

random_atom_expression = function() {
  symbol = sample(alist(V, W, X, Y, Z), 1)[[1]]
  values = sample(u[[deparse(symbol)]], sample(2, 1))
  substitute(symbol %among% values, list(symbol = symbol, values = values))
}

random_cnf_expression = function(depth, do_cnf = FALSE, ddepth = 0) {
  if (depth <= 1) return(random_atom_expression())
  if (do_cnf) {
    op = if (depth <= ddepth) quote(`|`) else quote(`&`)
  } else {
    op = sample(alist(`&`, `|`), 1)[[1]]
  }
  left_expr = random_cnf_expression(depth - 1, do_cnf, ddepth)
  right_expr = random_cnf_expression(depth - 1, do_cnf, ddepth)
  substitute(op(left_expr, right_expr), list(op = op, left_expr = left_expr, right_expr = right_expr))
}

formula_to_expression = function(formula) {
  if (is.logical(formula)) return(c(formula))
  clause_list = as.list(as.CnfFormula(formula))
  if (length(clause_list) == 0) return(TRUE)
  clause_exprs = lapply(clause_list, function(clause) {
    atom_list = as.list(clause)
    if (length(atom_list) == 0) return(FALSE)
    atom_exprs = lapply(atom_list, function(atom) {
      if (is.logical(atom)) return(c(atom))
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

varnames = ls(u)
domains = lapply(varnames, function(v) get(v, u))
names(domains) = varnames
assignments = expand.grid(domains, stringsAsFactors = FALSE)
colnames(assignments) = varnames

set.seed(42)

for (depth_to_check in 2:10) {
  n_reps = if (depth_to_check <= 5) 1000 else if (depth_to_check <= 8) 500 else 200
  for (repetition in seq_len(n_reps)) {
    if (depth_to_check == 9) {
      expression = random_cnf_expression(8, TRUE, 3)
    } else if (depth_to_check == 10) {
      expression = random_cnf_expression(5, TRUE, 2)
    } else {
      expression = random_cnf_expression(depth_to_check)
    }

    result = tryCatch({
      simplified = formula_to_expression(eval(expression))

      truevals = apply(assignments, 1, function(row) evaluate_expression(expression, as.list(row)))
      simpvals = apply(assignments, 1, function(row) evaluate_expression(simplified, as.list(row)))

      n_tests <<- n_tests + 1
      mismatches = which(truevals != simpvals)
      if (length(mismatches)) {
        n_failures <<- n_failures + 1
        idx = mismatches[1]
        cat(sprintf("FAIL [depth=%d, rep=%d]: mismatch at row %d (orig=%s, simp=%s)\n",
          depth_to_check, repetition, idx, truevals[idx], simpvals[idx]))
        cat(sprintf("  Expr: %s\n", deparse1(expression)))
        cat(sprintf("  Simplified: %s\n", deparse1(simplified)))
        cat(sprintf("  Assignment: %s\n",
          paste(names(assignments), "=", assignments[idx, ], collapse = ", ")))
      }
    }, error = function(e) {
      n_tests <<- n_tests + 1
      n_failures <<- n_failures + 1
      cat(sprintf("ERROR [depth=%d, rep=%d]: %s\n  Expr: %s\n",
        depth_to_check, repetition, e$message, deparse1(expression)))
    })
  }
  cat(sprintf("depth %d done: %d tests, %d failures total\n", depth_to_check, n_tests, n_failures))
}

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
