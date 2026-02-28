source("experiments/test_harness.R")

# Basic correctness fuzzer: generate random CNF expressions, evaluate them,
# and check that simplification preserves semantics.

u = CnfUniverse()
W = CnfSymbol(u, "W", c("p", "q", "r"))
X = CnfSymbol(u, "X", c("s", "t", "u"))
Y = CnfSymbol(u, "Y", c("v", "w", "x"))
Z = CnfSymbol(u, "Z", c("y", "z", "a"))

symbols = list(W = W, X = X, Y = Y, Z = Z)
sym_names = names(symbols)

# Truth table assignments
varnames = ls(u)
domains = lapply(varnames, function(v) get(v, u))
names(domains) = varnames
assignments = expand.grid(domains, stringsAsFactors = FALSE)
colnames(assignments) = varnames

# Generate a random atom expression
random_atom_expr = function() {
  sym_name = sample(sym_names, 1)
  sym = symbols[[sym_name]]
  domain = u[[sym_name]]
  n_vals = sample(length(domain), 1)  # 1 to |domain|
  vals = sample(domain, n_vals)
  substitute(sym %among% vals, list(sym = as.name(sym_name), vals = vals))
}

# Generate a random CNF expression (tree of & and |)
random_expr = function(depth, force_cnf = FALSE, or_depth = 0) {
  if (depth <= 1) return(random_atom_expr())
  if (force_cnf) {
    op = if (depth <= or_depth) quote(`|`) else quote(`&`)
  } else {
    op = sample(c(quote(`&`), quote(`|`)), 1)[[1]]
  }
  left = random_expr(depth - 1, force_cnf, or_depth)
  right = random_expr(depth - 1, force_cnf, or_depth)
  substitute(op(left, right), list(op = op, left = left, right = right))
}

# Evaluate an expression with given assignments
eval_expr = function(expr, assignments) {
  apply(assignments, 1, function(row) {
    env = as.list(row)
    env[["%among%"]] = `%in%`
    eval(expr, envir = env, enclos = baseenv())
  })
}

# Convert a CnfFormula back to an expression for evaluation
formula_to_truth = function(formula) {
  evaluate_formula(formula, u)
}

n_failures = 0
n_tests = 0

test_expression = function(expr, label) {
  n_tests <<- n_tests + 1
  result = tryCatch({
    formula = eval(expr)
    truth_original = eval_expr(expr, assignments)
    truth_simplified = formula_to_truth(as.CnfFormula(formula))
    mismatches = which(truth_original != truth_simplified)
    if (length(mismatches)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: %s\n", label, deparse1(expr)))
      idx = mismatches[1]
      cat(sprintf("  Assignment: %s\n", paste(names(assignments), "=", assignments[idx, ], collapse = ", ")))
      cat(sprintf("  Original: %s, Simplified: %s\n", truth_original[idx], truth_simplified[idx]))
    }
  }, error = function(e) {
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR [%s]: %s\n  %s\n", label, deparse1(expr), e$message))
  })
}

set.seed(42)

# Phase 1: Random mixed expressions (depth 2-6)
cat("=== Phase 1: Random mixed expressions ===\n")
for (depth in 2:6) {
  for (i in 1:500) {
    expr = random_expr(depth)
    test_expression(expr, sprintf("depth=%d, i=%d", depth, i))
  }
  cat(sprintf("  depth %d: %d tests, %d failures\n", depth, n_tests, n_failures))
}

# Phase 2: Pure CNF expressions (to exercise simplifier more)
cat("\n=== Phase 2: Pure CNF expressions ===\n")
for (or_depth in 1:3) {
  for (total_depth in (or_depth + 1):(or_depth + 3)) {
    for (i in 1:200) {
      expr = random_expr(total_depth, TRUE, or_depth)
      test_expression(expr, sprintf("cnf or_depth=%d, total=%d, i=%d", or_depth, total_depth, i))
    }
  }
  cat(sprintf("  or_depth %d: %d tests, %d failures\n", or_depth, n_tests, n_failures))
}

# Phase 3: Expressions with negation
cat("\n=== Phase 3: Expressions with negation ===\n")
for (i in 1:200) {
  # Generate a formula and negate it
  expr = random_expr(3)
  neg_expr = substitute(!expr, list(expr = expr))
  test_expression(neg_expr, sprintf("negated, i=%d", i))
}
cat(sprintf("  After negation tests: %d tests, %d failures\n", n_tests, n_failures))

# Phase 4: Double negation
cat("\n=== Phase 4: Double negation ===\n")
for (i in 1:100) {
  expr = random_expr(3)
  formula = eval(expr)
  if (is.logical(formula)) next  # skip trivial
  truth_orig = formula_to_truth(as.CnfFormula(formula))
  result = tryCatch({
    double_neg = !(!formula)
    truth_dn = formula_to_truth(as.CnfFormula(double_neg))
    mismatches = which(truth_orig != truth_dn)
    if (length(mismatches)) {
      n_failures <<- n_failures + 1
      idx = mismatches[1]
      cat(sprintf("FAIL [double_neg i=%d]: %s\n", i, deparse1(expr)))
      cat(sprintf("  Assignment: %s\n", paste(names(assignments), "=", assignments[idx, ], collapse = ", ")))
    }
    n_tests <<- n_tests + 1
  }, error = function(e) {
    n_failures <<- n_failures + 1
    n_tests <<- n_tests + 1
    cat(sprintf("ERROR [double_neg i=%d]: %s\n  %s\n", i, deparse1(expr), e$message))
  })
}
cat(sprintf("  After double negation: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== SUMMARY: %d tests, %d failures ===\n", n_tests, n_failures))
