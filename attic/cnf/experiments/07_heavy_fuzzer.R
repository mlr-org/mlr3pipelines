source("experiments/test_harness.R")

# Heavy fuzzer: many variables, many clauses, many iterations
# Key insight: the attic experiments used 4 variables × 3 values each = 81 assignments
# and ran thousands of random expressions. We need to do the same but more targeted.

n_failures = 0
n_tests = 0
failure_details = list()

check = function(formula, universe, label) {
  n_tests <<- n_tests + 1
  truth = tryCatch(evaluate_formula(formula, universe), error = function(e) e)
  if (inherits(truth, "error")) {
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR [%s]: %s\n", label, truth$message))
    return(FALSE)
  }

  formula_bare = unclass(formula)
  if (isTRUE(formula_bare)) {
    if (!all(truth)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: simplified to TRUE but not tautology\n", label))
      failure_details[[length(failure_details) + 1]] <<- list(label = label, type = "false_tautology")
      return(FALSE)
    }
    return(TRUE)
  }
  if (isFALSE(formula_bare)) {
    if (!all(!truth)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: simplified to FALSE but not contradiction\n", label))
      failure_details[[length(failure_details) + 1]] <<- list(label = label, type = "false_contradiction")
      return(FALSE)
    }
    return(TRUE)
  }

  # Cross-check: evaluate the raw clauses directly
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  raw_truth = rep(TRUE, nrow(assignments))
  for (clause in unclass(formula)) {
    clause_truth = rep(FALSE, nrow(assignments))
    for (sym in names(clause)) {
      clause_truth = clause_truth | (assignments[[sym]] %in% clause[[sym]])
    }
    raw_truth = raw_truth & clause_truth
  }
  if (!all(truth == raw_truth)) {
    n_failures <<- n_failures + 1
    idx = which(truth != raw_truth)[1]
    cat(sprintf("FAIL [%s]: truth mismatch at row %d\n", label, idx))
    return(FALSE)
  }
  TRUE
}

# Helper: make a random clause as a CnfClause
make_clause = function(syms, universe, n_atoms_range = 1:3, val_range = NULL) {
  sym_names = names(syms)
  n_atoms = sample(n_atoms_range, 1)
  n_atoms = min(n_atoms, length(sym_names))
  chosen = sample(sym_names, n_atoms)
  atoms = lapply(chosen, function(s) {
    dom = universe[[s]]
    if (is.null(val_range)) {
      n_vals = sample(1:length(dom), 1)
    } else {
      n_vals = sample(val_range, 1)
      n_vals = min(n_vals, length(dom))
      n_vals = max(n_vals, 1)
    }
    syms[[s]] %among% sample(dom, n_vals)
  })
  as.CnfClause(Reduce(`|`, atoms))
}

# ============================================================
# Test A: The attic-style test but with direct construction
# Generate random CNF formulas and check truth tables
# ============================================================
cat("=== Test A: Attic-style random CNF (4 vars, ternary) ===\n")

set.seed(2025)
for (batch in 1:5) {
  u_a = CnfUniverse()
  s_a = list(
    W = CnfSymbol(u_a, "W", c("p", "q", "r")),
    X = CnfSymbol(u_a, "X", c("s", "t", "u")),
    Y = CnfSymbol(u_a, "Y", c("v", "w", "x")),
    Z = CnfSymbol(u_a, "Z", c("y", "z", "a"))
  )

  for (i in 1:400) {
    n_clauses = sample(2:8, 1)
    clauses = lapply(1:n_clauses, function(j) make_clause(s_a, u_a, 1:3))

    # Method 1: Build via CnfFormula constructor
    result = tryCatch({
      f = CnfFormula(clauses)

      # Also evaluate clauses individually to get the "original" truth table
      varnames = ls(u_a)
      domains = lapply(varnames, function(v) get(v, u_a))
      names(domains) = varnames
      assignments = expand.grid(domains, stringsAsFactors = FALSE)

      original_truth = rep(TRUE, nrow(assignments))
      for (cl in clauses) {
        cl_bare = unclass(cl)
        if (isTRUE(cl_bare)) next
        if (isFALSE(cl_bare)) { original_truth = rep(FALSE, nrow(assignments)); break }
        cl_truth = rep(FALSE, nrow(assignments))
        for (sym in names(cl_bare)) {
          cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
        }
        original_truth = original_truth & cl_truth
      }

      simplified_truth = evaluate_formula(f, u_a)
      mismatches = which(original_truth != simplified_truth)
      if (length(mismatches)) {
        n_failures <<- n_failures + 1
        idx = mismatches[1]
        cat(sprintf("FAIL [A batch=%d, i=%d]: truth mismatch at row %d (original=%s, simplified=%s)\n",
          batch, i, idx, original_truth[idx], simplified_truth[idx]))
        cat(sprintf("  Clauses: %s\n", paste(sapply(clauses, format), collapse = " & ")))
        cat(sprintf("  Formula: %s\n", format(f)))
      } else {
        n_tests <<- n_tests + 1
      }
    }, error = function(e) {
      n_tests <<- n_tests + 1
      n_failures <<- n_failures + 1
      cat(sprintf("ERROR [A batch=%d, i=%d]: %s\n", batch, i, e$message))
    })
  }
  cat(sprintf("  batch %d: %d tests, %d failures\n", batch, n_tests, n_failures))
}

# ============================================================
# Test B: Build formulas incrementally with &
# This tests the & path which calls simplify_cnf on already-simplified formulas
# ============================================================
cat("\n=== Test B: Incremental formula building ===\n")

set.seed(777)
for (trial in 1:200) {
  u_b = CnfUniverse()
  s_b = list(
    A = CnfSymbol(u_b, "A", c("a1", "a2", "a3")),
    B = CnfSymbol(u_b, "B", c("b1", "b2", "b3")),
    C = CnfSymbol(u_b, "C", c("c1", "c2", "c3")),
    D = CnfSymbol(u_b, "D", c("d1", "d2"))
  )

  # Build up a formula clause by clause using &
  n_clauses = sample(3:10, 1)
  clauses = lapply(1:n_clauses, function(j) make_clause(s_b, u_b, 1:3))

  # Method 1: All at once
  f_batch = tryCatch(CnfFormula(clauses), error = function(e) NULL)

  # Method 2: One at a time
  f_incr = tryCatch({
    result = as.CnfFormula(clauses[[1]])
    for (j in 2:length(clauses)) {
      result = result & clauses[[j]]
    }
    result
  }, error = function(e) NULL)

  if (!is.null(f_batch) && !is.null(f_incr)) {
    truth_batch = evaluate_formula(f_batch, u_b)
    truth_incr = evaluate_formula(f_incr, u_b)
    n_tests <<- n_tests + 1
    if (!all(truth_batch == truth_incr)) {
      n_failures <<- n_failures + 1
      idx = which(truth_batch != truth_incr)[1]
      cat(sprintf("FAIL [B trial=%d]: batch vs incremental differ at row %d\n", trial, idx))
    }
  }
}
cat(sprintf("  After B: %d tests, %d failures\n", n_tests, n_failures))

# ============================================================
# Test C: Formula | Formula (distribution) with many clauses
# ============================================================
cat("\n=== Test C: Heavy formula | formula distribution ===\n")

set.seed(444)
for (trial in 1:100) {
  u_c = CnfUniverse()
  s_c = list(
    P = CnfSymbol(u_c, "P", c("p1", "p2", "p3")),
    Q = CnfSymbol(u_c, "Q", c("q1", "q2")),
    R = CnfSymbol(u_c, "R", c("r1", "r2", "r3"))
  )

  f1_clauses = lapply(1:sample(2:4, 1), function(j) make_clause(s_c, u_c, 1:2))
  f2_clauses = lapply(1:sample(2:4, 1), function(j) make_clause(s_c, u_c, 1:2))

  f1 = tryCatch(CnfFormula(f1_clauses), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(f2_clauses), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  f_or = tryCatch(f1 | f2, error = function(e) e)
  if (inherits(f_or, "error")) {
    n_tests <<- n_tests + 1
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR [C trial=%d]: %s\n", trial, f_or$message))
    next
  }

  truth1 = evaluate_formula(f1, u_c)
  truth2 = evaluate_formula(f2, u_c)
  truth_or = evaluate_formula(f_or, u_c)
  n_tests <<- n_tests + 1
  if (!all(truth_or == (truth1 | truth2))) {
    n_failures <<- n_failures + 1
    idx = which(truth_or != (truth1 | truth2))[1]
    cat(sprintf("FAIL [C trial=%d]: f1 | f2 incorrect at row %d\n", trial, idx))
  }
}
cat(sprintf("  After C: %d tests, %d failures\n", n_tests, n_failures))

# ============================================================
# Test D: Build via expression evaluation (like the attic)
# Generate random expression trees and compare truth tables
# ============================================================
cat("\n=== Test D: Expression tree evaluation ===\n")

u_d = CnfUniverse()
s_d = list(
  V = CnfSymbol(u_d, "V", c("m", "n", "o")),
  W = CnfSymbol(u_d, "W", c("p", "q", "r")),
  X = CnfSymbol(u_d, "X", c("s", "t", "u")),
  Y = CnfSymbol(u_d, "Y", c("v", "w", "x")),
  Z = CnfSymbol(u_d, "Z", c("y", "z", "a"))
)

varnames_d = ls(u_d)
domains_d = lapply(varnames_d, function(v) get(v, u_d))
names(domains_d) = varnames_d
assignments_d = expand.grid(domains_d, stringsAsFactors = FALSE)
colnames(assignments_d) = varnames_d

random_atom_expr = function() {
  sym_name = sample(names(s_d), 1)
  domain = u_d[[sym_name]]
  n_vals = sample(1:2, 1)
  vals = sample(domain, n_vals)
  substitute(sym %among% vals, list(sym = as.name(sym_name), vals = vals))
}

random_expr = function(depth) {
  if (depth <= 1) return(random_atom_expr())
  op = sample(c(quote(`&`), quote(`|`)), 1)[[1]]
  left = random_expr(depth - 1)
  right = random_expr(depth - 1)
  substitute(op(left, right), list(op = op, left = left, right = right))
}

eval_bool = function(expr, asgn) {
  env = as.list(asgn)
  env[["%among%"]] = `%in%`
  eval(expr, envir = env, enclos = baseenv())
}

set.seed(12345)
for (depth in c(3, 4, 5, 6, 7, 8)) {
  n_per_depth = if (depth <= 5) 500 else if (depth <= 7) 200 else 100
  for (i in 1:n_per_depth) {
    expr = random_expr(depth)
    result = tryCatch({
      formula = eval(expr)
      simplified_truth = evaluate_formula(as.CnfFormula(formula), u_d)
      original_truth = apply(assignments_d, 1, function(row) eval_bool(expr, row))
      mismatches = which(original_truth != simplified_truth)
      n_tests <<- n_tests + 1
      if (length(mismatches)) {
        n_failures <<- n_failures + 1
        idx = mismatches[1]
        cat(sprintf("FAIL [D depth=%d, i=%d]: mismatch at row %d (orig=%s, simp=%s)\n",
          depth, i, idx, original_truth[idx], simplified_truth[idx]))
        cat(sprintf("  Expr: %s\n", deparse1(expr)))
      }
    }, error = function(e) {
      n_tests <<- n_tests + 1
      n_failures <<- n_failures + 1
      cat(sprintf("ERROR [D depth=%d, i=%d]: %s\n", depth, i, e$message))
    })
  }
  cat(sprintf("  depth %d: %d tests, %d failures\n", depth, n_tests, n_failures))
}

# ============================================================
# Test E: Negation round-trip on complex formulas
# ============================================================
cat("\n=== Test E: Negation round-trip on complex formulas ===\n")

set.seed(88888)
u_e = CnfUniverse()
s_e = list(
  X = CnfSymbol(u_e, "X", c("a", "b", "c")),
  Y = CnfSymbol(u_e, "Y", c("d", "e", "f")),
  Z = CnfSymbol(u_e, "Z", c("g", "h", "i"))
)

for (i in 1:100) {
  n_cl = sample(2:4, 1)
  clauses = lapply(1:n_cl, function(j) make_clause(s_e, u_e, 1:2, 1:2))
  f = tryCatch(CnfFormula(clauses), error = function(e) NULL)
  if (is.null(f) || is.logical(unclass(f))) next

  truth_f = evaluate_formula(f, u_e)

  # Negation
  neg_f = tryCatch(!f, error = function(e) e)
  if (inherits(neg_f, "error")) {
    n_tests <<- n_tests + 1
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR [E neg i=%d]: %s\n", i, neg_f$message))
    next
  }
  truth_neg = evaluate_formula(neg_f, u_e)
  n_tests <<- n_tests + 1
  if (!all(truth_neg == !truth_f)) {
    n_failures <<- n_failures + 1
    cat(sprintf("FAIL [E neg i=%d]: negation incorrect\n", i))
    next
  }

  # Double negation
  dn_f = tryCatch(!neg_f, error = function(e) e)
  if (inherits(dn_f, "error")) {
    n_tests <<- n_tests + 1
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR [E dn i=%d]: %s\n", i, dn_f$message))
    next
  }
  truth_dn = evaluate_formula(dn_f, u_e)
  n_tests <<- n_tests + 1
  if (!all(truth_dn == truth_f)) {
    n_failures <<- n_failures + 1
    cat(sprintf("FAIL [E dn i=%d]: double negation incorrect\n", i))
  }
}
cat(sprintf("  After E: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== GRAND TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
