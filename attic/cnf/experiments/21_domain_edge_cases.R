source("experiments/test_harness.R")

# Test edge cases with symbol domains:
# 1. Duplicate domain values (missing validation in CnfSymbol)
# 2. Single-value domains
# 3. Large domains
# 4. Mixed operations with TRUE/FALSE from logical conversions

n_failures = 0
n_tests = 0

check_fc = function(formula, raw_clauses, universe, label) {
  n_tests <<- n_tests + 1
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)

  raw_truth = rep(TRUE, nrow(assignments))
  for (cl in raw_clauses) {
    cl_bare = unclass(cl)
    if (isTRUE(cl_bare)) next
    if (isFALSE(cl_bare)) { raw_truth = rep(FALSE, nrow(assignments)); break }
    cl_truth = rep(FALSE, nrow(assignments))
    for (sym in names(cl_bare)) {
      cl_truth = cl_truth | (assignments[[sym]] %in% cl_bare[[sym]])
    }
    raw_truth = raw_truth & cl_truth
  }

  simplified_truth = evaluate_formula(formula, universe)
  mismatches = which(raw_truth != simplified_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: row %d (raw=%s, simp=%s)\n",
      label, idx, raw_truth[idx], simplified_truth[idx]))
    cat(sprintf("  Assignment: %s\n",
      paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
    return(FALSE)
  }
  TRUE
}

cat("=== Bug #3 candidate: Duplicate domain values ===\n")
# CnfSymbol allows duplicate domain values.
# This can cause simplification bugs because length(domain) != length(unique(domain))

# Test: create universe with duplicate domain
u_dup = CnfUniverse()
# This should arguably fail, but doesn't
X_dup = tryCatch(CnfSymbol(u_dup, "X", c("a", "a", "b")), error = function(e) e)
if (inherits(X_dup, "error")) {
  cat(sprintf("  CnfSymbol with dups correctly errors: %s\n", X_dup$message))
} else {
  cat(sprintf("  CnfSymbol with dups ALLOWED (domain stored: %s)\n", paste(u_dup[["X"]], collapse = ",")))
  # Check if this causes simplification issues
  Y_dup = CnfSymbol(u_dup, "Y", c("c", "d"))

  # X %among% c("a", "b") should be TRUE (covers all unique values)
  atom_full = X_dup %among% c("a", "b")
  n_tests <<- n_tests + 1
  if (!isTRUE(as.logical(atom_full))) {
    cat("  NOTE: X %among% c('a','b') with dup domain is NOT detected as TRUE atom\n")
    # Actually, CnfAtom constructor checks all(domain %in% values), which works fine
    # because domain c("a","a","b") %in% c("a","b") = c(T,T,T)
  } else {
    cat("  X %among% c('a','b') with dup domain correctly detected as TRUE atom\n")
  }

  # The real issue is in simplify_cnf where length comparisons are used
  # Create a formula that requires simplification
  cl1 = as.CnfClause(X_dup %among% "a" | Y_dup %among% "c")
  cl2 = as.CnfClause(X_dup %among% "b" | Y_dup %among% "c")
  # Together: Y must be "c" (since X must be "a" or "b", which covers the whole domain)
  # The simplifier should recognize Y %among% "c" as the simplified form
  f_dup = tryCatch(CnfFormula(list(cl1, cl2)), error = function(e) e)
  if (inherits(f_dup, "error")) {
    cat(sprintf("  ERROR creating formula with dup domain: %s\n", f_dup$message))
  } else {
    # Check if simplification is correct
    truth = evaluate_formula(f_dup, u_dup)
    # Manually compute expected truth
    varnames = ls(u_dup)
    domains = lapply(varnames, function(v) get(v, u_dup))
    names(domains) = varnames
    assignments = expand.grid(domains, stringsAsFactors = FALSE)
    raw_truth = (assignments[["X"]] %in% "a" | assignments[["Y"]] %in% "c") &
      (assignments[["X"]] %in% "b" | assignments[["Y"]] %in% "c")
    n_tests <<- n_tests + 1
    mismatches = which(raw_truth != truth)
    if (length(mismatches)) {
      n_failures <<- n_failures + 1
      cat(sprintf("  FAIL: Dup domain causes simplification bug! Row %d\n", mismatches[1]))
    } else {
      cat("  Formula with dup domain simplifies correctly (no semantic bug)\n")
    }
  }
}

cat("\n=== Single-value domains ===\n")
# Variables with domain size 1 are always determined
u1 = CnfUniverse()
A1 = CnfSymbol(u1, "A", "only_val")
B1 = CnfSymbol(u1, "B", c("b1", "b2"))

# A %among% "only_val" should be TRUE
n_tests <<- n_tests + 1
if (!isTRUE(as.logical(A1 %among% "only_val"))) {
  n_failures <<- n_failures + 1
  cat("  FAIL: A %among% 'only_val' should be TRUE\n")
}

# A %among% character(0) should be FALSE
n_tests <<- n_tests + 1
if (!isFALSE(as.logical(A1 %among% character(0)))) {
  n_failures <<- n_failures + 1
  cat("  FAIL: A %among% character(0) should be FALSE\n")
}

# Formula with single-value variable
clauses1 = list(
  as.CnfClause(A1 %among% "only_val" | B1 %among% "b1"),
  as.CnfClause(B1 %among% "b2")
)
f1 = tryCatch(CnfFormula(clauses1), error = function(e) e)
if (inherits(f1, "error")) {
  n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
  cat(sprintf("  ERROR: %s\n", f1$message))
} else {
  check_fc(f1, clauses1, u1, "single-val-domain")
}
cat(sprintf("  Single-val done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Large domains ===\n")
# Variables with many values
set.seed(456)
for (trial in 1:100) {
  u_large = CnfUniverse()
  dom_sizes = sample(2:8, 3, replace = TRUE)
  syms = list()
  for (v in 1:3) {
    vname = paste0("V", v)
    dom = paste0(vname, "_", 1:dom_sizes[v])
    syms[[vname]] = CnfSymbol(u_large, vname, dom)
  }

  n_clauses = sample(2:6, 1)
  clauses = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u_large[[s]]
      n_vals = sample(1:max(1, length(dom)-1), 1)
      syms[[s]] %among% sample(dom, n_vals)
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [large-dom trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, clauses, u_large, sprintf("large-dom-%d", trial))
}
cat(sprintf("  Large domains done: %d tests, %d failures\n", n_tests, n_failures))

cat("\n=== Mixed with logical TRUE/FALSE ===\n")
u2 = CnfUniverse()
X2 = CnfSymbol(u2, "X", c("a", "b", "c"))
Y2 = CnfSymbol(u2, "Y", c("d", "e"))

# Test: CnfFormula constructor with TRUE and FALSE elements
f_mixed1 = tryCatch(
  CnfFormula(list(as.CnfClause(TRUE), as.CnfClause(X2 %among% "a"), as.CnfClause(TRUE))),
  error = function(e) e
)
if (!inherits(f_mixed1, "error")) {
  n_tests <<- n_tests + 1
  truth = evaluate_formula(f_mixed1, u2)
  varnames = ls(u2)
  domains = lapply(varnames, function(v) get(v, u2))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  expected = assignments[["X"]] %in% "a"
  if (!all(truth == expected)) {
    n_failures <<- n_failures + 1
    cat("  FAIL: Formula with TRUE clauses mixed in\n")
  }
}

# Test: CnfFormula with one FALSE clause should be FALSE
f_mixed2 = tryCatch(
  CnfFormula(list(as.CnfClause(X2 %among% "a"), as.CnfClause(FALSE), as.CnfClause(Y2 %among% "d"))),
  error = function(e) e
)
if (!inherits(f_mixed2, "error")) {
  n_tests <<- n_tests + 1
  if (!isFALSE(as.logical(f_mixed2))) {
    n_failures <<- n_failures + 1
    cat("  FAIL: Formula with FALSE clause should be FALSE\n")
  }
}

# Test: Nesting formulas in CnfFormula constructor
inner = (X2 %among% "a" | Y2 %among% "d") & X2 %among% c("a", "b")
f_nested = tryCatch(
  CnfFormula(list(inner, as.CnfClause(Y2 %among% c("d", "e")))),
  error = function(e) e
)
if (!inherits(f_nested, "error")) {
  expected_f = inner & as.CnfFormula(as.CnfClause(Y2 %among% c("d", "e")))
  n_tests <<- n_tests + 1
  tn = evaluate_formula(f_nested, u2)
  te = evaluate_formula(expected_f, u2)
  if (!all(tn == te)) {
    n_failures <<- n_failures + 1
    cat("  FAIL: Nested formula in constructor\n")
  }
}

cat(sprintf("  Mixed done: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
