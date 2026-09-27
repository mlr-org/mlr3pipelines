source("experiments/test_harness.R")

# Specifically target 2nd-order self-subsumption elimination.
# The idea: create formulas that REQUIRE two clauses to "resolve" on a pivot variable
# to derive information about a target variable.

n_failures = 0
n_tests = 0

# Helper: check formula semantics against pre-simplification clauses
check_formula_against_clauses = function(formula, raw_clauses, universe, label) {
  n_tests <<- n_tests + 1
  varnames = ls(universe)
  domains = lapply(varnames, function(v) get(v, universe))
  names(domains) = varnames
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  colnames(assignments) = varnames

  # Evaluate raw clauses
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

  # Evaluate simplified formula
  simplified_truth = evaluate_formula(formula, universe)

  mismatches = which(raw_truth != simplified_truth)
  if (length(mismatches)) {
    n_failures <<- n_failures + 1
    idx = mismatches[1]
    cat(sprintf("FAIL [%s]: mismatch at row %d (raw=%s, simplified=%s)\n",
      label, idx, raw_truth[idx], simplified_truth[idx]))
    cat(sprintf("  Assignment: %s\n",
      paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
    cat(sprintf("  Formula: %s\n", format(formula)))
    cat(sprintf("  Raw clauses: %s\n", paste(sapply(raw_clauses, format), collapse = " & ")))
    return(FALSE)
  }
  TRUE
}

cat("=== 2nd-order SSE: Systematically crafted cases ===\n")

# Case 1: Classic resolution pattern
# (pivot=a | target=d) & (pivot=b | target=e) & (target in {d,e})
# The third clause is implied by the first two (when domain of pivot is {a,b})
for (trial in 1:20) {
  u1 = CnfUniverse()
  pivot_dom = paste0("pv", 1:sample(2:4, 1))
  target_dom = paste0("tv", 1:sample(2:4, 1))
  extra_dom = paste0("ev", 1:sample(2:3, 1))

  P = CnfSymbol(u1, "P", pivot_dom)
  T_sym = CnfSymbol(u1, "T", target_dom)
  E = CnfSymbol(u1, "E", extra_dom)

  # Split pivot domain
  n_split = sample(1:(length(pivot_dom) - 1), 1)
  p1 = pivot_dom[1:n_split]
  p2 = pivot_dom[(n_split + 1):length(pivot_dom)]

  # Random target values
  tv1 = sample(target_dom, sample(1:(length(target_dom) - 1), 1))
  tv2 = sample(target_dom, sample(1:(length(target_dom) - 1), 1))

  # Build clauses
  cl1 = as.CnfClause(P %among% p1 | T_sym %among% tv1)
  cl2 = as.CnfClause(P %among% p2 | T_sym %among% tv2)
  # Clause 3: target in union (maybe with extras) -- this is the one that might be subsumable
  tv3 = unique(c(tv1, tv2))
  if (length(tv3) < length(target_dom) && runif(1) < 0.5) {
    tv3 = unique(c(tv3, sample(setdiff(target_dom, tv3), 1)))
  }
  cl3 = as.CnfClause(T_sym %among% tv3)

  # Maybe add extra clause for noise
  extra_clauses = if (runif(1) < 0.5) {
    list(as.CnfClause(E %among% sample(extra_dom, sample(1:length(extra_dom), 1))))
  } else list()

  all_clauses = c(list(cl1, cl2, cl3), extra_clauses)
  f = tryCatch(CnfFormula(all_clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [2nd-order case1 trial %d]: %s\n", trial, f$message))
    next
  }
  check_formula_against_clauses(f, all_clauses, u1, sprintf("2nd-order case1 trial %d", trial))
}

# Case 2: Three-way resolution
# Three clauses each covering 1/3 of pivot domain, each with different target values
cat("\n=== 2nd-order SSE: Three-way resolution ===\n")

for (trial in 1:20) {
  u2 = CnfUniverse()
  P2 = CnfSymbol(u2, "P", c("p1", "p2", "p3"))
  T2 = CnfSymbol(u2, "T", c("t1", "t2", "t3"))
  R2 = CnfSymbol(u2, "R", c("r1", "r2", "r3"))

  # Each clause covers one pivot value
  tvs = lapply(1:3, function(i) sample(c("t1", "t2", "t3"), sample(1:2, 1)))
  cl1 = as.CnfClause(P2 %among% "p1" | T2 %among% tvs[[1]])
  cl2 = as.CnfClause(P2 %among% "p2" | T2 %among% tvs[[2]])
  cl3 = as.CnfClause(P2 %among% "p3" | T2 %among% tvs[[3]])

  # Target clause
  tv_union = unique(unlist(tvs))
  cl4 = as.CnfClause(T2 %among% tv_union)

  # Maybe add noise
  noise = if (runif(1) < 0.3) {
    list(as.CnfClause(R2 %among% sample(c("r1", "r2", "r3"), sample(1:2, 1))))
  } else list()

  all_clauses = c(list(cl1, cl2, cl3, cl4), noise)
  f = tryCatch(CnfFormula(all_clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [3-way trial %d]: %s\n", trial, f$message))
    next
  }
  check_formula_against_clauses(f, all_clauses, u2, sprintf("3-way trial %d", trial))
}

# Case 3: Cascading 2nd-order SSE
# Resolution on one variable restricts another, which enables further resolution
cat("\n=== 2nd-order SSE: Cascading resolution ===\n")

for (trial in 1:30) {
  u3 = CnfUniverse()
  A3 = CnfSymbol(u3, "A", c("a1", "a2"))
  B3 = CnfSymbol(u3, "B", c("b1", "b2"))
  C3 = CnfSymbol(u3, "C", c("c1", "c2"))
  D3 = CnfSymbol(u3, "D", c("d1", "d2"))

  # Many interacting clauses
  clauses = list(
    as.CnfClause(A3 %among% "a1" | B3 %among% sample(c("b1", "b2"), 1)),
    as.CnfClause(A3 %among% "a2" | B3 %among% sample(c("b1", "b2"), 1)),
    as.CnfClause(B3 %among% "b1" | C3 %among% sample(c("c1", "c2"), 1)),
    as.CnfClause(B3 %among% "b2" | C3 %among% sample(c("c1", "c2"), 1)),
    as.CnfClause(C3 %among% "c1" | D3 %among% sample(c("d1", "d2"), 1)),
    as.CnfClause(C3 %among% "c2" | D3 %among% sample(c("d1", "d2"), 1))
  )

  # Add some target clauses
  extra = lapply(1:sample(1:3, 1), function(i) {
    syms = list(A3, B3, C3, D3)
    sym_names = c("A", "B", "C", "D")
    chosen = sample(1:4, sample(1:2, 1))
    atoms = lapply(chosen, function(j) {
      dom = u3[[sym_names[j]]]
      syms[[j]] %among% sample(dom, sample(1:length(dom), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  all_clauses = c(clauses, extra)
  f = tryCatch(CnfFormula(all_clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [cascade trial %d]: %s\n", trial, f$message))
    next
  }
  check_formula_against_clauses(f, all_clauses, u3, sprintf("cascade trial %d", trial))
}

# Case 4: Many clauses, all with the same symbols
# This maximizes self-subsumption and resolution opportunities
cat("\n=== 2nd-order SSE: Dense formulas (same symbols) ===\n")

set.seed(789)
for (trial in 1:50) {
  u4 = CnfUniverse()
  X4 = CnfSymbol(u4, "X", c("x1", "x2", "x3"))
  Y4 = CnfSymbol(u4, "Y", c("y1", "y2", "y3"))

  n_clauses = sample(3:8, 1)
  clauses = lapply(1:n_clauses, function(i) {
    xv = sample(c("x1", "x2", "x3"), sample(1:2, 1))
    yv = sample(c("y1", "y2", "y3"), sample(1:2, 1))
    as.CnfClause(X4 %among% xv | Y4 %among% yv)
  })

  f = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [dense trial %d]: %s\n", trial, f$message))
    next
  }
  check_formula_against_clauses(f, clauses, u4, sprintf("dense trial %d", trial))
}

# Case 5: HLA + 2nd order interaction
# Build patterns where HLA reveals tautologies or subsumption
cat("\n=== 2nd-order SSE: HLA interaction ===\n")

for (trial in 1:50) {
  u5 = CnfUniverse()
  A5 = CnfSymbol(u5, "A", c("a1", "a2", "a3"))
  B5 = CnfSymbol(u5, "B", c("b1", "b2", "b3"))
  C5 = CnfSymbol(u5, "C", c("c1", "c2", "c3"))

  # Target clause: many symbols
  target = as.CnfClause(
    A5 %among% sample(c("a1", "a2", "a3"), sample(1:2, 1)) |
    B5 %among% sample(c("b1", "b2", "b3"), sample(1:2, 1)) |
    C5 %among% sample(c("c1", "c2", "c3"), sample(1:2, 1))
  )

  # Helper clauses designed for HLA: almost subsets of the target
  helpers = lapply(1:sample(3:6, 1), function(i) {
    syms = list(A = A5, B = B5, C = C5)
    sym_names = c("A", "B", "C")
    chosen = sample(sym_names, sample(1:2, 1))
    atoms = lapply(chosen, function(s) {
      dom = u5[[s]]
      syms[[s]] %among% sample(dom, sample(1:2, 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  all_clauses = c(list(target), helpers)
  f = tryCatch(CnfFormula(all_clauses), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [HLA interaction trial %d]: %s\n", trial, f$message))
    next
  }
  check_formula_against_clauses(f, all_clauses, u5, sprintf("HLA interaction trial %d", trial))
}

# Case 6: Exhaustive 2-variable small-domain patterns
# Enumerate ALL possible 2-clause formulas over 2 binary variables
cat("\n=== Exhaustive: 2 binary vars, 2-4 clauses ===\n")

u6 = CnfUniverse()
A6 = CnfSymbol(u6, "A", c("0", "1"))
B6 = CnfSymbol(u6, "B", c("0", "1"))

# All possible non-trivial clauses over A, B:
# Each clause can have A in {0}, {1}, {0,1} and B in {0}, {1}, {0,1}
# (excluding full tautology and full contradiction)
all_ranges_A = list(c("0"), c("1"), c("0", "1"))
all_ranges_B = list(c("0"), c("1"), c("0", "1"))

all_clauses_possible = list()
for (ra in all_ranges_A) {
  for (rb in all_ranges_B) {
    # A in ra | B in rb
    cl = as.CnfClause(A6 %among% ra | B6 %among% rb)
    if (!is.logical(unclass(cl))) {
      all_clauses_possible[[length(all_clauses_possible) + 1]] = cl
    }
  }
}
# Also add unit clauses
all_clauses_possible[[length(all_clauses_possible) + 1]] = as.CnfClause(A6 %among% "0")
all_clauses_possible[[length(all_clauses_possible) + 1]] = as.CnfClause(A6 %among% "1")
all_clauses_possible[[length(all_clauses_possible) + 1]] = as.CnfClause(B6 %among% "0")
all_clauses_possible[[length(all_clauses_possible) + 1]] = as.CnfClause(B6 %among% "1")

n_total = length(all_clauses_possible)
cat(sprintf("  %d possible clauses\n", n_total))

# Test all combinations of 2 clauses
for (i in 1:n_total) {
  for (j in i:n_total) {
    cls = list(all_clauses_possible[[i]], all_clauses_possible[[j]])
    f = tryCatch(CnfFormula(cls), error = function(e) e)
    if (inherits(f, "error")) {
      n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
      cat(sprintf("ERROR [exhaustive 2-clause i=%d j=%d]: %s\n", i, j, f$message))
      next
    }
    check_formula_against_clauses(f, cls, u6, sprintf("exhaustive i=%d j=%d", i, j))
  }
}
cat(sprintf("  After 2-clause: %d tests, %d failures\n", n_tests, n_failures))

# Test all combinations of 3 clauses
for (i in 1:n_total) {
  for (j in i:n_total) {
    for (k in j:n_total) {
      cls = list(all_clauses_possible[[i]], all_clauses_possible[[j]], all_clauses_possible[[k]])
      f = tryCatch(CnfFormula(cls), error = function(e) e)
      if (inherits(f, "error")) {
        n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
        next
      }
      check_formula_against_clauses(f, cls, u6, sprintf("exhaustive3 i=%d j=%d k=%d", i, j, k))
    }
  }
}
cat(sprintf("  After 3-clause: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
