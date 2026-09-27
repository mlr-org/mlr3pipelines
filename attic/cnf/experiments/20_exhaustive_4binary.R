source("experiments/test_harness.R")

# Exhaustive test: 4 binary variables (16 assignments total)
# Enumerate ALL possible non-trivial clauses and test many combinations

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
    cat(sprintf("  Clauses (%d):\n", length(raw_clauses)))
    for (ci in seq_along(raw_clauses)) {
      cl = unclass(raw_clauses[[ci]])
      if (is.logical(cl)) {
        cat(sprintf("    %d: %s\n", ci, cl))
      } else {
        cat(sprintf("    %d: %s\n", ci, paste(names(cl), "=", sapply(cl, paste, collapse = ","), collapse = " | ")))
      }
    }
    return(FALSE)
  }
  TRUE
}

u = CnfUniverse()
A = CnfSymbol(u, "A", c("0", "1"))
B = CnfSymbol(u, "B", c("0", "1"))
C = CnfSymbol(u, "C", c("0", "1"))
D = CnfSymbol(u, "D", c("0", "1"))

syms = list(A = A, B = B, C = C, D = D)
sym_names = c("A", "B", "C", "D")

# Generate all non-trivial clauses
# A clause is a disjunction of atoms. Each atom says "variable X in S" where S is a non-empty proper subset of {0,1} = {"0"} or {"1"}.
# So each atom is X="0" or X="1".
# A clause can mention 1-4 variables, with each being either 0 or 1.
# (Full domain = tautology, so we only use proper subsets)

all_clauses = list()

# 1-variable clauses
for (v in sym_names) {
  for (val in c("0", "1")) {
    all_clauses[[length(all_clauses) + 1]] = as.CnfClause(syms[[v]] %among% val)
  }
}

# 2-variable clauses
for (i in 1:3) {
  for (j in (i+1):4) {
    for (val_i in c("0", "1")) {
      for (val_j in c("0", "1")) {
        cl = as.CnfClause(syms[[sym_names[i]]] %among% val_i | syms[[sym_names[j]]] %among% val_j)
        if (!is.logical(unclass(cl))) {
          all_clauses[[length(all_clauses) + 1]] = cl
        }
      }
    }
  }
}

# 3-variable clauses
for (i in 1:2) {
  for (j in (i+1):3) {
    for (k in (j+1):4) {
      for (val_i in c("0", "1")) {
        for (val_j in c("0", "1")) {
          for (val_k in c("0", "1")) {
            cl = as.CnfClause(
              syms[[sym_names[i]]] %among% val_i |
              syms[[sym_names[j]]] %among% val_j |
              syms[[sym_names[k]]] %among% val_k
            )
            if (!is.logical(unclass(cl))) {
              all_clauses[[length(all_clauses) + 1]] = cl
            }
          }
        }
      }
    }
  }
}

# 4-variable clauses
for (val_a in c("0", "1")) {
  for (val_b in c("0", "1")) {
    for (val_c in c("0", "1")) {
      for (val_d in c("0", "1")) {
        cl = as.CnfClause(A %among% val_a | B %among% val_b | C %among% val_c | D %among% val_d)
        if (!is.logical(unclass(cl))) {
          all_clauses[[length(all_clauses) + 1]] = cl
        }
      }
    }
  }
}

nc = length(all_clauses)
cat(sprintf("Total non-trivial clauses: %d\n", nc))

# Test all pairs
cat("\n=== All 2-clause combinations ===\n")
for (i in 1:nc) {
  for (j in i:nc) {
    cls = list(all_clauses[[i]], all_clauses[[j]])
    f = tryCatch(CnfFormula(cls), error = function(e) e)
    if (inherits(f, "error")) {
      n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
      cat(sprintf("ERROR [2-cl i=%d j=%d]: %s\n", i, j, f$message)); next
    }
    check_fc(f, cls, u, sprintf("2-cl %d,%d", i, j))
  }
  if (i %% 10 == 0) cat(sprintf("  i=%d/%d: %d tests, %d failures\n", i, nc, n_tests, n_failures))
}
cat(sprintf("  2-clause done: %d tests, %d failures\n", n_tests, n_failures))

# Test random triples
cat("\n=== Random 3-clause combinations ===\n")
set.seed(123)
for (trial in 1:10000) {
  indices = sort(sample(nc, 3, replace = TRUE))
  cls = lapply(indices, function(i) all_clauses[[i]])
  f = tryCatch(CnfFormula(cls), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [3-cl trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, cls, u, sprintf("3-cl %d", trial))
}
cat(sprintf("  3-clause done: %d tests, %d failures\n", n_tests, n_failures))

# Test random 4-6 clause combinations
cat("\n=== Random 4-6 clause combinations ===\n")
for (trial in 1:5000) {
  n_cl = sample(4:6, 1)
  indices = sort(sample(nc, n_cl, replace = TRUE))
  cls = lapply(indices, function(i) all_clauses[[i]])
  f = tryCatch(CnfFormula(cls), error = function(e) e)
  if (inherits(f, "error")) {
    n_tests <<- n_tests + 1; n_failures <<- n_failures + 1
    cat(sprintf("ERROR [4-6cl trial %d]: %s\n", trial, f$message)); next
  }
  check_fc(f, cls, u, sprintf("4-6cl %d", trial))
}
cat(sprintf("  4-6clause done: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
