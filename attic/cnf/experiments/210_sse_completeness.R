#!/usr/bin/env Rscript
# SSE completeness verification:
# After simplification, verify that no basic SSE opportunity remains.
# For each pair of clauses, if one subsumes the other on all but one symbol,
# the remaining symbol should already be restricted.
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

check_no_sse_opportunity = function(formula, universe, label) {
  f_bare = unclass(formula)
  if (is.logical(f_bare)) return(TRUE)

  n = length(f_bare)
  if (n < 2) return(TRUE)

  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) next
      # Check if clause i subsumes clause j on all but one symbol
      clause_i = f_bare[[i]]
      clause_j = f_bare[[j]]

      non_subset_count = 0
      non_subset_symbol = NULL

      for (sym in names(clause_i)) {
        j_range = clause_j[[sym]]
        if (is.null(j_range)) {
          non_subset_count = non_subset_count + 1
          non_subset_symbol = sym
          next
        }
        if (!all(clause_i[[sym]] %in% j_range)) {
          non_subset_count = non_subset_count + 1
          non_subset_symbol = sym
        }
        if (non_subset_count > 1) break
      }

      if (non_subset_count == 1 && !is.null(non_subset_symbol)) {
        # SSE opportunity: clause i subsumes clause j on all but non_subset_symbol
        # After SSE, clause j's range for non_subset_symbol should be intersected with clause i's
        i_range = clause_i[[non_subset_symbol]]
        j_range = clause_j[[non_subset_symbol]]
        if (!is.null(j_range) && !is.null(i_range)) {
          # Check if j's range is already a subset of i's range
          if (!all(j_range %in% i_range)) {
            cat(sprintf("FAIL [%s]: SSE opportunity remains: clause %d vs %d on %s\n",
                label, i, j, non_subset_symbol))
            cat(sprintf("  clause_i[[%s]] = {%s}\n", non_subset_symbol, paste(i_range, collapse=",")))
            cat(sprintf("  clause_j[[%s]] = {%s}\n", non_subset_symbol, paste(j_range, collapse=",")))
            return(FALSE)
          }
        }
      }
    }
  }
  TRUE
}

# === Pattern 1: Random formula SSE completeness ===
cat("=== Random SSE completeness ===\n")
set.seed(210001)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  n_cl = sample(3:8, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 2) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [randsse-%d]: %s\n", trial, result$message)); next
  }
  if (!check_no_sse_opportunity(result, u, sprintf("randsse-%d", trial))) {
    n_failures = n_failures + 1
  }
}
cat(sprintf("  Random SSE completeness: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Designed SSE patterns ===
cat("\n=== Designed SSE patterns ===\n")
set.seed(210002)

for (trial in 1:1000) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Create pairs that should trigger SSE
  a1 = sample(dom, sample(1:3, 1))
  a2 = sample(dom, sample(2:4, 1))
  b1 = sample(dom, sample(1:3, 1))
  c1 = sample(dom, sample(1:3, 1))

  clauses = list(
    as.CnfClause(A %among% a1 | B %among% b1),
    as.CnfClause(A %among% a2 | B %among% b1 | C %among% c1)
  )

  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [dessse-%d]: %s\n", trial, result$message)); next
  }
  if (!check_no_sse_opportunity(result, u, sprintf("dessse-%d", trial))) {
    n_failures = n_failures + 1
  }
}
cat(sprintf("  Designed SSE: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: 4 vars SSE ===
cat("\n=== 4 var SSE ===\n")
set.seed(210003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  D = CnfSymbol(u, "D", dom)
  syms = list(A = A, B = B, C = C, D = D)

  n_cl = sample(4:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(2:4, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:2, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [4vsse-%d]: %s\n", trial, result$message)); next
  }
  if (!check_no_sse_opportunity(result, u, sprintf("4vsse-%d", trial))) {
    n_failures = n_failures + 1
  }
}
cat(sprintf("  4 var SSE: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: SSE completeness after operations ===
cat("\n=== Post-operation SSE ===\n")
set.seed(210004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  make_f = function() {
    n_cl = sample(1:3, 1)
    cls = lapply(1:n_cl, function(j) {
      n_sym = sample(1:3, 1)
      chosen = sample(names(syms), n_sym)
      atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
      as.CnfClause(Reduce(`|`, atoms))
    })
    cls = cls[!sapply(cls, function(x) isTRUE(unclass(x)))]
    if (length(cls) < 1) return(NULL)
    tryCatch(CnfFormula(cls), error = function(e) NULL)
  }

  f1 = make_f(); f2 = make_f()
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1

  # Test &
  result = tryCatch(f1 & f2, error = function(e) NULL)
  if (!is.null(result)) {
    if (!check_no_sse_opportunity(result, u, sprintf("opsse_and-%d", trial))) {
      n_failures = n_failures + 1
    }
  }

  # Test |
  result = tryCatch(f1 | f2, error = function(e) NULL)
  if (!is.null(result)) {
    if (!check_no_sse_opportunity(result, u, sprintf("opsse_or-%d", trial))) {
      n_failures = n_failures + 1
    }
  }

  # Test !
  result = tryCatch(!f1, error = function(e) NULL)
  if (!is.null(result)) {
    if (!check_no_sse_opportunity(result, u, sprintf("opsse_not-%d", trial))) {
      n_failures = n_failures + 1
    }
  }
}
cat(sprintf("  Post-operation SSE: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
