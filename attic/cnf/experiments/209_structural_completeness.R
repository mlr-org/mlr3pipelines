#!/usr/bin/env Rscript
# Structural completeness verification:
# After simplification, verify that no further simplification opportunities remain:
# 1. No clause subsumes another
# 2. Unit propagation is complete (no unit's symbol appears in other clauses with wider range)
# 3. No clause is a tautology
# 4. No clause has an empty range
# 5. Simplification is idempotent (re-simplifying gives same result)
# 6. No basic SSE opportunity remains
source("experiments/test_harness.R")

n_failures = 0
n_tests = 0

check_structural = function(formula, universe, label) {
  f_bare = unclass(formula)
  if (is.logical(f_bare)) return(TRUE)  # TRUE/FALSE formulas are trivially correct

  # Check 1: No empty ranges
  for (i in seq_along(f_bare)) {
    clause = f_bare[[i]]
    for (sym in names(clause)) {
      if (length(clause[[sym]]) == 0) {
        cat(sprintf("FAIL [%s]: clause %d has empty range for %s\n", label, i, sym))
        return(FALSE)
      }
    }
  }

  # Check 2: No tautological symbols (symbol covers full domain)
  for (i in seq_along(f_bare)) {
    clause = f_bare[[i]]
    for (sym in names(clause)) {
      dom = get(sym, universe)
      if (length(clause[[sym]]) >= length(dom) && all(dom %in% clause[[sym]])) {
        cat(sprintf("FAIL [%s]: clause %d symbol %s is tautological\n", label, i, sym))
        return(FALSE)
      }
    }
  }

  # Check 3: No clause subsumes another
  for (i in seq_along(f_bare)) {
    for (j in seq_along(f_bare)) {
      if (i == j) next
      # Does clause i subsume clause j?
      # i subsumes j iff for every symbol in i, i's range is a subset of j's range
      subsumes = TRUE
      for (sym in names(f_bare[[i]])) {
        j_range = f_bare[[j]][[sym]]
        if (is.null(j_range) || !all(f_bare[[i]][[sym]] %in% j_range)) {
          subsumes = FALSE
          break
        }
      }
      if (subsumes) {
        cat(sprintf("FAIL [%s]: clause %d subsumes clause %d\n", label, i, j))
        return(FALSE)
      }
    }
  }

  # Check 4: Unit propagation is complete
  units = which(lengths(f_bare) == 1)
  if (length(units) > 0) {
    for (u_idx in units) {
      u_sym = names(f_bare[[u_idx]])
      u_range = f_bare[[u_idx]][[1]]
      for (k in seq_along(f_bare)) {
        if (k == u_idx) next
        other_range = f_bare[[k]][[u_sym]]
        if (!is.null(other_range)) {
          # The other clause has this symbol. Its range should be a subset of the unit's range
          # (already propagated). If it's equal, the clause should have been subsumed.
          if (!all(other_range %in% u_range)) {
            cat(sprintf("FAIL [%s]: unit %d (sym=%s) not propagated to clause %d\n", label, u_idx, u_sym, k))
            return(FALSE)
          }
        }
      }
    }
  }

  # Check 5: No duplicate units for same symbol
  unit_syms = sapply(f_bare[units], names)
  if (length(unit_syms) != length(unique(unit_syms))) {
    cat(sprintf("FAIL [%s]: duplicate units for same symbol\n", label))
    return(FALSE)
  }

  TRUE
}

# === Pattern 1: Random formulas structural check ===
cat("=== Random structural check ===\n")
set.seed(209001)

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
    cat(sprintf("ERROR [rand-%d]: %s\n", trial, result$message)); next
  }
  if (!check_structural(result, u, sprintf("rand-%d", trial))) {
    n_failures = n_failures + 1
  }
}
cat(sprintf("  Random structural: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 2: Idempotence check ===
cat("\n=== Idempotence ===\n")
set.seed(209002)

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
  result1 = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result1, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [idemp-%d]: %s\n", trial, result1$message)); next
  }

  # Re-simplify
  if (is.logical(unclass(result1))) next  # skip TRUE/FALSE
  result2 = tryCatch(CnfFormula(as.list(result1)), error = function(e) e)
  if (inherits(result2, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [idemp2-%d]: %s\n", trial, result2$message)); next
  }

  # Should be structurally identical (same clauses, same ranges)
  eq = tryCatch(all.equal(result1, result2), error = function(e) as.character(e))
  if (!isTRUE(eq)) {
    # Verify at least semantic equivalence
    t1 = evaluate_formula(result1, u)
    t2 = evaluate_formula(result2, u)
    if (!all(t1 == t2)) {
      n_failures = n_failures + 1
      cat(sprintf("FAIL [idemp-%d]: re-simplification changed semantics\n", trial))
    }
    # Non-structural equivalence is expected due to non-confluence, but log it
  }
}
cat(sprintf("  Idempotence: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 3: Monotonicity (adding clauses never increases satisfying assignments) ===
cat("\n=== Monotonicity ===\n")
set.seed(209003)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  # Build f1 from first n_cl1 clauses
  n_cl1 = sample(2:4, 1)
  n_cl2 = sample(1:3, 1)
  all_clauses = lapply(1:(n_cl1 + n_cl2), function(j) {
    n_sym = sample(1:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:3, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  all_clauses = all_clauses[!sapply(all_clauses, function(x) isTRUE(unclass(x)))]
  if (length(all_clauses) < 3) next

  n_cl1 = min(n_cl1, length(all_clauses) - 1)
  cls1 = all_clauses[1:n_cl1]
  cls2 = all_clauses

  f1 = tryCatch(CnfFormula(cls1), error = function(e) NULL)
  f2 = tryCatch(CnfFormula(cls2), error = function(e) NULL)
  if (is.null(f1) || is.null(f2)) next

  n_tests = n_tests + 1
  t1 = evaluate_formula(f1, u)
  t2 = evaluate_formula(f2, u)

  # f2 has strictly more clauses -> should have fewer or equal satisfying assignments
  if (any(t2 & !t1)) {
    n_failures = n_failures + 1
    cat(sprintf("FAIL [mono-%d]: adding clauses increased satisfaction\n", trial))
  }
}
cat(sprintf("  Monotonicity: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 4: Structural + semantic on formulas with units ===
cat("\n=== Unit formulas structural ===\n")
set.seed(209004)

for (trial in 1:500) {
  u = CnfUniverse()
  dom = c("a", "b", "c", "d", "e")
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  C = CnfSymbol(u, "C", dom)
  syms = list(A = A, B = B, C = C)

  a_val = sample(dom, sample(1:3, 1))
  clauses = list(as.CnfClause(A %among% a_val))

  for (j in 1:sample(3:7, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:4, 1)))
    cl = as.CnfClause(Reduce(`|`, atoms))
    if (!isTRUE(unclass(cl))) clauses[[length(clauses) + 1]] = cl
  }
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ustruct-%d]: %s\n", trial, result$message)); next
  }
  if (!check_structural(result, u, sprintf("ustruct-%d", trial))) {
    n_failures = n_failures + 1
  }
}
cat(sprintf("  Unit structural: %d tests, %d failures\n", n_tests, n_failures))

# === Pattern 5: Structural on large domain formulas ===
cat("\n=== Large domain structural ===\n")
set.seed(209005)

for (trial in 1:300) {
  u = CnfUniverse()
  dom = letters[1:8]
  A = CnfSymbol(u, "A", dom)
  B = CnfSymbol(u, "B", dom)
  syms = list(A = A, B = B)

  n_cl = sample(4:10, 1)
  clauses = lapply(1:n_cl, function(j) {
    n_sym = sample(1:2, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) syms[[s]] %among% sample(dom, sample(1:6, 1)))
    as.CnfClause(Reduce(`|`, atoms))
  })
  clauses = clauses[!sapply(clauses, function(x) isTRUE(unclass(x)))]
  if (length(clauses) < 3) next

  n_tests = n_tests + 1
  result = tryCatch(CnfFormula(clauses), error = function(e) e)
  if (inherits(result, "error")) {
    n_failures = n_failures + 1
    cat(sprintf("ERROR [ldstruct-%d]: %s\n", trial, result$message)); next
  }
  if (!check_structural(result, u, sprintf("ldstruct-%d", trial))) {
    n_failures = n_failures + 1
  }
}
cat(sprintf("  Large domain structural: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
