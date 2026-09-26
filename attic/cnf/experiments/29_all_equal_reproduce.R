#!/usr/bin/env Rscript
# Reproduce and investigate the all.equal failure from experiment 26
source("experiments/test_harness.R")

set.seed(11235)

u = CnfUniverse()
A = CnfSymbol(u, "A", c("a1", "a2", "a3"))
B = CnfSymbol(u, "B", c("b1", "b2"))
C = CnfSymbol(u, "C", c("c1", "c2", "c3"))
syms = list(A = A, B = B, C = C)

# Skip to trial 175
for (trial in 1:200) {
  # Build random formula
  n_clauses = sample(2:5, 1)
  clause_list = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u[[s]]
      syms[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f_a = tryCatch(CnfFormula(clause_list), error = function(e) NULL)
  if (is.null(f_a)) next

  # Rebuild with shuffled clause order
  shuffled = sample(clause_list)
  f_b = tryCatch(CnfFormula(shuffled), error = function(e) NULL)
  if (is.null(f_b)) next

  result = all.equal(f_a, f_b)
  if (!isTRUE(result)) {
    cat(sprintf("=== FOUND FAILURE at trial %d ===\n", trial))
    cat(sprintf("Clause count: %d\n", n_clauses))
    cat("\n--- Original clauses ---\n")
    for (i in seq_along(clause_list)) {
      cl = unclass(clause_list[[i]])
      cat(sprintf("  Clause %d: ", i))
      for (s in names(cl)) cat(sprintf("%s in {%s} ", s, paste(cl[[s]], collapse = ",")))
      cat("\n")
    }

    cat("\n--- Shuffled order ---\n")
    for (i in seq_along(shuffled)) {
      cl = unclass(shuffled[[i]])
      cat(sprintf("  Clause %d: ", i))
      for (s in names(cl)) cat(sprintf("%s in {%s} ", s, paste(cl[[s]], collapse = ",")))
      cat("\n")
    }

    cat("\n--- Simplified f_a ---\n")
    f_a_bare = unclass(f_a)
    if (is.logical(f_a_bare)) {
      cat(sprintf("  Logical: %s\n", f_a_bare))
    } else {
      for (i in seq_along(f_a_bare)) {
        cl = f_a_bare[[i]]
        cat(sprintf("  Clause %d: ", i))
        for (s in names(cl)) cat(sprintf("%s in {%s} ", s, paste(cl[[s]], collapse = ",")))
        cat("\n")
      }
    }

    cat("\n--- Simplified f_b ---\n")
    f_b_bare = unclass(f_b)
    if (is.logical(f_b_bare)) {
      cat(sprintf("  Logical: %s\n", f_b_bare))
    } else {
      for (i in seq_along(f_b_bare)) {
        cl = f_b_bare[[i]]
        cat(sprintf("  Clause %d: ", i))
        for (s in names(cl)) cat(sprintf("%s in {%s} ", s, paste(cl[[s]], collapse = ",")))
        cat("\n")
      }
    }

    cat(sprintf("\n--- all.equal message: %s ---\n", result))

    # Check semantic equivalence
    t_a = evaluate_formula(f_a, u)
    t_b = evaluate_formula(f_b, u)
    if (all(t_a == t_b)) {
      cat("\nSemantically EQUIVALENT! This is a simplifier order-dependence issue.\n")
    } else {
      cat("\nSemantically DIFFERENT! This is a simplifier BUG!\n")
      mismatches = which(t_a != t_b)
      varnames = ls(u)
      domains = lapply(varnames, function(v) get(v, u))
      names(domains) = varnames
      assignments = expand.grid(domains, stringsAsFactors = FALSE)
      for (idx in mismatches[1:min(3, length(mismatches))]) {
        cat(sprintf("  Row %d: f_a=%s, f_b=%s | %s\n",
          idx, t_a[idx], t_b[idx],
          paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
      }
    }
  }
}

cat("\n=== Now systematically find ALL order-dependent formulas ===\n")
n_order_deps = 0
n_semantic_bugs = 0
n_total = 0

set.seed(54321)
for (trial in 1:2000) {
  u2 = CnfUniverse()
  A2 = CnfSymbol(u2, "A", c("a1", "a2", "a3"))
  B2 = CnfSymbol(u2, "B", c("b1", "b2"))
  C2 = CnfSymbol(u2, "C", c("c1", "c2", "c3"))
  syms2 = list(A = A2, B = B2, C = C2)

  n_clauses = sample(2:6, 1)
  clause_list = lapply(1:n_clauses, function(j) {
    n_atoms = sample(1:3, 1)
    chosen = sample(names(syms2), n_atoms)
    atoms = lapply(chosen, function(s) {
      dom = u2[[s]]
      syms2[[s]] %among% sample(dom, sample(1:max(1, length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  f_a = tryCatch(CnfFormula(clause_list), error = function(e) NULL)
  if (is.null(f_a)) next

  # Try multiple shuffles
  for (shuffle in 1:3) {
    shuffled = sample(clause_list)
    f_b = tryCatch(CnfFormula(shuffled), error = function(e) NULL)
    if (is.null(f_b)) next

    n_total = n_total + 1
    result = all.equal(f_a, f_b)
    if (!isTRUE(result)) {
      # Check if semantic or structural
      t_a = evaluate_formula(f_a, u2)
      t_b = evaluate_formula(f_b, u2)
      if (all(t_a == t_b)) {
        n_order_deps = n_order_deps + 1
        if (n_order_deps <= 5) {
          cat(sprintf("ORDER-DEP [trial %d, shuffle %d]: structurally different but semantically equal\n", trial, shuffle))
          cat(sprintf("  all.equal says: %s\n", result))
          # Print the two simplified forms
          f_a_bare = unclass(f_a)
          f_b_bare = unclass(f_b)
          cat("  f_a:")
          if (is.logical(f_a_bare)) { cat(sprintf(" %s\n", f_a_bare)) } else {
            cat("\n")
            for (i in seq_along(f_a_bare)) {
              cl = f_a_bare[[i]]; cat("    ")
              for (s in names(cl)) cat(sprintf("%s in {%s} ", s, paste(cl[[s]], collapse=",")))
              cat("\n")
            }
          }
          cat("  f_b:")
          if (is.logical(f_b_bare)) { cat(sprintf(" %s\n", f_b_bare)) } else {
            cat("\n")
            for (i in seq_along(f_b_bare)) {
              cl = f_b_bare[[i]]; cat("    ")
              for (s in names(cl)) cat(sprintf("%s in {%s} ", s, paste(cl[[s]], collapse=",")))
              cat("\n")
            }
          }
        }
      } else {
        n_semantic_bugs = n_semantic_bugs + 1
        cat(sprintf("SEMANTIC BUG [trial %d, shuffle %d]!\n", trial, shuffle))
        mismatches = which(t_a != t_b)
        varnames = ls(u2)
        domains = lapply(varnames, function(v) get(v, u2))
        names(domains) = varnames
        assignments = expand.grid(domains, stringsAsFactors = FALSE)
        for (idx in mismatches[1:min(3, length(mismatches))]) {
          cat(sprintf("  Row %d: f_a=%s, f_b=%s | %s\n",
            idx, t_a[idx], t_b[idx],
            paste(colnames(assignments), "=", assignments[idx, ], collapse = ", ")))
        }
      }
    }
  }
}
cat(sprintf("\n=== TOTAL: %d tests, %d order-dependent, %d semantic bugs ===\n",
  n_total, n_order_deps, n_semantic_bugs))
