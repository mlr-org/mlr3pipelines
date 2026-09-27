source("experiments/test_harness.R")

# Directed fuzzer for simplify_cnf
# Generates formulas designed to trigger specific simplification paths:
# 1. Many overlapping clauses (triggers subsumption / self-subsumption)
# 2. Nearly-contradictory formulas (triggers unit propagation cascades)
# 3. HLA-triggerable patterns (clauses with specific subset relationships)
# 4. 2nd-order SSE patterns (resolution-like interactions)
# 5. Formulas built from combining sub-formulas (via &, |, CnfFormula())

n_failures = 0
n_tests = 0

check_simplification = function(formula, universe, label) {
  n_tests <<- n_tests + 1
  truth = tryCatch(evaluate_formula(formula, universe), error = function(e) e)
  if (inherits(truth, "error")) {
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR [%s]: evaluation failed: %s\n", label, truth$message))
    return(FALSE)
  }

  # Verify by reconstructing a raw formula and checking against it
  formula_bare = unclass(formula)
  if (isTRUE(formula_bare)) {
    if (!all(truth)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: formula is TRUE but not all assignments satisfy it\n", label))
      return(FALSE)
    }
    return(TRUE)
  }
  if (isFALSE(formula_bare)) {
    if (!all(!truth)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [%s]: formula is FALSE but some assignments satisfy it\n", label))
      return(FALSE)
    }
    return(TRUE)
  }

  # For proper formulas, also check via raw evaluation
  raw_truth = rep(TRUE, length(truth))
  for (clause in unclass(formula)) {
    clause_truth = rep(FALSE, length(truth))
    varnames = ls(universe)
    domains = lapply(varnames, function(v) get(v, universe))
    names(domains) = varnames
    assignments = expand.grid(domains, stringsAsFactors = FALSE)
    colnames(assignments) = varnames
    for (sym in names(clause)) {
      clause_truth = clause_truth | (assignments[[sym]] %in% clause[[sym]])
    }
    raw_truth = raw_truth & clause_truth
  }
  if (!all(truth == raw_truth)) {
    n_failures <<- n_failures + 1
    idx = which(truth != raw_truth)[1]
    cat(sprintf("FAIL [%s]: truth mismatch at assignment %d\n", label, idx))
    return(FALSE)
  }
  TRUE
}

# ============================================================
# Strategy 1: Many symbols with small domains (binary-like)
# This maximizes the chance of triggering complex interactions
# ============================================================
cat("=== Strategy 1: Binary-domain symbols, many clauses ===\n")

for (trial in 1:50) {
  u_bin = CnfUniverse()
  n_syms = sample(3:6, 1)
  sym_names = paste0("V", 1:n_syms)
  syms = lapply(sym_names, function(nm) CnfSymbol(u_bin, nm, c("0", "1")))
  names(syms) = sym_names

  # Generate random clauses: each clause picks 1-3 symbols and gives them one value
  n_clauses = sample(3:10, 1)
  clause_list = lapply(1:n_clauses, function(i) {
    cl_syms = sample(sym_names, sample(1:min(3, n_syms), 1))
    atoms = lapply(cl_syms, function(s) syms[[s]] %among% sample(c("0", "1"), 1))
    as.CnfClause(Reduce(`|`, atoms))
  })

  result = tryCatch({
    f = CnfFormula(clause_list)
    check_simplification(f, u_bin, sprintf("binary trial %d", trial))
  }, error = function(e) {
    n_tests <<- n_tests + 1
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR [binary trial %d]: %s\n", trial, e$message))
    FALSE
  })
}
cat(sprintf("  After binary: %d tests, %d failures\n", n_tests, n_failures))

# ============================================================
# Strategy 2: Ternary-domain symbols with near-contradictory patterns
# ============================================================
cat("\n=== Strategy 2: Ternary domains, near-contradictions ===\n")

for (trial in 1:50) {
  u_ter = CnfUniverse()
  n_syms = sample(3:5, 1)
  sym_names = paste0("V", 1:n_syms)
  domains = lapply(1:n_syms, function(i) paste0(sym_names[i], c("a", "b", "c")))
  syms = Map(function(nm, dom) CnfSymbol(u_ter, nm, dom), sym_names, domains)

  # Create clauses that are "almost contradictory":
  # Many unit-like clauses restricting different symbols
  n_clauses = sample(4:12, 1)
  clause_list = lapply(1:n_clauses, function(i) {
    cl_syms = sample(sym_names, sample(1:min(3, n_syms), 1))
    atoms = lapply(cl_syms, function(s) {
      dom = get(s, u_ter)
      n_vals = sample(1:(length(dom) - 1), 1)  # never full domain (to avoid tautologies)
      vals = sample(dom, n_vals)
      syms[[s]] %among% vals
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  result = tryCatch({
    f = CnfFormula(clause_list)
    check_simplification(f, u_ter, sprintf("ternary trial %d", trial))
  }, error = function(e) {
    n_tests <<- n_tests + 1
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR [ternary trial %d]: %s\n", trial, e$message))
    FALSE
  })
}
cat(sprintf("  After ternary: %d tests, %d failures\n", n_tests, n_failures))

# ============================================================
# Strategy 3: Formulas designed to trigger HLA
# Pattern: clause A is almost a subset of clause B for all but one symbol
# ============================================================
cat("\n=== Strategy 3: HLA-triggering patterns ===\n")

for (trial in 1:100) {
  u_hla = CnfUniverse()
  n_syms = sample(3:5, 1)
  sym_names = paste0("V", 1:n_syms)
  domains = lapply(1:n_syms, function(i) paste0(sym_names[i], letters[1:3]))
  syms = Map(function(nm, dom) CnfSymbol(u_hla, nm, dom), sym_names, domains)

  # Create a "target" clause with many symbols
  target_syms = sample(sym_names, min(n_syms, sample(2:n_syms, 1)))
  target_atoms = lapply(target_syms, function(s) {
    dom = get(s, u_hla)
    vals = sample(dom, sample(1:2, 1))
    syms[[s]] %among% vals
  })
  target = as.CnfClause(Reduce(`|`, target_atoms))

  # Create several "helper" clauses that share symbols with the target
  n_helpers = sample(2:5, 1)
  helper_list = lapply(1:n_helpers, function(i) {
    # Pick most of the target's symbols, plus possibly one extra
    h_syms = sample(target_syms, max(1, length(target_syms) - 1))
    if (runif(1) < 0.3 && length(h_syms) < n_syms) {
      extra = sample(setdiff(sym_names, h_syms), 1)
      h_syms = c(h_syms, extra)
    }
    atoms = lapply(h_syms, function(s) {
      dom = get(s, u_hla)
      vals = sample(dom, sample(1:2, 1))
      syms[[s]] %among% vals
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  all_clauses = c(list(target), helper_list)

  result = tryCatch({
    f = CnfFormula(all_clauses)
    check_simplification(f, u_hla, sprintf("HLA trial %d", trial))
  }, error = function(e) {
    n_tests <<- n_tests + 1
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR [HLA trial %d]: %s\n", trial, e$message))
    FALSE
  })
}
cat(sprintf("  After HLA: %d tests, %d failures\n", n_tests, n_failures))

# ============================================================
# Strategy 4: 2nd-order SSE patterns (resolution-like)
# Pattern: two clauses resolve on one variable, restricting a third variable in a target
# ============================================================
cat("\n=== Strategy 4: 2nd-order SSE patterns ===\n")

for (trial in 1:100) {
  u_sse = CnfUniverse()
  n_syms = sample(3:5, 1)
  sym_names = paste0("V", 1:n_syms)
  domains = lapply(1:n_syms, function(i) paste0(sym_names[i], letters[1:3]))
  syms = Map(function(nm, dom) CnfSymbol(u_sse, nm, dom), sym_names, domains)

  if (n_syms < 3) next

  # Pick pivot, target, and extra symbols
  pivot = sample(sym_names, 1)
  target = sample(setdiff(sym_names, pivot), 1)
  pivot_domain = get(pivot, u_sse)

  # Split the pivot domain into two parts
  split_point = sample(1:(length(pivot_domain) - 1), 1)
  part1 = pivot_domain[1:split_point]
  part2 = pivot_domain[(split_point + 1):length(pivot_domain)]

  # Clause 1: pivot in part1 | target in some values
  target_domain = get(target, u_sse)
  t_vals1 = sample(target_domain, sample(1:2, 1))
  clause1 = (syms[[pivot]] %among% part1) | (syms[[target]] %among% t_vals1)

  # Clause 2: pivot in part2 | target in some values
  t_vals2 = sample(target_domain, sample(1:2, 1))
  clause2 = (syms[[pivot]] %among% part2) | (syms[[target]] %among% t_vals2)

  # Clause 3: target in the union of t_vals1 and t_vals2 (plus maybe more) -- this should be subsumable
  t_vals3 = unique(c(t_vals1, t_vals2))
  if (length(t_vals3) < length(target_domain)) {
    maybe_extra = sample(setdiff(target_domain, t_vals3), sample(0:1, 1))
    t_vals3 = unique(c(t_vals3, maybe_extra))
  }
  clause3 = syms[[target]] %among% t_vals3

  # Add some extra clauses for noise
  extra_clauses = lapply(1:sample(0:3, 1), function(i) {
    cs = sample(sym_names, sample(1:min(3, n_syms), 1))
    atoms = lapply(cs, function(s) {
      dom = get(s, u_sse)
      syms[[s]] %among% sample(dom, sample(1:length(dom), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  all_clauses = c(list(as.CnfClause(clause1), as.CnfClause(clause2), as.CnfClause(clause3)), extra_clauses)

  result = tryCatch({
    f = CnfFormula(all_clauses)
    check_simplification(f, u_sse, sprintf("SSE trial %d", trial))
  }, error = function(e) {
    n_tests <<- n_tests + 1
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR [SSE trial %d]: %s\n", trial, e$message))
    FALSE
  })
}
cat(sprintf("  After SSE: %d tests, %d failures\n", n_tests, n_failures))

# ============================================================
# Strategy 5: Combining sub-formulas via & and |
# This tests the CnfFormula constructor path that merges formulas
# ============================================================
cat("\n=== Strategy 5: Combining sub-formulas ===\n")

u_comb = CnfUniverse()
X_c = CnfSymbol(u_comb, "X", c("a", "b", "c"))
Y_c = CnfSymbol(u_comb, "Y", c("d", "e", "f"))
Z_c = CnfSymbol(u_comb, "Z", c("g", "h", "i"))
W_c = CnfSymbol(u_comb, "W", c("j", "k", "l"))
syms_c = list(X = X_c, Y = Y_c, Z = Z_c, W = W_c)

set.seed(99)
for (trial in 1:100) {
  # Generate 2-4 sub-formulas
  n_subformulas = sample(2:4, 1)
  subformulas = lapply(1:n_subformulas, function(sf) {
    n_cl = sample(1:3, 1)
    clauses = lapply(1:n_cl, function(i) {
      cs = sample(names(syms_c), sample(1:3, 1))
      atoms = lapply(cs, function(s) {
        dom = u_comb[[s]]
        syms_c[[s]] %among% sample(dom, sample(1:length(dom), 1))
      })
      as.CnfClause(Reduce(`|`, atoms))
    })
    tryCatch(CnfFormula(clauses), error = function(e) NULL)
  })
  subformulas = Filter(Negate(is.null), subformulas)
  if (length(subformulas) < 2) next

  # Combine via &
  combined_and = tryCatch(
    CnfFormula(subformulas),
    error = function(e) e
  )
  if (!inherits(combined_and, "error")) {
    # Verify: the combined formula should equal the conjunction of all sub-formulas
    truth_parts = lapply(subformulas, function(sf) evaluate_formula(sf, u_comb))
    truth_conj = Reduce(`&`, truth_parts)
    truth_combined = evaluate_formula(combined_and, u_comb)
    n_tests <<- n_tests + 1
    if (!all(truth_combined == truth_conj)) {
      n_failures <<- n_failures + 1
      cat(sprintf("FAIL [combine-& trial %d]: CnfFormula() conjunction differs from individual conjunction\n", trial))
    }
  }

  # Also combine first two via |
  if (length(subformulas) >= 2) {
    combined_or = tryCatch(
      subformulas[[1]] | subformulas[[2]],
      error = function(e) e
    )
    if (!inherits(combined_or, "error")) {
      truth1 = evaluate_formula(subformulas[[1]], u_comb)
      truth2 = evaluate_formula(subformulas[[2]], u_comb)
      truth_or = evaluate_formula(combined_or, u_comb)
      n_tests <<- n_tests + 1
      if (!all(truth_or == (truth1 | truth2))) {
        n_failures <<- n_failures + 1
        idx = which(truth_or != (truth1 | truth2))[1]
        cat(sprintf("FAIL [combine-| trial %d]: OR distribution incorrect\n", trial))
      }
    }
  }
}
cat(sprintf("  After combining: %d tests, %d failures\n", n_tests, n_failures))

# ============================================================
# Strategy 6: Larger variable counts (5-6 symbols)
# ============================================================
cat("\n=== Strategy 6: 5-6 symbols, many clauses ===\n")

set.seed(2024)
for (trial in 1:30) {
  u_big = CnfUniverse()
  n_syms = sample(5:6, 1)
  sym_names = paste0("S", 1:n_syms)
  domains = lapply(1:n_syms, function(i) paste0("v", i, "_", 1:sample(2:4, 1)))
  syms = Map(function(nm, dom) CnfSymbol(u_big, nm, dom), sym_names, domains)

  n_clauses = sample(5:15, 1)
  clause_list = lapply(1:n_clauses, function(i) {
    cs = sample(sym_names, sample(1:min(4, n_syms), 1))
    atoms = lapply(cs, function(s) {
      dom = get(s, u_big)
      syms[[s]] %among% sample(dom, sample(1:(length(dom)-1), 1))
    })
    as.CnfClause(Reduce(`|`, atoms))
  })

  result = tryCatch({
    f = CnfFormula(clause_list)
    check_simplification(f, u_big, sprintf("big trial %d", trial))
  }, error = function(e) {
    n_tests <<- n_tests + 1
    n_failures <<- n_failures + 1
    cat(sprintf("ERROR [big trial %d]: %s\n", trial, e$message))
    FALSE
  })
}
cat(sprintf("  After big: %d tests, %d failures\n", n_tests, n_failures))

cat(sprintf("\n=== GRAND TOTAL: %d tests, %d failures ===\n", n_tests, n_failures))
