# Scale testing: verify simplify_cnf on formulas far beyond truth-table reach
# (10-20 symbols, domains up to 8, up to ~80 clauses) using an independent
# multivalued DPLL implication checker.
#
# Equivalence F == G is decided clause-wise:
#   G |= F  iff  for every clause c in F:  UNSAT(G && !c)
#   F |= G  iff  for every clause c in G:  UNSAT(F && !c)
# where !c contributes, for each symbol s in c, the unit restriction
# "s in domain \ c[[s]]". UNSAT is decided by DPLL with unit propagation on an
# integer-encoded representation (independent implementation, validated against
# truth tables on small instances before use).
#
# The interesting regime: the O(n^2) is_not_subset_of bookkeeping, cascading
# events, and HLA with many clauses -- structures barely reachable with
# truth-table-sized universes.
#
# Parameters: CNF_TRIALS (default 5000), CNF_CORES, CNF_SEED,
#   CNF_DPLL_NODES (node budget per UNSAT check, default 200000; trials
#   exceeding it are counted as skipped, not failed)
#
# Run: Rscript attic/cnf_verify/exp07_scale_dpll.R

source(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "harness.R"))
source_cnf()

node_budget = as.integer(Sys.getenv("CNF_DPLL_NODES", "200000"))

# --- integer-encoded multivalued CNF + DPLL ---------------------------------
# encoding: domains = named list sym -> character values;
# int clause = list of list(sym = <int index into domains>, vals = <int vector>)

encode_clauses = function(clauses_bare, domains) {
  sym_idx = setNames(seq_along(domains), names(domains))
  lapply(clauses_bare, function(cl) {
    lapply(names(cl), function(s) list(sym = sym_idx[[s]], vals = match(cl[[s]], domains[[s]])))
  })
}

# DPLL: doms = list of int vectors (current domains); clauses as above.
# returns TRUE if satisfiable, FALSE if unsat, NA if node budget exhausted
dpll = function(clauses, doms, budget_env) {
  budget_env$nodes = budget_env$nodes + 1L
  if (budget_env$nodes > node_budget) return(NA)
  # unit propagation loop
  repeat {
    changed = FALSE
    keep = rep(TRUE, length(clauses))
    for (ci in seq_along(clauses)) {
      cl = clauses[[ci]]
      live = list()
      satisfied = FALSE
      for (lit in cl) {
        d = doms[[lit$sym]]
        v = lit$vals[lit$vals %in% d]
        if (length(v) == 0) next            # literal dead
        if (length(v) == length(d)) { satisfied = TRUE; break }  # literal covers domain
        live[[length(live) + 1L]] = list(sym = lit$sym, vals = v)
      }
      if (satisfied) { keep[ci] = FALSE; next }
      if (length(live) == 0) return(FALSE)  # clause unsatisfiable
      if (length(live) == 1) {              # unit: force domain restriction
        doms[[live[[1]]$sym]] = live[[1]]$vals
        keep[ci] = FALSE
        changed = TRUE
      }
    }
    clauses = clauses[keep]
    if (!changed) break
  }
  if (length(clauses) == 0) return(TRUE)
  # branch: smallest domain among symbols still in clauses; only symbols with
  # >= 2 remaining values are eligible (symbols of dead literals may linger in
  # the clause structures with fixed domains -- branching on them cannot make
  # progress). After propagation every remaining clause has >= 2 live literals,
  # and live literals always have 2+ domain values, so eligible symbols exist.
  active_syms = unique(unlist(lapply(clauses, function(cl) vapply(cl, `[[`, 0L, "sym"))))
  active_syms = active_syms[lengths(doms)[active_syms] >= 2L]
  stopifnot(length(active_syms) > 0)
  branch_sym = active_syms[which.min(lengths(doms)[active_syms])]
  for (v in doms[[branch_sym]]) {
    doms2 = doms
    doms2[[branch_sym]] = v
    res = dpll(clauses, doms2, budget_env)
    if (isTRUE(res)) return(TRUE)
    if (is.na(res)) return(NA)
  }
  FALSE
}

# is `clauses` (int-encoded) unsat under full domains?
is_unsat = function(clauses, domains) {
  budget_env = new.env(); budget_env$nodes = 0L
  res = dpll(clauses, lapply(domains, seq_along), budget_env)
  if (is.na(res)) return(NA)
  !res
}

# does set1 (bare clauses) imply every clause of set2? TRUE/FALSE/NA
implies = function(enc1, set2_bare, domains) {
  sym_idx = setNames(seq_along(domains), names(domains))
  for (cl in set2_bare) {
    neg_units = list()
    for (s in names(cl)) {
      compl = setdiff(seq_along(domains[[s]]), match(cl[[s]], domains[[s]]))
      if (length(compl) == 0) next  # full-range literal: !c unsatisfiable -> implied trivially
      neg_units[[length(neg_units) + 1L]] = list(list(sym = sym_idx[[s]], vals = compl))
    }
    if (length(neg_units) < length(cl)) next  # some literal had full range: clause is tautological
    res = is_unsat(c(enc1, neg_units), domains)
    if (is.na(res)) return(NA)
    if (!res) return(FALSE)
  }
  TRUE
}

check_equiv_dpll = function(clauses_in, result_obj, domains) {
  r_bare = unclass(result_obj)
  enc_in = encode_clauses(clauses_in, domains)
  if (isTRUE(r_bare)) {
    # every input clause must be implied by the empty set (tautology): check emptyset |= F
    fwd = implies(list(), clauses_in, domains)
    return(if (is.na(fwd)) NA else fwd)
  }
  if (isFALSE(r_bare)) {
    return(is_unsat(enc_in, domains))
  }
  r_clauses = lapply(r_bare, identity)
  enc_out = encode_clauses(r_clauses, domains)
  fwd = implies(enc_in, r_clauses, domains)   # F |= G
  if (is.na(fwd)) return(NA)
  if (!fwd) return(FALSE)
  bwd = implies(enc_out, clauses_in, domains) # G |= F
  if (is.na(bwd)) return(NA)
  bwd
}

# --- validation of the checker against truth tables --------------------------
cat("validating DPLL checker against truth tables on small instances...\n")
set.seed(99)
for (i in 1:800) {
  n_sym = sample(2:4, 1)
  dom_sizes = sample(2:4, n_sym, replace = TRUE)
  uinfo = gen_universe(n_sym, dom_sizes)
  cls = gen_random_clauses(uinfo$domains, n_clauses = sample(1:8, 1), max_lit = min(3L, n_sym))
  amat = all_assignments(uinfo$domains)
  # fabricate a "result": either the true simplification, or a deliberately
  # perturbed clause set (to test that the checker FAILS wrong results)
  f = CnfFormula(lapply(cls, bare_to_clause, universe = uinfo$universe))
  verdict = check_equiv_dpll(cls, f, uinfo$domains)
  tt_equal = identical(tt_clauses(cls, amat), tt_obj(f, amat))
  if (!identical(verdict, tt_equal)) stop("checker disagrees with truth table (correct case), trial ", i)
  # perturbation: drop a clause / a literal / a value from the result
  r_bare = unclass(f)
  if (is.list(r_bare) && length(r_bare) >= 1) {
    mode = sample(c("drop_clause", "widen", "shrink"), 1)
    r2 = r_bare
    ci = sample.int(length(r2), 1)
    if (mode == "drop_clause") {
      r2[[ci]] = NULL
    } else if (mode == "widen") {
      s = sample(names(r2[[ci]]), 1)
      extra = setdiff(uinfo$domains[[s]], r2[[ci]][[s]])
      if (length(extra)) r2[[ci]][[s]] = c(r2[[ci]][[s]], extra[1]) else next
    } else {
      s = sample(names(r2[[ci]]), 1)
      if (length(r2[[ci]][[s]]) > 1) r2[[ci]][[s]] = r2[[ci]][[s]][-1] else next
    }
    f2 = structure(r2, universe = uinfo$universe, class = "CnfFormula")
    verdict2 = check_equiv_dpll(cls, f2, uinfo$domains)
    tt_equal2 = identical(tt_clauses(cls, amat), tt_obj(f2, amat))
    if (!identical(verdict2, tt_equal2)) stop("checker disagrees with truth table (perturbed case), trial ", i)
  }
}
cat("checker validated (800 instances incl. perturbed results)\n")

# --- scale trials -------------------------------------------------------------
syms_max = as.integer(Sys.getenv("CNF_SYMS_MAX", "20"))
clauses_max = as.integer(Sys.getenv("CNF_CLAUSES_MAX", "60"))

one_trial = function(i) {
  set.seed(base_seed() * 3000017L + i)
  n_sym = sample(8:syms_max, 1)
  dom_sizes = sample(2:8, n_sym, replace = TRUE)
  uinfo = gen_universe(n_sym, dom_sizes, shared_values = runif(1) < 0.3)
  gen = sample(c("random", "redundant", "unit_heavy", "chain", "unit_merge", "mixed_big"), 1)
  max_lit = sample(2:5, 1)
  clauses = switch(gen,
    random = gen_random_clauses(uinfo$domains, n_clauses = sample(10:clauses_max, 1), max_lit = max_lit),
    redundant = gen_redundant_clauses(uinfo$domains, n_base = sample(5:15, 1), n_extra = sample(5:25, 1), max_lit = max_lit),
    unit_heavy = gen_unit_heavy_clauses(uinfo$domains, n_units = sample(2:6, 1), n_other = sample(10:30, 1), max_lit = max_lit),
    chain = do.call(c, replicate(sample(3:6, 1), gen_chain_clauses(uinfo$domains, n_chain = sample(4:8, 1), max_lit = max_lit), simplify = FALSE)),
    unit_merge = do.call(c, replicate(sample(2:5, 1), gen_unit_merge_hla_clauses(uinfo$domains), simplify = FALSE)),
    mixed_big = c(
      gen_random_clauses(uinfo$domains, n_clauses = sample(5:20, 1), max_lit = max_lit),
      gen_redundant_clauses(uinfo$domains, n_base = sample(3:8, 1), n_extra = sample(3:10, 1), max_lit = max_lit),
      gen_unit_heavy_clauses(uinfo$domains, n_units = sample(1:3, 1), n_other = sample(3:10, 1), max_lit = max_lit)
    )
  )
  f = tryCatch({
    setTimeLimit(elapsed = trial_timeout(), transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
    CnfFormula(lapply(clauses, bare_to_clause, universe = uinfo$universe))
  }, error = function(e) e)
  if (inherits(f, "error")) {
    return(list(kind = "error", message = conditionMessage(f), domains = uinfo$domains, clauses = clauses, gen = gen))
  }
  probs = check_formula_structure(f, uinfo$domains)
  if (length(probs)) {
    return(list(kind = "structure", message = paste(probs, collapse = "; "), domains = uinfo$domains, clauses = clauses, gen = gen))
  }
  verdict = check_equiv_dpll(clauses, f, uinfo$domains)
  if (is.na(verdict)) return(list(kind = "skipped", message = "DPLL budget exhausted", gen = gen))
  if (!verdict) {
    return(list(kind = "semantic", message = "DPLL: not equivalent", domains = uinfo$domains,
      clauses = clauses, result = unclass(f), gen = gen))
  }
  NULL
}

n = n_trials(5000)
cat(sprintf("exp07 scale DPLL: %d trials, seed %d\n", n, base_seed()))
results = parallel::mclapply(seq_len(n), function(i) {
  tryCatch(one_trial(i), error = function(e) list(kind = "harness_error", message = conditionMessage(e), trial = i))
}, mc.cores = n_cores())
failures = Filter(function(x) !is.null(x) && x$kind != "skipped", results)
skipped = sum(vapply(results, function(x) !is.null(x) && x$kind == "skipped", NA))
cat(sprintf("trials: %d, failures: %d, skipped (budget): %d\n", n, length(failures), skipped))
if (length(failures)) {
  for (f in head(failures, 5)) cat(sprintf("  [%s] %s (gen=%s)\n", f$kind, f$message, f$gen))
  saveRDS(failures, file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "results",
    sprintf("exp07_failures_seed%d.rds", base_seed())))
  quit(status = 1)
}
