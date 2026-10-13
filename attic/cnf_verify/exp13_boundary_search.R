# Boundary-focused adversarial search.
#
# Rationale: exp03's near-miss mutants (M19: use_inso skip slightly too eager;
# M14: propagation skipped; M07/M08: HTE threshold off by one; M09: 2nd-order
# disjointness dropped) each delineate a decision boundary in simplify_cnf
# where correctness is delicate. Inputs that KILL those mutants sit exactly on
# these boundaries. If the real code had a residual bug near such a boundary,
# inputs in the *neighborhood* of mutant-killers would be the most likely
# witnesses. This experiment:
#   1. collects mutant-killing inputs for each near-miss mutant,
#   2. explores the neighborhood of each killer (add/remove/replace values,
#      clauses, symbols) with many random perturbations,
#   3. tests every neighbor against the REAL code with BOTH the truth-table
#      oracle and the invariant instrumentation of exp08.
#
# Parameters: CNF_KILLERS (default 40 killers/mutant max), CNF_NEIGHBORS
# (default 300 per killer), CNF_CORES, CNF_SEED.
#
# Run: Rscript attic/cnf_verify/exp13_boundary_search.R

source(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "harness.R"))
source_cnf()

simplify_src = paste(readLines(file.path(CNF_SRC_DIR, "CnfFormula_simplify.R")), collapse = "\n")
`%||%` = function(a, b) if (is.null(a)) b else a

# --- the near-miss mutants ---------------------------------------------------
orig_inso = "        if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]] && !is_not_subset_of[[s_clause_idx_meta]][unit_idx_meta, nu]) next"
mutants = list(
  M19 = sub(orig_inso, "        if (s_clause_idx_meta <= meta_idx_outer && inso_column[[s_clause_idx_meta]]) next", simplify_src, fixed = TRUE),
  M14 = sub(orig_inso, "        if (s_clause_idx_meta <= meta_idx_outer) next", simplify_src, fixed = TRUE),
  M07 = sub("if (length(range_new) == length(universe[[symbol]])) {\n        # hidden tautology elimination\n\n        # We still need the symbol registry",
            "if (length(range_new) >= length(universe[[symbol]]) - 1L) {\n        # hidden tautology elimination\n\n        # We still need the symbol registry", simplify_src, fixed = TRUE),
  M09 = sub("    if (any(clause_oneend_symbol_intersect %in% clause_twoends[[symbol_intersect]] &\n        !clause_oneend_symbol_intersect %in% entries[[idx_target]][[symbol_intersect]])) {\n      return(FALSE)\n    }",
            "    # disjointness check removed (mutant)", simplify_src, fixed = TRUE)
)
stopifnot(!vapply(mutants, identical, NA, y = simplify_src))

# --- invariant hook (reuse exp08's checker) ----------------------------------
exp08 = readLines(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "exp08_invariant_fuzz.R"))
hook_start = grep("^invariant_hook = function", exp08)
hook_end = grep("^gen_directed_trial = function", exp08) - 1L
eval(parse(text = paste(exp08[hook_start:hook_end], collapse = "\n")), envir = globalenv())

inject = function(src, anchor, hook_call) {
  stopifnot(length(gregexpr(anchor, src, fixed = TRUE)[[1]]) == 1)
  sub(anchor, paste0(hook_call, "\n", anchor), src, fixed = TRUE)
}
instrumented_src = simplify_src
instrumented_src = inject(instrumented_src, "  # let's start with (self)-subsumption.", '  invariant_hook(environment(), "post_preprocess")')
instrumented_src = inject(instrumented_src, "  # Now for the big one: Asymmetric Hidden Literal Addition (Marijn et al.)", '  invariant_hook(environment(), "pre_hla")')
instrumented_src = inject(instrumented_src, "  return_entries(entries[!eliminated])", '  invariant_hook(environment(), "final")')

n_killers = as.integer(Sys.getenv("CNF_KILLERS", "40"))
n_neighbors = as.integer(Sys.getenv("CNF_NEIGHBORS", "300"))

gen_trial_for_boundary = function(i) {
  set.seed(base_seed() * 7000003L + i)
  if (i %% 2L == 0L) {
    n_sym = sample(2:4, 1)
    dom_sizes = sample(3:5, n_sym, replace = TRUE)
    while (prod(dom_sizes) > 4096) dom_sizes[which.max(dom_sizes)] = 3L
    uinfo = gen_universe(n_sym, dom_sizes, shared_values = runif(1) < 0.3)
    list(uinfo = uinfo, clauses = gen_unit_merge_hla_clauses(uinfo$domains))
  } else {
    gen_standard_trial(i)
  }
}

# --- phase 1: collect killers -------------------------------------------------
cat("phase 1: collecting mutant-killing inputs...\n")
collect_killers = function(mutant_name) {
  eval(parse(text = mutants[[mutant_name]]), envir = globalenv())
  killers = list()
  for (i in seq_len(60000)) {
    trial = gen_trial_for_boundary(i)
    rec = tryCatch(check_simplify(trial$clauses, trial$uinfo), error = function(e) list(kind = "error"))
    if (!is.null(rec)) {
      killers[[length(killers) + 1L]] = list(domains = trial$uinfo$domains, clauses = trial$clauses)
      if (length(killers) >= n_killers) break
    }
  }
  killers
}
killer_sets = parallel::mclapply(names(mutants), collect_killers, mc.cores = min(4L, n_cores()))
names(killer_sets) = names(mutants)
for (nm in names(killer_sets)) cat(sprintf("  %s: %d killers found\n", nm, length(killer_sets[[nm]])))

# --- phase 2: neighborhood exploration against the REAL (instrumented) code ---
cat("phase 2: exploring killer neighborhoods against real code + invariants...\n")

perturb = function(clauses, domains) {
  cl = clauses
  op = sample(c("add_val", "rm_val", "swap_val", "add_clause", "rm_clause", "dup_clause", "rm_sym", "add_sym"), 1)
  ci = sample.int(length(cl), 1)
  eligible = names(domains)[lengths(domains) >= 2L]
  tryCatch({
    if (op == "add_val") {
      s = sample(names(cl[[ci]]), 1)
      extra = setdiff(domains[[s]], cl[[ci]][[s]])
      if (length(extra) > 1) cl[[ci]][[s]] = c(cl[[ci]][[s]], sample(extra, 1))
    } else if (op == "rm_val") {
      s = sample(names(cl[[ci]]), 1)
      if (length(cl[[ci]][[s]]) > 1) cl[[ci]][[s]] = cl[[ci]][[s]][-sample.int(length(cl[[ci]][[s]]), 1)]
    } else if (op == "swap_val") {
      s = sample(names(cl[[ci]]), 1)
      extra = setdiff(domains[[s]], cl[[ci]][[s]])
      if (length(extra) > 1) {
        vals = cl[[ci]][[s]]
        vals[sample.int(length(vals), 1)] = sample(extra, 1)
        cl[[ci]][[s]] = unique(vals)
      }
    } else if (op == "add_clause") {
      cl[[length(cl) + 1L]] = gen_bare_clause(domains, max_lit = min(3L, length(eligible)))
    } else if (op == "rm_clause") {
      if (length(cl) > 1) cl[[ci]] = NULL
    } else if (op == "dup_clause") {
      cl[[length(cl) + 1L]] = cl[[ci]]
    } else if (op == "rm_sym") {
      if (length(cl[[ci]]) > 1) cl[[ci]][[sample.int(length(cl[[ci]]), 1)]] = NULL
    } else {
      missing_syms = setdiff(eligible, names(cl[[ci]]))
      if (length(missing_syms)) {
        s = sample(missing_syms, 1)
        d = domains[[s]]
        cl[[ci]][[s]] = sample(d, sample.int(length(d) - 1L, 1))
      }
    }
    cl
  }, error = function(e) clauses)
}

eval(parse(text = instrumented_src), envir = globalenv())  # REAL code + invariant hooks

all_killers = unlist(unname(killer_sets), recursive = FALSE)
cat(sprintf("  %d killers total, %d neighbors each\n", length(all_killers), n_neighbors))
results = parallel::mclapply(seq_along(all_killers), function(ki) {
  killer = all_killers[[ki]]
  set.seed(base_seed() * 9000011L + ki)
  fails = list()
  n_run = 0L
  for (j in seq_len(n_neighbors)) {
    cl = killer$clauses
    for (steps in seq_len(sample(1:3, 1))) cl = perturb(cl, killer$domains)
    if (length(cl) == 0) next
    u = CnfUniverse()
    for (nm in names(killer$domains)) CnfSymbol(u, nm, killer$domains[[nm]])
    uinfo = list(universe = u, domains = killer$domains)
    n_run = n_run + 1L
    rec = tryCatch(check_simplify(cl, uinfo), error = function(e) list(kind = "error", message = conditionMessage(e)))
    if (!is.null(rec)) {
      rec$domains = killer$domains; rec$clauses = cl
      fails[[length(fails) + 1L]] = rec
    }
  }
  list(n = n_run, fails = fails)
}, mc.cores = n_cores())

n_total = sum(vapply(results, `[[`, 0L, "n"))
failures = unlist(lapply(results, `[[`, "fails"), recursive = FALSE)
cat(sprintf("boundary search: %d neighbor formulas tested, %d failures\n", n_total, length(failures)))
if (length(failures)) {
  saveRDS(failures, file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "results",
    sprintf("exp13_failures_seed%d.rds", base_seed())))
  for (f in head(failures, 5)) cat(sprintf("  [%s] %s\n", f$kind, f$message))
  quit(status = 1)
}
