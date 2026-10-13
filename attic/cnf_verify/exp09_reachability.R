# Reachability probes for rare/suspected-dead branches in simplify_cnf.
#
# Coverage analysis (exp06) found that the hidden tautology elimination (HTE)
# outcome is never executed in either HLA loop. Analysis suggests it is
# unreachable when the subset bookkeeping is consistent: a donor clause is
# selected only if its exceptional symbol m satisfies donor[m] not-subset-of
# C[m], which makes range_new = C[m] u (dom \ (C[m] u donor[m])) != dom always;
# whenever a clause's last non-subset symbol becomes covered, the count drops
# to 0 and hidden *subsumption* elimination preempts. If HTE ever fires, either
# this analysis is wrong or bookkeeping was inconsistent -- both would be
# important. This experiment counts executions of such branches over large
# fuzzing + exhaustive workloads.
#
# Probes:
#   P1_nonunit_hte     hidden tautology elimination, non-unit HLA loop
#   P2_unit_hte        hidden tautology elimination, unit HLA loop
#   P3_twoend_reval    mid-loop revalidation `next` in handle_sse_2nd_order_oneend
#   P4_twoend_rowsum2  rowsum==2 path entered in on_updated_subset_relations
#   P5_symbol_target   length(symbol_target) != 1 `next` in oneend handler
#   P6_adr_escape      cascade invalidates clause_idx inside apply_domain_restriction
#   P7_esc_escape      cascade invalidates clause_idx inside eliminate_symbol_from_clause
#   P8_hse_nonunit     hidden subsumption elimination fires (non-unit; sanity: should be > 0)
#   P9_hse_unit        hidden subsumption elimination fires (unit; sanity: should be > 0)
#
# Parameters: CNF_TRIALS (default 50000 fuzz trials; exhaustive part fixed),
# CNF_CORES, CNF_SEED.
#
# Run: Rscript attic/cnf_verify/exp09_reachability.R

source(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "harness.R"))
source_cnf()

simplify_src = paste(readLines(file.path(CNF_SRC_DIR, "CnfFormula_simplify.R")), collapse = "\n")

`%||%` = function(a, b) if (is.null(a)) b else a

.probes = new.env(parent = emptyenv())
probe_hit = function(id) {
  assign(id, get0(id, .probes, ifnotfound = 0L) + 1L, .probes)
  invisible(NULL)
}
probe_counts = function() {
  out = mget(ls(.probes), .probes)
  if (length(out)) unlist(out) else integer(0)
}
probe_reset = function() rm(list = ls(.probes), envir = .probes)

patch = function(src, anchor, replacement, id) {
  n_occ = length(gregexpr(anchor, src, fixed = TRUE)[[1]])
  if (n_occ != 1 || !grepl(anchor, src, fixed = TRUE)) stop("bad anchor for ", id, " (", n_occ, " occurrences)")
  sub(anchor, replacement, src, fixed = TRUE)
}

p = simplify_src
p = patch(p,
  "        # We still need the symbol registry (see below), so this can not be replaced by just `eliminated[[clause_idx]] = TRUE`\n        eliminate_clause_update_sr(clause_idx)",
  "        # We still need the symbol registry (see below), so this can not be replaced by just `eliminated[[clause_idx]] = TRUE`\n        probe_hit(\"P1_nonunit_hte\")\n        eliminate_clause_update_sr(clause_idx)",
  "P1")
p = patch(p,
  "        # hidden tautology elimination\n        # no more need to update the unit registry\n        eliminated[[clause_idx]] = TRUE",
  "        # hidden tautology elimination\n        # no more need to update the unit registry\n        probe_hit(\"P2_unit_hte\")\n        eliminated[[clause_idx]] = TRUE",
  "P2")
p = patch(p,
  "      if (eliminated[[clause_idx_twoend]] || is_unit[[clause_idx_twoend]] ||\n          not_subset_count[meta_idx_twoend, meta_idx_target] != 2L) {\n        next\n      }",
  "      if (eliminated[[clause_idx_twoend]] || is_unit[[clause_idx_twoend]] ||\n          not_subset_count[meta_idx_twoend, meta_idx_target] != 2L) {\n        probe_hit(\"P3_twoend_reval\")\n        next\n      }",
  "P3")
p = patch(p,
  "      # we usually don't get here when we are second_order_only; however, it may be possible that a oneend clause\n      # gets turned into a twoend clause during 2nd order triggering before it gets triggered itself.\n      hs2oo = handle_sse_2nd_order_twoend(meta_idx, meta_idx_other, NULL)",
  "      # we usually don't get here when we are second_order_only; however, it may be possible that a oneend clause\n      # gets turned into a twoend clause during 2nd order triggering before it gets triggered itself.\n      probe_hit(\"P4_twoend_rowsum2\")\n      hs2oo = handle_sse_2nd_order_twoend(meta_idx, meta_idx_other, NULL)",
  "P4")
p = patch(p,
  "      if (length(symbol_target) != 1L) next  # this can happen if a previous loop changed the clause in some way",
  "      if (length(symbol_target) != 1L) { probe_hit(\"P5_symbol_target\"); next }",
  "P5")
p = patch(p,
  "      if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) return(NULL)  # need to check more directly if things escalated somehow and clause_idx was eliminated indirectly",
  "      if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) { probe_hit(\"P6_adr_escape\"); return(NULL) }",
  "P6")
p = patch(p,
  "      # on_updated_subset_relations could cascade down to eliminating meta_idx (i.e. clause_idx)\n      if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) return(NULL)",
  "      # on_updated_subset_relations could cascade down to eliminating meta_idx (i.e. clause_idx)\n      if (eliminated[[clause_idx]] || is_unit[[clause_idx]]) { probe_hit(\"P7_esc_escape\"); return(NULL) }",
  "P7")
p = patch(p,
  "          if (not_subset_count_current[[roe_idx]] == 0L) {\n            # hidden subsumption elimination\n            eliminate_clause_update_sr(clause_idx)",
  "          if (not_subset_count_current[[roe_idx]] == 0L) {\n            # hidden subsumption elimination\n            probe_hit(\"P8_hse_nonunit\")\n            eliminate_clause_update_sr(clause_idx)",
  "P8")
p = patch(p,
  "          if (not_subset_count[[updating_hla_clause_idx]] == 0) {\n            # hidden subsumption elimination\n            eliminated[[clause_idx]] = TRUE",
  "          if (not_subset_count[[updating_hla_clause_idx]] == 0) {\n            # hidden subsumption elimination\n            probe_hit(\"P9_hse_unit\")\n            eliminated[[clause_idx]] = TRUE",
  "P9")
eval(parse(text = p), envir = globalenv())
cat("all probes patched\n")

gen_directed_trial = function(i, seed = base_seed()) {
  set.seed(seed * 7000003L + i)
  n_sym = sample(2:4, 1)
  dom_sizes = sample(3:5, n_sym, replace = TRUE)
  while (prod(dom_sizes) > 4096) dom_sizes[which.max(dom_sizes)] = 3L
  uinfo = gen_universe(n_sym, dom_sizes, shared_values = runif(1) < 0.3)
  list(uinfo = uinfo, clauses = gen_unit_merge_hla_clauses(uinfo$domains))
}

# --- workload 1: fuzz (standard + directed), counts collected per chunk -----
n = n_trials(50000)
chunks = split(seq_len(n), (seq_len(n) - 1L) %/% 500L)
res = parallel::mclapply(chunks, function(idx) {
  probe_reset()
  sem_fail = 0L
  for (i in idx) {
    trial = if (i %% 2L == 0L) gen_directed_trial(i) else gen_standard_trial(i)
    rec = tryCatch(check_simplify(trial$clauses, trial$uinfo), error = function(e) list(kind = "err"))
    if (!is.null(rec)) sem_fail = sem_fail + 1L
  }
  list(counts = probe_counts(), sem_fail = sem_fail)
}, mc.cores = n_cores())

agg = list()
for (r in res) for (nm in names(r$counts)) agg[[nm]] = (agg[[nm]] %||% 0L) + r$counts[[nm]]
sem_fails = sum(vapply(res, `[[`, 0L, "sem_fail"))
cat(sprintf("\nfuzz workload (%d trials, %d oracle failures):\n", n, sem_fails))
all_probes = c("P1_nonunit_hte", "P2_unit_hte", "P3_twoend_reval", "P4_twoend_rowsum2",
  "P5_symbol_target", "P6_adr_escape", "P7_esc_escape", "P8_hse_nonunit", "P9_hse_unit")
for (nm in all_probes) cat(sprintf("  %-18s %d\n", nm, agg[[nm]] %||% 0L))

# --- workload 2: exhaustive 3-var binary, k <= 5 ----------------------------
uinfo = gen_universe(3, 2)
domains = uinfo$domains
amat = all_assignments(domains)
subsets_of = function(d) lapply(seq_len(2^length(d) - 2), function(m) d[as.logical(bitwAnd(m, 2^(seq_along(d) - 1)))])
per_sym_options = lapply(domains, function(d) c(list(NULL), subsets_of(d)))
option_grid = do.call(expand.grid, c(lapply(per_sym_options, seq_along), list(KEEP.OUT.ATTRS = FALSE)))
pool = list()
for (r in seq_len(nrow(option_grid))) {
  cl = list()
  for (j in seq_along(domains)) {
    opt = per_sym_options[[j]][[option_grid[r, j]]]
    if (!is.null(opt)) cl[[names(domains)[j]]] = opt
  }
  if (length(cl)) pool[[length(pool) + 1L]] = cl
}
pool_tt = lapply(pool, tt_clause, amat = amat)
pool_objs = lapply(pool, bare_to_clause, universe = uinfo$universe)
n_pool = length(pool)

agg2 = list()
sem_fails2 = 0L
for (k in 1:5) {
  res2 = parallel::mclapply(seq_len(n_pool - k + 1L), function(first) {
    probe_reset()
    cand = seq.int(first + 1L, length.out = n_pool - first)
    rest = if (k == 1L) matrix(integer(0), 0, 1) else if (length(cand) == 1L) matrix(cand, 1, 1) else utils::combn(cand, k - 1L)
    sem_fail = 0L
    combos = if (k == 1L) list(first) else lapply(seq_len(ncol(rest)), function(ci) c(first, rest[, ci]))
    for (combo in combos) {
      tt_in = Reduce(`&`, pool_tt[combo])
      f = tryCatch(CnfFormula(pool_objs[combo]), error = function(e) e)
      if (inherits(f, "error") || !identical(tt_in, tt_obj(f, amat))) sem_fail = sem_fail + 1L
    }
    list(counts = probe_counts(), sem_fail = sem_fail)
  }, mc.cores = n_cores())
  for (r in res2) { for (nm in names(r$counts)) agg2[[nm]] = (agg2[[nm]] %||% 0L) + r$counts[[nm]]; sem_fails2 = sem_fails2 + r$sem_fail }
}
cat(sprintf("\nexhaustive 3v binary k<=5 (%s formulas, %d oracle failures):\n",
  format(sum(choose(n_pool, 1:5)), big.mark = ","), sem_fails2))
for (nm in all_probes) cat(sprintf("  %-18s %d\n", nm, agg2[[nm]] %||% 0L))

saveRDS(list(fuzz = agg, exhaustive = agg2), file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "results", "exp09_probes.rds"))
