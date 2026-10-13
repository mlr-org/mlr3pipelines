# Exhaustive verification: enumerate ALL formulas that are k-subsets of the
# complete clause pool over a small universe, and verify each against the
# brute-force truth table. Within the chosen space this is a *proof* (modulo
# clause order, which is covered separately by permutation tests) that
# simplify_cnf is sound.
#
# The clause pool consists of every clause expressible over n_vars symbols with
# domain size d: each symbol is either absent or carries one of the (2^d - 2)
# proper nonempty subsets of its domain; the all-absent combination is excluded.
# Pool size = (2^d - 1)^n - 1.
#
# Parameters (env vars):
#   CNF_NVARS   (default 3)  number of symbols
#   CNF_DOMSIZE (default 2)  domain size for all symbols
#   CNF_MINK    (default 1)  smallest formula size (number of clauses)
#   CNF_MAXK    (default 6)  largest formula size
#   CNF_CORES   (default 16)
#
# Suggested configurations:
#   3 vars binary, k<=6:   313,911 formulas  (~1 min on 16 cores)   [default]
#   3 vars binary, k<=8:   ~2.5M formulas    (~15 min)
#   2 vars domain 3, k<=4: ~213k formulas
#   2 vars domain 3, k<=5: ~1.9M formulas
#   2 vars domain 4, k<=3: ~1.9M formulas
#
# Run: Rscript attic/cnf_verify/exp04_exhaustive.R

source(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "harness.R"))
source_cnf()

n_vars = as.integer(Sys.getenv("CNF_NVARS", "3"))
dom_size = as.integer(Sys.getenv("CNF_DOMSIZE", "2"))
min_k = as.integer(Sys.getenv("CNF_MINK", "1"))
max_k = as.integer(Sys.getenv("CNF_MAXK", "6"))

uinfo = gen_universe(n_vars, dom_size)
domains = uinfo$domains
amat = all_assignments(domains)

# build the complete clause pool
subsets_of = function(d) {
  # all proper nonempty subsets of d
  out = list()
  for (m in seq_len(2^length(d) - 2)) {
    out[[m]] = d[as.logical(bitwAnd(m, 2^(seq_along(d) - 1)))]
  }
  out
}
per_sym_options = lapply(domains, function(d) c(list(NULL), subsets_of(d)))
option_grid = do.call(expand.grid, c(lapply(per_sym_options, seq_along), list(KEEP.OUT.ATTRS = FALSE)))
pool = list()
for (r in seq_len(nrow(option_grid))) {
  cl = list()
  for (j in seq_len(n_vars)) {
    opt = per_sym_options[[j]][[option_grid[r, j]]]
    if (!is.null(opt)) cl[[names(domains)[j]]] = opt
  }
  if (length(cl)) pool[[length(pool) + 1L]] = cl
}
n_pool = length(pool)
pool_tt = lapply(pool, tt_clause, amat = amat)
pool_objs = lapply(pool, bare_to_clause, universe = uinfo$universe)

count_total = sum(choose(n_pool, min_k:max_k))
cat(sprintf("exp04: %d vars, domain %d -> pool of %d clauses; k in [%d, %d]: %s formulas\n",
  n_vars, dom_size, n_pool, min_k, max_k, format(count_total, big.mark = ",")))

# check one formula given pool indices; returns NULL or diagnostic
check_combo = function(idx) {
  tt_in = Reduce(`&`, pool_tt[idx])
  f = tryCatch(CnfFormula(pool_objs[idx]), error = function(e) e)
  if (inherits(f, "error")) {
    return(list(kind = "error", message = conditionMessage(f), idx = idx))
  }
  tt_out = tt_obj(f, amat)
  if (!identical(tt_in, tt_out)) {
    return(list(kind = "semantic", message = sprintf("%d/%d assignments differ", sum(tt_in != tt_out), length(tt_in)),
      idx = idx, clauses = pool[idx], result = unclass(f)))
  }
  probs = check_formula_structure(f, domains)
  if (length(probs)) {
    return(list(kind = "structure", message = paste(probs, collapse = "; "), idx = idx,
      clauses = pool[idx], result = unclass(f)))
  }
  NULL
}

# enumerate k-combinations chunked by first element, parallel over chunks
all_failures = list()
n_checked = 0
t0 = proc.time()[["elapsed"]]
for (k in min_k:max_k) {
  chunk_results = parallel::mclapply(seq_len(n_pool - k + 1L), function(first) {
    cand = seq.int(first + 1L, length.out = n_pool - first)
    rest = if (k == 1L) {
      matrix(integer(0), nrow = 0, ncol = 1)
    } else if (length(cand) == 1L) {
      # combn(scalar, m) would enumerate 1:scalar
      matrix(cand, nrow = 1L, ncol = 1L)
    } else {
      utils::combn(cand, k - 1L)
    }
    fails = list()
    if (k == 1L) {
      f = check_combo(first)
      if (!is.null(f)) fails[[1L]] = f
      n = 1L
    } else {
      n = ncol(rest)
      for (ci in seq_len(ncol(rest))) {
        f = check_combo(c(first, rest[, ci]))
        if (!is.null(f)) fails[[length(fails) + 1L]] = f
      }
    }
    list(n = n, fails = fails)
  }, mc.cores = n_cores())
  bad = Filter(function(x) !is.list(x) || is.null(x$n), chunk_results)
  if (length(bad)) stop("worker failure in k=", k)
  n_k = sum(vapply(chunk_results, `[[`, 0, "n"))
  fails_k = unlist(lapply(chunk_results, `[[`, "fails"), recursive = FALSE)
  n_checked = n_checked + n_k
  all_failures = c(all_failures, fails_k)
  cat(sprintf("  k=%d: %s formulas checked, %d failures  (%.0fs elapsed)\n",
    k, format(n_k, big.mark = ","), length(fails_k), proc.time()[["elapsed"]] - t0))
}

cat(sprintf("exp04 done: %s formulas, %d failures\n", format(n_checked, big.mark = ","), length(all_failures)))
if (length(all_failures)) {
  out = file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "results",
    sprintf("exp04_failures_v%d_d%d_k%d-%d.rds", n_vars, dom_size, min_k, max_k))
  saveRDS(all_failures, out)
  cat("failures saved to", out, "\n")
  for (f in head(all_failures, 3)) cat(sprintf("  [%s] %s\n", f$kind, f$message))
  quit(status = 1)
}
