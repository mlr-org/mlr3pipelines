# Negation and OR-distribution at scale, verified with the DPLL checker from
# exp07 (no truth tables needed).
#
# For random formulas f, g beyond truth-table size:
#   1. f & !f must be UNSAT              (checks !.CnfFormula soundness one way)
#   2. f | !f must be a tautology        (checks the other direction + |)
#   3. !!f must be equivalent to f       (round trip through negation)
#   4. f | g must be equivalent to the distributed clause set built naively
#      by an independent implementation (checks |.CnfFormula's distribution
#      incl. its tautology elimination and the operand-swap optimization)
#
# Sizes are kept moderate because ! is exponential by nature; the point is to
# exceed truth-table reach (up to 12 symbols, domains up to 6), not to explode.
#
# Parameters: CNF_TRIALS (default 3000), CNF_CORES, CNF_SEED, CNF_DPLL_NODES.
#
# Run: Rscript attic/cnf_verify/exp10_negation_scale.R

source(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "harness.R"))
source_cnf()

# pull in the DPLL machinery from exp07 (checker part only)
exp07 = readLines(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "exp07_scale_dpll.R"))
dpll_start = grep("^node_budget = ", exp07)
dpll_end = grep("^# --- validation of the checker", exp07) - 1L
eval(parse(text = paste(exp07[dpll_start:dpll_end], collapse = "\n")), envir = globalenv())

formula_bare_clauses = function(f) {
  b = unclass(f)
  if (is.logical(b)) return(b)
  lapply(b, identity)
}

# independent naive OR of two clause sets: cross product with per-symbol
# unions, dropping tautological products
naive_or = function(cl1, cl2, domains) {
  out = list()
  for (a in cl1) {
    for (b in cl2) {
      merged = a
      for (s in names(b)) merged[[s]] = unique(c(merged[[s]], b[[s]]))
      taut = any(vapply(names(merged), function(s) all(domains[[s]] %in% merged[[s]]), NA))
      if (!taut) out[[length(out) + 1L]] = merged
    }
  }
  out
}

one_trial = function(i) {
  set.seed(base_seed() * 4000037L + i)
  n_sym = sample(5:12, 1)
  dom_sizes = sample(2:6, n_sym, replace = TRUE)
  uinfo = gen_universe(n_sym, dom_sizes, shared_values = runif(1) < 0.3)
  domains = uinfo$domains
  mk = function(n_cl, max_lit) {
    cls = gen_random_clauses(domains, n_clauses = n_cl, max_lit = max_lit)
    CnfFormula(lapply(cls, bare_to_clause, universe = uinfo$universe))
  }
  f = mk(sample(2:5, 1), sample(2:3, 1))
  g = mk(sample(2:4, 1), sample(2:3, 1))
  fb = formula_bare_clauses(f)
  if (is.logical(fb)) return(NULL)  # degenerate draw; skip
  fail = function(kind, msg) list(kind = kind, message = msg, trial = i, domains = domains,
    f = unclass(f), g = unclass(g))

  notf = tryCatch(`!.CnfFormula`(f), error = function(e) e)
  if (inherits(notf, "error")) return(fail("error", paste("!f:", conditionMessage(notf))))
  nb = formula_bare_clauses(notf)

  # 1. f & !f UNSAT
  conj = `&.CnfFormula`(f, notf)
  cb = formula_bare_clauses(conj)
  if (isTRUE(cb)) return(fail("semantic", "f & !f simplified to TRUE"))
  if (!isFALSE(cb)) {
    r = is_unsat(encode_clauses(cb, domains), domains)
    if (is.na(r)) return(list(kind = "skipped", message = "budget"))
    if (!r) return(fail("semantic", "f & !f is satisfiable"))
  }

  # 2. f | !f tautology
  disj = `|.CnfFormula`(f, notf)
  db = formula_bare_clauses(disj)
  if (!isTRUE(db)) {
    if (isFALSE(db)) return(fail("semantic", "f | !f simplified to FALSE"))
    r = implies(list(), db, domains)
    if (is.na(r)) return(list(kind = "skipped", message = "budget"))
    if (!r) return(fail("semantic", "f | !f is not a tautology"))
  }

  # 3. !!f == f
  notnotf = tryCatch(`!.CnfFormula`(notf), error = function(e) e)
  if (inherits(notnotf, "error")) return(fail("error", paste("!!f:", conditionMessage(notnotf))))
  nnb = formula_bare_clauses(notnotf)
  enc_f = encode_clauses(fb, domains)
  if (is.logical(nnb)) {
    ok = if (isTRUE(nnb)) implies(list(), fb, domains) else is_unsat(enc_f, domains)
    if (is.na(ok)) return(list(kind = "skipped", message = "budget"))
    if (!ok) return(fail("semantic", "!!f logical but f is not"))
  } else {
    enc_nn = encode_clauses(nnb, domains)
    r1 = implies(enc_f, nnb, domains)
    r2 = implies(enc_nn, fb, domains)
    if (is.na(r1) || is.na(r2)) return(list(kind = "skipped", message = "budget"))
    if (!r1 || !r2) return(fail("semantic", "!!f not equivalent to f"))
  }

  # 4. f | g equals independent naive distribution
  gb = formula_bare_clauses(g)
  if (!is.logical(gb)) {
    org = `|.CnfFormula`(f, g)
    ob = formula_bare_clauses(org)
    naive = naive_or(fb, gb, domains)
    if (length(naive) == 0) {
      # all products tautological -> f | g == TRUE
      if (!isTRUE(ob)) {
        r = implies(list(), ob, domains)
        if (is.na(r)) return(list(kind = "skipped", message = "budget"))
        if (!r) return(fail("semantic", "f | g should be tautology"))
      }
    } else if (isTRUE(ob)) {
      r = implies(list(), naive, domains)
      if (is.na(r)) return(list(kind = "skipped", message = "budget"))
      if (!r) return(fail("semantic", "f | g simplified to TRUE but naive OR is not tautological"))
    } else if (isFALSE(ob)) {
      r = is_unsat(encode_clauses(naive, domains), domains)
      if (is.na(r)) return(list(kind = "skipped", message = "budget"))
      if (!r) return(fail("semantic", "f | g simplified to FALSE but naive OR is satisfiable"))
    } else {
      enc_o = encode_clauses(ob, domains)
      enc_n = encode_clauses(naive, domains)
      r1 = implies(enc_o, naive, domains)
      r2 = implies(enc_n, ob, domains)
      if (is.na(r1) || is.na(r2)) return(list(kind = "skipped", message = "budget"))
      if (!r1 || !r2) return(fail("semantic", "f | g not equivalent to naive distribution"))
    }
  }
  NULL
}

n = n_trials(3000)
cat(sprintf("exp10 negation/or at scale: %d trials, seed %d\n", n, base_seed()))
results = parallel::mclapply(seq_len(n), function(i) {
  tryCatch(one_trial(i), error = function(e) list(kind = "harness_error", message = conditionMessage(e), trial = i))
}, mc.cores = n_cores())
failures = Filter(function(x) !is.null(x) && x$kind != "skipped", results)
skipped = sum(vapply(results, function(x) !is.null(x) && identical(x$kind, "skipped"), NA))
cat(sprintf("trials: %d, failures: %d, skipped (budget): %d\n", n, length(failures), skipped))
if (length(failures)) {
  for (f in head(failures, 5)) cat(sprintf("  [%s] %s\n", f$kind, f$message))
  saveRDS(failures, file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "results",
    sprintf("exp10_failures_seed%d.rds", base_seed())))
  quit(status = 1)
}
