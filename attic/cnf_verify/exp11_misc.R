# Miscellaneous directed checks:
#  A. permutation invariance: any clause order must give semantically equal
#     results (structure may differ; the simplifier is non-confluent by design)
#  B. large domains (10-30 values): stresses character-set operations and the
#     length-based subset/equality shortcuts
#  C. OR symmetry: f | g and g | f semantically equal (exercises the
#     operand-swap optimization in |.CnfFormula)
#  D. duplicate-clause stress: many identical/near-identical clauses
#  E. constructor universe inference with logical entries (documents the
#     known-bug family #3/#4 boundaries, incl. the CnfClause variant)
#
# Parameters: CNF_TRIALS (default 20000 across modes), CNF_CORES, CNF_SEED.
#
# Run: Rscript attic/cnf_verify/exp11_misc.R

source(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "harness.R"))
source_cnf()

one_trial = function(i) {
  set.seed(base_seed() * 5000011L + i)
  mode = sample(c("perm", "bigdom", "orsym", "dup", "alleq"), 1)
  fail = function(kind, msg, ...) list(kind = kind, message = msg, trial = i, mode = mode, ...)

  if (mode == "bigdom") {
    n_sym = sample(2:3, 1)
    dom_sizes = sample(10:30, n_sym, replace = TRUE)
    while (prod(dom_sizes) > 3000) dom_sizes[which.max(dom_sizes)] = 10L
    uinfo = gen_universe(n_sym, dom_sizes)
    clauses = gen_random_clauses(uinfo$domains, n_clauses = sample(2:10, 1), max_lit = n_sym)
    return(check_simplify(clauses, uinfo, extra_info = list(trial = i, mode = mode)))
  }

  n_sym = sample(2:5, 1)
  dom_sizes = sample(2:4, n_sym, replace = TRUE)
  while (prod(dom_sizes) > 2048) dom_sizes[which.max(dom_sizes)] = 2L
  uinfo = gen_universe(n_sym, dom_sizes)
  amat = all_assignments(uinfo$domains)

  if (mode == "perm") {
    clauses = gen_random_clauses(uinfo$domains, n_clauses = sample(3:8, 1), max_lit = min(3L, n_sym))
    tt_ref = tt_clauses(clauses, amat)
    for (rep in 1:3) {
      perm = clauses[sample.int(length(clauses))]
      f = CnfFormula(lapply(perm, bare_to_clause, universe = uinfo$universe))
      if (!identical(tt_obj(f, amat), tt_ref)) {
        return(fail("semantic", sprintf("permutation %d differs", rep), domains = uinfo$domains, clauses = clauses, perm = perm))
      }
    }
    return(NULL)
  }

  if (mode == "orsym") {
    mk = function() {
      cls = gen_random_clauses(uinfo$domains, n_clauses = sample(1:4, 1), max_lit = min(3L, n_sym))
      CnfFormula(lapply(cls, bare_to_clause, universe = uinfo$universe))
    }
    f = mk(); g = mk()
    fg = `|.CnfFormula`(f, g)
    gf = `|.CnfFormula`(g, f)
    expected = tt_obj(f, amat) | tt_obj(g, amat)
    if (!identical(tt_obj(fg, amat), expected)) return(fail("semantic", "f|g wrong", domains = uinfo$domains, f = unclass(f), g = unclass(g)))
    if (!identical(tt_obj(gf, amat), expected)) return(fail("semantic", "g|f wrong", domains = uinfo$domains, f = unclass(f), g = unclass(g)))
    return(NULL)
  }

  if (mode == "alleq") {
    # all.equal soundness: TRUE result must imply identical truth tables;
    # permuted/reordered representations of the same formula must compare TRUE
    cls = gen_random_clauses(uinfo$domains, n_clauses = sample(1:5, 1), max_lit = min(3L, n_sym))
    f = CnfFormula(lapply(cls, bare_to_clause, universe = uinfo$universe))
    fb = unclass(f)
    if (is.list(fb)) {
      # reorder clauses, symbols within clauses, and values within ranges
      fb2 = lapply(fb, function(cl) {
        cl2 = lapply(cl, function(v) v[sample.int(length(v))])
        cl2[sample.int(length(cl2))]
      })
      f2 = structure(fb2[sample.int(length(fb2))], universe = uinfo$universe, class = "CnfFormula")
      if (!isTRUE(all.equal(f, f2))) {
        return(fail("alleq", "reordered representation not all.equal", domains = uinfo$domains, f = fb))
      }
      # random perturbation: all.equal returning TRUE would be unsound if the
      # truth tables differ
      fb3 = fb
      ci = sample.int(length(fb3), 1)
      s = sample(names(fb3[[ci]]), 1)
      d = uinfo$domains[[s]]
      fb3[[ci]][[s]] = sample(d, sample.int(length(d) - 1L, 1))
      f3 = structure(fb3, universe = uinfo$universe, class = "CnfFormula")
      if (isTRUE(all.equal(f, f3)) && !identical(tt_obj(f, amat), tt_obj(f3, amat))) {
        return(fail("alleq", "all.equal TRUE for semantically different formulas", domains = uinfo$domains, f = fb, f3 = fb3))
      }
    }
    return(NULL)
  }

  # dup mode: many copies and near-copies of few base clauses
  base = gen_random_clauses(uinfo$domains, n_clauses = sample(1:3, 1), max_lit = min(3L, n_sym))
  clauses = list()
  for (r in seq_len(sample(8:30, 1))) {
    cl = base[[sample.int(length(base), 1)]]
    if (runif(1) < 0.3) {  # near-copy: tweak one range
      s = sample(names(cl), 1)
      d = uinfo$domains[[s]]
      cl[[s]] = sample(d, sample.int(length(d) - 1L, 1))
    }
    clauses[[r]] = cl
  }
  check_simplify(clauses, uinfo, extra_info = list(trial = i, mode = mode))
}

n = n_trials(20000)
cat(sprintf("exp11 misc: %d trials, seed %d\n", n, base_seed()))
failures = run_trials(n, one_trial,
  results_file = file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "results",
    sprintf("exp11_failures_seed%d.rds", base_seed())))

# --- E. constructor universe inference (deterministic, documents known bugs) --
cat("\nconstructor universe-inference status:\n")
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
atom = X %among% "a"
r1 = tryCatch({CnfClause(list(as.CnfAtom(FALSE), atom)); "works"}, error = function(e) paste("ERROR:", conditionMessage(e)))
cat("  CnfClause(list(FALSE-atom, atom)):", r1, "\n")
r2 = tryCatch({CnfClause(list(as.CnfAtom(TRUE), atom)); "works"}, error = function(e) paste("ERROR:", conditionMessage(e)))
cat("  CnfClause(list(TRUE-atom, atom)): ", r2, "\n")
r3 = tryCatch({CnfFormula(list(as.CnfClause(FALSE), as.CnfClause(atom))); "works"}, error = function(e) paste("ERROR:", conditionMessage(e)))
cat("  CnfFormula(list(FALSE-clause, clause)):", r3, "\n")
r4 = tryCatch({CnfFormula(list(as.CnfClause(TRUE), as.CnfClause(atom))); "works"}, error = function(e) paste("ERROR:", conditionMessage(e)))
cat("  CnfFormula(list(TRUE-clause, clause)): ", r4, "\n")

if (length(failures)) quit(status = 1)
