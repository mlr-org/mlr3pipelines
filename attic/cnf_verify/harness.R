# Shared harness for CNF correctness verification experiments.
#
# Everything here is written *independently* from the code under test and from
# the earlier attic/cnf/experiments harnesses, so that it can serve as an
# independent oracle: formulas are evaluated by brute-force truth tables using
# only the documented bare data representation (named list of clauses; clause =
# named list mapping symbol -> character vector of allowed values; logical
# scalar TRUE/FALSE for tautology/contradiction).
#
# Usage from an experiment script:
#   source(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "harness.R"))
#   source_cnf()
#
# Conventions:
# - all generators take an explicit `rng` seed or assume set.seed() was called.
# - experiments are parameterized via environment variables (with defaults),
#   so they can be scaled up on bigger machines:
#     CNF_TRIALS, CNF_CORES, CNF_SEED, CNF_TIMEOUT (per-trial seconds)

CNF_SRC_DIR = Sys.getenv("CNF_SRC_DIR", "R")  # relative to repo root by default

source_cnf = function(dir = CNF_SRC_DIR) {
  suppressMessages({
    library(checkmate)
    library(mlr3misc)
  })
  for (f in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
    source(file.path(dir, paste0(f, ".R")))
  }
}

n_cores = function() as.integer(Sys.getenv("CNF_CORES", "16"))
n_trials = function(default) as.integer(Sys.getenv("CNF_TRIALS", as.character(default)))
base_seed = function() as.integer(Sys.getenv("CNF_SEED", "1"))
trial_timeout = function() as.numeric(Sys.getenv("CNF_TIMEOUT", "120"))

###############################
## Independent oracle        ##
###############################

# domains: named list symbol -> character vector (the full domain)
# returns a data.frame of all assignments, one column per symbol (character)
all_assignments = function(domains) {
  stopifnot(length(domains) >= 1)
  do.call(expand.grid, c(lapply(domains, identity), list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)))
}

# evaluate one bare clause (named list sym -> values) on all assignments at once.
# amat: data.frame from all_assignments. Returns logical vector nrow(amat).
tt_clause = function(clause_bare, amat) {
  if (isTRUE(clause_bare)) return(rep(TRUE, nrow(amat)))
  if (isFALSE(clause_bare)) return(rep(FALSE, nrow(amat)))
  res = rep(FALSE, nrow(amat))
  for (sym in names(clause_bare)) {
    res = res | (amat[[sym]] %in% clause_bare[[sym]])
  }
  res
}

# evaluate a list of bare clauses (conjunction) on all assignments.
tt_clauses = function(clauses_bare, amat) {
  res = rep(TRUE, nrow(amat))
  for (cl in clauses_bare) {
    res = res & tt_clause(cl, amat)
  }
  res
}

# evaluate any Cnf* object (or bare logical) on all assignments
tt_obj = function(x, amat) {
  if (inherits(x, "CnfAtom")) {
    b = unclass(x)
    if (is.logical(b)) return(rep(c(b), nrow(amat)))
    return(amat[[b$symbol]] %in% b$values)
  }
  if (inherits(x, "CnfClause")) {
    b = unclass(x)
    if (is.logical(b)) return(rep(c(b), nrow(amat)))
    return(tt_clause(b, amat))
  }
  if (inherits(x, "CnfFormula")) {
    b = unclass(x)
    if (is.logical(b)) return(rep(c(b), nrow(amat)))
    return(tt_clauses(b, amat))
  }
  if (is.logical(x) && length(x) == 1) return(rep(c(x), nrow(amat)))
  stop("tt_obj: unsupported object")
}

###############################
## Structural invariants     ##
###############################

# check structural well-formedness of a CnfFormula result.
# domains: named list symbol -> full domain.
# returns character(0) if fine, else a vector of violation descriptions.
check_formula_structure = function(f, domains) {
  probs = character(0)
  if (!inherits(f, "CnfFormula")) return("not a CnfFormula")
  b = unclass(f)
  if (is.logical(b)) {
    if (length(b) != 1 || is.na(b)) probs = c(probs, "logical formula not scalar TRUE/FALSE")
    return(probs)
  }
  if (!is.list(b)) return("formula neither logical nor list")
  if (length(b) == 0) probs = c(probs, "empty clause list (should be logical TRUE)")
  for (i in seq_along(b)) {
    cl = b[[i]]
    if (!is.list(cl) || length(cl) == 0) {
      probs = c(probs, sprintf("clause %d: not a nonempty list", i))
      next
    }
    ns = names(cl)
    if (is.null(ns) || any(ns == "") || anyDuplicated(ns)) {
      probs = c(probs, sprintf("clause %d: bad/duplicated symbol names", i))
      next
    }
    for (sym in ns) {
      rng = cl[[sym]]
      if (!is.character(rng) || length(rng) == 0) {
        probs = c(probs, sprintf("clause %d, symbol %s: empty or non-character range", i, sym))
        next
      }
      if (anyDuplicated(rng)) probs = c(probs, sprintf("clause %d, symbol %s: duplicated values", i, sym))
      if (!all(rng %in% domains[[sym]])) probs = c(probs, sprintf("clause %d, symbol %s: values outside domain", i, sym))
      if (all(domains[[sym]] %in% rng)) probs = c(probs, sprintf("clause %d, symbol %s: full-domain (tautological) range", i, sym))
    }
  }
  # no output clause may be subsumed by another output clause (covers exact
  # duplicates too). The pairwise phase eliminates subsumed pairs among
  # non-units, and unit propagation (incl. at unit merges, since the
  # 2026-07 register_unit fix) subsumes clauses whose range on a unit symbol
  # is not a strict subset of the unit range.
  subsumes = function(a, b) {
    all(names(a) %in% names(b)) && all(vapply(names(a), function(s) all(a[[s]] %in% b[[s]]), NA))
  }
  for (i in seq_along(b)) {
    for (j in seq_along(b)) {
      if (i != j && subsumes(b[[i]], b[[j]])) {
        probs = c(probs, sprintf("output clause %d is subsumed by output clause %d", j, i))
      }
    }
  }
  probs
}

###############################
## Generators                ##
###############################

# create a universe with n_sym symbols; dom_sizes recycled to n_sym.
# value names deliberately overlap between symbols sometimes (they may:
# ranges are always interpreted per-symbol).
gen_universe = function(n_sym, dom_sizes, shared_values = FALSE) {
  u = CnfUniverse()
  dom_sizes = rep_len(dom_sizes, n_sym)
  domains = list()
  syms = list()
  for (i in seq_len(n_sym)) {
    nm = paste0("V", i)
    vals = if (shared_values) {
      paste0("x", seq_len(dom_sizes[i]))
    } else {
      paste0("v", i, "_", seq_len(dom_sizes[i]))
    }
    syms[[nm]] = CnfSymbol(u, nm, vals)
    domains[[nm]] = vals
  }
  list(universe = u, domains = domains, syms = syms)
}

# random bare clause: named list of symbol -> proper nonempty subset of domain.
# Contract (as guaranteed by the public CnfClause/CnfAtom API): ranges are
# nonempty *proper* subsets of the domain. Symbols with domain size 1 can thus
# never occur inside a clause (their only atoms are TRUE and FALSE).
# n_lit: number of symbols in the clause
gen_bare_clause = function(domains, n_lit = NULL, max_lit = 3L) {
  eligible = names(domains)[lengths(domains) >= 2L]
  stopifnot(length(eligible) >= 1L)
  if (is.null(n_lit)) n_lit = sample.int(min(length(eligible), max_lit), 1)
  n_lit = min(n_lit, length(eligible))
  chosen = sample(eligible, n_lit)
  cl = list()
  for (sym in chosen) {
    d = domains[[sym]]
    k = sample.int(length(d) - 1L, 1)
    cl[[sym]] = sample(d, k)
  }
  cl
}

bare_to_clause = function(cl, universe) {
  structure(cl, universe = universe, class = "CnfClause")
}

# random redundancy-rich clause set: base clauses plus clauses *implied* by them
# (supersets and resolvents), to exercise the elimination machinery.
gen_redundant_clauses = function(domains, n_base = 3L, n_extra = 3L, max_lit = 3L) {
  base = replicate(n_base, gen_bare_clause(domains, max_lit = max_lit), simplify = FALSE)
  extras = list()
  guard = 0
  while (length(extras) < n_extra && guard < 50 * n_extra) {
    guard = guard + 1
    mode = sample(c("superset", "resolvent"), 1)
    cand = NULL
    if (mode == "superset") {
      cl = base[[sample.int(length(base), 1)]]
      # widen: add a symbol and/or widen a range
      if (runif(1) < 0.7) {
        missing_syms = setdiff(names(domains)[lengths(domains) >= 2L], names(cl))
        if (length(missing_syms)) {
          sym = sample(missing_syms, 1)
          d = domains[[sym]]
          cl[[sym]] = sample(d, sample.int(length(d) - 1L, 1))
        }
      }
      sym = sample(names(cl), 1)
      d = domains[[sym]]
      extra_vals = setdiff(d, cl[[sym]])
      if (length(extra_vals) > 1) {
        cl[[sym]] = c(cl[[sym]], sample(extra_vals, sample.int(length(extra_vals) - 1L, 1)))
      }
      # drop if any range became full domain (would be tautology)
      if (!any(vapply(names(cl), function(s) all(domains[[s]] %in% cl[[s]]), NA))) cand = cl
    } else {
      i = sample.int(length(base), 1); j = sample.int(length(base), 1)
      c1 = base[[i]]; c2 = base[[j]]
      shared = intersect(names(c1), names(c2))
      if (length(shared) && i != j) {
        s = sample(shared, 1)
        isct = intersect(c1[[s]], c2[[s]])
        cand = list()
        for (sym in union(names(c1), names(c2))) {
          vals = unique(c(c1[[sym]], c2[[sym]]))
          if (sym == s) vals = isct
          if (length(vals)) cand[[sym]] = vals
        }
        if (length(cand) == 0 ||
            any(vapply(names(cand), function(x) all(domains[[x]] %in% cand[[x]]), NA))) {
          cand = NULL  # empty resolvent (contradictory base) or tautology; skip
        }
      }
    }
    if (!is.null(cand)) extras[[length(extras) + 1L]] = cand
  }
  out = c(base, extras)
  out[sample.int(length(out))]
}

# unit-heavy clause set
gen_unit_heavy_clauses = function(domains, n_units = 2L, n_other = 3L, max_lit = 3L) {
  units = replicate(n_units, gen_bare_clause(domains, n_lit = 1L), simplify = FALSE)
  other = replicate(n_other, gen_bare_clause(domains, max_lit = max_lit), simplify = FALSE)
  out = c(units, other)
  out[sample.int(length(out))]
}

# subset-chain clause set: clauses form nested chains on shared symbols to
# trigger cascades of (self-)subsumption bookkeeping
gen_chain_clauses = function(domains, n_chain = 4L, max_lit = 3L) {
  eligible = names(domains)[lengths(domains) >= 2L]
  syms = sample(eligible, min(length(eligible), max_lit))
  # base ranges
  cl = list()
  for (sym in syms) {
    d = domains[[sym]]
    cl[[sym]] = sample(d, 1)
  }
  out = list(cl)
  for (i in seq_len(n_chain - 1L)) {
    prev = out[[length(out)]]
    nxt = prev
    sym = sample(names(nxt), 1)
    extra = setdiff(domains[[sym]], nxt[[sym]])
    if (length(extra) > 1) {
      nxt[[sym]] = c(nxt[[sym]], sample(extra, 1))
    } else if (runif(1) < 0.5 && length(nxt) > 1) {
      nxt[[sym]] = NULL
    }
    # perturb: sometimes replace a range instead
    if (runif(1) < 0.3) {
      sym2 = sample(names(domains)[lengths(domains) >= 2L], 1)
      d = domains[[sym2]]
      nxt[[sym2]] = sample(d, sample.int(length(d) - 1L, 1))
    }
    if (length(nxt) && !any(vapply(names(nxt), function(s) all(domains[[s]] %in% nxt[[s]]), NA))) {
      out[[length(out) + 1L]] = nxt
    }
  }
  out[sample.int(length(out))]
}

# uniform random clause set
gen_random_clauses = function(domains, n_clauses, max_lit = 3L) {
  replicate(n_clauses, gen_bare_clause(domains, max_lit = max_lit), simplify = FALSE)
}

# directed generator: unit merge chains + partial overlaps + unit-HLA donors.
# Motivated by mutation testing (exp03): the unit-HLA phase relies on the
# invariant that every clause in symbol_registry[[unitsymbol]] has its range on
# that symbol inside the unit range (established by unit propagation). Mutants
# that break propagation in the use_inso skip path (M14/M19) take thousands of
# standard trials to kill; this generator concentrates on the triggering regime:
# - a "unit symbol" nu with several (possibly conflicting-free) units on it,
# - clauses whose nu-ranges are subsets / equal / partially overlapping,
# - clauses designed to *become* units on nu during simplification
#   (their second symbol gets emptied by another unit),
# - 2-symbol unit-HLA donor clauses.
gen_unit_merge_hla_clauses = function(domains) {
  eligible = names(domains)[lengths(domains) >= 3L]
  if (!length(eligible)) eligible = names(domains)[lengths(domains) >= 2L]
  nu = sample(eligible, 1)
  d_nu = domains[[nu]]
  others = setdiff(names(domains)[lengths(domains) >= 2L], nu)
  clauses = list()
  # explicit units on nu, ranges overlapping but usually not nested
  n_units = sample(1:2, 1)
  for (i in seq_len(n_units)) {
    k = sample.int(length(d_nu) - 1L, 1)
    clauses[[length(clauses) + 1L]] = structure(list(sample(d_nu, max(k, min(2, length(d_nu) - 1L)))), names = nu)
  }
  # clauses that become units on nu mid-simplification: (nu in R | s2 in T)
  # plus a unit on s2 that is disjoint from T
  if (length(others)) {
    for (i in seq_len(sample(1:2, 1))) {
      s2 = sample(others, 1)
      d2 = domains[[s2]]
      t_size = sample.int(length(d2) - 1L, 1)
      T2 = sample(d2, t_size)
      rest = setdiff(d2, T2)
      clauses[[length(clauses) + 1L]] = structure(
        list(sample(d_nu, sample.int(length(d_nu) - 1L, 1)), T2), names = c(nu, s2))
      if (length(rest) && runif(1) < 0.8) {
        # unit on s2 disjoint from T2 (or partially overlapping sometimes)
        r_size = sample.int(max(length(rest) - (length(rest) == length(d2) - t_size && length(rest) > 1), 1), 1)
        clauses[[length(clauses) + 1L]] = structure(list(sample(rest, r_size)), names = s2)
      }
    }
  }
  # unit-HLA donor clauses: (nu in R | s3 in T) with varied nu-range relations
  if (length(others)) {
    for (i in seq_len(sample(1:3, 1))) {
      s3 = sample(others, 1)
      d3 = domains[[s3]]
      clauses[[length(clauses) + 1L]] = structure(
        list(sample(d_nu, sample.int(length(d_nu) - 1L, 1)),
             sample(d3, sample.int(length(d3) - 1L, 1))), names = c(nu, s3))
    }
  }
  # a couple of generic clauses for noise
  for (i in seq_len(sample(0:2, 1))) {
    clauses[[length(clauses) + 1L]] = gen_bare_clause(domains, max_lit = min(3L, sum(lengths(domains) >= 2L)))
  }
  clauses[sample.int(length(clauses))]
}

# standard random trial input, shared by exp02 (baseline fuzz) and exp03
# (mutation testing) so that mutation kill rates measure the power of the
# actual baseline fuzzer distribution.
gen_standard_trial = function(i, seed = base_seed()) {
  set.seed(seed * 1000003L + i)
  n_sym = sample(2:6, 1)
  dom_sizes = sample(1:5, n_sym, replace = TRUE)
  while (prod(dom_sizes) > 4096) dom_sizes[which.max(dom_sizes)] = 2L
  if (all(dom_sizes < 2)) dom_sizes[sample.int(n_sym, 1)] = 2L
  uinfo = gen_universe(n_sym, dom_sizes, shared_values = runif(1) < 0.3)
  gen = sample(c("random", "redundant", "unit_heavy", "chain", "mixed"), 1)
  max_lit = sample(2:min(4, n_sym), 1)
  clauses = switch(gen,
    random = gen_random_clauses(uinfo$domains, n_clauses = sample(1:10, 1), max_lit = max_lit),
    redundant = gen_redundant_clauses(uinfo$domains, n_base = sample(2:5, 1), n_extra = sample(1:5, 1), max_lit = max_lit),
    unit_heavy = gen_unit_heavy_clauses(uinfo$domains, n_units = sample(1:3, 1), n_other = sample(1:5, 1), max_lit = max_lit),
    chain = gen_chain_clauses(uinfo$domains, n_chain = sample(3:7, 1), max_lit = max_lit),
    mixed = c(
      gen_random_clauses(uinfo$domains, n_clauses = sample(1:4, 1), max_lit = max_lit),
      gen_redundant_clauses(uinfo$domains, n_base = 2L, n_extra = sample(1:3, 1), max_lit = max_lit),
      if (runif(1) < 0.5) gen_unit_heavy_clauses(uinfo$domains, n_units = 1L, n_other = 1L, max_lit = max_lit)
    )
  )
  list(uinfo = uinfo, clauses = clauses, gen = gen)
}

###############################
## Checking                  ##
###############################

# core check: build CnfFormula from bare clauses; compare truth table against
# direct evaluation of the input clause list; check structural invariants.
# Returns NULL if OK, else a diagnostic list.
check_simplify = function(clauses_bare, uinfo, extra_info = NULL) {
  amat = all_assignments(uinfo$domains)
  tt_in = tt_clauses(clauses_bare, amat)
  clause_objs = lapply(clauses_bare, bare_to_clause, universe = uinfo$universe)
  f = tryCatch({
    setTimeLimit(elapsed = trial_timeout(), transient = TRUE)
    on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
    CnfFormula(clause_objs)
  }, error = function(e) e)
  if (inherits(f, "error")) {
    return(list(kind = "error", message = conditionMessage(f),
      domains = uinfo$domains, clauses = clauses_bare, extra = extra_info))
  }
  tt_out = tt_obj(f, amat)
  if (!identical(tt_in, tt_out)) {
    return(list(kind = "semantic", message = sprintf("%d/%d assignments differ", sum(tt_in != tt_out), length(tt_in)),
      domains = uinfo$domains, clauses = clauses_bare, result = unclass(f), extra = extra_info))
  }
  probs = check_formula_structure(f, uinfo$domains)
  if (length(probs)) {
    return(list(kind = "structure", message = paste(probs, collapse = "; "),
      domains = uinfo$domains, clauses = clauses_bare, result = unclass(f), extra = extra_info))
  }
  NULL
}

###############################
## Runner                    ##
###############################

# run one_trial(trial_idx) for trials 1..n in parallel; one_trial must return
# NULL (ok) or a diagnostic list. Prints summary, saves failures, returns them.
run_trials = function(n, one_trial, results_file = NULL, cores = n_cores()) {
  results = parallel::mclapply(seq_len(n), function(i) {
    tryCatch(one_trial(i), error = function(e) {
      list(kind = "harness_error", message = conditionMessage(e), trial = i)
    })
  }, mc.cores = cores, mc.preschedule = TRUE)
  failures = Filter(Negate(is.null), results)
  # tag trial index
  cat(sprintf("trials: %d, failures: %d\n", n, length(failures)))
  if (length(failures)) {
    for (f in head(failures, 5)) {
      cat(sprintf("  [%s] %s\n", f$kind, f$message))
    }
    if (!is.null(results_file)) {
      saveRDS(failures, results_file)
      cat(sprintf("  full failure list saved to %s\n", results_file))
    }
  }
  invisible(failures)
}

# reproduce a failure record: returns the reconstructed formula for interactive poking
reproduce_failure = function(rec) {
  u = CnfUniverse()
  for (nm in names(rec$domains)) CnfSymbol(u, nm, rec$domains[[nm]])
  clause_objs = lapply(rec$clauses, function(cl) structure(cl, universe = u, class = "CnfClause"))
  CnfFormula(clause_objs)
}
