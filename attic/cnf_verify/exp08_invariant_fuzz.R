# Invariant-instrumented fuzzing of simplify_cnf.
#
# Rationale (from exp03 mutation study): several phases of simplify_cnf rely on
# internal bookkeeping invariants whose violation is only *rarely* semantically
# visible (M19-style mutants need thousands of trials to produce a wrong truth
# table). Instead of waiting for semantic visibility, this experiment patches
# hook calls into simplify_cnf (source-level, the real file is untouched) at
# consistent phase boundaries and asserts the invariants directly:
#
#   post_preprocess (after unit registration + unit propagation of the input,
#                    before the pairwise subsumption phase)
#   pre_hla         (after pairwise + 2nd-order SSE trigger, before HLA)
#   final           (before returning; matrices excluded: HLA mutates
#                    is_not_subset_of based on virtual extensions by design)
#
# Invariants:
#  I1 unit consistency: unit_registry[[s]] is a non-eliminated length-1 clause
#     on s; unit_domains[[s]] == its range; range is a nonempty proper subset
#     of the domain.
#  I2 registry exactness: symbol_registry[[s]] == indices of non-eliminated,
#     non-unit clauses containing s (each exactly once).
#  I3 propagation completeness: for every registered unit symbol s and every
#     clause C in symbol_registry[[s]]: C[[s]] strictly inside unit_domains[[s]]
#     (the load-bearing assumption of the unit-HLA donor counting).
#  I4 clause shape: non-eliminated clauses have nonempty, duplicate-free ranges
#     that are proper nonempty subsets of their domains; non-eliminated
#     non-unit clauses have >= 2 symbols; units have exactly 1.
#  I5 matrix consistency (pre_hla only): for built, valid pairs (i, j):
#     is_not_subset_of[[i]][j, k] == !(range_i[k] subseteq range_j[k]) for all
#     current symbols k of clause i (absent-in-i columns all FALSE), diagonal
#     row FALSE, and not_subset_count[i, j] == sum(is_not_subset_of[[i]][j, ]).
#
# Parameters: CNF_TRIALS (default 30000), CNF_CORES, CNF_SEED, CNF_GEN
# (standard | directed | both; default both -- alternates).
#
# Run: Rscript attic/cnf_verify/exp08_invariant_fuzz.R

source(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "harness.R"))
source_cnf()

simplify_src = paste(readLines(file.path(CNF_SRC_DIR, "CnfFormula_simplify.R")), collapse = "\n")

inject = function(src, anchor, hook_call, before = TRUE) {
  n_occ = length(gregexpr(anchor, src, fixed = TRUE)[[1]])
  if (n_occ != 1 || !grepl(anchor, src, fixed = TRUE)) stop("anchor not unique: ", anchor)
  sub(anchor, if (before) paste0(hook_call, "\n", anchor) else paste0(anchor, "\n", hook_call), src, fixed = TRUE)
}

patched = simplify_src
patched = inject(patched,
  "  # let's start with (self)-subsumption.",
  "  invariant_hook(environment(), \"post_preprocess\")")
patched = inject(patched,
  "  # Now for the big one: Asymmetric Hidden Literal Addition (Marijn et al.)",
  "  invariant_hook(environment(), \"pre_hla\")")
patched = inject(patched,
  "  return_entries(entries[!eliminated])",
  "  invariant_hook(environment(), \"final\")")
eval(parse(text = patched), envir = globalenv())

# the invariant checker; env is simplify_cnf's evaluation environment
invariant_hook = function(env, phase) {
  viol = character(0)
  entries = env$entries
  eliminated = env$eliminated
  is_unit = env$is_unit
  universe = env$universe
  symbol_registry = env$symbol_registry
  unit_registry = env$unit_registry
  unit_domains = env$unit_domains
  live = which(!eliminated)

  # I4 clause shape
  for (i in live) {
    cl = entries[[i]]
    if (!is.list(cl) || length(cl) == 0) { viol = c(viol, sprintf("I4: clause %d empty/non-list", i)); next }
    if (is_unit[[i]] && length(cl) != 1) viol = c(viol, sprintf("I4: unit %d has %d symbols", i, length(cl)))
    if (!is_unit[[i]] && length(cl) < 2 && phase != "post_preprocess")
      viol = c(viol, sprintf("I4: non-unit %d has %d symbols", i, length(cl)))
    for (s in names(cl)) {
      rng = cl[[s]]
      dom = universe[[s]]
      if (length(rng) == 0) viol = c(viol, sprintf("I4: clause %d sym %s empty range", i, s))
      if (anyDuplicated(rng)) viol = c(viol, sprintf("I4: clause %d sym %s duplicated values", i, s))
      if (!all(rng %in% dom)) viol = c(viol, sprintf("I4: clause %d sym %s out-of-domain", i, s))
      if (all(dom %in% rng)) viol = c(viol, sprintf("I4: clause %d sym %s full-domain range", i, s))
    }
  }

  # I1 unit consistency
  for (s in ls(unit_registry)) {
    ui = unit_registry[[s]]
    cl = entries[[ui]]
    if (eliminated[[ui]] && phase != "final")  # unit-HLA may eliminate units at the end
      viol = c(viol, sprintf("I1: unit_registry[%s] -> eliminated clause %d", s, ui))
    if (!eliminated[[ui]]) {
      if (!identical(names(cl), s)) viol = c(viol, sprintf("I1: unit %d not on symbol %s", ui, s))
      if (!identical(sort(unit_domains[[s]]), sort(cl[[1]]))) viol = c(viol, sprintf("I1: unit_domains[%s] != unit range", s))
      if (length(unit_domains[[s]]) == 0) viol = c(viol, sprintf("I1: unit_domains[%s] empty", s))
    }
  }

  # I2 registry exactness
  expected = new.env()
  for (i in live) {
    if (is_unit[[i]]) next
    for (s in names(entries[[i]])) assign(s, c(get0(s, expected, ifnotfound = integer(0)), i), expected)
  }
  all_syms = union(ls(symbol_registry), ls(expected))
  for (s in all_syms) {
    reg = sort(unlist(get0(s, symbol_registry, ifnotfound = integer(0))))
    exp = sort(get0(s, expected, ifnotfound = integer(0)))
    if (!identical(as.integer(reg), as.integer(exp))) {
      viol = c(viol, sprintf("I2: registry[%s] = {%s}, expected {%s}", s,
        paste(reg, collapse = ","), paste(exp, collapse = ",")))
    }
  }

  # I3 propagation completeness (the unit-HLA load-bearing invariant)
  for (s in ls(unit_registry)) {
    if (eliminated[[unit_registry[[s]]]]) next
    ud = unit_domains[[s]]
    for (i in unlist(get0(s, symbol_registry, ifnotfound = integer(0)))) {
      if (eliminated[[i]]) next
      rng = entries[[i]][[s]]
      if (is.null(rng)) next  # registry exactness is I2's job
      if (!all(rng %in% ud)) viol = c(viol, sprintf("I3: clause %d sym %s range not inside unit", i, s))
      else if (length(rng) >= length(ud)) viol = c(viol, sprintf("I3: clause %d sym %s range not PROPER subset of unit", i, s))
    }
  }

  # I5 matrix consistency (only where matrices are guaranteed current)
  if (phase == "pre_hla" && !is.null(env$is_not_subset_of)) {
    available = env$available
    inso = env$is_not_subset_of
    nsc = env$not_subset_count
    meta_valid = which(!eliminated[available] & !is_unit[available])
    for (mi in meta_valid) {
      i = available[[mi]]
      m = inso[[mi]]
      if (is.null(m)) next  # never built (eliminated before its outer pass, then revived? should not happen for valid)
      cl_i = entries[[i]]
      for (mj in meta_valid) {
        if (is.na(nsc[mi, mj])) next  # pair not built
        j = available[[mj]]
        cl_j = entries[[j]]
        row = m[mj, ]
        expected_row = logical(ncol(m))
        names(expected_row) = colnames(m)
        for (k in colnames(m)) {
          ri = cl_i[[k]]
          if (is.null(ri)) { expected_row[[k]] = FALSE; next }  # symbol gone from i
          rj = cl_j[[k]]
          expected_row[[k]] = if (mi == mj) FALSE else !(!is.null(rj) && all(ri %in% rj))
        }
        if (mi == mj) expected_row[] = FALSE
        if (!identical(unname(row), unname(expected_row))) {
          viol = c(viol, sprintf("I5: inso[[meta %d]][meta %d,] = {%s}, expected {%s} (clauses %d vs %d)",
            mi, mj, paste(as.integer(row), collapse = ""), paste(as.integer(expected_row), collapse = ""), i, j))
        }
        if (nsc[mi, mj] != sum(row)) {
          viol = c(viol, sprintf("I5: not_subset_count[%d,%d] = %d != rowsum %d", mi, mj, nsc[mi, mj], sum(row)))
        }
      }
    }
  }

  if (length(viol)) {
    stop("INVARIANT VIOLATION [", phase, "]: ", paste(viol, collapse = " || "))
  }
  invisible(NULL)
}

gen_directed_trial = function(i, seed = base_seed()) {
  set.seed(seed * 7000003L + i)
  n_sym = sample(2:4, 1)
  dom_sizes = sample(3:5, n_sym, replace = TRUE)
  while (prod(dom_sizes) > 4096) dom_sizes[which.max(dom_sizes)] = 3L
  uinfo = gen_universe(n_sym, dom_sizes, shared_values = runif(1) < 0.3)
  list(uinfo = uinfo, clauses = gen_unit_merge_hla_clauses(uinfo$domains), gen = "directed")
}

gen_mode = Sys.getenv("CNF_GEN", "both")
one_trial = function(i) {
  use_directed = switch(gen_mode, standard = FALSE, directed = TRUE, both = i %% 2L == 0L)
  trial = if (use_directed) gen_directed_trial(i) else gen_standard_trial(i)
  # invariant violations arrive as errors from the hook; semantic/structural
  # failures are still checked as usual
  check_simplify(trial$clauses, trial$uinfo, extra_info = list(trial = i, gen = trial$gen))
}

n = n_trials(30000)
cat(sprintf("exp08 invariant-instrumented fuzz: %d trials, gen=%s, seed %d\n", n, gen_mode, base_seed()))
failures = run_trials(n, one_trial,
  results_file = file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "results",
    sprintf("exp08_failures_%s_seed%d.rds", gen_mode, base_seed())))
if (length(failures)) quit(status = 1)
