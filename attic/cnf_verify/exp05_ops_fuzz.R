# Operator algebra fuzzing: random expression trees over CnfAtom leaves with
# &, |, ! plus public constructors (%among%, CnfClause, CnfFormula, as.*),
# verified against an independently computed truth table.
#
# Note on R < 4.3: mixed-class Ops dispatch (atom & clause) errors there, so we
# dispatch to the S3 methods ourselves exactly like chooseOpsMethod would on
# R >= 4.3 (the method of the first Cnf* operand wins; all Cnf* methods
# delegate consistently).
#
# Also checks logical identities on random formulas: double negation,
# De Morgan, idempotence, complementation, distributivity, absorption, and
# as.list round-trips.
#
# Parameters: CNF_TRIALS (default 20000), CNF_CORES, CNF_SEED.
#
# Run: Rscript attic/cnf_verify/exp05_ops_fuzz.R

source(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "harness.R"))
source_cnf()

# --- dispatch helpers (mimic R >= 4.3 chooseOpsMethod resolution) -----------
first_cnf_class = function(a, b) {
  for (x in list(a, b)) {
    for (cl in c("CnfAtom", "CnfClause", "CnfFormula")) if (inherits(x, cl)) return(cl)
  }
  stop("no Cnf operand")
}
op_and = function(a, b) switch(first_cnf_class(a, b),
  CnfAtom = `&.CnfAtom`(a, b), CnfClause = `&.CnfClause`(a, b), CnfFormula = `&.CnfFormula`(a, b))
op_or = function(a, b) switch(first_cnf_class(a, b),
  CnfAtom = `|.CnfAtom`(a, b), CnfClause = `|.CnfClause`(a, b), CnfFormula = `|.CnfFormula`(a, b))
op_not = function(a) {
  if (inherits(a, "CnfAtom")) `!.CnfAtom`(a)
  else if (inherits(a, "CnfClause")) `!.CnfClause`(a)
  else `!.CnfFormula`(a)
}

# --- random expression trees ------------------------------------------------
# returns list(obj = Cnf* object, tt = expected truth table)
gen_expr = function(depth, uinfo, amat) {
  syms = names(uinfo$domains)[lengths(uinfo$domains) >= 2L]
  if (depth <= 0 || runif(1) < 0.25) {
    # leaf: random atom; occasionally TRUE/FALSE atom
    r = runif(1)
    if (r < 0.06) {
      obj = as.CnfAtom(TRUE)
    } else if (r < 0.12) {
      obj = as.CnfAtom(FALSE)
    } else {
      sym = sample(syms, 1)
      d = uinfo$domains[[sym]]
      vals = sample(d, sample.int(length(d), 1))  # may be full domain (-> TRUE atom)
      if (runif(1) < 0.1) vals = character(0)     # FALSE atom
      obj = CnfAtom(uinfo$syms[[sym]], vals)
    }
    return(list(obj = obj, tt = tt_obj(obj, amat)))
  }
  op = sample(c("and", "or", "not"), 1, prob = c(0.4, 0.4, 0.2))
  if (op == "not") {
    sub = gen_expr(depth - 1L, uinfo, amat)
    # guard against negation blow-up: only negate small formulas
    if (inherits(sub$obj, "CnfFormula") && !is.logical(unclass(sub$obj)) && length(unclass(sub$obj)) > 4) {
      return(sub)
    }
    return(list(obj = op_not(sub$obj), tt = !sub$tt))
  }
  l = gen_expr(depth - 1L, uinfo, amat)
  r = gen_expr(depth - 1L, uinfo, amat)
  if (op == "and") list(obj = op_and(l$obj, r$obj), tt = l$tt & r$tt)
  else list(obj = op_or(l$obj, r$obj), tt = l$tt | r$tt)
}

check_tt = function(obj, expected_tt, amat, domains, what, ctx) {
  got = tt_obj(obj, amat)
  if (!identical(unname(got), unname(expected_tt))) {
    return(list(kind = "semantic", message = sprintf("%s: %d/%d assignments differ", what, sum(got != expected_tt), length(got)), ctx = ctx))
  }
  if (inherits(obj, "CnfFormula")) {
    probs = check_formula_structure(obj, domains)
    if (length(probs)) return(list(kind = "structure", message = paste0(what, ": ", paste(probs, collapse = "; ")), ctx = ctx))
  }
  NULL
}

one_trial = function(i) {
  set.seed(base_seed() * 2000003L + i)
  n_sym = sample(2:5, 1)
  dom_sizes = sample(2:4, n_sym, replace = TRUE)
  while (prod(dom_sizes) > 2048) dom_sizes[which.max(dom_sizes)] = 2L
  uinfo = gen_universe(n_sym, dom_sizes, shared_values = runif(1) < 0.3)
  amat = all_assignments(uinfo$domains)
  ctx = list(trial = i, domains = uinfo$domains)

  mode = sample(c("tree", "identities", "constructors"), 1)

  if (mode == "tree") {
    expr = tryCatch(gen_expr(sample(2:7, 1), uinfo, amat), error = function(e) e)
    if (inherits(expr, "error")) return(list(kind = "error", message = conditionMessage(expr), ctx = ctx))
    return(check_tt(expr$obj, expr$tt, amat, uinfo$domains, "expr tree", ctx))
  }

  if (mode == "identities") {
    mk = function(n_cl) {
      cls = gen_random_clauses(uinfo$domains, n_clauses = n_cl, max_lit = min(3L, n_sym))
      CnfFormula(lapply(cls, bare_to_clause, universe = uinfo$universe))
    }
    f = mk(sample(1:4, 1)); g = mk(sample(1:4, 1)); h = mk(sample(1:3, 1))
    tf = tt_obj(f, amat); tg = tt_obj(g, amat); th = tt_obj(h, amat)
    checks = list(
      list(op_not(op_not(f)), tf, "!!f"),
      list(op_not(op_and(f, g)), !(tf & tg), "!(f&g) De Morgan"),
      list(op_not(op_or(f, g)), !(tf | tg), "!(f|g) De Morgan"),
      list(op_and(f, f), tf, "f&f idempotent"),
      list(op_or(f, f), tf, "f|f idempotent"),
      list(op_and(f, op_not(f)), tf & !tf, "f&!f contradiction"),
      list(op_or(f, op_not(f)), tf | !tf, "f|!f tautology"),
      list(op_or(op_and(f, g), h), (tf & tg) | th, "(f&g)|h distribute"),
      list(op_and(op_or(f, g), f), (tf | tg) & tf, "(f|g)&f absorption"),
      list(op_and(f, op_and(g, h)), tf & tg & th, "f&(g&h)"),
      list(op_or(f, op_or(g, h)), tf | tg | th, "f|(g|h)")
    )
    for (ch in checks) {
      rec = check_tt(ch[[1]], ch[[2]], amat, uinfo$domains, ch[[3]], ctx)
      if (!is.null(rec)) return(rec)
    }
    # as.list round trip (formula)
    lst = as.list(f)
    f2 = if (length(lst)) CnfFormula(lst) else as.CnfFormula(TRUE)
    return(check_tt(f2, tf, amat, uinfo$domains, "as.list round trip", ctx))
  }

  # constructors mode
  syms2 = names(uinfo$domains)[lengths(uinfo$domains) >= 2L]
  n_at = sample(1:5, 1)
  atoms = lapply(seq_len(n_at), function(j) {
    sym = sample(syms2, 1)
    d = uinfo$domains[[sym]]
    vals = sample(d, sample.int(length(d), 1))
    if (runif(1) < 0.08) vals = character(0)
    CnfAtom(uinfo$syms[[sym]], vals)
  })
  tt_atoms = lapply(atoms, tt_obj, amat = amat)
  # CnfClause from atoms (disjunction)
  cl = CnfClause(atoms)
  rec = check_tt(cl, Reduce(`|`, tt_atoms), amat, uinfo$domains, "CnfClause(atoms)", ctx)
  if (!is.null(rec)) return(rec)
  # CnfClause from mixed atoms and clauses
  cl2 = CnfClause(list(atoms[[1]], cl))
  rec = check_tt(cl2, tt_atoms[[1]] | tt_obj(cl, amat), amat, uinfo$domains, "CnfClause(mixed)", ctx)
  if (!is.null(rec)) return(rec)
  # negation of atom / clause
  rec = check_tt(op_not(atoms[[1]]), !tt_atoms[[1]], amat, uinfo$domains, "!atom", ctx)
  if (!is.null(rec)) return(rec)
  rec = check_tt(op_not(cl), !tt_obj(cl, amat), amat, uinfo$domains, "!clause", ctx)
  if (!is.null(rec)) return(rec)
  # CnfFormula from list of formulas (conjunction); avoid known bug #3/#4
  # configurations (logical formulas inside lists) -- those are documented.
  fs = lapply(seq_len(sample(2:3, 1)), function(j) {
    cls = gen_random_clauses(uinfo$domains, n_clauses = sample(1:3, 1), max_lit = min(3L, n_sym))
    CnfFormula(lapply(cls, bare_to_clause, universe = uinfo$universe))
  })
  fs_nonlogical = Filter(function(f) !is.logical(unclass(f)), fs)
  if (length(fs_nonlogical) >= 2) {
    ff = CnfFormula(fs_nonlogical)
    rec = check_tt(ff, Reduce(`&`, lapply(fs_nonlogical, tt_obj, amat = amat)), amat, uinfo$domains, "CnfFormula(formulas)", ctx)
    if (!is.null(rec)) return(rec)
  }
  # clause subsetting: dropping symbols must equal the sub-clause semantics
  cl_bare = unclass(cl)
  if (!is.logical(cl_bare) && length(cl_bare) >= 2) {
    keep = sample(names(cl_bare), sample.int(length(cl_bare) - 1L, 1))
    sub_cl = cl[keep]
    rec = check_tt(sub_cl, tt_clause(cl_bare[keep], amat), amat, uinfo$domains, "clause[keep]", ctx)
    if (!is.null(rec)) return(rec)
  }
  NULL
}

n = n_trials(20000)
cat(sprintf("exp05 operator fuzz: %d trials, seed %d\n", n, base_seed()))
failures = run_trials(n, one_trial,
  results_file = file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "results",
    sprintf("exp05_failures_seed%d.rds", base_seed())))
if (length(failures)) quit(status = 1)
