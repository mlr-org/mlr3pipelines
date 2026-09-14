# Independent branch-trace check for the value-refinement claim.
# Run at repository root: Rscript attic/cnf_verify3/representation/trace_refinement.R
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
for (nm in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(nm, ".R")))
}

trace_buffer = new.env(parent = emptyenv())
trace_buffer$events = character()
branch_decision = function(label, value) {
  force(value)
  trace_buffer$events[[length(trace_buffer$events) + 1L]] = paste(label, paste(value, collapse = ","))
  value
}
mode = Sys.getenv("CNF_REP_MODE", "refinement")
stopifnot(mode %in% c("refinement", "duplicates"))
trace_calls = 0L
instrument = function(expr, owner) {
  if (!is.call(expr)) return(expr)
  if (identical(expr[[1L]], as.name("if"))) {
    trace_calls <<- trace_calls + 1L
    label = paste0(owner, ":", trace_calls)
    expr[[2L]] = as.call(list(as.name("branch_decision"), label, expr[[2L]]))
  }
  # A function's argument pairlist is not changed; only executable call trees.
  for (i in seq_along(expr)[-1L]) if (is.call(expr[[i]])) expr[[i]] = instrument(expr[[i]], owner)
  expr
}
for (nm in c("CnfAtom", "CnfClause", "CnfFormula", "simplify_cnf")) {
  fn = get(nm)
  body(fn) = instrument(body(fn), nm)
  assign(nm, fn)
}

make_formula = function(domains, clauses) {
  u = CnfUniverse()
  syms = lapply(names(domains), function(s) CnfSymbol(u, s, domains[[s]]))
  names(syms) = names(domains)
  trace_buffer$events = character()
  objects = lapply(clauses, function(clause) {
    CnfClause(lapply(names(clause), function(s) CnfAtom(syms[[s]], clause[[s]])))
  })
  f = CnfFormula(objects)
  list(formula = c(f), trace = trace_buffer$events)
}

lift_input = function(domains, clauses) {
  maps = lapply(seq_along(domains), function(si) {
    domain = domains[[si]]
    out = lapply(seq_along(domain), function(vi) {
      if (mode == "duplicates") rep(domain[[vi]], sample.int(9L, 1L)) else {
        paste0("sym", si, "_value", vi, "_part", seq_len(sample.int(9L, 1L)))
      }
    })
    names(out) = domain
    out
  })
  names(maps) = names(domains)
  new_domains = lapply(maps, function(m) sample(unlist(m, use.names = FALSE)))
  new_clauses = lapply(clauses, function(clause) {
    out = lapply(names(clause), function(s) {
      result = unlist(maps[[s]][clause[[s]]], use.names = FALSE)
      if (length(result)) sample(result) else character()
    })
    names(out) = names(clause)
    out
  })
  list(domains = new_domains, clauses = new_clauses, maps = maps)
}
lift_formula = function(formula, maps) {
  if (is.logical(formula)) return(formula)
  lapply(formula, function(clause) {
    out = lapply(names(clause), function(s) unlist(maps[[s]][clause[[s]]], use.names = FALSE))
    names(out) = names(clause)
    out
  })
}
canon = function(formula) {
  if (is.logical(formula)) return(unname(formula))
  # Preserve clause and symbol order: these are part of the control claim.
  lapply(formula, function(clause) lapply(clause, function(range) sort(unique(unname(range)))))
}

set.seed(610061L)
n_trials = as.integer(Sys.getenv("CNF_REP_TRIALS", "3000"))
failures = list()
n_branches = 0L
for (trial in seq_len(n_trials)) {
  degenerate = trial %% 5L == 0L
  n_symbols = if (degenerate) sample.int(5L, 1L) else sample(2:5, 1L)
  domains = lapply(seq_len(n_symbols), function(i) {
    paste0("v", seq_len(if (degenerate) sample.int(8L, 1L) else sample(2:8, 1L)))
  })
  names(domains) = paste0("X", seq_len(n_symbols))
  clauses = lapply(seq_len(sample.int(12L, 1L)), function(ci) {
    symbols = sample(names(domains), sample.int(n_symbols, 1L))
    ranges = lapply(symbols, function(s) {
      size = if (degenerate) sample.int(length(domains[[s]]) + 1L, 1L) - 1L else sample.int(length(domains[[s]]) - 1L, 1L)
      sample(domains[[s]], size)
    })
    names(ranges) = symbols
    ranges
  })
  base = make_formula(domains, clauses)
  lifted = lift_input(domains, clauses)
  refined = make_formula(lifted$domains, lifted$clauses)
  if (!identical(base$trace, refined$trace) || !identical(canon(lift_formula(base$formula, lifted$maps)), canon(refined$formula))) {
    failures[[length(failures) + 1L]] = list(trial = trial, domains = domains, clauses = clauses,
      base = base, lifted = lifted, refined = refined)
    stop("Refinement discrepancy in trial ", trial)
  }
  n_branches = n_branches + length(base$trace)
  if (trial %% 250L == 0L) cat("trials=", trial, " branch_decisions=", n_branches, " failures=", length(failures), "\n", sep = "")
}
cat("PASS: ", mode, "; ", n_trials, " transformations; ", n_branches, " matched branch decisions; ", trace_calls, " instrumented condition sites\n", sep = "")
saveRDS(list(mode = mode, trials = n_trials, branches = n_branches, condition_sites = trace_calls,
  failures = failures, R_version = R.version.string), paste0("attic/cnf_verify3/representation/trace_", mode, "_results.rds"))
