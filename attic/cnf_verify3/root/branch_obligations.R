# Enumerate conditional sites in an in-memory copy of the actual kernel.
# Record both outcomes and their first exact input, while checking that the
# observer leaves each returned object identical to the unmodified function.
suppressPackageStartupMessages({
  library(checkmate)
  library(mlr3misc)
  library(jsonlite)
})
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}

out_dir = Sys.getenv("CNF_BRANCH_OUT", "attic/cnf_verify3/root")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
profile = Sys.getenv("CNF_BRANCH_PROFILE", "mixed")
stopifnot(profile %in% c("mixed", "dense"))
plain_simplify = simplify_cnf
sites = list()
instrument_conditions = function(expr, owner = "simplify_cnf") {
  if (!is.call(expr)) return(expr)
  if (identical(expr[[1L]], as.name("=")) && length(expr) == 3L &&
      is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], as.name("function"))) {
    expr[[3L]] = instrument_conditions(expr[[3L]], as.character(expr[[2L]]))
    return(expr)
  }
  if (identical(expr[[1L]], as.name("if"))) {
    id = length(sites) + 1L
    sites[[id]] <<- list(id = id, owner = owner,
      condition = paste(deparse(expr[[2L]], width.cutoff = 500L), collapse = " "))
    expr[[2L]] = as.call(list(as.name("branch_obligation_decision"), id, expr[[2L]]))
  }
  for (i in seq_along(expr)[-1L]) {
    if (is.call(expr[[i]])) expr[[i]] = instrument_conditions(expr[[i]], owner)
  }
  expr
}
observed_simplify = plain_simplify
body(observed_simplify) = instrument_conditions(body(observed_simplify))
observations = new.env(parent = emptyenv())
observations$counts = matrix(0, nrow = length(sites), ncol = 2L,
  dimnames = list(NULL, c("false", "true")))
observations$first = vector("list", length(sites) * 2L)
observations$case = NULL
branch_obligation_decision = function(id, value) {
  force(value)
  if (length(value) != 1L || is.na(value)) {
    saveRDS(list(site = sites[[id]], value = value, case = observations$case),
      file.path(out_dir, "branch_invalid_condition.rds"))
    stop("A condition did not yield one nonmissing value")
  }
  column = as.integer(as.logical(value)) + 1L
  observations$counts[id, column] = observations$counts[id, column] + 1
  index = id + (column - 1L) * length(sites)
  if (is.null(observations$first[[index]])) observations$first[[index]] = observations$case
  value
}

cases_done = 0L
check_case = function(domains, clauses, label) {
  universe = CnfUniverse()
  for (symbol in names(domains)) CnfSymbol(universe, symbol, domains[[symbol]])
  observations$case = list(label = label, domains = domains, clauses = clauses)
  baseline = plain_simplify(clauses, universe)
  observed = observed_simplify(clauses, universe)
  if (!identical(observed, baseline)) {
    saveRDS(list(case = observations$case, baseline = baseline, observed = observed),
      file.path(out_dir, "branch_observer_difference.rds"))
    stop("Condition observation changed the returned object: ", label)
  }
  cases_done <<- cases_done + 1L
}

save_progress = function(completed) {
  table = do.call(rbind, lapply(seq_along(sites), function(i) {
    data.frame(id = i, owner = sites[[i]]$owner, condition = sites[[i]]$condition,
      false = observations$counts[i, 1L], true = observations$counts[i, 2L],
      stringsAsFactors = FALSE)
  }))
  write.table(table, file.path(out_dir, "branch_obligations.tsv"),
    sep = "\t", quote = TRUE, row.names = FALSE)
  saveRDS(list(sites = sites, counts = observations$counts, first_inputs = observations$first,
    cases = cases_done, complete = completed, version = R.version.string,
    source_hashes = readLines("attic/cnf_verify3/independent_solver/SOURCE.sha256")),
    file.path(out_dir, "branch_obligations.rds"))
  cat("cases=", cases_done, " conditions=", length(sites), " evaluations=",
    sum(observations$counts), " unseen_true=", sum(observations$counts[, 2L] == 0),
    " unseen_false=", sum(observations$counts[, 1L] == 0), " complete=", completed, "\n", sep = "")
}

fixtures = c("minimized_subsumption", "minimized_sse1", "minimized_first_order_phase_sse1",
  "minimized_sse2", "directed_oneend_shrink_min", "minimized_oneend_symbol_removal",
  "minimized_deferred_skip")
for (fixture in fixtures) {
  data = fromJSON(file.path("attic/cnf_verify3/independent_solver", paste0(fixture, ".json")),
    simplifyVector = FALSE)
  domains = lapply(data$domains, unlist, use.names = FALSE)
  clauses = lapply(data$clauses, function(clause) lapply(clause, unlist, use.names = FALSE))
  for (reverse in c(FALSE, TRUE)) {
    current = if (reverse) rev(clauses) else clauses
    check_case(domains, current, paste0(fixture, ":", reverse))
  }
}
for (constant in list(TRUE, FALSE, list())) check_case(list(), constant, "constant")

set.seed(as.integer(Sys.getenv("CNF_BRANCH_SEED", "9061301")))
random_trials = as.integer(Sys.getenv("CNF_BRANCH_TRIALS", "10000"))
for (trial in seq_len(random_trials)) {
  n_symbols = sample(if (profile == "mixed") 2:8 else 2:6, 1L)
  domains = lapply(seq_len(n_symbols), function(si) paste0("v", seq_len(sample(2:6, 1L))))
  names(domains) = paste0("X", seq_len(n_symbols))
  n_clauses = sample(if (profile == "mixed") 2:24 else 8:40, 1L)
  planted = trial %% 3L == 0L
  assignment = lapply(domains, function(domain) sample(domain, 1L))
  clauses = lapply(seq_len(n_clauses), function(ci) {
    n_occurrences = if (profile == "mixed") sample.int(n_symbols, 1L) else sample(2:min(4L, n_symbols), 1L)
    symbols = sample(names(domains), n_occurrences)
    ranges = lapply(symbols, function(symbol) {
      domain = domains[[symbol]]
      sample(domain, sample.int(length(domain) - 1L, 1L))
    })
    names(ranges) = symbols
    if (planted && !any(vapply(symbols, function(symbol) assignment[[symbol]] %in% ranges[[symbol]], FALSE))) {
      symbol = sample(symbols, 1L)
      # Replace one selected value, so the planted range remains nonempty/proper.
      ranges[[symbol]][[1L]] = assignment[[symbol]]
    }
    ranges
  })
  check_case(domains, clauses, paste0("random:", trial))
  if (trial %% 1000L == 0L) save_progress(FALSE)
}
save_progress(TRUE)
