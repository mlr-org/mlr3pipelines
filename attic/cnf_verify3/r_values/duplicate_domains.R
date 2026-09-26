source("attic/cnf_verify3/r_values/bootstrap.R")

set.seed(915732)
n_trials = as.integer(Sys.getenv("CNF_DUP_TRIALS", "2000"))
counts = c(formulas = 0L, worlds = 0L, structural_differences = 0L)
for (trial in seq_len(n_trials)) {
  domains = lapply(seq_len(sample(2:4, 1L)), function(i) letters[seq_len(sample(2:5, 1L))])
  # Assign names after sampling the symbol count, independently of domain size.
  names(domains) = paste0("X", seq_along(domains))
  raw = lapply(seq_len(sample(1:7, 1L)), function(ci) {
    selected = sample(names(domains), sample.int(length(domains), 1L))
    values = lapply(selected, function(s) sample(domains[[s]], sample.int(length(domains[[s]]) - 1L, 1L)))
    setNames(values, selected)
  })
  assignments = expand.grid(domains, stringsAsFactors = FALSE)
  expected = formula_truth(raw, assignments)
  outputs = list()
  for (variant in c("plain", "duplicates", "named", "matrix")) {
    universe = CnfUniverse()
    symbols = lapply(names(domains), function(sym) {
      values = domains[[sym]]
      if (variant != "plain") values = sample(rep(values, sample(1:5, length(values), replace = TRUE)))
      if (variant == "named") names(values) = sample(c("metadata", "", NA_character_), length(values), replace = TRUE)
      if (variant == "matrix") values = matrix(values, nrow = 1L)
      CnfSymbol(universe, sym, values)
    })
    names(symbols) = names(domains)
    clauses = lapply(raw, function(cl) {
      CnfClause(lapply(names(cl), function(sym) {
        values = cl[[sym]]
        if (variant != "plain") values = sample(rep(values, sample(1:5, length(values), replace = TRUE)))
        if (variant == "named") names(values) = sample(c("ignored", "", NA_character_), length(values), replace = TRUE)
        if (variant == "matrix") values = matrix(values, nrow = 1L)
        CnfAtom(symbols[[sym]], values)
      }))
    })
    stopifnot(all(vapply(clauses, valid_clause, logical(1), universe = universe)))
    output = CnfFormula(clauses)
    stopifnot(valid_formula(output, universe), identical(expected, formula_truth(output, assignments)))
    outputs[[variant]] = c(output)
    counts[["formulas"]] = counts[["formulas"]] + 1L
    counts[["worlds"]] = counts[["worlds"]] + nrow(assignments)
  }
  # Label order is not a semantic claim. Normalize only value order for this
  # extra structural comparison; formula/clause scheduling order is untouched.
  normalize = function(out) {
    if (is.logical(out)) return(out)
    lapply(out, function(cl) lapply(cl, sort))
  }
  baseline = normalize(outputs$plain)
  for (variant in names(outputs)[-1L]) {
    counts[["structural_differences"]] = counts[["structural_differences"]] +
      as.integer(!identical(baseline, normalize(outputs[[variant]])))
  }
  if (trial %% 250L == 0L) cat("duplicate-domain trials", trial, "of", n_trials, "\n")
}
cat("R:", R.version.string, "\n")
print(counts)
cat("All independently evaluated truth tables and final range/name invariants passed.\n")
saveRDS(list(counts = counts, trials = n_trials, seed = 915732L, version = R.version.string),
  file.path("attic/cnf_verify3/r_values", paste0("duplicates_r", getRversion(), ".rds")))
