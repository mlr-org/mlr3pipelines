source("attic/cnf_verify3/normalization_component_review/observer.R")
suppressPackageStartupMessages(library(checkmate))
suppressPackageStartupMessages(library(jsonlite))
source("R/CnfUniverse.R")
source("R/CnfSymbol.R")
source("R/CnfAtom.R")
source("R/CnfClause.R")

# Independently check that the AST catalog covers the intended source sites.
tokens = getParseData(parse("R/CnfFormula_simplify.R", keep.source = TRUE))$token
sites = table(vapply(observed_source$sites, `[[`, "", "kind"))
stopifnot(sites[["if"]] == sum(tokens == "IF"), sites[["for"]] == sum(tokens == "FOR"),
  sites[["&&1"]] == sum(tokens == "AND2"), sites[["&&2"]] == sum(tokens == "AND2"),
  sites[["||1"]] == sum(tokens == "OR2"), sites[["||2"]] == sum(tokens == "OR2"))

counts = c(domains = 0L, proper_payloads = 0L, empty_atoms = 0L, full_atoms = 0L,
  unused_universe_payloads = 0L, unused_universe_traces = 0L)
for (n in 1:4) {
  values = c("", "b", "a", "c")[seq_len(n)]
  repeated = rev(rep(values, each = 2L))
  forms = list(values, rev(values), repeated,
    setNames(repeated, rep(c("", NA_character_), length.out = length(repeated))),
    matrix(repeated, nrow = 2L), array(repeated, dim = c(2L, 1L, n)),
    structure(repeated, review_metadata = "inert", class = "ReviewUnusedCharacterMarker"))
  subsets = expand.grid(rep(list(c(FALSE, TRUE)), n))
  for (stored in forms) {
    universe = CnfUniverse()
    symbol = CnfSymbol(universe, "[non syntactic]", stored)
    counts[["domains"]] = counts[["domains"]] + 1L
    stopifnot(identical(bare_output(CnfAtom(symbol, character())), FALSE),
      identical(bare_output(CnfAtom(symbol, values)), TRUE))
    counts[["empty_atoms"]] = counts[["empty_atoms"]] + 1L
    counts[["full_atoms"]] = counts[["full_atoms"]] + 1L
    for (i in seq_len(nrow(subsets))) {
      selected = values[as.logical(subsets[i, ])]
      if (!length(selected) || length(selected) == n) next
      incoming = list(selected, setNames(rep(selected, 2L), rep("name", 2L * length(selected))),
        matrix(rep(selected, 2L), nrow = 2L))
      for (literal in incoming) {
        clause = bare_output(CnfClause(list(CnfAtom(symbol, literal))))
        stopifnot(identical(clause, setNames(list(selected), "[non syntactic]")))
        counts[["proper_payloads"]] = counts[["proper_payloads"]] + 1L
      }
    }
  }
}

set.seed(1942L)
for (i in 1:80) {
  domains = list(X = c("a", "b", "c"), Y = c("a", "b", "c"), Z = c("a", "b", "c"))
  clauses = lapply(seq_len(sample.int(8L, 1L)), function(j) {
    symbols = names(domains)[sample.int(3L, sample.int(2L, 1L) + 1L)]
    setNames(lapply(symbols, function(s) domains[[s]][sample.int(3L, sample.int(2L, 1L))]), symbols)
  })
  original = run_observed(clauses, domains)
  unused = setNames(rep(list(c("a", "b", "c")), 40L), paste0("unused", seq_len(40L)))
  extended = run_observed(clauses, c(domains, unused))
  stopifnot(identical(original$output, extended$output), identical(original$events, extended$events))
  counts[["unused_universe_payloads"]] = counts[["unused_universe_payloads"]] + 1L
  counts[["unused_universe_traces"]] = counts[["unused_universe_traces"]] + 1L
}

version = if (getRversion() < "4") "r36" else "r46"
result = list(R = R.version.string, checkmate = as.character(packageVersion("checkmate")),
  counts = as.list(counts), source_site_counts = as.list(sites))
write_json(result, file.path(review_dir, paste0("boundary_checks_", version, ".json")),
  auto_unbox = TRUE, pretty = TRUE)
print(result)
