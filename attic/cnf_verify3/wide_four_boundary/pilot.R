suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}
result = list()
for (file in c("minimized_first_order_phase_sse1.json", "minimized_sse1.json")) {
  case = read_json(file.path("attic/cnf_verify3/independent_solver", file))
  domains = lapply(case$domains, unlist, use.names = FALSE)
  clauses = lapply(case$clauses, function(clause) lapply(clause, unlist, use.names = FALSE))
  for (padding in c(0L, 1L, 2L, 5L, 64L)) {
    universe = CnfUniverse()
    for (symbol in names(domains)) CnfSymbol(universe, symbol, domains[[symbol]])
    pad_names = paste0("P", seq_len(padding))
    if (!padding) pad_names = character()
    for (symbol in pad_names) CnfSymbol(universe, symbol, c("off", "on"))
    padded = lapply(clauses, function(clause) c(clause, setNames(rep(list("on"), padding), pad_names)))
    public = lapply(padded, function(clause) CnfClause(lapply(seq_along(clause), function(i) {
      CnfAtom(`$.CnfUniverse`(universe, names(clause)[[i]]), clause[[i]])
    })))
    first = CnfFormula(public)
    second = CnfFormula(as.list(first))
    strip = function(f) lapply(c(f), function(clause) clause[!names(clause) %in% pad_names])
    record = list(file = file, padding = padding, first = strip(first), second = strip(second),
      widths_first = lengths(c(first)), widths_second = lengths(c(second)), changed = !identical(first, second))
    result[[length(result) + 1L]] = record
    cat(file, padding, "changed", record$changed, "\n")
    dput(record$first)
    dput(record$second)
  }
}
write_json(result, "attic/cnf_verify3/wide_four_boundary/pilot.json", pretty = TRUE, auto_unbox = TRUE)
