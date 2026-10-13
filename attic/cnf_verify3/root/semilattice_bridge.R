# Initial unit-propagation boundary and unchanged full-kernel JSON bridge.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}
full_simplify = simplify_cnf
src = paste(readLines("R/CnfFormula_simplify.R"), collapse = "\n")
boundary = "  available = which(!(eliminated | is_unit))"
stopifnot(length(gregexpr(boundary, src, fixed = TRUE)[[1L]]) == 1L)
src = sub(boundary, paste0("  return(return_entries(entries[!eliminated]))\n", boundary), src, fixed = TRUE)
eval(parse(text = src))
prefix_simplify = simplify_cnf
simplify_cnf = full_simplify

run_request = function(request) {
  domains = lapply(request$domains, function(x) as.character(unlist(x, use.names = FALSE)))
  universe = CnfUniverse()
  for (symbol in names(domains)) CnfSymbol(universe, symbol, domains[[symbol]])
  clauses = request$clauses
  if (!is.logical(clauses)) {
    clauses = lapply(clauses, function(clause) {
      lapply(clause, function(x) as.character(unlist(x, use.names = FALSE)))
    })
  }
  prefix = prefix_simplify(clauses, universe)
  full = full_simplify(clauses, universe)
  list(ok = TRUE, prefix = c(prefix), full = c(full))
}

input = file("stdin", "r")
repeat {
  line = readLines(input, n = 1L, warn = FALSE)
  if (!length(line)) break
  result = tryCatch(run_request(fromJSON(line, simplifyVector = FALSE)),
    error = function(e) list(ok = FALSE, message = conditionMessage(e)))
  cat(toJSON(result, auto_unbox = TRUE, null = "null", digits = NA), "\n", sep = "")
  flush.console()
}
