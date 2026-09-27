# Transfer real classed CNF objects from an ordinary sourced R session into
# independent namespace-loaded package sessions. Only local artifacts are used.
mode = commandArgs(trailingOnly = TRUE)[[1L]]
save_dir = "attic/cnf_verify3/execution_modes"
transfer_path = file.path(save_dir, "source_transfer.rds")
if (mode == "write_source") {
  suppressPackageStartupMessages({ library(checkmate); library(mlr3misc) })
  for (nm in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) source(file.path("R", paste0(nm, ".R")))
} else if (mode == "read_development") {
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
} else {
  stopifnot(mode == "read_installed")
  .libPaths(c(normalizePath(file.path(save_dir, "library")), .libPaths()))
  suppressPackageStartupMessages(library(mlr3pipelines))
}

evaluate = function(formula, assignments) {
  if (is.logical(formula)) return(rep(as.vector(formula), nrow(assignments)))
  result = rep(TRUE, nrow(assignments))
  for (clause in formula) {
    value = rep(FALSE, nrow(assignments))
    for (i in seq_along(clause)) value = value | assignments[[names(clause)[[i]]]] %in% clause[[i]]
    result = result & value
  }
  result
}
if (mode == "write_source") {
  # The earlier independent runtime oracle stores plain mathematical inputs;
  # reconstruct actual CNF objects here before serializing across R sessions.
  inputs = lapply(readRDS(file.path(save_dir, "source_jit0.rds"))$results, `[[`, "input")
  bundles = lapply(names(inputs), function(name) {
    input = inputs[[name]]
    universe = CnfUniverse()
    symbols = lapply(names(input$domains), function(s) CnfSymbol(universe, s, input$domains[[s]]))
    names(symbols) = names(input$domains)
    clauses = lapply(input$clauses, function(cl) CnfClause(lapply(names(cl), function(s) CnfAtom(symbols[[s]], cl[[s]]))))
    if (name == "selector") clauses[[1L]] = clauses[[1L]][matrix(c(1L, 1L), nrow = 1L)]
    first = CnfFormula(clauses)
    second = CnfFormula(as.list(first))
    assignments = expand.grid(input$domains, stringsAsFactors = FALSE)
    list(input = input, first = first, second = second, clauses = clauses, universe = universe,
      symbols = symbols, expected = evaluate(input$clauses, assignments), actual = evaluate(first, assignments))
  })
  names(bundles) = names(inputs)
  saveRDS(bundles, transfer_path, version = 3L, compress = FALSE)
  cat("Wrote", length(bundles), "joint formula/clause/symbol/universe bundles from source functions.\n")
} else {
  bundles = readRDS(transfer_path)
  for (name in names(bundles)) {
    b = bundles[[name]]
    assignments = expand.grid(b$input$domains, stringsAsFactors = FALSE)
    stopifnot(identical(evaluate(b$input$clauses, assignments), b$expected))
    stopifnot(identical(evaluate(b$first, assignments), b$actual))
    stopifnot(identical(attr(b$first, "universe"), b$universe),
      all(vapply(b$clauses, function(cl) identical(attr(cl, "universe"), b$universe), logical(1))),
      all(vapply(b$symbols, function(s) identical(attr(s, "universe"), b$universe), logical(1))))
    replay = CnfFormula(b$clauses)
    second = CnfFormula(as.list(b$first))
    stopifnot(identical(c(replay), c(b$first)), identical(c(second), c(b$second)))
    stopifnot(identical(evaluate(b$first & b$second, assignments), b$actual & evaluate(b$second, assignments)))
    if (name != "selector") stopifnot(identical(b$actual, b$expected))
    if (name == "selector") stopifnot(identical(which(b$actual != b$expected), 3L))
    cat(name, ": class dispatch, constructor replay, truth function and shared universe preserved.\n")
  }
}
cat("R:", R.version.string, "; mode:", mode, "\n")
