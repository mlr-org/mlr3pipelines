suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
old_ctype = Sys.getlocale("LC_CTYPE")
records = list()
for (ctype in c("C.UTF-8", "C")) {
  invisible(Sys.setlocale("LC_CTYPE", ctype))
  warnings = character()
  record = withCallingHandlers({
    utf8 = "\u00e9"
    latin1 = iconv(utf8, from = "UTF-8", to = "latin1")
    stopifnot(identical(utf8, latin1), utf8 == latin1, length(unique(c(utf8, latin1))) == 1L)
    universe = CnfUniverse()
    X = CnfSymbol(universe, utf8, c("a", "b", "c"))
    alias_retrieval = tryCatch(`$.CnfUniverse`(universe, latin1), error = function(e) conditionMessage(e))
    duplicate = tryCatch(CnfSymbol(universe, latin1, c("a", "b", "c")), error = function(e) conditionMessage(e))
    accepted = inherits(duplicate, "CnfSymbol")
    alias = if (accepted) duplicate else `$.CnfUniverse`(universe, latin1)
    clause = CnfClause(list(X %among% "a", alias %among% "b"))
    formula = CnfFormula(list(clause))
    list(ctype = ctype, equivalent_names = TRUE,
      alias_retrieved = inherits(alias_retrieval, "CnfSymbol"), second_registration_accepted = accepted,
      universe_binding_count = length(universe), clause_width = length(clause),
      clause_duplicate_r_names = anyDuplicated(names(clause)) > 0L,
      returned_clause_width = length(formula[[1L]]),
      kernel_received_duplicate_names = anyDuplicated(names(clause)) > 0L,
      formula_returned_duplicate_names = anyDuplicated(names(formula[[1L]])) > 0L)
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  record$warnings = unique(warnings)
  if (ctype == "C") stopifnot(!record$alias_retrieved, record$second_registration_accepted,
    record$kernel_received_duplicate_names, record$formula_returned_duplicate_names)
  else stopifnot(record$alias_retrieved, !record$second_registration_accepted,
    !record$kernel_received_duplicate_names, !record$formula_returned_duplicate_names)
  records[[length(records) + 1L]] = record
}
invisible(Sys.setlocale("LC_CTYPE", old_ctype))
suffix = if (getRversion() < "4.0") "r36" else "r46"
result = list(runtime = as.character(getRversion()), records = records)
write_json(result, file.path("attic/cnf_verify3/character_identity_review", paste0("aliases_", suffix, ".json")),
  auto_unbox = TRUE, pretty = TRUE)
cat(toJSON(result, auto_unbox = TRUE, pretty = TRUE), "\n")
