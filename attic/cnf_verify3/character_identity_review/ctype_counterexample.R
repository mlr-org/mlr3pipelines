# Accepted public names under LC_CTYPE=C: no universe mutation or custom CNF.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
for (source_name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(source_name, ".R")))
}
source("attic/cnf_verify3/character_identity_review/harness.R")
plain = simplify_cnf
observed = identity_observer(plain)
sites = identity_sites
old_ctype = Sys.getlocale("LC_CTYPE")
old_collate = Sys.getlocale("LC_COLLATE")
records = list()
for (ctype in c("C.UTF-8", "C")) {
  stopifnot(nzchar(Sys.setlocale("LC_CTYPE", ctype)), nzchar(Sys.setlocale("LC_COLLATE", "C")))
  for (encoding in c("utf8", "latin1")) {
    warning_text = character()
    record = withCallingHandlers({
      name = if (encoding == "utf8") "\u00e9" else iconv("\u00e9", from = "UTF-8", to = "latin1")
      universe = CnfUniverse()
      X = CnfSymbol(universe, name, c("a", "b", "c"))
      Y = CnfSymbol(universe, "Y", c("a", "b"))
      clauses = list(CnfClause(list(X %among% "a")),
        CnfClause(list(X %among% "b", Y %among% "a")),
        CnfClause(list(X %among% "c", Y %among% "b")))
      assignments = expand.grid(X = c("a", "b", "c"), Y = c("a", "b"), stringsAsFactors = FALSE)
      evaluate = function(entries) {
        if (is.logical(entries)) return(rep(entries, nrow(assignments)))
        result = rep(TRUE, nrow(assignments))
        for (clause in entries) {
          disjunction = rep(FALSE, nrow(assignments))
          for (i in seq_along(clause)) {
            # Match code points, not environment/list key conversion.
            actual = utf8ToInt(enc2utf8(names(clause)[[i]]))
            expected = utf8ToInt(enc2utf8(name))
            stopifnot(identical(actual, expected) || identical(actual, utf8ToInt("Y")))
            id = if (identical(actual, expected)) "X" else "Y"
            disjunction = disjunction | assignments[[id]] %in% clause[[i]]
          }
          result = result & disjunction
        }
        result
      }
      entries = lapply(clauses, identity_bare)
      before = evaluate(entries)
      output = CnfFormula(clauses)
      after = evaluate(identity_bare(output))
      identity_events = list()
      traced = observed(entries, universe)
      stopifnot(identical(output, traced), length(universe) == 2L,
        identical(universe[[name]], c("a", "b", "c")), identical(universe[["Y"]], c("a", "b")))
      ids = vapply(identity_events, function(e) e$site, 1L)
      early = which(vapply(sites[ids], function(site) site$kind == "for_sequence" &&
        site$expression == "clause_symbol_isct", FALSE))
      hidden_elimination = which(vapply(sites[ids], function(site) site$kind == "if" &&
        site$expression == "not_subset_count[[updating_hla_clause_idx]] == 0", FALSE))
      unit_domains = new.env(parent = emptyenv())
      unit_domains[[name]] = "a"
      list(ctype = ctype, encoding = encoding, source_models = sum(before), output_models = sum(after),
        changed_truth = !identical(before, after), output = identity_bare(output),
        name = name, name_encoding = Encoding(name), name_bytes = as.integer(charToRaw(name)),
        universe_names = names(universe), same_name_is_enumerated = name %in% names(universe),
        unit_domain_names = names(unit_domains), unit_domain_names_encoding = Encoding(names(unit_domains)),
        unit_domain_name_bytes = lapply(names(unit_domains), function(n) as.integer(charToRaw(n))),
        propagation_sequences = lapply(identity_events[early], function(event) event$value),
        hidden_elimination_branches = lapply(identity_events[hidden_elimination], function(event) event$value),
        entries = entries, trace = identity_events,
        witnesses = assignments[before != after, , drop = FALSE])
    }, warning = function(w) {
      warning_text <<- c(warning_text, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    record$warnings = unique(warning_text)
    stopifnot(record$source_models == 0L)
    if (ctype == "C") stopifnot(record$output_models == 2L, record$changed_truth,
      !record$same_name_is_enumerated,
      all(vapply(record$propagation_sequences, length, 1L) == 0L),
      any(vapply(record$hidden_elimination_branches, isTRUE, FALSE)))
    else stopifnot(record$output_models == 0L, !record$changed_truth, record$same_name_is_enumerated)
    records[[length(records) + 1L]] = record
    cat(ctype, encoding, "source_models", record$source_models, "output_models", record$output_models,
      "changed", record$changed_truth, "\n")
  }
}
invisible(Sys.setlocale("LC_CTYPE", old_ctype))
invisible(Sys.setlocale("LC_COLLATE", old_collate))
suffix = if (getRversion() < "4.0") "r36" else "r46"
saveRDS(list(runtime = as.character(getRversion()), sites = sites, records = records),
  file.path("attic/cnf_verify3/character_identity_review", paste0("ctype_counterexample_", suffix, ".rds")), version = 2)
summary = lapply(records, function(record) record[c("ctype", "encoding", "source_models", "output_models",
  "changed_truth", "same_name_is_enumerated", "name_encoding", "name_bytes", "unit_domain_names",
  "unit_domain_names_encoding", "unit_domain_name_bytes", "propagation_sequences", "hidden_elimination_branches",
  "warnings", "witnesses")])
write_json(list(runtime = as.character(getRversion()), records = summary),
  file.path("attic/cnf_verify3/character_identity_review", paste0("ctype_counterexample_", suffix, ".json")),
  pretty = TRUE, auto_unbox = TRUE)
