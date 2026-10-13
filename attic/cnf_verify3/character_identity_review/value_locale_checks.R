# Character-value controls with ASCII symbol names, including LC_CTYPE=C.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
audit_dir = "attic/cnf_verify3/character_identity_review"
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
source(file.path(audit_dir, "harness.R"))
suffix = if (getRversion() < "4.0") "r36" else "r46"
cases = readRDS(file.path(audit_dir, paste0("checks_", suffix, ".rds")))$cases
observed = identity_observer(simplify_cnf)
values = c("\u00e9", "e\u0301", "\u00f6", "")
assignments = as.matrix(expand.grid(rep(list(seq_len(4L)), 3L)))
baseline = list()
old_ctype = Sys.getlocale("LC_CTYPE")
count = 0L
events = 0L
for (ctype in c("C.UTF-8", "C")) {
  stopifnot(nzchar(Sys.setlocale("LC_CTYPE", ctype)))
  for (domain_profile in c("utf8", "latin1")) {
    universe = CnfUniverse()
    for (name in c("s1", "s2", "s3")) CnfSymbol(universe, name, identity_encode(values, domain_profile))
    for (literal_profile in c("utf8", "latin1")) {
      for (case in names(cases)) {
        spec = cases[[case]]
        clauses = lapply(spec, function(clause) CnfClause(lapply(seq_along(clause), function(i) {
          CnfAtom(`$.CnfUniverse`(universe, names(clause)[[i]]), identity_encode(values[clause[[i]]], literal_profile))
        })))
        result = CnfFormula(clauses)
        truth = identity_eval_output(result, assignments, c("s1", "s2", "s3"), values)
        stopifnot(identical(truth, identity_eval_spec(spec, assignments)))
        identity_events = list()
        traced = observed(lapply(clauses, identity_bare), universe)
        stopifnot(identical(result, traced))
        check = list(output = identity_payload(result), trace = identity_tree(identity_events))
        if (is.null(baseline[[case]])) baseline[[case]] = check else stopifnot(identical(check, baseline[[case]]))
        count = count + 1L
        events = events + length(identity_events)
      }
    }
  }
}
invisible(Sys.setlocale("LC_CTYPE", old_ctype))
summary = list(runtime = as.character(getRversion()), formulas = count, assignments = count * nrow(assignments),
  source_events = events, exact_normalized_traces_equal = TRUE, truth_preserved = TRUE,
  symbol_names = "ASCII", value_encodings = c("UTF-8", "Latin-1 where representable"),
  ctype_locales = c("C.UTF-8", "C"))
write_json(summary, file.path(audit_dir, paste0("value_locales_", suffix, ".json")), pretty = TRUE, auto_unbox = TRUE)
saveRDS(list(summary = summary, baselines = baseline),
  file.path(audit_dir, paste0("value_locales_", suffix, ".rds")), version = 2)
cat(toJSON(summary, pretty = TRUE, auto_unbox = TRUE), "\n")
