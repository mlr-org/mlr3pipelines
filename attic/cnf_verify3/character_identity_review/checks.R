suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
suppressMessages(library(jsonlite))
audit_dir = "attic/cnf_verify3/character_identity_review"
for (source_name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(source_name, ".R")))
}
source(file.path(audit_dir, "harness.R"))
plain_simplify = simplify_cnf
observed_simplify = identity_observer(plain_simplify)
sites = identity_sites

# New integer-ID corpus, built without reading prior stored corpus outputs.
cases = list(
  distinct_value_conflict = list(list(s1 = 1L), list(s1 = 2L)),
  distinct_symbol_units = list(list(s1 = 1L), list(s2 = 2L)),
  merge_units = list(list(s1 = c(1L, 2L)), list(s1 = c(2L, 3L))),
  unit_cascade = list(list(s1 = 1L), list(s1 = 2L, s2 = 1L), list(s2 = 2L, s3 = 1L)),
  unit_shrink = list(list(s1 = c(1L, 2L)), list(s1 = c(2L, 3L), s2 = 2L)),
  born_unit = list(list(s1 = 1L, s2 = c(1L, 2L)), list(s1 = 2L, s2 = 1L)),
  subsumption = list(list(s1 = 1L, s2 = 2L), list(s1 = c(1L, 3L), s2 = 2L, s3 = 4L)),
  virtual_range = list(list(s1 = c(1L, 2L), s2 = c(1L, 2L)), list(s1 = c(2L, 3L), s2 = 1L)),
  virtual_unit = list(list(s1 = c(1L, 2L)), list(s1 = 1L, s2 = c(1L, 2L)), list(s1 = 2L, s2 = c(3L, 4L))),
  hidden_chain = list(list(s1 = c(1L, 2L), s2 = c(1L, 2L)), list(s1 = 1L, s3 = c(1L, 2L)),
    list(s1 = 2L, s3 = c(2L, 3L)), list(s2 = c(1L, 2L), s3 = c(1L, 3L))),
  order_and_empty_value = list(list(s3 = 4L, s2 = c(3L, 1L), s1 = c(2L, 1L)),
    list(s2 = c(2L, 4L), s1 = c(3L, 2L)), list(s3 = 1L, s2 = 3L)),
  repeated_clauses = list(list(s2 = 2L, s1 = c(3L, 1L)), list(s2 = 2L, s1 = c(3L, 1L))))
set.seed(260906L)
for (trial in seq_len(8L)) {
  cases[[paste0("generated_", trial)]] = lapply(seq_len(7L), function(j) {
    symbols = sample.int(3L, sample.int(2L, 1L) + 1L)
    ranges = lapply(symbols, function(s) sample.int(4L, sample.int(3L, 1L)))
    setNames(ranges, paste0("s", symbols))
  })
}

symbol_names = c("\u00e9", "e\u0301", "\u00f6")
values = c("\u00e9", "e\u0301", "\u00f6", "")
assignments = as.matrix(expand.grid(rep(list(seq_len(4L)), 3L)))
profiles = c("utf8", "latin1", "native", "mixed")
baseline = list()
records = list()
api_records = list()
site_counts = integer(length(sites))
helper_counts = integer()
original_ctype = Sys.getlocale("LC_CTYPE")
original_collate = Sys.getlocale("LC_COLLATE")

for (ctype in c("C.UTF-8", "en_US.UTF-8")) {
  stopifnot(nzchar(Sys.setlocale("LC_CTYPE", ctype)), l10n_info()[["UTF-8"]])
  # Universes are constructed once per domain profile, then unchanged across
  # all formula construction, simplification, and collation configurations.
  for (domain_profile in profiles) {
    universe = CnfUniverse()
    for (i in seq_along(symbol_names)) CnfSymbol(universe,
      identity_encode(symbol_names[[i]], domain_profile, i), identity_encode(values, domain_profile, i))
    original_domains = lapply(symbol_names, function(s) universe[[s]])
    for (collate in c("C", "C.UTF-8", "en_US.UTF-8")) {
      stopifnot(nzchar(Sys.setlocale("LC_COLLATE", collate)))
      for (literal_profile in profiles) {
        # Public symbol retrieval and repeated-name construction: encoding
        # aliases must resolve to one registered symbol and one list entry.
        first_symbol = `$.CnfUniverse`(universe, identity_encode(symbol_names[[1L]], literal_profile))
        alias_symbol = `$.CnfUniverse`(universe, identity_encode(symbol_names[[1L]], "mixed", 1L))
        duplicate = tryCatch(CnfSymbol(universe, c(first_symbol), values), error = function(e) conditionMessage(e))
        stopifnot(is.character(duplicate), grepl("already exists", duplicate, fixed = TRUE), length(universe) == 3L)
        first_atom = CnfAtom(first_symbol, identity_encode(values[c(1L, 1L, 3L)], literal_profile))
        alias_atom = CnfAtom(alias_symbol, identity_encode(values[3L], "mixed"))
        merged = CnfClause(list(first_atom, alias_atom))
        stopifnot(length(merged) == 1L, identical(identity_strings(merged[[1L]]), identity_strings(values[c(1L, 3L)])))
        all_atom = CnfAtom(first_symbol, identity_encode(values, literal_profile))
        no_atom = CnfAtom(first_symbol, character())
        stopifnot(identical(c(all_atom), TRUE), identical(c(no_atom), FALSE))
        negative_atom = !first_atom
        stopifnot(identical(identity_strings(negative_atom$values), identity_strings(values[c(2L, 4L)])))
        second_symbol = `$.CnfUniverse`(universe, identity_encode(symbol_names[[2L]], literal_profile))
        two_symbols = CnfClause(list(first_atom, CnfAtom(second_symbol, values[2L])))
        selected = two_symbols[identity_encode(symbol_names[c(2L, 1L, 2L)], literal_profile)]
        stopifnot(length(selected) == 2L,
          identical(identity_strings(names(selected)), identity_strings(symbol_names[c(2L, 1L)])))
        rebuilt = CnfClause(as.list(two_symbols))
        stopifnot(identical(identity_payload(rebuilt), identity_payload(two_symbols)))
        api_records[[length(api_records) + 1L]] = list(ctype = ctype, collate = collate,
          domain_profile = domain_profile, literal_profile = literal_profile, passed = TRUE)

        for (case_name in names(cases)) {
          spec = cases[[case_name]]
          clauses = lapply(seq_along(spec), function(j) CnfClause(lapply(seq_along(spec[[j]]), function(k) {
            id = as.integer(sub("s", "", names(spec[[j]])[[k]], fixed = TRUE))
            symbol = `$.CnfUniverse`(universe, identity_encode(symbol_names[[id]], literal_profile, j + k))
            CnfAtom(symbol, identity_encode(values[spec[[j]][[k]]], literal_profile, j + 2L * k))
          })))
          entries = lapply(clauses, identity_bare)
          # Public construction -> unmodified production kernel.
          output = CnfFormula(clauses)
          plain = plain_simplify(entries, universe)
          stopifnot(identical(output, plain), identical(attr(output, "universe"), universe))
          truth = identity_eval_spec(spec, assignments)
          stopifnot(identical(truth, identity_eval_output(output, assignments, symbol_names, values)))
          # Private observed copy must itself give precisely the production output.
          identity_events = list()
          observed = observed_simplify(entries, universe)
          stopifnot(identical(output, observed))
          event_ids = vapply(identity_events, function(event) event$site, 1L)
          site_counts = site_counts + tabulate(event_ids, nbins = length(sites))
          trace = identity_tree(identity_events)
          payload = identity_payload(output)
          input_payload = lapply(clauses, identity_payload)
          record = list(ctype = ctype, collate = collate, domain_profile = domain_profile,
            literal_profile = literal_profile, case = case_name,
            events = length(trace), models = sum(truth))
          if (is.null(baseline[[case_name]])) {
            baseline[[case_name]] = list(trace = trace, output = payload, input = input_payload, truth = truth)
          } else {
            stopifnot(identical(input_payload, baseline[[case_name]]$input),
              identical(payload, baseline[[case_name]]$output), identical(trace, baseline[[case_name]]$trace),
              identical(truth, baseline[[case_name]]$truth))
          }
          records[[length(records) + 1L]] = record
        }
      }
    }
    stopifnot(length(universe) == 3L,
      identical(original_domains, lapply(symbol_names, function(s) universe[[s]])))
  }
}
invisible(Sys.setlocale("LC_CTYPE", original_ctype))
invisible(Sys.setlocale("LC_COLLATE", original_collate))

# Semantic negative controls: no output comparison is allowed to collapse
# different range IDs or interchange associations of distinct symbol names.
stopifnot(!identical(identity_eval_spec(list(list(s1 = 1L)), assignments),
  identity_eval_spec(list(list(s1 = 2L)), assignments)),
  !identical(identity_eval_spec(list(list(s1 = 1L, s2 = 2L)), assignments),
    identity_eval_spec(list(list(s1 = 2L, s2 = 1L)), assignments)))

# Observer negative control: an encoding-dependent branch is visible, even
# though exact string normalization rightly makes its data payload equal.
control = plain_simplify
body(control) = substitute({if (Encoding(entries[[1L]][[1L]][[1L]]) == "UTF-8") invisible(NULL); BODY},
  list(BODY = body(control)))
observed_control = identity_observer(control)
universe = CnfUniverse()
control_symbol = CnfSymbol(universe, "X", c("\u00e9", "other"))
control_traces = lapply(c("utf8", "native"), function(profile) {
  entries = list(list(X = identity_encode("\u00e9", profile)))
  identity_events <<- list()
  observed_control(entries, universe)
  identity_tree(identity_events)
})
stopifnot(!identical(control_traces[[1L]], control_traces[[2L]]))

observed_helpers = vapply(sites, function(site) site$kind == "helper", FALSE) & site_counts > 0L
result = list(runtime = as.character(getRversion()), checkmate = as.character(packageVersion("checkmate")),
  cases = length(cases), formulas = length(records), public_api_configurations = length(api_records),
  truth_assignments = nrow(assignments) * length(records),
  trace_events = sum(site_counts), site_count = length(sites), observed_sites = sum(site_counts > 0L),
  helpers = vapply(sites[observed_helpers], function(site) site$owner, ""),
  independent_semantic_controls = TRUE, observer_control_detected = TRUE,
  payloads_equal = TRUE, source_traces_equal = TRUE, universe_unchanged = TRUE)
suffix = if (getRversion() < "4.0") "r36" else "r46"
site_table = do.call(rbind, lapply(seq_along(sites), function(i) {
  data.frame(sites[[i]], count = site_counts[[i]], stringsAsFactors = FALSE)
}))
write.table(site_table, file.path(audit_dir, paste0("sites_", suffix, ".tsv")), sep = "\t", row.names = FALSE, quote = TRUE)
write_json(result, file.path(audit_dir, paste0("results_", suffix, ".json")), pretty = TRUE, auto_unbox = TRUE)
saveRDS(list(summary = result, records = records, api_records = api_records, cases = cases,
  baselines = baseline, sites = sites, site_counts = site_counts, controls = control_traces),
  file.path(audit_dir, paste0("checks_", suffix, ".rds")), version = 2)
cat(toJSON(result, pretty = TRUE, auto_unbox = TRUE), "\n")
