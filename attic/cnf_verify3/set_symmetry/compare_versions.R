library(jsonlite)
here = "attic/cnf_verify3/set_symmetry"
one = readRDS(file.path(here, "checks_r36.rds"))
two = readRDS(file.path(here, "checks_r46.rds"))
stopifnot(identical(one$cases, two$cases), identical(one$sites, two$sites),
  identical(one$summary$baseline_events, two$summary$baseline_events),
  identical(one$summary$paired_events, two$summary$paired_events))
# R 4.6 prints the matrix attribute as `dim`, whereas R 3.6 prints `.Dim`.
# Compare the actual saved values, not this version-dependent spelling.
decode = function(catalog) lapply(catalog, function(values) {
  lapply(values, function(value) eval(parse(text = value)))
})
stopifnot(identical(decode(one$observed_values), decode(two$observed_values)))
controls_one = readRDS(file.path(here, "source_controls_r36.rds"))
controls_two = readRDS(file.path(here, "source_controls_r46.rds"))
stopifnot(identical(controls_one$traces, controls_two$traces),
  identical(controls_one$source_sha256, controls_two$source_sha256))
occurrences_one = readRDS(file.path(here, "occurrence_controls_r36.rds"))
occurrences_two = readRDS(file.path(here, "occurrence_controls_r46.rds"))
stopifnot(identical(occurrences_one$records, occurrences_two$records),
  identical(occurrences_one$counts, occurrences_two$counts))
summary = list(success = TRUE, versions = c(one$summary$version, two$summary$version),
  source_sha256 = controls_one$source_sha256,
  paired_cases_per_version = length(one$cases),
  complete_trace_control_pairs = length(controls_one$traces),
  complete_trace_events_per_version = sum(vapply(controls_one$traces, function(item) length(item$trace), 0L)),
  occurrence_control_pairs = length(occurrences_one$records),
  occurrence_trace_events_per_version = sum(vapply(occurrences_one$records, function(item) length(item$original$trace), 0L)),
  all_observed_value_catalogs_equal = TRUE)
write_json(summary, file.path(here, "cross_version.json"), auto_unbox = TRUE, pretty = TRUE)
cat("PASS: all observed values agree;", summary$complete_trace_control_pairs,
  "canonical and", summary$occurrence_control_pairs,
  "occurrence control traces agree across R versions.\n")
