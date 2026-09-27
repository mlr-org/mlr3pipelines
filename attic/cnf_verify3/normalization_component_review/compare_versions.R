suppressPackageStartupMessages(library(jsonlite))
here = "attic/cnf_verify3/normalization_component_review"
a = readRDS(file.path(here, "results_r36.rds"))
b = readRDS(file.path(here, "results_r46.rds"))
stopifnot(identical(a[!names(a) %in% c("R", "digests")], b[!names(b) %in% c("R", "digests")]))
for (version in c("r36", "r46")) {
  z = readRDS(file.path(here, paste0("results_", version, ".rds")))
  z$digests = NULL
  z$records = NULL
  z$normalization = as.list(z$normalization)
  z$components = as.list(z$components)
  write_json(z, file.path(here, paste0("results_", version, ".json")), auto_unbox = TRUE, pretty = TRUE)
}
for (stem in c("local_algebra", "boundary_checks")) {
  x = fromJSON(file.path(here, paste0(stem, "_r36.json")), simplifyVector = FALSE)
  y = fromJSON(file.path(here, paste0(stem, "_r46.json")), simplifyVector = FALSE)
  stopifnot(identical(x[names(x) != "R"], y[names(y) != "R"]))
}
result = list(identical_saved_records = TRUE,
  normalization_case_trace_records = length(a$records$normalization),
  component_repeated_trace_records = length(a$records$components),
  binary_serialization_digests_not_compared = TRUE,
  exact_counter_and_control_comparisons = TRUE)
write_json(result, file.path(here, "cross_version.json"), auto_unbox = TRUE, pretty = TRUE)
print(result)
