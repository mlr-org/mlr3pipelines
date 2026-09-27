suppressMessages(library(jsonlite))
audit_dir = "attic/cnf_verify3/wide_four_boundary"
one = readRDS(file.path(audit_dir, "checks_r36.rds"))
two = readRDS(file.path(audit_dir, "checks_r46.rds"))
result = list(records_equal = identical(one$records, two$records),
  all_core_traces_equal = identical(one$traces, two$traces),
  inputs_equal = identical(one$core, two$core) && identical(one$domains, two$domains))
stopifnot(all(unlist(result)))
write_json(result, file.path(audit_dir, "cross_version.json"), pretty = TRUE, auto_unbox = TRUE)
cat(toJSON(result, pretty = TRUE, auto_unbox = TRUE), "\n")
