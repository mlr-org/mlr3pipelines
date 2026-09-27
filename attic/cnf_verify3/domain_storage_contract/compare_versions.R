suppressPackageStartupMessages(library(jsonlite))
here = "attic/cnf_verify3/domain_storage_contract"
one = readRDS(file.path(here, "checks_r36.rds"))
two = readRDS(file.path(here, "checks_r46.rds"))
stopifnot(identical(one$cases, two$cases), identical(one$results, two$results),
  identical(one$first, two$first), identical(one$if_seen, two$if_seen),
  identical(one$report$counts, two$report$counts),
  identical(one$report$source_sha256, two$report$source_sha256))
primitive_one = read_json(file.path(here, "primitives_r36.json"))
primitive_two = read_json(file.path(here, "primitives_r46.json"))
stopifnot(identical(primitive_one$counts, primitive_two$counts))
report = list(versions = c(one$report$version, two$report$version),
  exact_case_output_and_digest_records = length(one$cases),
  exact_first_virtual_witness_records = length(one$first),
  if_outcomes_equal = TRUE, all_run_counts_equal = TRUE, primitive_counts_equal = TRUE)
write_json(report, file.path(here, "cross_version.json"), pretty = TRUE, auto_unbox = TRUE)
cat(toJSON(report, pretty = TRUE, auto_unbox = TRUE), "\n")
