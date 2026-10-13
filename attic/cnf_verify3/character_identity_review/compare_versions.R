suppressMessages(library(jsonlite))
audit_dir = "attic/cnf_verify3/character_identity_review"
old = readRDS(file.path(audit_dir, "checks_r36.rds"))
new = readRDS(file.path(audit_dir, "checks_r46.rds"))
result = list(cases_equal = identical(old$cases, new$cases),
  exact_normalized_baselines_equal = identical(old$baselines, new$baselines),
  sites_equal = identical(old$sites, new$sites), site_counts_equal = identical(old$site_counts, new$site_counts),
  per_formula_records_equal = identical(old$records, new$records),
  public_api_records_equal = identical(old$api_records, new$api_records))
old_values = readRDS(file.path(audit_dir, "value_locales_r36.rds"))
new_values = readRDS(file.path(audit_dir, "value_locales_r46.rds"))
result$value_locale_baselines_equal = identical(old_values$baselines, new_values$baselines)
stopifnot(all(unlist(result)))
write_json(result, file.path(audit_dir, "cross_version.json"), auto_unbox = TRUE, pretty = TRUE)
cat(toJSON(result, auto_unbox = TRUE, pretty = TRUE), "\n")
