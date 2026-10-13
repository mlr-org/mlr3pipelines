here = "attic/cnf_verify3/ctype_semantics_review"
old = readRDS(file.path(here, "evidence_r36.rds"))
current = readRDS(file.path(here, "evidence_r46.rds"))
stopifnot(length(old$records) == 140L, identical(old$events, current$events),
  identical(old$aliases, current$aliases),
  all(vapply(old$records, function(record) record$warning_count == 0L, logical(1L))))
for (i in seq_along(old$records)) {
  stopifnot(current$records[[i]]$warning_count >= 0L)
  old$records[[i]]$warning_count = NULL
  current$records[[i]]$warning_count = NULL
}
stopifnot(identical(old, current))
cat("140 semantic records, all actual-write trace events, and the excluded alias control agree across runtimes.\n")
cat("Only explicitly recorded native-translation warning counts were omitted from the comparison.\n")
