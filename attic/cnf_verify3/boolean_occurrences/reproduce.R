source("attic/cnf_verify3/boolean_occurrences/common.R")
install_instrumentation()

examples = list(
  first_copy_orphan = list(c(-1L, 2L), c(1L, 1L, 2L, 3L)),
  unrelated_same_name_exception = list(c(1L, 2L), c(1L, 1L, 3L), c(-1L, -1L, 3L)),
  old_registry_snapshot_skip = list(c(-2L, 3L), c(-1L, -3L), c(3L, 2L, 1L, 1L), c(2L, -1L)),
  eventual_orphan_deletion = list(c(-1L, 2L), c(1L, 1L, 2L, 3L), c(-1L, 2L), c(1L, -3L)),
  hidden_deletion_with_orphan_present = list(c(-1L, 2L), c(1L, 1L, 2L, 3L), c(1L, -2L), c(-2L, -3L))
)
for (name in names(examples)) {
  words = examples[[name]]
  context = boolean_context(max(abs(unlist(words))))
  audit$examples = list()
  result = one_case(words, context, name)
  cat("\n", name, "\n", sep = "")
  cat("Input signed occurrence words:\n")
  dput(words)
  cat("Output positional ranges:\n")
  dput(c(result))
  print(data.frame(context$assignments,
    input = word_truth(words, context$assignments),
    output = occurrence_truth(c(result), context$assignments)))
  cat("Observed events:", paste(names(audit$examples), collapse = ", "), "\n")
  if (name == "first_copy_orphan") stopifnot(!is.null(audit$examples$orphan_occurrence))
  if (name == "unrelated_same_name_exception") stopifnot(!is.null(audit$examples$same_name_extra_exception))
  if (name == "old_registry_snapshot_skip") {
    stopifnot(!is.null(audit$examples$unit_cache_skip),
      identical(audit$examples$unit_cache_skip$detail$registered_now, FALSE))
  }
  if (name == "eventual_orphan_deletion") stopifnot(!is.null(audit$examples$delete_orphan_clause))
  if (name == "hidden_deletion_with_orphan_present") {
    stopifnot(!is.null(audit$examples$orphan_occurrence), !is.null(audit$examples$nonunit_hla_deletion))
  }
}

cat("\nAll public-constructor examples preserve their full positional truth tables.\n")
