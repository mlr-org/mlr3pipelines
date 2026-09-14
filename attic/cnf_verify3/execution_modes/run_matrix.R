save_dir = "attic/cnf_verify3/execution_modes"
modes = c("source", "cmpfun0", "cmpfun3", "development", "installed", "installed_no_bytecode")
for (mode in modes) for (jit in 0:3) {
  log = file.path(save_dir, paste0(mode, "_jit", jit, ".log"))
  status = system2(file.path(R.home("bin"), "Rscript"),
    c(file.path(save_dir, "runtime.R"), mode, jit), stdout = log, stderr = log)
  cat(mode, "JIT", jit, "exit", status, "\n")
  if (status != 0L) {
    cat(tail(readLines(log), 20L), sep = "\n")
    stop("Runtime matrix failed.")
  }
}
baseline = readRDS(file.path(save_dir, "source_jit0.rds"))
counts = c(runtimes = 0L, fixture_cases = 0L, path_cases = 0L, serialization_cases = 0L)
for (mode in modes) for (jit in 0:3) {
  result = readRDS(file.path(save_dir, paste0(mode, "_jit", jit, ".rds")))
  stopifnot(identical(result$results, baseline$results), identical(result$serialization, baseline$serialization),
    identical(result$class_loss, baseline$class_loss), identical(result$provenance$source_hashes, baseline$provenance$source_hashes))
  counts[["runtimes"]] = counts[["runtimes"]] + 1L
  counts[["fixture_cases"]] = counts[["fixture_cases"]] + length(result$results)
  counts[["path_cases"]] = counts[["path_cases"]] + sum(vapply(result$results, function(f) length(f$paths), integer(1)))
  counts[["serialization_cases"]] = counts[["serialization_cases"]] + sum(lengths(result$serialization))
}
print(counts)
cat("All runtime outputs, error outcomes, truth functions, and serialization comparisons agree exactly.\n")
saveRDS(counts, file.path(save_dir, "runtime_matrix_counts.rds"))
