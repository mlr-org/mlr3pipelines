# Load this reviewer's definitions and instrumentation, stopping before tests.
definitions = parse("attic/cnf_verify3/pass_bound_review/check_frozen.R")
first_test = which(vapply(as.list(definitions), function(x) is.call(x) &&
  identical(x[[1L]], as.name("for")) && identical(x[[2L]], as.name("q")), logical(1)))[[1L]]
for (i in seq_len(first_test - 1L)) eval(definitions[[i]], envir = .GlobalEnv)
results = list()
for (q in 2:4) {
  domains = setNames(rep(list(c("a", "b", "c", "d")), q), paste0("s", seq_len(q)))
  blocks = list(c("a", "b"), c("b", "c"), c("b", "d"))
  clauses = lapply(blocks, function(block) setNames(rep(list(block), q), names(domains)))
  checked = run_case(paste0("overlapping_threshold_", q), domains, clauses)
  prepared = make_objects(domains, clauses)
  output = c(CnfFormula(prepared$objects))
  rows = expand.grid(domains, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  if (q == 2L) {
    reference = list(setNames(rep(list("b"), q), names(domains)))
    stopifnot(identical(truth(clauses, rows), truth(reference, rows)), checked$productive_passes > 0L)
    cat("Two copies: equivalent to s1=b OR s2=b, with a proper nonempty range shrink.\n")
  } else {
    stopifnot(identical(normal(output), normal(clauses)), checked$productive_passes == 0L,
      checked$frozen_symbols == q)
    cat(q, " copies: all original ranges frozen; output unchanged.\n", sep = "")
  }
  results[[as.character(q)]] = list(check = checked, domains = domains, clauses = clauses, output = output,
    models = sum(truth(clauses, rows)), worlds = nrow(rows))
}
saveRDS(results, file.path(save_dir, "threshold_overlap.rds"))
jsonlite::write_json(results, file.path(save_dir, "threshold_overlap.json"), pretty = TRUE, auto_unbox = TRUE)
