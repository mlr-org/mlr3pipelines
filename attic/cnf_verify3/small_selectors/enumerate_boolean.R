source("attic/cnf_verify3/small_selectors/common.R")
context = make_context(list(X = c("a", "b"), Y = c("a", "b")))
bank = make_bank(context, max_occurrences = 3L)
stopifnot(length(bank) == 44L)
cat(R.version.string, "bank", length(bank), "\n")
for (n in 1:3) {
  record = new_recorder(paste0("boolean_", n, "_clauses"))
  started = proc.time()[[3L]]
  for (a in seq_along(bank)) {
    if (n == 1L) {
      record(bank[a], context)
    } else for (b in seq_along(bank)) {
      if (n == 2L) {
        record(bank[c(a, b)], context)
      } else for (c in seq_along(bank)) {
        record(bank[c(a, b, c)], context)
      }
    }
    if (a %% 5L == 0L) {
      cat("PROGRESS", n, a, "of", length(bank), "seconds", proc.time()[[3L]] - started, "\n")
      flush.console()
    }
  }
  record(finish = TRUE, extra = list(bank_size = length(bank), max_occurrences = 3L,
    clauses = n, seconds = proc.time()[[3L]] - started))
}
