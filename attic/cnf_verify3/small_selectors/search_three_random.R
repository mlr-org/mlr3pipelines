source("attic/cnf_verify3/small_selectors/common.R")
set.seed(26090637)
record = new_recorder("three_random_100000")
started = proc.time()[[3L]]
for (trial in seq_len(100000L)) {
  n_symbols = sample.int(5L, 1L)
  domains = setNames(lapply(seq_len(n_symbols), function(i) letters[seq_len(sample.int(4L, 1L) + 1L)]),
    LETTERS[seq_len(n_symbols)])
  context = make_context(domains)
  selected = lapply(1:3, function(ci) {
    indices = sort(sample.int(n_symbols, sample.int(min(n_symbols, 4L), 1L)))
    raw = lapply(domains[indices], function(d) d[sort(sample.int(length(d), sample.int(length(d) - 1L, 1L)))])
    selector = c(seq_along(raw), if (runif(1L) < .85) sample.int(length(raw), sample.int(3L, 1L), replace = TRUE))
    selector = selector[sample.int(length(selector))]
    make_selected(raw, selector, context)
  })
  record(selected, context)
  if (trial %% 5000L == 0L) {
    cat("PROGRESS", trial, "seconds", proc.time()[[3L]] - started, "\n")
    flush.console()
  }
}
record(finish = TRUE, extra = list(seed = 26090637L, max_symbols = 5L,
  max_domain = 5L, max_extra_occurrences = 3L, seconds = proc.time()[[3L]] - started))
