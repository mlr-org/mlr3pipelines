source("attic/cnf_verify3/small_selectors/common.R")
context = make_context(list(X = letters[1:3], Y = letters[1:3]))
all_bank = make_bank(context, 3L)
plain = Filter(function(item) !anyDuplicated(item$selector), all_bank)
# Independent per-symbol relabeling can send an anchored range of size k
# to the first k values. X/Y relabeling permits a one-symbol anchor to use X.
anchors = Filter(function(item) {
  anyDuplicated(item$selector) &&
    (length(item$raw) > 1L || identical(names(item$raw), "X")) &&
    all(vapply(item$raw, function(r) identical(r, letters[seq_along(r)]), logical(1)))
}, all_bank)
stopifnot(length(plain) == 84L, length(anchors) == 28L)
record = new_recorder("three_ternary_one_duplicate")
started = proc.time()[[3L]]
cat(R.version.string, "plain", length(plain), "anchors", length(anchors), "\n")
for (d in seq_along(anchors)) {
  anchor = anchors[[d]]
  for (a in seq_along(plain)) for (b in seq_along(plain)) {
    # Length-three anchors sort after the two canonical clauses regardless
    # of input position. For length-two anchors, cover every tie ordering.
    for (position in if (length(anchor$selector) == 2L) 0:2 else 2L) {
      selected = append(list(plain[[a]], plain[[b]]), list(anchor), after = position)
      record(selected, context)
    }
  }
  cat("PROGRESS", d, "of", length(anchors), "seconds", proc.time()[[3L]] - started, "\n")
  flush.console()
}
record(finish = TRUE, extra = list(plain = length(plain), anchors = length(anchors),
  max_occurrences = 3L, seconds = proc.time()[[3L]] - started))
