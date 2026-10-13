source("attic/cnf_verify3/small_selectors/common.R")
# Every possible nonempty set of the eight membership cells of three proper
# X ranges. Y occurs in only the two ordinary binary clauses, hence 4 cells.
range_profiles = function(n_ranges) {
  cells = seq.int(0L, bitwShiftL(1L, n_ranges) - 1L)
  result = list()
  for (selection in seq_len(bitwShiftL(1L, length(cells)) - 1L)) {
    present = cells[bitwAnd(selection, bitwShiftL(1L, seq_along(cells) - 1L)) != 0L]
    proper = vapply(seq_len(n_ranges), function(j) {
      inside = bitwAnd(present, bitwShiftL(1L, j - 1L)) != 0L
      any(inside) && any(!inside)
    }, logical(1))
    if (all(proper)) result[[length(result) + 1L]] = present
  }
  result
}
x_profiles = range_profiles(3L)
y_profiles = range_profiles(2L)
stopifnot(length(x_profiles) == 193L, length(y_profiles) == 7L)
record = new_recorder("three_pseudounit_profiles")
started = proc.time()[[3L]]
for (xi in seq_along(x_profiles)) for (yi in seq_along(y_profiles)) {
  xc = x_profiles[[xi]]
  yc = y_profiles[[yi]]
  context = make_context(list(X = paste0("v", xc), Y = paste0("v", yc)))
  xr = lapply(0:2, function(j) paste0("v", xc)[bitwAnd(xc, bitwShiftL(1L, j)) != 0L])
  yr = lapply(0:1, function(j) paste0("v", yc)[bitwAnd(yc, bitwShiftL(1L, j)) != 0L])
  duplicate = make_selected(list(X = xr[[1L]]), c(1L, 1L), context)
  for (a_order in list(1:2, 2:1)) for (b_order in list(1:2, 2:1)) {
    a = make_selected(list(X = xr[[2L]], Y = yr[[1L]]), a_order, context)
    b = make_selected(list(X = xr[[3L]], Y = yr[[2L]]), b_order, context)
    for (position in 0:2) {
      # x/y membership profiles include both orders of the two binary
      # clauses, so no additional A/B swap is needed.
      record(append(list(a, b), list(duplicate), after = position), context)
    }
  }
  if (yi == length(y_profiles) && xi %% 20L == 0L) {
    cat("PROGRESS", xi, "of", length(x_profiles), "seconds", proc.time()[[3L]] - started, "\n")
    flush.console()
  }
}
record(finish = TRUE, extra = list(x_profiles = length(x_profiles), y_profiles = length(y_profiles),
  seconds = proc.time()[[3L]] - started))
