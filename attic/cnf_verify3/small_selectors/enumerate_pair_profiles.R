source("attic/cnf_verify3/small_selectors/common.R")
# For two input ranges, every value has one of four membership profiles.
# Retain one representative of every present profile. Each present input
# range must be nonempty and proper; a range absent from a clause is empty.
profiles = list(c(0L, 1L), c(0L, 2L), c(0L, 3L), c(1L, 2L),
  c(0L, 1L, 2L), c(0L, 1L, 3L), c(0L, 2L, 3L), c(1L, 2L, 3L), 0:3)
record = new_recorder("pair_profiles_4_occurrences")
started = proc.time()[[3L]]
for (n_symbols in 1:2) {
profile_grid = expand.grid(setNames(rep(list(seq_along(profiles)), n_symbols), c("X", "Y")[seq_len(n_symbols)]))
for (p in seq_len(nrow(profile_grid))) {
  cells = lapply(profile_grid, function(column) profiles[[column[[p]]]])
  domains = lapply(cells, function(x) paste0("v", x))
  context = make_context(domains)
  raw = lapply(1:2, function(ci) {
    cl = lapply(cells, function(x) paste0("v", x)[bitwAnd(x, bitwShiftL(1L, ci - 1L)) != 0L])
    cl[lengths(cl) > 0L]
  })
  if (any(lengths(raw) == 0L)) next
  banks = lapply(raw, function(cl) lapply(selector_words(length(cl), 4L), function(sel) {
    make_selected(cl, sel, context)
  }))
  for (a in seq_along(banks[[1L]])) for (b in seq_along(banks[[2L]])) {
    record(list(banks[[1L]][[a]], banks[[2L]][[b]]), context)
  }
  if (p %% 10L == 0L) {
    cat("PROGRESS", p, "of", nrow(profile_grid), "seconds", proc.time()[[3L]] - started, "\n")
    flush.console()
  }
}
}
record(finish = TRUE, extra = list(profiles = profiles, max_occurrences = 4L,
  max_symbols = 2L, seconds = proc.time()[[3L]] - started))
