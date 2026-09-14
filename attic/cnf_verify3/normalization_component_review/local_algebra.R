# Exhaustive small storage-algebra check, with an integer-count oracle.
source("attic/cnf_verify3/normalization_component_review/observer.R")
suppressPackageStartupMessages(library(jsonlite))
counts = c(multiplicity_vectors = 0L, selected_states = 0L, shape_checks = 0L,
  repeated_results = 0L, named_results = 0L, two_step_checks = 0L)
for (n in 1:3) {
  labels = c("", "same", "[v]")[seq_len(n)]
  capacities = expand.grid(rep(list(1:2), n))
  donors = expand.grid(rep(list(0:1), n))
  for (mi in seq_len(nrow(capacities))) {
    mu = as.integer(capacities[mi, ])
    counts[["multiplicity_vectors"]] = counts[["multiplicity_vectors"]] + 1L
    old_counts = expand.grid(lapply(mu, function(m) seq.int(0L, m)))
    storage = rev(rep(labels, mu))
    # Dimensions use a doubled sequence to support a nontrivial matrix for
    # every multiplicity vector; the expected capacity doubles too.
    shapes = list(plain = storage,
      named = setNames(storage, rep(c(NA_character_, "", "n"), length.out = length(storage))),
      matrix = matrix(rep(storage, 2L), 2L),
      array = array(rep(storage, 2L), c(1L, 2L, length(storage))))
    for (oi in seq_len(nrow(old_counts))) {
      m = as.integer(old_counts[oi, ])
      old = rep(labels, m)
      for (di in seq_len(nrow(donors))) {
        d = as.integer(donors[di, ])
        if (!any(d == 1L & m == 0L)) next
        donor = labels[d == 1L]
        counts[["selected_states"]] = counts[["selected_states"]] + 1L
        for (shape in shapes) {
          cap = vapply(labels, function(v) sum(flat_values(shape) == v), 0L)
          expected = ifelse(m > 0L, m, ifelse(d == 1L, 0L, cap))
          got = c(old, shape[!shape %in% c(old, donor)])
          observed = vapply(labels, function(v) sum(flat_values(got) == v), 0L)
          stopifnot(all(observed == expected), length(got) < length(shape), all(observed <= cap))
          counts[["shape_checks"]] = counts[["shape_checks"]] + 1L
          counts[["repeated_results"]] = counts[["repeated_results"]] + (anyDuplicated(flat_values(got)) > 0L)
          counts[["named_results"]] = counts[["named_results"]] + !is.null(names(got))
          # A second extension uses the possibly repeated previous result.
          for (dj in seq_len(nrow(donors))) {
            d2 = as.integer(donors[dj, ])
            if (!any(d2 == 1L & expected == 0L)) next
            donor2 = labels[d2 == 1L]
            got2 = c(got, shape[!shape %in% c(got, donor2)])
            expected2 = ifelse(expected > 0L, expected, ifelse(d2 == 1L, 0L, cap))
            observed2 = vapply(labels, function(v) sum(flat_values(got2) == v), 0L)
            stopifnot(all(observed2 == expected2), length(got2) < length(shape), all(observed2 <= cap))
            counts[["two_step_checks"]] = counts[["two_step_checks"]] + 1L
          }
        }
      }
    }
  }
}

# Calibrated failed strengthenings.
stored = c("a", "a", "b")
old = "a"
donor = character()
got = c(old, stored[!stored %in% c(old, donor)])
stopifnot(setequal(got, stored), length(got) != length(stored))
stored2 = c("a", "b")
old2 = c("a", "a")
donor2 = "b"
got2 = c(old2, stored2[!stored2 %in% c(old2, donor2)])
stopifnot(length(got2) == length(stored2), !"b" %in% got2)
result = list(R = R.version.string, counts = as.list(counts),
  full_support_is_not_length_equality = TRUE, missing_value_without_capacity_is_insufficient = TRUE)
version = if (getRversion() < "4") "r36" else "r46"
write_json(result, file.path(review_dir, paste0("local_algebra_", version, ".json")), auto_unbox = TRUE, pretty = TRUE)
print(result)
