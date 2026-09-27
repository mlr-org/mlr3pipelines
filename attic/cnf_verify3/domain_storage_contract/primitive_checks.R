# Direct base-R check of the source HLA expression under ordinary domain
# storage. Expected multiplicities are computed from integer count vectors.
source("attic/cnf_verify3/domain_storage_contract/harness.R")
suppressPackageStartupMessages(library(jsonlite))
version_tag = if (getRversion() < "4.0.0") "r36" else "r46"
shapes = c("plain", "named", "matrix", "array", "attributes", "inert_class")
shape_domain = function(x, shape) {
  if (shape == "matrix") x = matrix(x, nrow = 1L, dimnames = list("row", rep("column", length(x))))
  if (shape == "array") x = array(x, dim = c(length(x), 1L, 1L), dimnames = list(rep("", length(x)), "x", "y"))
  if (shape %in% c("named", "matrix", "array", "attributes")) names(x) = rep(c(NA_character_, "", "same"), length.out = length(x))
  if (shape == "attributes") {
    attr(x, "audit_tag") = list(vector = 1:3, nested = list(text = "metadata"))
    comment(x) = "inert"
  }
  if (shape == "inert_class") class(x) = "cnf_storage_no_methods_exist"
  x
}
counts = c(extensions = 0L, repeated_new = 0L, named_new = 0L,
  atom_empty_classifications = 0L, atom_full_classifications = 0L, proper_clause_constructions = 0L)
for (n in 1:3) {
  keys = c("", "a.b", "[x]")[seq_len(n)]
  capacities = expand.grid(rep(list(1:3), n))
  for (ci in seq_len(nrow(capacities))) {
    capacity = as.integer(capacities[ci, ])
    stored_base = rep(keys, capacity)
    old_counts = expand.grid(lapply(capacity, function(x) 0:x))
    donors = expand.grid(rep(list(c(FALSE, TRUE)), n))
    for (shape in shapes) {
      stored = shape_domain(stored_base, shape)
      u = CnfUniverse()
      symbol = CnfSymbol(u, "X", stored)
      stopifnot(identical(u[["X"]], stored), isFALSE(CnfAtom(symbol, character())),
        isTRUE(CnfAtom(symbol, keys)))
      counts[["atom_empty_classifications"]] = counts[["atom_empty_classifications"]] + 1L
      counts[["atom_full_classifications"]] = counts[["atom_full_classifications"]] + 1L
      if (n > 1L) {
        cl = CnfClause(list(CnfAtom(symbol, keys[[1L]])))
        stopifnot(identical(domain_bare(cl), list(X = keys[[1L]])))
        counts[["proper_clause_constructions"]] = counts[["proper_clause_constructions"]] + 1L
      }
      for (oi in seq_len(nrow(old_counts))) {
        old_count = as.integer(old_counts[oi, ])
        old = rep(keys, old_count)
        for (di in seq_len(nrow(donors))) {
          donor = as.logical(donors[di, ])
          witness = donor & old_count == 0L
          if (!any(witness)) next
          values_donor = keys[donor]
          new = c(old, stored[!stored %in% c(old, values_donor)])
          expected_count = old_count + ifelse(old_count == 0L & !donor, capacity, 0L)
          actual_count = tabulate(match(domain_flat(new), keys), nbins = n)
          stopifnot(identical(actual_count, as.integer(expected_count)),
            all(expected_count <= capacity), all(expected_count[witness] == 0L),
            length(new) < length(stored), is.null(dim(new)),
            identical(all(keys %in% new), FALSE))
          counts[["extensions"]] = counts[["extensions"]] + 1L
          counts[["repeated_new"]] = counts[["repeated_new"]] + (anyDuplicated(new) > 0L)
          counts[["named_new"]] = counts[["named_new"]] + !is.null(names(new))
        }
      }
    }
  }
}
stopifnot(counts[["extensions"]] == 6L * (3L + 99L + 2457L))
report = list(version = R.version.string, shapes = shapes, counts = as.list(counts))
write_json(report, file.path(domain_here, paste0("primitives_", version_tag, ".json")), pretty = TRUE, auto_unbox = TRUE)
cat(toJSON(report, pretty = TRUE, auto_unbox = TRUE), "\n")
