source("attic/cnf_verify3/r_values/bootstrap.R")

prior = readRDS("attic/cnf_verify3/representation/duplicate_selector_results.rds")
seeds = Filter(function(x) length(x$named_mismatch) > 0L, prior$semantic)
save_dir = "attic/cnf_verify3/r_values"
mode = Sys.getenv("CNF_MIN_MODE", "both")
n_calls = 0L
accept = function(case) {
  n_calls <<- n_calls + 1L
  if (!length(case$raw) || any(lengths(case$raw) == 0L) || any(lengths(case$selectors) == 0L)) return(FALSE)
  # Every case continues to select every original symbol at least once.
  if (!all(vapply(seq_along(case$raw), function(i) setequal(case$selectors[[i]], seq_along(case$raw[[i]])), logical(1)))) return(FALSE)
  if (!all(vapply(seq_along(case$raw), function(i) {
    all(vapply(seq_along(case$raw[[i]]), function(j) {
      values = case$raw[[i]][[j]]
      domain = case$domains[[names(case$raw[[i]])[[j]]]]
      length(values) > 0L && all(values %in% domain) && !all(domain %in% values)
    }, logical(1)))
  }, logical(1)))) return(FALSE)
  out = evaluate_case(case)
  good = is.null(out$error) && length(out$positional_mismatch) > 0L
  if (mode == "both") good = good && length(out$named_mismatch) > 0L
  good
}

minimize = function(case) {
  repeat {
    changed = FALSE
    for (ci in seq_along(case$raw)) {
      next_case = case
      next_case$raw = next_case$raw[-ci]
      next_case$selectors = next_case$selectors[-ci]
      if (accept(next_case)) { case = next_case; changed = TRUE; break }
    }
    if (changed) next
    for (ci in seq_along(case$raw)) {
      for (ii in seq_along(case$selectors[[ci]])) {
        next_case = case
        next_case$selectors[[ci]] = next_case$selectors[[ci]][-ii]
        if (accept(next_case)) { case = next_case; changed = TRUE; break }
      }
      if (changed) break
    }
    if (changed) next
    for (ci in seq_along(case$raw)) {
      for (si in seq_along(case$raw[[ci]])) {
        next_case = case
        next_case$raw[[ci]] = next_case$raw[[ci]][-si]
        sel = next_case$selectors[[ci]]
        sel = sel[sel != si]
        sel[sel > si] = sel[sel > si] - 1L
        next_case$selectors[[ci]] = sel
        if (accept(next_case)) { case = next_case; changed = TRUE; break }
      }
      if (changed) break
    }
    if (changed) next
    for (ci in seq_along(case$raw)) {
      for (si in seq_along(case$raw[[ci]])) {
        for (vi in seq_along(case$raw[[ci]][[si]])) {
          next_case = case
          next_case$raw[[ci]][[si]] = next_case$raw[[ci]][[si]][-vi]
          if (accept(next_case)) { case = next_case; changed = TRUE; break }
        }
        if (changed) break
      }
      if (changed) break
    }
    if (changed) next
    for (sym in names(case$domains)) {
      for (v in case$domains[[sym]]) {
        next_case = case
        next_case$domains[[sym]] = setdiff(next_case$domains[[sym]], v)
        for (ci in seq_along(next_case$raw)) if (sym %in% names(next_case$raw[[ci]])) {
          next_case$raw[[ci]][[sym]] = setdiff(next_case$raw[[ci]][[sym]], v)
        }
        if (accept(next_case)) { case = next_case; changed = TRUE; break }
      }
      if (changed) break
    }
    if (!changed) break
  }
  used_symbols = unique(unlist(lapply(case$raw, names)))
  case$domains = case$domains[names(case$domains) %in% used_symbols]
  case
}

results = lapply(seq_along(seeds), function(i) {
  case = seeds[[i]][c("domains", "raw", "selectors")]
  stopifnot(accept(case))
  case = minimize(case)
  result = evaluate_case(case)
  vector_result = evaluate_case(case, dimensional = FALSE)
  stopifnot(is.null(vector_result$error), length(vector_result$positional_mismatch) == 0L)
  cat("Seed", seeds[[i]]$trial, "clauses", length(case$raw), "atoms", sum(lengths(case$raw)),
    "selected", sum(lengths(case$selectors)), "calls", n_calls, "\n")
  dput(result)
  result
})
saveRDS(results, file.path(save_dir, paste0("minimized_", mode, ".rds")))
cat("R:", R.version.string, "; checkmate:", as.character(packageVersion("checkmate")), "\n")
