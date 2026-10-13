# Targeted exploration of an unreachable-but-malformed lazy HLA row.
# Run from the repository root. Only this audit directory is written.
out_dir = "attic/cnf_verify3/boolean_occurrence_totality"
plain = new.env(parent = globalenv())
sys.source("R/CnfFormula_simplify.R", plain)
observed = new.env(parent = globalenv())
lines = readLines("R/CnfFormula_simplify.R")
anchor = "    is_not_subset_of_unit = vector(\"list\", length(remaining_nonunit_entries))"
stopifnot(sum(lines == anchor) == 1L)
at = match(anchor, lines)
lines = append(lines, "    observe_unit(environment())", after = at)
eval(parse(text = lines), observed)
found = NULL
observe_unit = function(e) {
  for (i in e$remaining_nonunit_entries) {
    nm = names(e$entries[[i]])
    if (all(nm == e$unitsymbol)) {
      found <<- list(words = words, unit = e$unitsymbol,
        target = e$clause_idx, donor = i, entries = e$entries,
        eliminated = e$eliminated, registry = as.list(e$symbol_registry),
        counts = e$not_subset_count,
        remaining = e$remaining_nonunit_entries,
        lazy = structure(nm != e$unitsymbol, names = nm))
      stop("FOUND", call. = FALSE)
    }
  }
}
u = list2env(setNames(rep(list(c("0", "1")), 6L), paste0("X", 1:6)),
  parent = emptyenv())
convert = function(words) lapply(words, function(w) {
  setNames(lapply(w, function(lit) if (lit > 0L) "1" else "0"), paste0("X", abs(w)))
})
check = function(candidate) {
  words <<- candidate
  result = tryCatch(observed$simplify_cnf(convert(words), u),
    error = function(e) e)
  if (inherits(result, "error")) {
    if (identical(conditionMessage(result), "FOUND")) return(TRUE)
    stop(result)
  }
  FALSE
}
base = list(c(-2L, 3L), c(-1L, -3L), c(3L, 2L, 1L, 1L), c(2L, -1L))
set.seed(6090621L)
limit = as.integer(Sys.getenv("CNF_ALL_FALSE_SEARCH", "10000"))
for (k in seq_len(limit)) {
  if (k <= 4000L) {
    candidate = base
    candidate[[3L]] = c(3L, 2L, rep(1L, sample.int(5L, 1L) + 1L))
    if (k %% 2L == 0L) candidate = lapply(candidate, rev)
    extra = lapply(seq_len(sample.int(3L, 1L)), function(i) {
      symbols = sample.int(4L, sample.int(3L, 1L))
      signed = symbols * sample(c(-1L, 1L), length(symbols), replace = TRUE)
      rep(signed, sample.int(4L, length(symbols), replace = TRUE))
    })
    candidate = c(candidate, extra)
    candidate = candidate[sample.int(length(candidate))]
  } else {
    candidate = lapply(seq_len(sample.int(7L, 1L) + 1L), function(i) {
      symbols = sample.int(4L, sample.int(3L, 1L))
      signed = symbols * sample(c(-1L, 1L), length(symbols), replace = TRUE)
      w = rep(signed, sample.int(4L, length(symbols), replace = TRUE))
      w[sample.int(length(w))]
    })
  }
  if (check(candidate)) break
}
stopifnot(!is.null(found))
# Greedy deletion preserves the observation while making the witness local.
repeat {
  current = found$words
  improved = FALSE
  for (i in seq_along(current)) {
    if (length(current) > 1L && check(current[-i])) {
      improved = TRUE
      break
    }
    for (j in seq_along(current[[i]])) {
      if (length(current[[i]]) == 1L) next
      candidate = current
      candidate[[i]] = candidate[[i]][-j]
      if (check(candidate)) {
        improved = TRUE
        break
      }
    }
    if (improved) break
  }
  if (!improved) break
}
saveRDS(found, file.path(out_dir, "all_false_fixture.rds"))
capture.output(dput(found), file = file.path(out_dir, "all_false_fixture.txt"))
cat("Found after", k, "search cases; reduced signed clauses:\n")
dput(found$words)
cat("Unit:", found$unit, "donor:", found$donor, "lazy row:\n")
print(found$lazy)
cat("Physical initial counts:\n")
print(found$counts)
