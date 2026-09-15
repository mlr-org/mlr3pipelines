source("attic/cnf_verify3/small_selectors/common.R")
cat(R.version.string, "; checkmate", as.character(packageVersion("checkmate")), "\n")

context = make_context(list(X = letters[1:3], Y = letters[1:3]))
one = make_selected(list(X = "a"), c(1L, 1L), context)
out = classify(list(one), context, keep_details = TRUE)
stopifnot(out$category == "noncanonical_correct")
cat("\nONE CLAUSE: correct positional semantics, duplicated representation\n")
dput(out$output)

# A physical unit whose range strictly contains the duplicated pseudo-unit
# drives the unit HLA phase into an empty symbol selector.
unit = make_selected(list(X = c("a", "b")), 1L, context)
out = classify(list(one, unit), context, keep_details = TRUE)
stopifnot(out$category == "error", grepl("get1index", out$error, fixed = TRUE))
cat("\nTWO CLAUSES: accepted clause selector; formula construction errors\n")
cat(out$error, "\n")
calls = NULL
invisible(tryCatch(withCallingHandlers(
  CnfFormula(list(one$clause, unit$clause)),
  error = function(e) calls <<- lapply(sys.calls(), deparse)), error = identity))
dput(calls)
flat = make_selected(list(X = "a"), 1L, context)
good = classify(list(flat, unit), context, keep_details = TRUE)
stopifnot(good$category == "canonical_correct")

context = make_context(list(X = c("a", "b"), Y = letters[1:3]))
a = make_selected(list(X = "a", Y = c("b", "c")), 2:1, context)
b = make_selected(list(X = "a", Y = c("c", "a")), c(2L, 1L, 2L), context)
out = classify(list(a, b), context, keep_details = TRUE)
stopifnot(out$category == "noncanonical_stale")
stopifnot(identical(which(out$truth$positional != out$truth$expected), 2L))
cat("\nTWO CLAUSES: stale repeated range; first-occurrence projection is correct\n")
dput(out$output)
print(out$truth)
flat = make_selected(b$raw, unique(b$selector), context)
good = classify(list(a, flat), context, keep_details = TRUE)
stopifnot(good$category == "canonical_correct")

context = make_context(list(X = letters[1:3], Y = letters[1:3]))
selected = list(
  make_selected(list(X = c("b", "c")), c(1L, 1L), context),
  make_selected(list(X = "b", Y = "c"), 1:2, context),
  make_selected(list(X = "c", Y = "b"), 1:2, context),
  make_selected(list(X = "a", Y = "a"), 1:2, context)
)
out = classify(selected, context, keep_details = TRUE)
stopifnot(out$category == "canonical_wrong", !any(out$truth$expected),
  identical(which(out$truth$positional), 3L))
cat("\nFOUR CLAUSES: wrong canonical satisfying output for contradictory input\n")
dput(out$output)
print(out$truth)
selected[[1L]] = make_selected(selected[[1L]]$raw, 1L, context)
good = classify(selected, context, keep_details = TRUE)
stopifnot(good$category == "canonical_correct", identical(good$output, FALSE))
cat("\nAll observed-behavior assertions and flattened-selector controls passed.\n")
