# A full three-symbol profile with one productive pass, followed by a
# nonproductive clause reorder. This distinguishes the enumerator's stopping
# criterion from exact ordered-object idempotence after the first call.
source("R/CnfFormula_simplify.R")
domains = list(X = c("a", "b"), Y = c("a", "b", "c"), Z = c("a", "b", "c"))
universe = list2env(domains, parent = emptyenv())
original = list(
  list(X = "a", Y = "a", Z = "a"),
  list(X = "b", Y = c("a", "b"), Z = c("a", "b")),
  list(X = "b", Y = c("a", "b"), Z = c("a", "b")))
bare = function(result) {
  attr(result, "universe") = NULL
  attr(result, "class") = NULL
  result
}
weight = function(clauses) sum(vapply(clauses, function(clause) sum(lengths(clause)), 0L))
first = bare(simplify_cnf(original, universe))
second = bare(simplify_cnf(first, universe))
third = bare(simplify_cnf(second, universe))
stopifnot(weight(first) < weight(original), weight(second) == weight(first),
  !identical(first, second), identical(first[order(lengths(first))], second),
  identical(second, third))
report = list(version = R.version.string, domains = domains, original = original,
  first = first, second = second, third = third,
  weights = c(original = weight(original), first = weight(first), second = weight(second), third = weight(third)),
  productive_passes = 1L, profile_masks = c(X = 66L, Y = 193L, Z = 193L))
suffix = if (getRversion() >= "4.6") "r46" else "r36"
dput(report, paste0("attic/cnf_verify3/symmetry_review/order_only_pass_", suffix, ".R"))
print(report)
