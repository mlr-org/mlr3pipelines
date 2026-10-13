# Public, independently small reproduction of the lazy-row distinction.
# Run from the repository root, using native R or run_r46.sh.
suppressPackageStartupMessages(library(checkmate))
stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
out_dir = "attic/cnf_verify3/boolean_occurrence_totality"
tag = if (getRversion() < "4") "r36" else "r46"
cnf = new.env(parent = globalenv())
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause",
  "CnfFormula", "CnfFormula_simplify")) sys.source(file.path("R", paste0(file, ".R")), cnf)
u = cnf$CnfUniverse()
x = lapply(1:3, function(i) cnf$CnfSymbol(u, paste0("X", i), c("0", "1")))
words = list(c(-1L, 2L), c(3L, -2L), c(1L, 1L, 1L), c(-3L, -1L))
clauses = lapply(words, function(word) {
  literals = unique(word)
  base = cnf$CnfClause(lapply(literals, function(lit) cnf$CnfAtom(x[[abs(lit)]], if (lit > 0L) "1" else "0")))
  cnf$`[.CnfClause`(base, matrix(match(word, literals), nrow = 1L))
})
stopifnot(identical(names(clauses[[3L]]), rep("X1", 3L)))
public_result = cnf$CnfFormula(clauses)

# One read-only observation inserted after the unit lazy-list allocation.
unit_states = list()
observe_unit = function(e) {
  unit_states[[length(unit_states) + 1L]] <<- list(unit = e$unitsymbol,
    entries = e$entries, eliminated = e$eliminated,
    registry = as.list(e$symbol_registry), remaining = e$remaining_nonunit_entries,
    counts = e$not_subset_count, used = e$was_used,
    selected = match(TRUE, e$not_subset_count == 1L & !e$was_used))
}
lines = readLines("R/CnfFormula_simplify.R")
anchor = '    is_not_subset_of_unit = vector("list", length(remaining_nonunit_entries))'
stopifnot(sum(lines == anchor) == 1L)
annotated = append(lines, "    observe_unit(environment())", after = match(anchor, lines))
observed = new.env(parent = globalenv())
eval(parse(text = annotated), observed)
stopifnot(identical(public_result, observed$simplify_cnf(lapply(clauses, c), u)))
stopifnot(length(unit_states) == 1L)
state = unit_states[[1L]]
stopifnot(identical(state$unit, "X1"), identical(state$counts, c(2L, 2L)),
  !length(state$registry$X1), is.na(state$selected))
donor = state$entries[[tail(state$remaining, 1L)]]
stopifnot(identical(donor, list(X1 = "1", X1 = "1")))
hypothetical_lazy = structure(names(donor) != state$unit, names = names(donor))
hypothetical_pivot = names(hypothetical_lazy)[hypothetical_lazy]
stopifnot(identical(unname(hypothetical_lazy), c(FALSE, FALSE)),
  identical(hypothetical_pivot, character(0L)), sum(hypothetical_lazy) != tail(state$counts, 1L))

# Positional independent truth tables: every occurrence is visited by number.
assignments = as.matrix(expand.grid(rep(list(c("0", "1")), 3L), stringsAsFactors = FALSE))
input_truth = apply(assignments, 1L, function(row) all(vapply(words, function(word) {
  any(vapply(word, function(lit) row[[abs(lit)]] == if (lit > 0L) "1" else "0", logical(1L)))
}, logical(1L))))
output_truth = apply(assignments, 1L, function(row) {
  if (is.logical(public_result)) return(as.vector(public_result))
  all(vapply(c(public_result), function(clause) any(vapply(seq_along(clause), function(j) {
    i = as.integer(sub("X", "", names(clause)[[j]], fixed = TRUE))
    row[[i]] %in% clause[[j]]
  }, logical(1L))), logical(1L)))
})
stopifnot(identical(input_truth, output_truth), !any(input_truth))

# The actual guard is necessary: bypassing it in a private source copy makes
# this exact all-FALSE row reach the zero-length list index at source line 754.
forced = lines
stopifnot(grepl("hla_clause_idx = match(TRUE, not_subset_count == 1L", forced[[745L]], fixed = TRUE))
forced[[745L]] = "      hla_clause_idx = length(remaining_nonunit_entries)"
broken = new.env(parent = globalenv())
eval(parse(text = forced), broken)
forced_error = tryCatch({ broken$simplify_cnf(lapply(clauses, c), u); NA_character_ }, error = conditionMessage)
direct_error = tryCatch({ list(X1 = "0")[[hypothetical_pivot]]; NA_character_ }, error = conditionMessage)
stopifnot(!is.na(forced_error), identical(forced_error, direct_error))

# Reduced ordinary-R shape checks for the duplicate-name operations used by
# the real source. These are not simulations of a complete simplifier.
shape_checks = 0L
verify = function(ok) {
  stopifnot(isTRUE(ok))
  shape_checks <<- shape_checks + 1L
}
m = matrix(TRUE, 3L, 4L, dimnames = list(NULL, c("X", "X", "Y", "X")))
verify(identical(m[, "X"], c(TRUE, TRUE, TRUE)))
verify(identical(unname(m[1L, "X"]), TRUE))
m[, "X"] = FALSE
verify(all(!m[, 1L]) && all(m[, 2:4, drop = FALSE]))
verify(identical(match(c("X", "X"), colnames(m)), c(1L, 1L)))
verify(identical(match(c("X", "absent"), colnames(m), nomatch = 0L), c(1L, 0L)))
verify(length(m[1L, c(0L, 0L)]) == 0L)
verify(length(m[1L, c(1L, 1L)]) == 2L)
verify(sum(m[1L, c(1L, 1L)]) == 0L)
clause = list(X = "1", X = "1", Y = "0")
verify(identical(clause[["X"]], "1"))
clause[["X"]] = NULL
verify(identical(clause, list(X = "1", Y = "0")))
saved = c(2L, 2L, 2L, 4L)
registry = saved[saved != 2L]
verify(identical(registry, 4L))
verify(identical(saved, c(2L, 2L, 2L, 4L)))
verify(identical(names(c(X = FALSE, X = FALSE))[c(FALSE, FALSE)], character(0L)))
for (n in c(0L, 1L, 2L, 7L)) verify(identical(match(TRUE, rep(FALSE, n)), NA_integer_))
verify(identical(list(X = "1")[["absent"]], NULL))
verify(identical(dim(matrix(TRUE, 0L, 2L)[, , drop = FALSE]), c(0L, 2L)))
verify(identical(dim(which(matrix(FALSE, 0L, 0L), arr.ind = TRUE)), c(0L, 2L)))

result = list(R = R.version.string, words = words, public_clauses = lapply(clauses, c),
  output = c(public_result), unit_hla = state, hypothetical_lazy = hypothetical_lazy,
  hypothetical_pivot = hypothetical_pivot, input_truth = input_truth, output_truth = output_truth,
  forced_donor_error = forced_error, reduced_shape_checks = shape_checks)
saveRDS(result, file.path(out_dir, paste0("reproduction_", tag, ".rds")))
print(result)
