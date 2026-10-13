# Independent ordinary-R boundary controls; no author's helper is imported.
out_dir = "attic/cnf_verify3/index_contract_review"
tag = Sys.getenv("CNF_REVIEW_TAG", "r36")
checked = 0L
check = function(x) { stopifnot(isTRUE(x)); checked <<- checked + 1L }

for (n in 0:1) {
  ct = matrix(NA_integer_, n, n)
  enabled = ct != 2L
  check(identical(dim(enabled), c(n, n)))
  disabled = logical(n)
  enabled[disabled, ] = TRUE
  enabled[, disabled] = TRUE
  check(identical(dim(enabled), c(n, n)))
  todo = which(!enabled, arr.ind = TRUE)
  check(identical(dim(todo), c(0L, 2L)))
  check(identical(seq_len(nrow(todo)), integer()))
}
ct = matrix(NA_integer_, 1L, 1L)
counts = ct[integer(), 1L]
check(length(counts) == 0L && is.null(dim(counts)))
check(identical(match(TRUE, counts == 1L & !logical()), NA_integer_))

mat = matrix(c(TRUE, FALSE), nrow = 1L, dimnames = list(NULL, c("X", "Z")))
check(is.null(dim(mat[, "X"])) && identical(unname(mat[, "X"]), TRUE))
check(identical(sum(mat[1L, c(0L, 0L)]), 0L))
check(identical(sum(mat[1L, c(0L, 1L)]), 1L))
clause = list(Z = "0")
check(is.null(clause[["X"]]))
check(identical(match("X", names(clause)), NA_integer_))
check(identical(match("X", colnames(mat)), 1L))

candidate = (1:3)[c(TRUE, NA, FALSE)]
check(identical(candidate, c(1L, NA_integer_)))
check(identical(candidate[!is.na(candidate)], 1L))

source_lines = readLines("R/CnfFormula_simplify.R")
records = list()
.record_allocation = function(entries, available, mats, ct, enabled) {
  n = length(available)
  check(length(mats) == n && identical(dim(ct), c(n, n)) && identical(dim(enabled), c(n, n)))
  check(all(vapply(mats, is.null, logical(1))))
  records[[length(records) + 1L]] <<- list(n = n, matrix_dim = dim(ct), entries = entries)
}
annotation = '.record_allocation(entries, available, is_not_subset_of, not_subset_count, second_order_enabled_matrix)'
instrumented = c(source_lines[1:550], annotation, source_lines[551:length(source_lines)])
e = new.env()
eval(parse(text = instrumented), envir = e)
u = as.environment(list(X = c("0", "1"), Y = c("0", "1")))
invisible(e$simplify_cnf(list(list(X = "0"), list(X = "0", Y = "0")), u))
invisible(e$simplify_cnf(list(list(X = "0", Y = "0")), u))
check(identical(vapply(records, function(x) x$n, integer(1)), 0:1))
saveRDS(list(version = R.version.string, checks = checked, allocations = records),
  file.path(out_dir, paste0("reduced_shapes_", tag, ".rds")))
cat("PASS", checked, "reduced shape assertions; exact-source allocation sizes 0 and 1\n")
