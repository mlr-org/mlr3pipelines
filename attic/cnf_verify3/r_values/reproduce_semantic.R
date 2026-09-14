source("attic/cnf_verify3/r_values/bootstrap.R")

case = list(
  domains = list(X = c("a", "b", "c"), Y = c("a", "b", "c")),
  raw = list(
    list(X = c("b", "c")),
    list(X = "b", Y = "c"),
    list(X = "c", Y = "b"),
    list(X = "a", Y = "a")
  ),
  selectors = list(c(1L, 1L), 1:2, 1:2, 1:2)
)

matrix_result = evaluate_case(case)
vector_result = evaluate_case(case, dimensional = FALSE)
stopifnot(is.null(matrix_result$error), is.null(vector_result$error))
stopifnot(!any(matrix_result$expected))
stopifnot(identical(matrix_result$output, list(list(X = "c"), list(Y = "a"))))
stopifnot(identical(matrix_result$positional_mismatch, 3L))
stopifnot(identical(matrix_result$named_mismatch, 3L))
stopifnot(identical(vector_result$output, FALSE))
stopifnot(length(vector_result$positional_mismatch) == 0L)

cat("R:", R.version.string, "; checkmate:", as.character(packageVersion("checkmate")), "\n")
cat("Input: (X in {b,c}) AND (X=b OR Y=c) AND (X=c OR Y=b) AND (X=a OR Y=a).\n")
cat("Only first clause uses a repeated index: matrix(c(1L, 1L), nrow=1L).\n")
cat("Constructor output with matrix selectors:\n")
dput(matrix_result$output)
cat("Constructor output with the same selectors flattened to vectors:\n")
dput(vector_result$output)
cat("Independent truth table, including every input occurrence separately:\n")
print(data.frame(matrix_result$assignments, input = matrix_result$expected,
  matrix = matrix_result$positional, vector = vector_result$positional))

# Every selected input is semantically unchanged by idempotence of OR.
# No custom CNF class is fabricated, and the source is not instrumented.
saveRDS(list(matrix = matrix_result, vector = vector_result),
  file.path("attic/cnf_verify3/r_values", paste0("semantic_r", getRversion(), ".rds")))

cat("\nTwo-clause example: a stale repeated range changes positional OR semantics.\n")
small_case = list(
  domains = list(X = c("a", "b"), Y = c("a", "b", "c")),
  raw = list(list(X = "a", Y = c("b", "c")), list(X = "a", Y = c("c", "a"))),
  selectors = list(2:1, c(2L, 1L, 2L))
)
small_matrix = evaluate_case(small_case)
small_vector = evaluate_case(small_case, dimensional = FALSE)
stopifnot(is.null(small_matrix$error), is.null(small_vector$error))
stopifnot(identical(small_matrix$output, list(list(Y = "c", X = "a", Y = c("c", "a")))))
stopifnot(identical(small_matrix$positional_mismatch, 2L), length(small_matrix$named_mismatch) == 0L)
stopifnot(length(small_vector$positional_mismatch) == 0L)
dput(small_matrix$output)
print(data.frame(small_matrix$assignments, input = small_matrix$expected,
  all_occurrences = small_matrix$positional, first_by_name = small_matrix$named,
  vector = small_vector$positional))
saveRDS(list(matrix = small_matrix, vector = small_vector),
  file.path("attic/cnf_verify3/r_values", paste0("semantic_small_r", getRversion(), ".rds")))
