source("attic/cnf_verify3/constructor_domain_closure/harness.R")
counts = c(domains = 0L, proper_atoms = 0L, dimensional_atoms = 0L,
  repeated_atoms = 0L, negations = 0L, clauses = 0L, constants = 0L,
  empty_value_types = 0L, rejected_nonempty_types = 0L)
for (size in 1:4) {
  sequences = expand.grid(rep(list(c("", "a", "b")), size), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(sequences))) {
    stored = as.character(sequences[i, ])
    domain = flat_unique(stored)
    for (domain_shape in shapes(stored)) {
      u = CnfUniverse()
      symbol = CnfSymbol(u, "X", domain_shape)
      stopifnot(identical(u[["X"]], domain_shape))
      counts[["domains"]] = counts[["domains"]] + 1L
      stopifnot(isFALSE(CnfAtom(symbol, character())), isTRUE(CnfAtom(symbol, domain)),
        isTRUE(!CnfAtom(symbol, character())), isFALSE(!CnfAtom(symbol, domain)))
      counts[["constants"]] = counts[["constants"]] + 4L
      for (selection in subsets(domain, proper = TRUE)) {
        # One copy and a differently ordered repeated copy exercise duplicate
        # slices, duplicate elements within a slice, and column-major order.
        for (sequence in list(selection, c(selection, rev(selection), selection))) {
          expected_values = flat_unique(sequence)
          complement = domain[!vapply(domain, function(v) any(v == expected_values), logical(1L))]
          for (value_shape in shapes(sequence)) {
            atom = CnfAtom(symbol, value_shape)
            stopifnot(!is.logical(atom), canonical_object(atom),
              identical(flat_unique(atom$values), expected_values))
            counts[["proper_atoms"]] = counts[["proper_atoms"]] + 1L
            counts[["dimensional_atoms"]] = counts[["dimensional_atoms"]] + !is.null(dim(atom$values))
            counts[["repeated_atoms"]] = counts[["repeated_atoms"]] + (anyDuplicated(flat_values(atom$values)) > 0L)
            negated = !atom
            twice = !negated
            expected_twice = domain[domain %in% expected_values]
            stopifnot(plain_character(negated$values), identical(negated$values, complement),
              plain_character(twice$values), identical(twice$values, expected_twice),
              identical(setdiff(domain_shape, atom$values), complement))
            counts[["negations"]] = counts[["negations"]] + 2L
            clause = as.CnfClause(atom)
            negated_clause = as.CnfClause(negated)
            stopifnot(proper_clause(unclass(clause), u), identical(clause[["X"]], expected_values),
              proper_clause(unclass(negated_clause), u), identical(negated_clause[["X"]], complement))
            counts[["clauses"]] = counts[["clauses"]] + 2L
          }
        }
      }
    }
  }
}

# Empty inputs of several storage types take the early FALSE branch before
# unique(); nonempty values of these noncharacter types are not accepted.
u = CnfUniverse()
x = CnfSymbol(u, "X", c("a", "b"))
for (empty in list(NULL, character(), numeric(), logical(), integer(), complex(),
    raw(), list(), matrix(character(), 0L, 2L), array(character(), c(2L, 0L, 1L)))) {
  atom = CnfAtom(x, empty)
  stopifnot(isFALSE(atom), isTRUE(!atom), isFALSE(as.CnfFormula(atom)))
  counts[["empty_value_types"]] = counts[["empty_value_types"]] + 1L
}
for (bad in list(TRUE, 1L, 1, 1i, as.raw(1), list("a"), factor("a"), NA_character_)) {
  stopifnot(nzchar(attempt(CnfAtom(x, bad))$error))
  counts[["rejected_nonempty_types"]] = counts[["rejected_nonempty_types"]] + 1L
}

# Record the runtime contract used by the proof, including the new guard.
contract_path = file.path(closure_here, paste0("base_contract_", closure_version, ".txt"))
sink(contract_path)
cat(R.version.string, "\ncheckmate ", as.character(packageVersion("checkmate")), "\n", sep = "")
print(base::setdiff)
if (exists(".set_ops_need_as_vector", baseenv())) print(get(".set_ops_need_as_vector", baseenv()))
print(base::unique.matrix)
print(base::unique.array)
for (label in names(shapes(c("a", "b", "b", "c")))) {
  object = shapes(c("a", "b", "b", "c"))[[label]]
  cat("\nShape:", label, "\n")
  cat("class(input):", paste(class(object), collapse = ","), "; class(input[0]):",
    paste(class(object[0L]), collapse = ","), "\n")
  str(setdiff(object, "a"))
  if (exists(".set_ops_need_as_vector", baseenv())) {
    cat("coercion guard:", get(".set_ops_need_as_vector", baseenv())(object, "a"), "\n")
  }
}
sink()
record_results("primitives", counts)
