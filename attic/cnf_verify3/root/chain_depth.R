# Run from repository root with Rscript. Uses only production constructors.
# CNF_CHAIN_N controls the number of binary symbols; CNF_CHAIN_ORDER is
# forward/reverse. Every clause is X_i = 0 OR X_(i+1) = 1, plus unit X_1 = 1.
# The exact result is the conjunction of all units X_i = 1, by induction.
source("attic/cnf_verify/harness.R")
source_cnf()

n_symbols = as.integer(Sys.getenv("CNF_CHAIN_N", "300"))
clause_order = Sys.getenv("CNF_CHAIN_ORDER", "reverse")
stopifnot(n_symbols >= 2L, clause_order %in% c("forward", "reverse"))

universe = CnfUniverse()
symbols = lapply(seq_len(n_symbols), function(i) {
  CnfSymbol(universe, paste0("X", i), c("0", "1"))
})
links = lapply(seq_len(n_symbols - 1L), function(i) {
  CnfClause(list(CnfAtom(symbols[[i]], "0"), CnfAtom(symbols[[i + 1L]], "1")))
})
if (clause_order == "reverse") links = rev(links)
clauses = c(list(as.CnfClause(CnfAtom(symbols[[1L]], "1"))), links)

cat(R.version.string, "\n")
cat("symbols:", n_symbols, "order:", clause_order, "\n")
cat("expressions limit:", getOption("expressions"), "\n")
print(Cstack_info())
start = proc.time()[["elapsed"]]
result = tryCatch(CnfFormula(clauses), error = identity)
cat("elapsed:", proc.time()[["elapsed"]] - start, "\n")
if (inherits(result, "error")) {
  cat("ERROR:", conditionMessage(result), "\n")
} else {
  bare = unclass(result)
  stopifnot(is.list(bare), length(bare) == n_symbols, all(lengths(bare) == 1L))
  seen = unlist(lapply(bare, names), use.names = FALSE)
  stopifnot(setequal(seen, paste0("X", seq_len(n_symbols))))
  stopifnot(all(vapply(bare, function(cl) identical(cl[[1L]], "1"), NA)))
  cat("PASS: exactly all", n_symbols, "entailed units\n")
}
