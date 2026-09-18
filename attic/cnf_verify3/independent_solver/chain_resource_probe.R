# Follow-up measurement of the root agent's reverse-implication-chain design.
# Standalone real constructors; no package-wide harness is required.
if (nzchar(Sys.getenv("CNF_PROOF_RLIB"))) .libPaths(c(Sys.getenv("CNF_PROOF_RLIB"), .libPaths()))
suppressMessages(library(checkmate))
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}
n = as.integer(Sys.getenv("CNF_CHAIN_N", "128"))
reverse = identical(Sys.getenv("CNF_CHAIN_ORDER", "reverse"), "reverse")
u = CnfUniverse()
syms = lapply(seq_len(n), function(i) CnfSymbol(u, paste0("S", i), c("0", "1")))
links = lapply(seq_len(n - 1L), function(i) {
  CnfClause(list(CnfAtom(syms[[i]], "0"), CnfAtom(syms[[i + 1L]], "1")))
})
if (reverse) links = rev(links)
input = c(list(as.CnfClause(CnfAtom(syms[[1L]], "1"))), links)
cat(R.version.string, "\n", sep = "")
cat("checkmate:", as.character(packageVersion("checkmate")), "\n")
cat("symbols:", n, "reverse:", reverse, "expressions:", getOption("expressions"), "\n")
print(Cstack_info())
start = proc.time()[[3L]]
result = tryCatch(CnfFormula(input), error = identity)
cat("seconds:", proc.time()[[3L]] - start, "\n")
if (inherits(result, "error")) {
  cat("ERROR:", conditionMessage(result), "\n")
} else {
  bare = c(result)
  stopifnot(is.list(bare), length(bare) == n, all(lengths(bare) == 1L))
  stopifnot(setequal(unlist(lapply(bare, names)), paste0("S", seq_len(n))))
  stopifnot(all(vapply(bare, function(cl) identical(cl[[1L]], "1"), logical(1))))
  cat("PASS: exactly the", n, "entailed units\n")
}
