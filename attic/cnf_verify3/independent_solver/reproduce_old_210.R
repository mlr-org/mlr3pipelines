# Replay the old campaign's exact experiment-210 candidate on current code.
# Input construction uses explicit constructors to work on R < 4.3.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}
set.seed(210002)
u = CnfUniverse()
dom = c("a", "b", "c", "d", "e")
syms = lapply(setNames(c("A", "B", "C"), c("A", "B", "C")), function(s) CnfSymbol(u, s, dom))
for (trial in seq_len(359)) {
  a1 = sample(dom, sample(1:3, 1))
  a2 = sample(dom, sample(2:4, 1))
  b1 = sample(dom, sample(1:3, 1))
  c1 = sample(dom, sample(1:3, 1))
  clauses = list(
    CnfClause(list(CnfAtom(syms$A, a1), CnfAtom(syms$B, b1))),
    CnfClause(list(CnfAtom(syms$A, a2), CnfAtom(syms$B, b1), CnfAtom(syms$C, c1)))
  )
  for (j in 1:sample(2:5, 1)) {
    n_sym = sample(2:3, 1)
    chosen = sample(names(syms), n_sym)
    atoms = lapply(chosen, function(s) CnfAtom(syms[[s]], sample(dom, sample(1:3, 1))))
    clauses[[length(clauses) + 1L]] = CnfClause(atoms)
  }
}
result = CnfFormula(clauses)
result2 = CnfFormula(as.list(result))
record = list(domains = setNames(rep(list(dom), 3), names(syms)),
  clauses = lapply(clauses, c), result = c(result), second_pass = c(result2))
cat(jsonlite::toJSON(record, auto_unbox = TRUE), "\n")
