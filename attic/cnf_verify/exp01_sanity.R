# Sanity checks:
# 1. harness oracle agrees with hand-computed truth tables
# 2. harness *detects* deliberately wrong "simplifications" (self-test)
# 3. documented simplification examples from ?CnfFormula behave as documented
# 4. status of the four known API bugs from attic/cnf/CLAUDE.md
#
# Run: Rscript attic/cnf_verify/exp01_sanity.R   (from repo root)

source(file.path(Sys.getenv("CNF_VERIFY_DIR", "attic/cnf_verify"), "harness.R"))
source_cnf()

ok = TRUE
expect = function(cond, what) {
  if (!isTRUE(cond)) {
    ok <<- FALSE
    cat("FAIL:", what, "\n")
  } else cat("ok:", what, "\n")
}

## 1. oracle sanity ---------------------------------------------------------
domains = list(X = c("a", "b", "c"), Y = c("d", "e", "f"))
amat = all_assignments(domains)
expect(nrow(amat) == 9, "9 assignments for 3x3")

cl1 = list(X = c("a", "b"), Y = "d")    # X in {a,b} | Y = d
tt = tt_clause(cl1, amat)
manual = (amat$X %in% c("a", "b")) | (amat$Y == "d")
expect(identical(tt, manual), "tt_clause matches manual evaluation")

cl2 = list(X = c("a", "c"))
expect(identical(tt_clauses(list(cl1, cl2), amat), manual & (amat$X %in% c("a", "c"))),
  "tt_clauses conjunction")

## 2. self-test: oracle must catch wrong results ----------------------------
u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("d", "e", "f"))
uinfo = list(universe = u, domains = domains)

# correct simplification passes
res = check_simplify(list(cl1, cl2), uinfo)
expect(is.null(res), "correct simplification accepted")

# fabricate a WRONG CnfFormula (claims X in {a}), compare truth tables directly
wrong = structure(list(list(X = "a")), universe = u, class = "CnfFormula")
tt_in = tt_clauses(list(cl1, cl2), amat)
tt_wrong = tt_obj(wrong, amat)
expect(!identical(tt_in, tt_wrong), "oracle detects fabricated wrong result")

# structural check catches empty range / full-domain range / duplicate clause
bad1 = structure(list(list(X = character(0))), universe = u, class = "CnfFormula")
expect(length(check_formula_structure(bad1, domains)) > 0, "structure check: empty range")
bad2 = structure(list(list(X = c("a", "b", "c"))), universe = u, class = "CnfFormula")
expect(length(check_formula_structure(bad2, domains)) > 0, "structure check: full-domain range")
bad3 = structure(list(list(X = "a"), list(X = "a")), universe = u, class = "CnfFormula")
expect(length(check_formula_structure(bad3, domains)) > 0, "structure check: duplicate clauses")

## 3. documented examples ----------------------------------------------------
Z = CnfSymbol(u, "Z", c("g", "h", "i"))
domains3 = list(X = domains$X, Y = domains$Y, Z = c("g", "h", "i"))
uinfo3 = list(universe = u, domains = domains3)

# unit propagation
f = `&.CnfFormula`(`|.CnfAtom`(X %among% c("a", "b"), Y %among% c("d", "e")), X %among% c("a", "c"))
expect(is.null(check_simplify(list(list(X = c("a", "b"), Y = c("d", "e")), list(X = c("a", "c"))), uinfo3)),
  "unit propagation example is equivalence-preserving")
bare = unclass(f)
expect(any(vapply(bare, function(cl) identical(sort(names(cl)), c("X", "Y")) && identical(sort(cl$X), "a"), NA)),
  "unit propagation shrinks X range in first clause to {a}")

# resolution subsumption elimination: third clause removed
f = CnfFormula(list(
  bare_to_clause(list(X = "a", Y = "d"), u),
  bare_to_clause(list(X = "b", Y = "e"), u),
  bare_to_clause(list(Y = c("d", "e")), u)
))
expect(length(unclass(f)) == 2, "resolution subsumption removes implied clause")
expect(is.null(check_simplify(list(list(X = "a", Y = "d"), list(X = "b", Y = "e"), list(Y = c("d", "e"))), uinfo3)),
  "resolution subsumption equivalence")

# hidden tautology elimination example from docs (4 clauses -> 3)
hte_clauses = list(
  list(X = c("a", "b"), Y = c("d", "e")),
  list(X = "a", Z = c("g", "h")),
  list(X = "b", Z = c("h", "i")),
  list(Y = c("d", "e"), Z = c("g", "i"))
)
f = CnfFormula(lapply(hte_clauses, bare_to_clause, universe = u))
expect(length(unclass(f)) < 4, "hidden tautology elimination removes a clause")
expect(is.null(check_simplify(hte_clauses, uinfo3)), "hidden tautology elimination equivalence")

# contradiction
f = CnfFormula(list(bare_to_clause(list(X = "a"), u), bare_to_clause(list(X = "b"), u)))
expect(isFALSE(unclass(f)), "contradicting units yield FALSE")

# unrecognized contradiction stays as clauses but must still be equivalent
contra = list(list(X = "a", Y = "d"), list(X = "b", Y = "e"), list(X = "c", Y = "f"))
expect(is.null(check_simplify(contra, uinfo3)), "unrecognized contradiction still equivalent")

## 4. status of known bugs ---------------------------------------------------
cat("\n-- known bug status --\n")
b1 = tryCatch(as.list(as.CnfClause(X %among% c("a", "b", "c"))), error = function(e) e)
cat("bug#1 as.list(TRUE clause):", if (inherits(b1, "error")) "STILL BROKEN" else "works", "\n")
b2 = u[["NoSuchVariable"]]
cat("bug#2 u[[missing]]:", if (is.null(b2)) "STILL silently NULL" else "errors/other", "\n")
b3 = tryCatch(CnfFormula(list(as.CnfClause(TRUE), bare_to_clause(list(X = "a"), u))), error = function(e) e)
cat("bug#3 TRUE clause first in CnfFormula():", if (inherits(b3, "error")) "STILL BROKEN" else "works", "\n")
f1 = as.CnfFormula(bare_to_clause(list(X = "a"), u))
f2 = as.CnfFormula(bare_to_clause(list(Y = "d"), u))
ffalse = as.CnfFormula(FALSE)
b4 = tryCatch(CnfFormula(list(f1, f2, ffalse)), error = function(e) e)
cat("bug#4 FALSE formula after formulas in CnfFormula():", if (inherits(b4, "error")) "STILL BROKEN" else "works", "\n")

cat("\n", if (ok) "ALL SANITY CHECKS PASSED" else "SANITY FAILURES PRESENT", "\n", sep = "")
if (!ok) quit(status = 1)
