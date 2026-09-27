# Mixed-class Ops dispatch test, intended to run on R >= 4.3 where
# chooseOpsMethod() resolves the CnfAtom/CnfClause/CnfFormula method conflicts.
# On R < 4.3 mixed-class operations (`atom & clause` etc.) fail with
# "Incompatible methods"; this script documents/verifies the R >= 4.3 behavior.
#
# Runs standalone without checkmate/mlr3misc (minimal shims below), e.g.:
#   podman run --rm -v "$PWD":/repo:ro docker.io/library/r-base \
#     Rscript /repo/attic/cnf_verify/exp12_dispatch_r43.R
#
# Verifies for every ordered pair of operand types (atom, clause, formula,
# logical) that &, | produce the correct truth table (against a mini oracle)
# without warnings, and that ! works on each type.

cat("R version:", R.version.string, "\n")

# --- shims for the few checkmate/mlr3misc functions the Cnf files use -------
assert_class = function(x, cls) { stopifnot(inherits(x, cls)); invisible(x) }
assert_string = function(x) { stopifnot(is.character(x), length(x) == 1); invisible(x) }
assert_character = function(x, any.missing = TRUE, min.len = 0) {
  stopifnot(is.character(x), !anyNA(x) || any.missing, length(x) >= min.len); invisible(x)
}
assert_subset = function(x, choices) { stopifnot(all(x %in% choices)); invisible(x) }
assert_flag = function(x) { stopifnot(is.logical(x), length(x) == 1, !is.na(x)); invisible(x) }
assert_list = function(x, types = NULL) {
  stopifnot(is.list(x))
  if (!is.null(types)) stopifnot(all(vapply(x, function(e) any(vapply(types, inherits, NA, x = e)), NA)))
  invisible(x)
}
assert_atomic = function(x) { stopifnot(is.atomic(x)); invisible(x) }
assert = function(..., .var.name = NULL) invisible(TRUE)  # only used in [.CnfClause
check_numeric = function(...) TRUE
check_subset = function(...) TRUE
check_logical = function(...) TRUE
stopf = function(fmt, ...) stop(sprintf(fmt, ...), call. = FALSE)
map_chr = function(x, f, ...) vapply(x, f, character(1), ...)

src_dir = if (dir.exists("/repo/R")) "/repo/R" else "R"
for (f in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path(src_dir, paste0(f, ".R")))
}

if (getRversion() < "4.3.0") {
  cat("R < 4.3: mixed-class Ops are expected to fail; nothing to verify here.\n")
  quit(status = 0)
}

u = CnfUniverse()
X = CnfSymbol(u, "X", c("a", "b", "c"))
Y = CnfSymbol(u, "Y", c("d", "e", "f"))
domains = list(X = c("a", "b", "c"), Y = c("d", "e", "f"))
assignments = expand.grid(domains, stringsAsFactors = FALSE)

tt_of = function(x) {
  if (inherits(x, "CnfAtom")) {
    b = unclass(x)
    if (is.logical(b)) return(rep(c(b), nrow(assignments)))
    return(assignments[[b$symbol]] %in% b$values)
  }
  if (inherits(x, "CnfClause")) {
    b = unclass(x)
    if (is.logical(b)) return(rep(c(b), nrow(assignments)))
    return(Reduce(`|`, lapply(names(b), function(s) assignments[[s]] %in% b[[s]])))
  }
  if (inherits(x, "CnfFormula")) {
    b = unclass(x)
    if (is.logical(b)) return(rep(c(b), nrow(assignments)))
    return(Reduce(`&`, lapply(b, function(cl) Reduce(`|`, lapply(names(cl), function(s) assignments[[s]] %in% cl[[s]])))))
  }
  if (is.logical(x)) return(rep(x, nrow(assignments)))
  stop("bad type")
}

operands = list(
  atom = X %among% c("a", "b"),
  atom2 = Y %among% "d",
  clause = X %among% "a" | Y %among% c("d", "e"),
  formula = (X %among% c("a", "b") | Y %among% "d") & (Y %among% c("d", "e")),
  true = TRUE,
  false = FALSE
)

n_ok = 0L
n_fail = 0L
for (n1 in names(operands)) {
  for (n2 in names(operands)) {
    if (n1 %in% c("true", "false") && n2 %in% c("true", "false")) next
    e1 = operands[[n1]]; e2 = operands[[n2]]
    for (op in c("&", "|")) {
      expected = if (op == "&") tt_of(e1) & tt_of(e2) else tt_of(e1) | tt_of(e2)
      res = withCallingHandlers(
        tryCatch(do.call(op, list(e1, e2)), error = function(e) e),
        warning = function(w) {
          cat(sprintf("WARNING in %s %s %s: %s\n", n1, op, n2, conditionMessage(w)))
          invokeRestart("muffleWarning")
        })
      if (inherits(res, "error")) {
        cat(sprintf("FAIL %s %s %s: ERROR %s\n", n1, op, n2, conditionMessage(res)))
        n_fail = n_fail + 1L
      } else if (!identical(tt_of(res), expected)) {
        cat(sprintf("FAIL %s %s %s: wrong truth table\n", n1, op, n2))
        n_fail = n_fail + 1L
      } else n_ok = n_ok + 1L
    }
  }
}
for (n1 in c("atom", "clause", "formula")) {
  res = tryCatch(!operands[[n1]], error = function(e) e)
  if (inherits(res, "error") || !identical(tt_of(res), !tt_of(operands[[n1]]))) {
    cat(sprintf("FAIL !%s\n", n1)); n_fail = n_fail + 1L
  } else n_ok = n_ok + 1L
}
cat(sprintf("dispatch checks: %d ok, %d failed\n", n_ok, n_fail))
quit(status = as.integer(n_fail > 0))
