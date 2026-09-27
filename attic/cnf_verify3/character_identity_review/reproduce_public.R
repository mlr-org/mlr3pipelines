# Run at repository root using Rscript or the existing cnf-review-r46 launcher.
# All CNF objects below come from unchanged public constructors.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
for (file in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(file, ".R")))
}
old_ctype = Sys.getlocale("LC_CTYPE")
old_collate = Sys.getlocale("LC_COLLATE")
for (ctype in c("C.UTF-8", "C")) {
  stopifnot(nzchar(Sys.setlocale("LC_CTYPE", ctype)))
  invisible(Sys.setlocale("LC_COLLATE", "C"))
  name = "\u00e9"
  u = CnfUniverse()
  X = CnfSymbol(u, name, c("a", "b", "c"))
  Y = CnfSymbol(u, "Y", c("a", "b"))
  clauses = list(
    CnfClause(list(X %among% "a")),
    CnfClause(list(X %among% "b", Y %among% "a")),
    CnfClause(list(X %among% "c", Y %among% "b"))
  )
  result = CnfFormula(clauses)
  # Every original domain assignment is identified by its integer position.
  grid = expand.grid(X = c("a", "b", "c"), Y = c("a", "b"), stringsAsFactors = FALSE)
  input_truth = grid$X == "a" & (grid$X == "b" | grid$Y == "a") &
    (grid$X == "c" | grid$Y == "b")
  output_truth = if (is.logical(result)) rep(c(result), nrow(grid)) else {
    vapply(seq_len(nrow(grid)), function(row) {
      all(vapply(c(result), function(clause) {
        any(vapply(seq_along(clause), function(position) {
          symbol_name = names(clause)[[position]]
          stopifnot(identical(enc2utf8(symbol_name), enc2utf8(name)) || identical(symbol_name, "Y"))
          value = if (identical(enc2utf8(symbol_name), enc2utf8(name))) grid$X[[row]] else grid$Y[[row]]
          any(vapply(clause[[position]], function(allowed) identical(value, allowed), FALSE))
        }, FALSE))
      }, FALSE))
    }, FALSE)
  }
  cat("R", as.character(getRversion()), "LC_CTYPE", ctype,
    "input models", sum(input_truth), "output models", sum(output_truth), "\n")
  print(result)
  print(grid[input_truth != output_truth, , drop = FALSE])
  # A fresh plain environment demonstrates the failing key-enumeration premise.
  unit_domains = new.env(parent = emptyenv())
  unit_domains[[name]] = "a"
  cat("name encoding", Encoding(name), "name bytes", as.integer(charToRaw(name)), "\n")
  print(names(unit_domains))
  cat("same name found by membership:", name %in% names(unit_domains), "\n")
  stopifnot(length(u) == 2L, identical(u[[name]], c("a", "b", "c")), identical(u[["Y"]], c("a", "b")))
  if (ctype == "C") stopifnot(sum(input_truth) == 0L, sum(output_truth) == 2L)
  else stopifnot(identical(input_truth, output_truth), identical(c(result), FALSE))
}
invisible(Sys.setlocale("LC_CTYPE", old_ctype))
invisible(Sys.setlocale("LC_COLLATE", old_collate))
