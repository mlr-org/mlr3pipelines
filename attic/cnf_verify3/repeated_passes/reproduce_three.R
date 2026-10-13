# Run from the repository root with the current-R launcher. Each call receives
# the actual previous return object, without reconstruction or simplification.
suppressMessages(library(checkmate))
suppressMessages(library(mlr3misc))
for (name in c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify")) {
  source(file.path("R", paste0(name, ".R")))
}

domains = list(G = paste0("g", 0:3), T = c("0", "1"),
  S0 = c("0", "1"), S1 = c("0", "1"), S2 = c("0", "1"))
universe = CnfUniverse()
for (symbol in names(domains)) CnfSymbol(universe, symbol, domains[[symbol]])
initial = list(
  list(S0 = "0", S1 = "0"),
  list(S0 = "1", T = "1", G = "g0"),
  list(S1 = "1", T = "1", G = "g1"),
  list(S2 = "1", T = "1"),
  list(T = "0", G = c("g0", "g1", "g2")),
  list(S2 = "0", T = "0", G = c("g0", "g1")),
  list(S1 = "0", S2 = "0", T = "0", G = "g0")
)

raw_entries = function(formula) {
  attributes(formula) = NULL
  formula
}
mass = function(formula) {
  formula = raw_entries(formula)
  if (is.logical(formula)) return(0L)
  sum(vapply(formula, function(clause) sum(lengths(clause)), 0L))
}
normalized = function(formula) {
  formula = raw_entries(formula)
  if (is.logical(formula)) return(formula)
  sort(vapply(formula, function(clause) {
    paste(vapply(sort(names(clause)), function(symbol) {
      paste0(symbol, "=", paste(sort(clause[[symbol]]), collapse = ","))
    }, ""), collapse = "|")
  }, ""))
}
valuations = expand.grid(domains, stringsAsFactors = FALSE)
truth = function(formula) {
  formula = raw_entries(formula)
  if (is.logical(formula)) return(rep(formula, nrow(valuations)))
  vapply(seq_len(nrow(valuations)), function(row) {
    all(vapply(formula, function(clause) {
      any(vapply(names(clause), function(symbol) {
        valuations[[symbol]][[row]] %in% clause[[symbol]]
      }, FALSE))
    }, FALSE))
  }, FALSE)
}

expected = truth(initial)
current = initial
observed_mass = mass(current)
productive = order_only = logical()
for (pass in seq_len(5L)) {
  result = simplify_cnf(current, universe)
  stopifnot(identical(truth(result), expected))
  productive[[pass]] = !identical(normalized(current), normalized(result))
  order_only[[pass]] = !productive[[pass]] &&
    !identical(raw_entries(current), raw_entries(result))
  observed_mass[[pass + 1L]] = mass(result)
  current = result
}
stopifnot(identical(observed_mass, c(22L, 21L, 20L, 19L, 19L, 19L)))
stopifnot(identical(productive, c(TRUE, TRUE, TRUE, FALSE, FALSE)))
stopifnot(identical(order_only, c(FALSE, FALSE, FALSE, TRUE, FALSE)))
cat("All", nrow(valuations), "truth assignments agree after every actual-object pass.\n")
cat("Value-occurrence counts:", observed_mass, "\n")
cat("Productive passes:", which(productive), "\n")
cat("Ordering-only passes:", which(order_only), "\n")
cat("Exact storage equality confirmed on pass 5.\n")
