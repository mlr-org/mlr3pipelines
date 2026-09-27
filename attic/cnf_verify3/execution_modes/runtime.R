# Run in a fresh R 4.6 process from the repository root.
args = commandArgs(trailingOnly = TRUE)
mode = args[[1L]]
jit = as.integer(args[[2L]])
save_dir = "attic/cnf_verify3/execution_modes"
stopifnot(getRversion() >= "4.3.0", jit %in% 0:3)
invisible(compiler::enableJIT(jit))
options(width = 160L)

files = file.path("R", paste0(c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "CnfFormula_simplify"), ".R"))
source_hashes = setNames(vapply(files, function(path) digest::digest(file = path, algo = "sha256"), character(1)), files)
source_env = new.env(parent = globalenv())
for (file in files) sys.source(file, envir = source_env, keep.source = FALSE)
cnf_names = ls(source_env, all.names = TRUE)

if (mode %in% c("source", "cmpfun0", "cmpfun3")) {
  suppressPackageStartupMessages({ library(checkmate); library(mlr3misc) })
  for (file in files) sys.source(file, envir = globalenv(), keep.source = FALSE)
  if (mode != "source") {
    optimization = as.integer(sub("cmpfun", "", mode))
    for (nm in cnf_names) assign(nm, compiler::cmpfun(get(nm, globalenv()), options = list(optimize = optimization)), envir = globalenv())
  }
  cnf_env = globalenv()
  package_path = NA_character_
  package_version = NA_character_
} else if (mode == "development") {
  pkgload::load_all(".", export_all = FALSE, helpers = FALSE, quiet = TRUE)
  cnf_env = asNamespace("mlr3pipelines")
  package_path = getNamespaceInfo(cnf_env, "path")
  package_version = as.character(getNamespaceVersion(cnf_env))
} else {
  stopifnot(mode %in% c("installed", "installed_no_bytecode"))
  lib_dir = if (mode == "installed") "library" else "library_no_bytecode"
  .libPaths(c(normalizePath(file.path(save_dir, lib_dir)), .libPaths()))
  suppressPackageStartupMessages(library(mlr3pipelines))
  cnf_env = asNamespace("mlr3pipelines")
  package_path = getNamespaceInfo(cnf_env, "path")
  package_version = as.character(packageVersion("mlr3pipelines"))
}

# Compare syntax/formals, excluding environment and source-location attributes.
# pkgload retains srcref/srcfile/wholeSrcref metadata on language objects;
# identical(body(a), body(b)) does not strip those recursively. removeSource
# changes only the temporary function copies used for this comparison.
# No namespace binding is modified by any mode.
raw_body_matches = vapply(cnf_names, function(nm) {
  a = get(nm, cnf_env, inherits = FALSE)
  b = get(nm, source_env, inherits = FALSE)
  identical(formals(a), formals(b)) && identical(body(a), body(b))
}, logical(1))
body_matches = vapply(cnf_names, function(nm) {
  a = utils::removeSource(get(nm, cnf_env, inherits = FALSE))
  b = utils::removeSource(get(nm, source_env, inherits = FALSE))
  identical(formals(a), formals(b)) && identical(body(a), body(b))
}, logical(1))
stopifnot(all(body_matches))
is_compiled = function(f) tryCatch({ invisible(capture.output(compiler::disassemble(f))); TRUE }, error = function(e) FALSE)
compiled_before = vapply(c("CnfFormula", "CnfClause", "simplify_cnf"), function(nm) is_compiled(get(nm, cnf_env)), logical(1))
api = mget(c("CnfUniverse", "CnfSymbol", "CnfAtom", "CnfClause", "CnfFormula", "as.CnfAtom", "as.CnfClause", "as.CnfFormula"), cnf_env)

# Independent finite-domain truth oracle: enumerate individual assignments and
# evaluate every literal occurrence using equality, without CNF rewrites or
# conversion methods. This also gives repeated-name clauses positional meaning.
truth = function(clauses, assignments) {
  if (is.logical(clauses)) return(rep(as.vector(clauses), nrow(assignments)))
  vapply(seq_len(nrow(assignments)), function(row) {
    all(vapply(clauses, function(clause) {
      if (is.logical(clause)) return(as.vector(clause))
      any(vapply(seq_along(clause), function(i) {
        sym = names(clause)[[i]]
        any(assignments[[sym]][[row]] == clause[[i]])
      }, logical(1)))
    }, logical(1)))
  }, logical(1))
}
weight = function(formula) {
  if (is.logical(formula)) return(0L)
  sum(vapply(formula, function(cl) sum(lengths(cl)), integer(1)))
}
attempt = function(expr) tryCatch(list(output = force(expr)), error = function(e) list(error = conditionMessage(e)))
bare = function(object) c(object)

fixture_names = c("minimized_subsumption.json", "minimized_sse1.json", "minimized_first_order_phase_sse1.json",
  "minimized_sse2.json", "directed_oneend_shrink_min.json", "minimized_oneend_symbol_removal.json", "minimized_deferred_skip.json")
fixtures = lapply(fixture_names, function(file) {
  input = jsonlite::fromJSON(file.path("attic/cnf_verify3/independent_solver", file), simplifyVector = FALSE)
  list(domains = lapply(input$domains, unlist, use.names = FALSE),
    clauses = lapply(input$clauses, function(clause) lapply(clause, unlist, use.names = FALSE)))
})
names(fixtures) = sub(".json", "", fixture_names, fixed = TRUE)
fixtures$selector = list(domains = list(X = c("a", "b", "c"), Y = c("a", "b", "c")),
  clauses = list(list(X = c("b", "c")), list(X = "b", Y = "c"), list(X = "c", Y = "b"), list(X = "a", Y = "a")))

results = list()
serialization_checks = list()
for (name in names(fixtures)) {
  input = fixtures[[name]]
  universe = api$CnfUniverse()
  symbols = lapply(names(input$domains), function(sym) api$CnfSymbol(universe, sym, input$domains[[sym]]))
  names(symbols) = names(input$domains)
  atoms = lapply(input$clauses, function(clause) lapply(names(clause), function(sym) api$CnfAtom(symbols[[sym]], clause[[sym]])))
  clauses = lapply(atoms, api$CnfClause)
  if (name == "selector") clauses[[1L]] = clauses[[1L]][matrix(c(1L, 1L), nrow = 1L)]
  assignments = expand.grid(input$domains, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  expected = truth(input$clauses, assignments)
  # Repetition in the selected first clause preserves its input truth.
  stopifnot(identical(expected, truth(lapply(clauses, bare), assignments)))

  first = api$CnfFormula(clauses)
  second = api$CnfFormula(as.list(first))
  paths = list(constructor = first, second_pass = second)

  # Build every atom with the public membership operator and every disjunction
  # with the public | operator. A single atom is then explicitly coerced.
  operator_clauses = lapply(input$clauses, function(clause) {
    operator_atoms = lapply(names(clause), function(sym) symbols[[sym]] %among% clause[[sym]])
    api$as.CnfClause(Reduce(function(left, right) left | right, operator_atoms))
  })
  if (name == "selector") operator_clauses[[1L]] = operator_clauses[[1L]][matrix(c(1L, 1L), nrow = 1L)]
  paths$operator_atoms_constructor = api$CnfFormula(operator_clauses)
  chain = attempt(api$as.CnfFormula(Reduce(function(left, right) left & right, operator_clauses)))
  if (is.null(chain$error)) paths$operator_and_chain = chain$output
  paths$coercion_identity = api$as.CnfFormula(first)
  paths$identity_left_and = TRUE & first
  paths$identity_right_and = first & TRUE
  paths$identity_left_or = FALSE | first
  paths$identity_right_or = first | FALSE
  stopifnot(identical(bare(paths$operator_atoms_constructor), bare(first)))

  path_results = lapply(names(paths), function(path) {
    object = paths[[path]]
    stopifnot(inherits(object, "CnfFormula"))
    actual = truth(bare(object), assignments)
    if (name != "selector") stopifnot(identical(actual, expected))
    list(path = path, output = bare(object), truth = actual, weight = weight(bare(object)),
      mismatches = which(actual != expected))
  })
  names(path_results) = names(paths)
  if (name == "selector") {
    stopifnot(identical(path_results$constructor$output, list(list(X = "c"), list(Y = "a"))))
    stopifnot(identical(path_results$constructor$mismatches, 3L))
    vector_clauses = clauses
    vector_clauses[[1L]] = api$CnfClause(atoms[[1L]])[c(1L, 1L)]
    vector_output = api$CnfFormula(vector_clauses)
    stopifnot(isFALSE(as.logical(vector_output)))
    path_results$vector_selector = list(path = "vector_selector", output = bare(vector_output),
      truth = truth(bare(vector_output), assignments), weight = 0L, mismatches = integer())
  }

  # These expectations follow from each actual operand's truth function, so the
  # selector's already-wrong first result does not contaminate an oracle.
  first_truth = truth(bare(first), assignments)
  negated = !first
  stopifnot(inherits(negated, "CnfFormula"), identical(truth(bare(negated), assignments), !first_truth))
  stopifnot(identical(truth(bare(first & !first), assignments), rep(FALSE, nrow(assignments))))
  stopifnot(identical(truth(bare(first | !first), assignments), rep(TRUE, nrow(assignments))))

  rounds = list()
  for (version in c(2L, 3L)) {
    restored = unserialize(serialize(first, NULL, version = version))
    restored_second = api$CnfFormula(as.list(restored))
    stopifnot(identical(bare(first), bare(restored)), identical(truth(bare(restored), assignments), first_truth))
    stopifnot(identical(bare(second), bare(restored_second)))
    restored_universe = attr(restored, "universe")
    same_as_original = identical(universe, restored_universe)
    stopifnot(!same_as_original)
    stopifnot(identical(as.list.environment(universe, all.names = TRUE), as.list.environment(restored_universe, all.names = TRUE)))

    bundle = unserialize(serialize(list(formula = first, clauses = clauses, symbols = symbols, universe = universe), NULL, version = version))
    same_in_bundle = identical(attr(bundle$formula, "universe"), bundle$universe) &&
      all(vapply(bundle$clauses, function(clause) identical(attr(clause, "universe"), bundle$universe), logical(1))) &&
      all(vapply(bundle$symbols, function(symbol) identical(attr(symbol, "universe"), bundle$universe), logical(1)))
    stopifnot(same_in_bundle)
    replay = api$CnfFormula(bundle$clauses)
    stopifnot(identical(bare(replay), bare(first)))

    separate = attempt(first & restored)
    stopifnot(!is.null(separate$error), identical(separate$error, "Both formulas must be in the same universe."))
    together = unserialize(serialize(list(first, second), NULL, version = version))
    combined = together[[1L]] & together[[2L]]
    stopifnot(identical(truth(bare(combined), assignments), first_truth & truth(bare(second), assignments)))
    rounds[[paste0("v", version)]] = list(same_universe_as_original = same_as_original,
      same_universe_within_bundle = same_in_bundle, separate_combination_error = separate$error,
      restored_replay = bare(restored_second))
  }
  serialization_checks[[name]] = rounds
  results[[name]] = list(input = input, assignments = assignments, expected = expected, paths = path_results,
    operator_chain_error = chain$error, negation = bare(negated))
  cat(name, ":", nrow(assignments), "worlds;", sum(expected), "input models; weight",
    weight(input$clauses), "->", weight(bare(first)), "->", weight(bare(second)),
    "; chain", if (is.null(chain$error)) weight(bare(chain$output)) else chain$error,
    "; first mismatches", length(path_results$constructor$mismatches), "\n")
}

# Recheck the reverse logical-OR class boundary through each runtime's real S3
# dispatch. Also confirm the accepted missing logical selector remains invalid.
u = api$CnfUniverse()
x = api$CnfSymbol(u, "X", c("a", "b"))
y = api$CnfSymbol(u, "Y", c("a", "b"))
clause = api$CnfClause(list(api$CnfAtom(x, "a"), api$CnfAtom(y, "a")))
class_loss = class(TRUE | clause)
stopifnot(identical(class_loss, "logical"), inherits(clause | TRUE, "CnfClause"))
malformed = clause[c(FALSE, NA)]
stopifnot(anyNA(names(malformed)), is.null(malformed[[1L]]))

compiled_after = vapply(c("CnfFormula", "CnfClause", "simplify_cnf"), function(nm) is_compiled(get(nm, cnf_env)), logical(1))
provenance = list(mode = mode, jit = jit, R = R.version.string, platform = R.version$platform,
  package_version = package_version, package_path = package_path, source_hashes = source_hashes,
  body_matches = body_matches, raw_body_matches = raw_body_matches,
  function_environment = environmentName(environment(api$CnfFormula)),
  compiled_before = compiled_before, compiled_after = compiled_after,
  dependencies = setNames(vapply(c("checkmate", "mlr3misc", "mlr3", "paradox", "pkgload"), function(p) as.character(packageVersion(p)), character(1)),
    c("checkmate", "mlr3misc", "mlr3", "paradox", "pkgload")))
cat("Runtime:", mode, "; JIT:", jit, "; R:", R.version.string, "; package:", package_version, "\n")
cat("Bytecode before:", paste(names(compiled_before), compiled_before, collapse = ", "), "\n")
cat("Bytecode after:", paste(names(compiled_after), compiled_after, collapse = ", "), "\n")
saveRDS(list(provenance = provenance, results = results, serialization = serialization_checks, class_loss = class_loss),
  file.path(save_dir, paste0(mode, "_jit", jit, ".rds")))
cat("All runtime assertions completed.\n")
